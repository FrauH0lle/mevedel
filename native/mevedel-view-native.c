/* Timed text surfaces for PGTK/Wayland.  No Emacs APIs run in GTK callbacks. */
#define _GNU_SOURCE
#include <emacs-module.h>
#include <gtk/gtk.h>
#include <gdk/gdkwayland.h>
#include <wayland-client.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <errno.h>

int plugin_is_GPL_compatible;

struct pixels {
  struct wl_buffer *buffer;
  void *data;
  int busy;
};
struct sample {double at; PangoLayout *layout;};
struct animation {
  int closed, counted, width, height, scale, ascent, x, y, previous;
  size_t bytes;
  double cycle, epoch, displayed;
  GdkRGBA background;
  GtkWidget *parent;
  gulong destroy_hook, unmap_hook, scale_hook, focus_hook;
  struct wl_surface *surface;
  struct wl_subsurface *subsurface;
  struct wl_callback *positioned;
  struct pixels pixels[3];
  struct sample *samples;
  ptrdiff_t count;
  guint timer;
};
static struct wl_display *display;
static struct wl_compositor *compositor;
static struct wl_subcompositor *subcompositor;
static struct wl_shm *shm;
static unsigned long active, submitted, released, dropped;

static int exited(emacs_env *env) {
  return env->non_local_exit_check(env) != emacs_funcall_exit_return;
}
static emacs_value nil(emacs_env *env) {return env->intern(env,"nil");}
static char *string(emacs_env *env, emacs_value value) {
  ptrdiff_t size=0;
  if (!env->copy_string_contents(env,value,NULL,&size) || size<1 || size>262144) return NULL;
  char *result=malloc((size_t)size);
  if (result && !env->copy_string_contents(env,value,result,&size)) {free(result);return NULL;}
  return result;
}
static void registry_add(void *data, struct wl_registry *registry, uint32_t name,
                         const char *interface, uint32_t version) {
  (void)data;
  if (!strcmp(interface,"wl_compositor") && version>=3)
    compositor=wl_registry_bind(registry,name,&wl_compositor_interface,3);
  else if (!strcmp(interface,"wl_subcompositor"))
    subcompositor=wl_registry_bind(registry,name,&wl_subcompositor_interface,1);
  else if (!strcmp(interface,"wl_shm"))
    shm=wl_registry_bind(registry,name,&wl_shm_interface,1);
}
static void registry_remove(void *data, struct wl_registry *registry, uint32_t name) {
  (void)data;(void)registry;(void)name;
}
static const struct wl_registry_listener registry_listener={registry_add,registry_remove};
static int connect_display(GdkDisplay *gdk) {
  if (!GDK_IS_WAYLAND_DISPLAY(gdk)) return 0;
  struct wl_display *candidate=gdk_wayland_display_get_wl_display(gdk);
  if (display) return candidate==display && compositor && subcompositor && shm;
  display=candidate;
  struct wl_registry *registry=wl_display_get_registry(display);
  if (!registry) return 0;
  wl_registry_add_listener(registry,&registry_listener,NULL);
  int status=wl_display_roundtrip(display);
  wl_registry_destroy(registry);
  return status>=0 && compositor && subcompositor && shm;
}
static GtkWidget *find_fixed(GtkWidget *widget) {
  if (!strcmp(G_OBJECT_TYPE_NAME(widget),"EmacsFixed")) return widget;
  if (!GTK_IS_CONTAINER(widget)) return NULL;
  GList *children=gtk_container_get_children(GTK_CONTAINER(widget));
  GtkWidget *result=NULL;
  for (GList *p=children;p && !result;p=p->next) result=find_fixed(p->data);
  g_list_free(children);
  return result;
}
/* An opaque frame ID is compared with live objects, never dereferenced. */
static GtkWidget *find_parent(const char *id) {
  if (!id || !*id || !gdk_display_get_default()) return NULL;
  char *end=NULL;
  errno=0;
  unsigned long long address=strtoull(id,&end,10);
  if (errno || *end) return NULL;
  GtkWidget *found=NULL;
  GList *windows=gtk_window_list_toplevels();
  for (GList *p=windows;p;p=p->next)
    if ((uintptr_t)p->data==address) {found=p->data;break;}
  g_list_free(windows);
  return found;
}
static void release_pixels(void *data, struct wl_buffer *buffer) {
  (void)buffer;((struct pixels *)data)->busy=0;released++;
}
static const struct wl_buffer_listener pixels_listener={release_pixels};
static void close_animation(struct animation *a) {
  if (!a || a->closed) return;
  a->closed=1;
  if (a->timer) {g_source_remove(a->timer);a->timer=0;}
  if (a->parent) {
    if (a->destroy_hook) g_signal_handler_disconnect(a->parent,a->destroy_hook);
    if (a->unmap_hook) g_signal_handler_disconnect(a->parent,a->unmap_hook);
    if (a->scale_hook) g_signal_handler_disconnect(a->parent,a->scale_hook);
    if (a->focus_hook) g_signal_handler_disconnect(a->parent,a->focus_hook);
    a->parent=NULL;
  }
  if (a->positioned) wl_callback_destroy(a->positioned);
  a->positioned=NULL;
  if (a->subsurface) wl_subsurface_destroy(a->subsurface);
  if (a->surface) wl_surface_destroy(a->surface);
  a->subsurface=NULL;a->surface=NULL;
  for (int i=0;i<3;i++) {
    if (a->pixels[i].buffer) wl_buffer_destroy(a->pixels[i].buffer);
    if (a->pixels[i].data) munmap(a->pixels[i].data,a->bytes);
  }
  if (a->samples) {
    for (ptrdiff_t i=0;i<a->count;i++) if (a->samples[i].layout) g_object_unref(a->samples[i].layout);
    free(a->samples);a->samples=NULL;
  }
  if (a->counted) {active--;a->counted=0;}
  if (display) wl_display_flush(display);
}
static void finalize(void *data) {close_animation(data);free(data);}
static void parent_unavailable(GtkWidget *widget, gpointer data) {
  (void)widget;close_animation(data);
}
static void scale_changed(GObject *object, GParamSpec *spec, gpointer data) {
  (void)object;(void)spec;close_animation(data);
}
static void focus_changed(GObject *object, GParamSpec *spec, gpointer data) {
  (void)spec;
  if (!gtk_window_is_active(GTK_WINDOW(object))) close_animation(data);
}
static void position_applied(void *data, struct wl_callback *callback, uint32_t time) {
  (void)time;
  struct animation *a=data;
  wl_callback_destroy(callback);a->positioned=NULL;
  /* The parent has committed the position.  Later samples can run freely. */
  wl_subsurface_set_desync(a->subsurface);
  wl_display_flush(display);
}
static const struct wl_callback_listener position_listener={position_applied};
static int place(struct animation *a, int x, int y) {
  GtkWidget *fixed=find_fixed(a->parent);
  if (!fixed || !gtk_widget_get_mapped(a->parent)) return 0;
  int sx,sy;
  if (!gtk_widget_translate_coordinates(fixed,a->parent,x,y,&sx,&sy)) return 0;
  /* Position belongs to the parent's pending state.  Committing a desynced
     child first would briefly show it at (0, 0), or at its old position. */
  wl_subsurface_set_sync(a->subsurface);
  wl_subsurface_set_position(a->subsurface,sx,sy);
  if (!a->positioned) {
    struct wl_surface *parent=gdk_wayland_window_get_wl_surface(gtk_widget_get_window(a->parent));
    if (!parent) return 0;
    a->positioned=wl_surface_frame(parent);
    if (!a->positioned) return 0;
    wl_callback_add_listener(a->positioned,&position_listener,a);
  }
  a->x=x;a->y=y;
  /* Subsurface positions apply with the next parent commit. */
  gtk_widget_queue_draw(fixed);
  return 1;
}
static gboolean tick(gpointer data) {
  struct animation *a=data;
  a->timer=0;
  if (a->closed) return G_SOURCE_REMOVE;
  if (!gtk_widget_get_mapped(a->parent)) {close_animation(a);return G_SOURCE_REMOVE;}
  double elapsed=g_get_monotonic_time()/1e6-a->epoch;
  double phase=fmod(elapsed,a->cycle);
  if (phase<0) phase+=a->cycle;
  ptrdiff_t index=0;
  while (index+1<a->count && a->samples[index+1].at<=phase) index++;
  if (index!=a->previous) {
    int slot=-1;
    for (int i=0;i<3;i++) if (!a->pixels[i].busy) {slot=i;break;}
    if (slot<0) dropped++;
    else {
      struct pixels *p=&a->pixels[slot];
      cairo_surface_t *image=cairo_image_surface_create_for_data(p->data,CAIRO_FORMAT_ARGB32,
                               a->width*a->scale,a->height*a->scale,a->width*a->scale*4);
      cairo_t *cr=cairo_create(image);
      cairo_scale(cr,a->scale,a->scale);
      cairo_set_operator(cr,CAIRO_OPERATOR_SOURCE);
      gdk_cairo_set_source_rgba(cr,&a->background);cairo_paint(cr);
      cairo_set_operator(cr,CAIRO_OPERATOR_OVER);
      PangoLayout *layout=a->samples[index].layout;
      double baseline=pango_layout_get_baseline(layout)/(double)PANGO_SCALE;
      cairo_move_to(cr,0,a->ascent-baseline);
      pango_cairo_show_layout(cr,layout);
      cairo_status_t status=cairo_status(cr);
      cairo_destroy(cr);cairo_surface_destroy(image);
      if (status!=CAIRO_STATUS_SUCCESS) {close_animation(a);return G_SOURCE_REMOVE;}
      p->busy=1;
      wl_surface_attach(a->surface,p->buffer,0,0);
      wl_surface_damage(a->surface,0,0,a->width,a->height);
      wl_surface_commit(a->surface);wl_display_flush(display);
      a->previous=(int)index;a->displayed=elapsed-phase+a->samples[index].at;submitted++;
    }
  }
  double next=index+1<a->count?a->samples[index+1].at:a->cycle;
  guint delay=(guint)MAX(1,ceil((next-phase)*1000));
  a->timer=g_timeout_add_full(G_PRIORITY_DEFAULT,delay,tick,a,NULL);
  return G_SOURCE_REMOVE;
}
static int allocate_pixels(struct animation *a) {
  for (int i=0;i<3;i++) {
    int fd=memfd_create("mevedel-animation",MFD_CLOEXEC);
    if (fd<0) return 0;
    if (ftruncate(fd,(off_t)a->bytes)<0) {close(fd);return 0;}
    void *pixels=mmap(NULL,a->bytes,PROT_READ|PROT_WRITE,MAP_SHARED,fd,0);
    if (pixels==MAP_FAILED) {close(fd);return 0;}
    a->pixels[i].data=pixels;
    struct wl_shm_pool *pool=wl_shm_create_pool(shm,fd,(int)a->bytes);
    close(fd);
    if (!pool) return 0;
    a->pixels[i].buffer=wl_shm_pool_create_buffer(pool,0,a->width*a->scale,a->height*a->scale,
                                               a->width*a->scale*4,WL_SHM_FORMAT_ARGB8888);
    wl_shm_pool_destroy(pool);
    if (!a->pixels[i].buffer) return 0;
    wl_buffer_add_listener(a->pixels[i].buffer,&pixels_listener,&a->pixels[i]);
  }
  return 1;
}
static emacs_value supported(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  (void)nargs;(void)data;
  char *id=string(env,args[0]);
  GtkWidget *parent=find_parent(id);free(id);
  if (exited(env)) return nil(env);
  return env->intern(env,parent && GDK_IS_WAYLAND_DISPLAY(gtk_widget_get_display(parent))?"t":"nil");
}
/* OPEN(ID, [X Y WIDTH HEIGHT ASCENT], FONT, BACKGROUND,
        [CYCLE [TIME MARKUP] ...], ELAPSED).  Font sizes use px units. */
static emacs_value open_animation(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  (void)nargs;(void)data;
  struct animation *a=calloc(1,sizeof(*a));
  if (!a) return nil(env);
  char *id=NULL,*font=NULL,*background=NULL;
  PangoFontDescription *description=NULL;
  PangoContext *context=NULL;
  id=string(env,args[0]);font=string(env,args[2]);background=string(env,args[3]);
  if (exited(env) || !id || !font || !background) goto fail;
  a->parent=find_parent(id);
  if (!a->parent || !gtk_widget_get_mapped(a->parent)) goto fail;
  if (env->vec_size(env,args[1])!=5 || exited(env)) goto fail;
  int geometry[5];
  for (int i=0;i<5;i++) {
    intmax_t value=env->extract_integer(env,env->vec_get(env,args[1],i));
    if (exited(env) || value<0 || value>65535) goto fail;
    geometry[i]=(int)value;
  }
  a->x=geometry[0];a->y=geometry[1];a->width=geometry[2];a->height=geometry[3];a->ascent=geometry[4];
  a->scale=gtk_widget_get_scale_factor(a->parent);
  if (a->width<1 || a->width>4096 || a->height<1 || a->height>256 ||
      a->ascent>a->height || a->scale<1 || a->scale>4) goto fail;
  a->bytes=(size_t)a->width*a->height*a->scale*a->scale*4;
  if (a->bytes>4*1024*1024) goto fail;
  if (!gdk_rgba_parse(&a->background,background)) goto fail;
  a->count=env->vec_size(env,args[4])-1;
  if (exited(env) || a->count<1 || a->count>512) goto fail;
  a->cycle=env->extract_float(env,env->vec_get(env,args[4],0));
  double elapsed=env->extract_float(env,args[5]);
  if (exited(env) || !isfinite(a->cycle) || a->cycle<=0 || a->cycle>86400 ||
      !isfinite(elapsed) || elapsed<0) goto fail;
  a->epoch=g_get_monotonic_time()/1e6-elapsed;
  a->samples=calloc((size_t)a->count,sizeof(*a->samples));
  if (!a->samples) goto fail;
  description=pango_font_description_from_string(font);
  context=gtk_widget_create_pango_context(a->parent);
  if (!description || !context) goto fail;
  for (ptrdiff_t i=0;i<a->count;i++) {
    emacs_value entry=env->vec_get(env,args[4],i+1);
    if (env->vec_size(env,entry)!=2 || exited(env)) goto fail;
    double at=env->extract_float(env,env->vec_get(env,entry,0));
    if (exited(env) || !isfinite(at) || at<0 || at>=a->cycle ||
        (i==0 && at!=0) || (i>0 && at<=a->samples[i-1].at)) goto fail;
    char *markup=string(env,env->vec_get(env,entry,1));
    if (exited(env) || !markup) {free(markup);goto fail;}
    PangoAttrList *attributes=NULL;
    char *text=NULL;
    gboolean valid=pango_parse_markup(markup,-1,0,&attributes,&text,NULL,NULL);
    free(markup);
    if (!valid) goto fail;
    PangoLayout *layout=pango_layout_new(context);
    pango_layout_set_font_description(layout,description);
    pango_layout_set_text(layout,text,-1);
    pango_layout_set_attributes(layout,attributes);
    pango_attr_list_unref(attributes);g_free(text);
    a->samples[i]=(struct sample){at,layout};
    int width,height;
    pango_layout_get_pixel_size(layout,&width,&height);
    /* Never cover neighboring text when font shaping differs. */
    if (abs(width-a->width)>1 || height>a->height+1) goto fail;
  }
  if (!connect_display(gtk_widget_get_display(a->parent))) goto fail;
  struct wl_surface *parent_surface=gdk_wayland_window_get_wl_surface(gtk_widget_get_window(a->parent));
  if (!parent_surface) goto fail;
  a->surface=wl_compositor_create_surface(compositor);
  if (!a->surface) goto fail;
  a->subsurface=wl_subcompositor_get_subsurface(subcompositor,a->surface,parent_surface);
  if (!a->subsurface) goto fail;
  wl_surface_set_buffer_scale(a->surface,a->scale);
  struct wl_region *empty=wl_compositor_create_region(compositor);
  if (!empty) goto fail;
  wl_surface_set_input_region(a->surface,empty);wl_region_destroy(empty);
  if (!allocate_pixels(a) || !place(a,a->x,a->y)) goto fail;
  a->destroy_hook=g_signal_connect(a->parent,"destroy",G_CALLBACK(parent_unavailable),a);
  a->unmap_hook=g_signal_connect(a->parent,"unmap",G_CALLBACK(parent_unavailable),a);
  a->scale_hook=g_signal_connect(a->parent,"notify::scale-factor",G_CALLBACK(scale_changed),a);
  a->focus_hook=g_signal_connect(a->parent,"notify::is-active",G_CALLBACK(focus_changed),a);
  a->counted=1;active++;a->previous=-1;
  free(id);free(font);free(background);
  pango_font_description_free(description);g_object_unref(context);
  tick(a);
  if (a->closed) {finalize(a);return nil(env);}
  emacs_value result=env->make_user_ptr(env,finalize,a);
  if (exited(env)) finalize(a);
  return result;
fail:
  free(id);free(font);free(background);
  if (description) pango_font_description_free(description);
  if (context) g_object_unref(context);
  finalize(a);
  return nil(env);
}
static struct animation *handle(emacs_env *env, emacs_value value) {
  if (env->get_user_finalizer(env,value)!=finalize || exited(env)) return NULL;
  return env->get_user_ptr(env,value);
}
static emacs_value move_animation(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  (void)nargs;(void)data;
  struct animation *a=handle(env,args[0]);
  intmax_t x=env->extract_integer(env,args[1]), y=env->extract_integer(env,args[2]);
  if (exited(env) || !a || a->closed || x<0 || y<0 || x>65535 || y>65535) return nil(env);
  if (a->x!=(int)x || a->y!=(int)y) if (!place(a,(int)x,(int)y)) return nil(env);
  return env->intern(env,"t");
}
static emacs_value stop_animation(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  (void)nargs;(void)data;
  struct animation *a=handle(env,args[0]);
  if (!exited(env)) close_animation(a);
  return nil(env);
}
static emacs_value sample_seconds(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  (void)nargs;(void)data;
  struct animation *a=handle(env,args[0]);
  return !exited(env) && a?env->make_float(env,a->displayed):nil(env);
}
static emacs_value statistics(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  (void)nargs;(void)args;(void)data;
  emacs_value values[]={env->make_integer(env,active),env->make_integer(env,submitted),
                        env->make_integer(env,released),env->make_integer(env,dropped)};
  return env->funcall(env,env->intern(env,"vector"),4,values);
}
int emacs_module_init(struct emacs_runtime *runtime) {
  if (runtime->size<(ptrdiff_t)sizeof(*runtime)) return 1;
  emacs_env *env=runtime->get_environment(runtime);
  if (env->size<(ptrdiff_t)sizeof(*env)) return 2;
  /* Spelled out: emacs_function only exists in Emacs 28 and later headers,
     and prebuilt modules use an older header to load into every Emacs. */
  struct binding {const char *name;ptrdiff_t arity;
                  emacs_value (*fn)(emacs_env *,ptrdiff_t,emacs_value *,void *);
                  const char *doc;} bindings[]={
    {"mevedel-view-native--supported-p",1,supported,"Whether frame ID has a Wayland parent."},
    {"mevedel-view-native--open",6,open_animation,"Present a timed styled-text sequence."},
    {"mevedel-view-native--move",3,move_animation,"Move a live surface; nil means it was closed."},
    {"mevedel-view-native--close",1,stop_animation,"Close a surface and release its timer and buffers."},
    {"mevedel-view-native--sample",1,sample_seconds,"Return the elapsed time of the last submitted sample."},
    {"mevedel-view-native--stats",0,statistics,"Return active, submitted, released and dropped counts."}
  };
  for (size_t i=0;i<sizeof(bindings)/sizeof(*bindings);i++) {
    struct binding *b=&bindings[i];
    emacs_value args[]={env->intern(env,b->name),env->make_function(env,b->arity,b->arity,b->fn,b->doc,NULL)};
    env->funcall(env,env->intern(env,"fset"),2,args);
  }
  return exited(env)?3:0;
}
