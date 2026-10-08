#define _GNU_SOURCE
/* Isolated GTK experiment, not a production module. */
#include <emacs-module.h>
#include <gtk/gtk.h>
#include <string.h>
#include <math.h>
#include <gdk/gdkwayland.h>
#include <wayland-client.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdint.h>
#include <stdio.h>
int plugin_is_GPL_compatible;
static GtkWidget *find_fixed(GtkWidget *widget) {
  if (strcmp(G_OBJECT_TYPE_NAME(widget), "EmacsFixed") == 0) return widget;
  if (!GTK_IS_CONTAINER(widget)) return NULL;
  GList *children = gtk_container_get_children(GTK_CONTAINER(widget));
  GtkWidget *result = NULL;
  for (GList *p = children; p && !result; p = p->next) result = find_fixed(p->data);
  g_list_free(children);
  return result;
}
static guint timer_id;
static GtkWidget *drawing;
static unsigned long ticks, submitted, released;
static gint64 phase_start;
static int label_x, label_y, interval_ms = 33;
static void surface_tick(void);
static gboolean native_tick(gpointer data) {
  (void)data;
  ticks++;
  surface_tick();
  if (drawing) gtk_widget_queue_draw(drawing);
  return G_SOURCE_CONTINUE;
}
static gboolean draw_label(GtkWidget *widget, cairo_t *cr, gpointer data) {
  (void)widget; (void)data;
  cairo_set_source_rgb(cr, 1, 1, 1);
  cairo_paint(cr);
  PangoLayout *layout = pango_cairo_create_layout(cr);
  PangoFontDescription *font = pango_font_description_from_string("Aporetic Serif Mono 16");
  pango_layout_set_font_description(layout, font);
  pango_layout_set_text(layout, "Working...", -1);
  double center = 60 - 60 * cos((g_get_monotonic_time()-phase_start) / 1e6 * M_PI / 1.8);
  cairo_pattern_t *gradient = cairo_pattern_create_linear(center-30, 0, center+30, 0);
  cairo_pattern_add_color_stop_rgb(gradient, 0, 0, 0, 0);
  cairo_pattern_add_color_stop_rgb(gradient, .5, .48, .48, .48);
  cairo_pattern_add_color_stop_rgb(gradient, 1, 0, 0, 0);
  cairo_set_source(cr, gradient);
  pango_cairo_show_layout(cr, layout);
  cairo_pattern_destroy(gradient);
  pango_font_description_free(font);
  g_object_unref(layout);
  return TRUE;
}
/* Separate compositor surface: no GTK draw or Emacs redisplay per frame. */
static struct wl_compositor *compositor;
static struct wl_subcompositor *subcompositor;
static struct wl_shm *shm;
static struct wl_surface *surface;
static struct wl_subsurface *subsurface;
static struct wl_display *display;
struct pixel_buffer {struct wl_buffer *buffer; void *pixels; int busy;};
static struct pixel_buffer buffers[3];
enum { WIDTH=400, HEIGHT=64, BYTES=WIDTH*HEIGHT*4 };
static void release_buffer(void *data, struct wl_buffer *buffer) {
  (void)buffer; ((struct pixel_buffer *)data)->busy=0; released++;
}
static const struct wl_buffer_listener release_listener = {release_buffer};
static void registry_global(void *data, struct wl_registry *registry,
                            uint32_t name, const char *interface, uint32_t version) {
  (void)data;
  if (!strcmp(interface,"wl_compositor"))
    compositor=wl_registry_bind(registry,name,&wl_compositor_interface,version<4?version:4);
  else if (!strcmp(interface,"wl_subcompositor"))
    subcompositor=wl_registry_bind(registry,name,&wl_subcompositor_interface,1);
  else if (!strcmp(interface,"wl_shm"))
    shm=wl_registry_bind(registry,name,&wl_shm_interface,1);
}
static void registry_remove(void *data, struct wl_registry *registry, uint32_t name) {
  (void)data; (void)registry; (void)name;
}
static const struct wl_registry_listener registry_listener={registry_global,registry_remove};
static void surface_stop(void) {
  if (subsurface) {wl_subsurface_destroy(subsurface);subsurface=NULL;}
  if (surface) {wl_surface_destroy(surface);surface=NULL;}
  for (int i=0;i<3;i++) {
    if (buffers[i].buffer) wl_buffer_destroy(buffers[i].buffer);
    if (buffers[i].pixels) munmap(buffers[i].pixels,BYTES);
    buffers[i]=(struct pixel_buffer){0};
  }
}
static int surface_start(GtkWidget *fixed) {
  GdkDisplay *gd=gtk_widget_get_display(fixed);
  if (!GDK_IS_WAYLAND_DISPLAY(gd)) return 0;
  display=gdk_wayland_display_get_wl_display(gd);
  if (!compositor) {
    struct wl_registry *registry=wl_display_get_registry(display);
    wl_registry_add_listener(registry,&registry_listener,NULL);
    wl_display_roundtrip(display);
    wl_registry_destroy(registry);
  }
  if (!compositor || !subcompositor || !shm) return 0;
  GtkWidget *top=gtk_widget_get_toplevel(fixed);
  struct wl_surface *parent=gdk_wayland_window_get_wl_surface(gtk_widget_get_window(top));
  if (!parent) return 0;
  surface=wl_compositor_create_surface(compositor);
  subsurface=wl_subcompositor_get_subsurface(subcompositor,surface,parent);
  int x=0,y=0;
  gtk_widget_translate_coordinates(fixed,top,label_x,label_y,&x,&y);
  wl_subsurface_set_position(subsurface,x,y);
  wl_subsurface_set_desync(subsurface);
  struct wl_region *empty=wl_compositor_create_region(compositor);
  wl_surface_set_input_region(surface,empty);
  wl_region_destroy(empty);
  wl_surface_set_buffer_scale(surface,2);
  for (int i=0;i<3;i++) {
    int fd=memfd_create("mevedel-animation-probe",MFD_CLOEXEC);
    if (fd<0) return 0;
    if (ftruncate(fd,BYTES)<0) {close(fd);return 0;}
    buffers[i].pixels=mmap(NULL,BYTES,PROT_READ|PROT_WRITE,MAP_SHARED,fd,0);
    struct wl_shm_pool *pool=wl_shm_create_pool(shm,fd,BYTES);
    buffers[i].buffer=wl_shm_pool_create_buffer(pool,0,WIDTH,HEIGHT,WIDTH*4,WL_SHM_FORMAT_ARGB8888);
    wl_buffer_add_listener(buffers[i].buffer,&release_listener,&buffers[i]);
    wl_shm_pool_destroy(pool);
    close(fd);
  }
  /* Subsurface position becomes effective on the next parent commit. */
  gtk_widget_queue_draw(fixed);
  return 1;
}
static void surface_tick(void) {
  if (!surface) return;
  for (int i=0;i<3;i++) if (!buffers[i].busy) {
    cairo_surface_t *image=cairo_image_surface_create_for_data(buffers[i].pixels,
                                         CAIRO_FORMAT_ARGB32,WIDTH,HEIGHT,WIDTH*4);
    cairo_t *cr=cairo_create(image);
    cairo_scale(cr,2,2);
    draw_label(NULL,cr,NULL);
    cairo_destroy(cr);
    cairo_surface_destroy(image);
    buffers[i].busy=1;
    wl_surface_attach(surface,buffers[i].buffer,0,0);
    wl_surface_damage(surface,0,0,WIDTH/2,HEIGHT/2);
    wl_surface_commit(surface);
    submitted++;
    wl_display_flush(display);
    break;
  }
}
static emacs_value set_mode(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  (void)nargs; (void)data;
  int mode = env->extract_integer(env, args[0]);
  label_x = env->extract_integer(env, args[1]);
  label_y = env->extract_integer(env, args[2]);
  interval_ms = env->extract_integer(env, args[3]);
  if (timer_id) {g_source_remove(timer_id); timer_id=0;}
  if (drawing) {gtk_widget_destroy(drawing); drawing=NULL;}
  ticks=0; submitted=0; released=0; phase_start=g_get_monotonic_time();
  surface_stop();
  GList *windows = gtk_window_list_toplevels();
  for (GList *p = windows; p; p = p->next) {
    GtkWidget *fixed = find_fixed(p->data);
    if (!fixed) continue;
    gtk_widget_set_double_buffered(fixed, mode != 1);
    gtk_widget_set_app_paintable(fixed, mode == 2);
    if (mode == 5 && !surface_start(fixed)) fprintf(stderr,"Native surface unavailable\n");
    if (mode == 4) {
      drawing = gtk_drawing_area_new();
      gtk_widget_set_size_request(drawing, 200, 32);
      gtk_fixed_put(GTK_FIXED(fixed), drawing, label_x, label_y);
      g_signal_connect(drawing, "draw", G_CALLBACK(draw_label), NULL);
      gtk_widget_show(drawing);
    }
  }
  g_list_free(windows);
  if (mode == 3 || mode == 4 || mode == 5) timer_id=g_timeout_add(interval_ms, native_tick, NULL);
  return env->intern(env, "t");
}
static emacs_value count_ticks(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  (void)nargs; (void)args; (void)data;
  return env->make_integer(env, ticks);
}
static emacs_value count_frames(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  (void)nargs; (void)args; (void)data;
  emacs_value values[]={env->make_integer(env,submitted),env->make_integer(env,released)};
  return env->funcall(env,env->intern(env,"list"),2,values);
}
int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env = runtime->get_environment(runtime);
  emacs_value args[] = {env->intern(env, "lab-gtk-mode"),
    env->make_function(env, 4, 4, set_mode, "Set diagnostic GTK mode.", NULL)};
  env->funcall(env, env->intern(env, "fset"), 2, args);
  emacs_value count_args[] = {env->intern(env, "lab-native-count"),
    env->make_function(env, 0, 0, count_ticks, "Count native timer callbacks.", NULL)};
  env->funcall(env, env->intern(env, "fset"), 2, count_args);
  emacs_value frame_args[] = {env->intern(env, "lab-native-frames"),
    env->make_function(env, 0, 0, count_frames, "Count submitted and released buffers.", NULL)};
  env->funcall(env, env->intern(env, "fset"), 2, frame_args);
  return 0;
}
