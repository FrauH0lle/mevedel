/* Cheaper PGTK presentation without rebuilding Emacs.

   Stock PGTK queues a redraw of the whole frame after every redisplay, and
   GTK 3 then copies the whole window into a new shared-memory buffer while
   the compositor still holds the last one.  This makes toplevels paint
   through GL and turns Emacs's whole-frame redraws into redraws of what its
   own drawing touched.  GL is current on one thread at a time: Emacs's
   GLib dispatch runs on the main thread alone and releases the context for
   threads that run GTK's loop themselves.

   The last two redirect GOT entries of the Emacs executable: only calls made
   by Emacs itself change, never those of GTK, Cairo or this module.  Nothing
   is redirected unless every import is accounted for. */
#define _GNU_SOURCE
#include <emacs-module.h>
#include <gtk/gtk.h>
#include <gdk/gdkwayland.h>
#include <link.h>
#include <elf.h>
#include <dlfcn.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

/* The executable's dynamic symbols and relocations. */
static struct {
  uintptr_t base, relro_start, relro_end;
  const ElfW(Sym) *symtab;
  const char *strtab;
  const ElfW(Rela) *rela[2];
  size_t relasz[2];
} exe;
static int find_executable(struct dl_phdr_info *info, size_t size, void *data) {
  (void)size;(void)data;
  /* The executable comes first. */
  exe.base=info->dlpi_addr;
  const ElfW(Dyn) *dyn=NULL;
  for (int i=0;i<info->dlpi_phnum;i++) {
    const ElfW(Phdr) *ph=&info->dlpi_phdr[i];
    if (ph->p_type==PT_DYNAMIC) dyn=(const void *)(exe.base+ph->p_vaddr);
    else if (ph->p_type==PT_GNU_RELRO) {
      exe.relro_start=exe.base+ph->p_vaddr;exe.relro_end=exe.relro_start+ph->p_memsz;
    }
  }
  /* ld.so normally relocates these addresses in place. */
#define ADDRESS(v) ((uintptr_t)(v)<exe.base?exe.base+(uintptr_t)(v):(uintptr_t)(v))
  for (;dyn && dyn->d_tag!=DT_NULL;dyn++)
    switch (dyn->d_tag) {
    case DT_SYMTAB: exe.symtab=(const void *)ADDRESS(dyn->d_un.d_ptr);break;
    case DT_STRTAB: exe.strtab=(const void *)ADDRESS(dyn->d_un.d_ptr);break;
    case DT_RELA: exe.rela[0]=(const void *)ADDRESS(dyn->d_un.d_ptr);break;
    case DT_RELASZ: exe.relasz[0]=dyn->d_un.d_val;break;
    case DT_JMPREL: exe.rela[1]=(const void *)ADDRESS(dyn->d_un.d_ptr);break;
    case DT_PLTRELSZ: exe.relasz[1]=dyn->d_un.d_val;break;
    }
#undef ADDRESS
  return 1;
}
#if defined __x86_64__
#define SLOT_RELOCATION(type) ((type)==R_X86_64_GLOB_DAT || (type)==R_X86_64_JUMP_SLOT)
#elif defined __aarch64__
#define SLOT_RELOCATION(type) ((type)==R_AARCH64_GLOB_DAT || (type)==R_AARCH64_JUMP_SLOT)
#endif
/* Point the executable's GOT slots for NAME at REPLACEMENT, or only count
   them when REPLACEMENT is NULL. */
static int redirect(const char *name, void *replacement) {
  int count=0;
#ifndef SLOT_RELOCATION
  (void)name;(void)replacement;return 0;
#endif
  long page=sysconf(_SC_PAGESIZE);
#ifdef SLOT_RELOCATION
  for (int t=0;t<2;t++)
    for (size_t i=0;exe.rela[t] && i<exe.relasz[t]/sizeof(ElfW(Rela));i++) {
      const ElfW(Rela) *r=&exe.rela[t][i];
      if (!SLOT_RELOCATION(ELF64_R_TYPE(r->r_info))
          || strcmp(exe.strtab+exe.symtab[ELF64_R_SYM(r->r_info)].st_name,name)) continue;
      count++;
      if (!replacement) continue;
      void **slot=(void **)(exe.base+r->r_offset);
      uintptr_t mask=~(uintptr_t)(page-1);
      void *start=(void *)((uintptr_t)slot&mask);
      /* ld.so protects whole pages only: the page RELRO ends in stays
         writable, and may hold data Emacs writes. */
      int relro=(uintptr_t)slot>=(exe.relro_start&mask) && (uintptr_t)slot<(exe.relro_end&mask);
      if (relro && mprotect(start,(size_t)page,PROT_READ|PROT_WRITE)) return -1;
      *slot=replacement;
      if (relro) mprotect(start,(size_t)page,PROT_READ);
    }
#endif
  return count;
}

/* GLib dispatch may paint through GL.  Emacs's own dispatch runs on the
   main thread only; other Lisp threads wait for their own descriptors.  A
   context still current on one thread cannot be made current on another,
   so each dispatch releases it: a Lisp thread showing a frame runs GTK's
   loop itself while it waits for the map event.  Emacs's global lock
   serializes the threads. */
static int main_thread(void) {return gettid()==getpid();}
static gint (*real_query)(GMainContext *,gint,gint *,GPollFD *,gint);
static gboolean (*real_pending)(GMainContext *);
static void (*real_dispatch)(GMainContext *);
static gboolean (*real_main_iteration)(void);
static gint hook_query(GMainContext *c, gint priority, gint *timeout, GPollFD *fds, gint n) {
  if (!main_thread()) {*timeout=-1;return 0;}
  return real_query(c,priority,timeout,fds,n);
}
static gboolean hook_pending(GMainContext *c) {return main_thread() && real_pending(c);}
static void hook_dispatch(GMainContext *c) {
  if (!main_thread()) return;
  real_dispatch(c);
  gdk_gl_context_clear_current();
}
static gboolean hook_main_iteration(void) {
  gboolean result=real_main_iteration();
  gdk_gl_context_clear_current();
  return result;
}

/* An Emacs frame widget and what differs between its surface and the screen. */
struct target {
  GtkWidget *widget;
  cairo_surface_t *shown;	/* The surface its last draw showed, if current. */
  cairo_surface_t *drawn;	/* The surface its running draw shows. */
  cairo_surface_t *newest;	/* The last surface Emacs created for it. */
  cairo_region_t *damage;
  int drawing;
  int copies;			/* Live copies of its shown surface. */
};
enum {MAX_TARGETS=64};
static struct target targets[MAX_TARGETS];
static int ntargets;
static GtkWidget *drawing;
static unsigned long presented, skipped, whole;

static struct target *by_widget(gpointer widget) {
  for (int i=0;i<ntargets;i++) if (targets[i].widget==widget) return &targets[i];
  return NULL;
}
static struct target *by_surface(cairo_surface_t *surface) {
  for (int i=0;i<ntargets;i++) if (targets[i].shown==surface) return &targets[i];
  return NULL;
}
/* Add the user-space rectangle X1 Y1 X2 Y2, within the clip, to the damage
   of the target CR draws on. */
static void damage(cairo_t *cr, double x1, double y1, double x2, double y2) {
  struct target *t=by_surface(cairo_get_target(cr));
  if (!t) return;
  double cx1,cy1,cx2,cy2;
  cairo_clip_extents(cr,&cx1,&cy1,&cx2,&cy2);
  x1=fmax(x1,cx1);y1=fmax(y1,cy1);x2=fmin(x2,cx2);y2=fmin(y2,cy2);
  if (x1>=x2 || y1>=y2) return;
  cairo_user_to_device(cr,&x1,&y1);
  cairo_user_to_device(cr,&x2,&y2);
  int left=(int)floor(fmin(x1,x2)),top=(int)floor(fmin(y1,y2));
  cairo_rectangle_int_t rect={left,top,(int)ceil(fmax(x1,x2))-left,(int)ceil(fmax(y1,y2))-top};
  cairo_region_union_rectangle(t->damage,&rect);
}
static void damage_clip(cairo_t *cr) {damage(cr,-INFINITY,-INFINITY,INFINITY,INFINITY);}

static void (*real_fill)(cairo_t *);
static void (*real_fill_preserve)(cairo_t *);
static void (*real_stroke)(cairo_t *);
static void (*real_paint)(cairo_t *);
static void (*real_mask)(cairo_t *,cairo_pattern_t *);
static void (*real_show_glyphs)(cairo_t *,const cairo_glyph_t *,int);
static cairo_surface_t *(*real_get_target)(cairo_t *);
static void (*real_queue_draw)(GtkWidget *);
static cairo_surface_t *(*real_create_similar_surface)(GdkWindow *,cairo_content_t,int,int);
static cairo_surface_t *(*real_surface_create_similar)(cairo_surface_t *,cairo_content_t,int,int);

static void hook_fill(cairo_t *cr) {
  double x1,y1,x2,y2;
  cairo_fill_extents(cr,&x1,&y1,&x2,&y2);damage(cr,x1,y1,x2,y2);real_fill(cr);
}
static void hook_fill_preserve(cairo_t *cr) {
  double x1,y1,x2,y2;
  cairo_fill_extents(cr,&x1,&y1,&x2,&y2);damage(cr,x1,y1,x2,y2);real_fill_preserve(cr);
}
static void hook_stroke(cairo_t *cr) {
  double x1,y1,x2,y2;
  cairo_stroke_extents(cr,&x1,&y1,&x2,&y2);damage(cr,x1,y1,x2,y2);real_stroke(cr);
}
static void hook_paint(cairo_t *cr) {damage_clip(cr);real_paint(cr);}
static void hook_mask(cairo_t *cr, cairo_pattern_t *pattern) {damage_clip(cr);real_mask(cr,pattern);}
/* Emacs clips every glyph string it draws. */
static void hook_show_glyphs(cairo_t *cr, const cairo_glyph_t *glyphs, int n) {
  damage_clip(cr);real_show_glyphs(cr,glyphs,n);
}
/* Emacs's draw handler asks its context for the surface it shows. */
static cairo_surface_t *hook_get_target(cairo_t *cr) {
  cairo_surface_t *surface=real_get_target(cr);
  struct target *t=drawing?by_widget(drawing):NULL;
  if (t) t->drawn=surface;
  return surface;
}
/* After a resize Emacs draws on a new surface, while the widget shows the
   old one until Emacs flips to it. */
static cairo_surface_t *hook_create_similar_surface(GdkWindow *window, cairo_content_t content,
                                                    int width, int height) {
  gpointer widget=NULL;
  gdk_window_get_user_data(window,&widget);
  struct target *t=widget?by_widget(widget):NULL;
  cairo_surface_t *surface=real_create_similar_surface(window,content,width,height);
  if (t) {t->shown=NULL;t->newest=surface;}
  return surface;
}
/* The visible bell shows a copy of the frame's surface, drawn without
   damage, until it destroys the copy; scrolling copies through one it
   destroys before queueing its draw.  A draw while a copy lives is whole. */
static cairo_user_data_key_t copy_key;
static void copy_destroyed(void *widget) {
  struct target *t=by_widget(widget);
  if (t && t->copies) t->copies--;
}
static cairo_surface_t *hook_surface_create_similar(cairo_surface_t *other, cairo_content_t content,
                                                    int width, int height) {
  cairo_surface_t *surface=real_surface_create_similar(other,content,width,height);
  struct target *t=by_surface(other);
  if (t && !cairo_surface_set_user_data(surface,&copy_key,t->widget,copy_destroyed)) t->copies++;
  return surface;
}
static void hook_queue_draw(GtkWidget *widget) {
  struct target *t=by_widget(widget);
  if (!t || !t->shown || t->copies) {whole++;real_queue_draw(widget);return;}
  if (cairo_region_is_empty(t->damage)) {skipped++;return;}
  presented++;
  gtk_widget_queue_draw_region(widget,t->damage);
  cairo_region_destroy(t->damage);
  t->damage=cairo_region_create();
}

static void paint_with_gl(GtkWidget *widget) {
  GtkWidget *top=gtk_widget_get_toplevel(widget);
  GdkWindow *window=top?gtk_widget_get_window(top):NULL;
  if (!window || g_object_get_data(G_OBJECT(top),"mevedel-present-gl")) return;
  g_object_set_data(G_OBJECT(top),"mevedel-present-gl",GINT_TO_POINTER(1));
  /* GDK paints a window through GL once it has created this context for it,
     and keeps shared memory when it cannot. */
  GdkGLContext *context=gdk_window_create_gl_context(window,NULL);
  if (context) g_object_unref(context);
}
static void forget(gpointer data, GObject *gone) {
  (void)data;
  for (int i=0;i<ntargets;i++)
    if ((GObject *)targets[i].widget==gone) {
      cairo_region_destroy(targets[i].damage);
      targets[i]=targets[--ntargets];
      return;
    }
}
static int emacs_widget(GtkWidget *widget) {return !strcmp(G_OBJECT_TYPE_NAME(widget),"EmacsFixed");}
static gboolean draw_finished(GtkWidget *widget, cairo_t *cr, gpointer data) {
  (void)cr;(void)data;
  struct target *t=by_widget(widget);
  if (t && t->drawing) {
    if (!t->newest) t->newest=t->drawn;
    /* Showing its newest surface, the screen matches it.  Anything else,
       an old surface before the flip or the visible bell, is redrawn whole. */
    cairo_surface_t *shown=t->drawn && t->drawn==t->newest?t->drawn:NULL;
    if (shown!=t->shown) {cairo_region_destroy(t->damage);t->damage=cairo_region_create();}
    t->shown=shown;t->drawing=0;
  }
  if (drawing==widget) drawing=NULL;
  return FALSE;
}
/* Runs before any draw handler.  A child frame's widget draws inside its
   parent's, so each target tracks its own draw. */
static gboolean draw_started(GSignalInvocationHint *hint, guint n, const GValue *params, gpointer data) {
  (void)hint;(void)n;(void)data;
  GtkWidget *widget=g_value_get_object(&params[0]);
  if (!emacs_widget(widget)) return TRUE;
  struct target *t=by_widget(widget);
  if (!t && ntargets<MAX_TARGETS) {
    t=&targets[ntargets++];
    *t=(struct target){widget,NULL,NULL,NULL,cairo_region_create(),0,0};
    g_object_weak_ref(G_OBJECT(widget),forget,NULL);
    /* After Emacs's own handler. */
    g_signal_connect_after(widget,"draw",G_CALLBACK(draw_finished),NULL);
    paint_with_gl(widget);
  }
  if (t) {t->drawn=NULL;t->drawing=1;drawing=widget;}
  return TRUE;
}

/* REQUIRED hooks must find an import; Emacs may skip the others. */
#define HOOKED(name,symbol,required) {symbol,(void **)&real_##name,(void *)hook_##name,required}
static const struct {const char *symbol;void **real;void *hook;int required;} hooks[]={
  HOOKED(queue_draw,"gtk_widget_queue_draw",1),
  HOOKED(create_similar_surface,"gdk_window_create_similar_surface",1),
  HOOKED(get_target,"cairo_get_target",1),
  HOOKED(query,"g_main_context_query",1),
  HOOKED(pending,"g_main_context_pending",1),
  HOOKED(dispatch,"g_main_context_dispatch",1),
  HOOKED(main_iteration,"gtk_main_iteration",0),
  HOOKED(surface_create_similar,"cairo_surface_create_similar",0),
  HOOKED(fill,"cairo_fill",0),
  HOOKED(fill_preserve,"cairo_fill_preserve",0),
  HOOKED(stroke,"cairo_stroke",0),
  HOOKED(paint,"cairo_paint",0),
  HOOKED(mask,"cairo_mask",0),
  HOOKED(show_glyphs,"cairo_show_glyphs",0),
};
/* Drawing calls whose damage is not tracked: an executable using any of
   them could leave stale pixels. */
static const char *untracked[]={
  "cairo_paint_with_alpha","cairo_mask_surface","cairo_stroke_preserve",
  "cairo_show_text","cairo_show_text_glyphs",
  "pango_cairo_show_layout","pango_cairo_show_glyph_string","pango_cairo_show_layout_line",
};
static int enabled;

static emacs_value enable(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  (void)nargs;(void)args;(void)data;
  const char *reason=NULL;
  char buffer[160];
  if (enabled) return env->intern(env,"t");
  GdkDisplay *display=gdk_display_get_default();
  if (!display || !GDK_IS_WAYLAND_DISPLAY(display)) reason="not a Wayland display";
#ifndef SLOT_RELOCATION
  reason="unsupported architecture";
#endif
  if (!reason) {
    dl_iterate_phdr(find_executable,NULL);
    if (!exe.symtab || !exe.strtab) reason="Emacs has no dynamic symbols";
  }
  for (size_t i=0;!reason && i<sizeof(hooks)/sizeof(*hooks);i++)
    if (hooks[i].required && redirect(hooks[i].symbol,NULL)<1) {
      snprintf(buffer,sizeof buffer,"Emacs does not import %s",hooks[i].symbol);reason=buffer;
    }
  for (size_t i=0;!reason && i<sizeof(untracked)/sizeof(*untracked);i++)
    if (redirect(untracked[i],NULL)>0) {
      snprintf(buffer,sizeof buffer,"Emacs draws with %s",untracked[i]);reason=buffer;
    }
  for (size_t i=0;!reason && i<sizeof(hooks)/sizeof(*hooks);i++)
    if (!(*hooks[i].real=dlsym(RTLD_DEFAULT,hooks[i].symbol))) {
      snprintf(buffer,sizeof buffer,"%s is unavailable",hooks[i].symbol);reason=buffer;
    }
  /* Last to first: redraws are cut to damage only once everything that
     records it is in place, and GL starts with the draw hook, once GLib
     dispatch stays on the main thread and releases its context.  A failure
     on the way leaves only hooks that change nothing. */
  for (size_t i=sizeof(hooks)/sizeof(*hooks);!reason && i-->0;)
    if (redirect(hooks[i].symbol,hooks[i].hook)<0) reason="Emacs's GOT is not writable";
  if (reason) return env->make_string(env,reason,(ptrdiff_t)strlen(reason));
  g_signal_add_emission_hook(g_signal_lookup("draw",GTK_TYPE_WIDGET),0,draw_started,NULL,NULL);
  enabled=1;
  return env->intern(env,"t");
}
static emacs_value statistics(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  (void)nargs;(void)args;(void)data;
  emacs_value values[]={env->make_integer(env,(intmax_t)presented),env->make_integer(env,(intmax_t)skipped),
                        env->make_integer(env,(intmax_t)whole),env->make_integer(env,ntargets)};
  return env->funcall(env,env->intern(env,"vector"),4,values);
}
struct binding {const char *name;ptrdiff_t arity;
                emacs_value (*fn)(emacs_env *,ptrdiff_t,emacs_value *,void *);
                const char *doc;};
/* Spelled out: emacs_function only exists in Emacs 28 and later headers,
   and prebuilt modules use an older header to load into every Emacs. */
static void bind(emacs_env *env, const struct binding *bindings, size_t count) {
  for (size_t i=0;i<count;i++) {
    emacs_value args[]={env->intern(env,bindings[i].name),
                        env->make_function(env,bindings[i].arity,bindings[i].arity,
                                           bindings[i].fn,bindings[i].doc,NULL)};
    env->funcall(env,env->intern(env,"fset"),2,args);
  }
}
void mevedel_present_init(emacs_env *env) {
  static const struct binding bindings[]={
    {"mevedel-present--enable",0,enable,"Present PGTK frames cheaply; t, or why not."},
    {"mevedel-present--stats",0,statistics,"Return partial, skipped and whole redraws, and frame widgets."}
  };
  bind(env,bindings,sizeof(bindings)/sizeof(*bindings));
}
