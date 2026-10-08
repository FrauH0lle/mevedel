/* Canvas pixel-update experiment for Emacs 32, only for disposable editors. */
#include <emacs-module.h>
#include <cairo.h>
#include <pango/pangocairo.h>
#include <math.h>
int plugin_is_GPL_compatible;
static emacs_value draw(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data) {
  (void)nargs; (void)data;
  uint32_t *pixels=env->canvas_data(env,args[0]);
  double seconds=env->extract_float(env,args[1]);
  if (!pixels) return env->intern(env,"nil");
  cairo_surface_t *surface=cairo_image_surface_create_for_data((unsigned char *)pixels,
                                           CAIRO_FORMAT_ARGB32,400,64,1600);
  cairo_t *cr=cairo_create(surface);
  cairo_scale(cr,2,2);
  cairo_set_source_rgb(cr,1,1,1); cairo_paint(cr);
  PangoLayout *layout=pango_cairo_create_layout(cr);
  PangoFontDescription *font=pango_font_description_from_string("Aporetic Serif Mono 16");
  pango_layout_set_font_description(layout,font);
  pango_layout_set_text(layout,"Working...",-1);
  double center=60-60*cos(seconds*M_PI/1.8);
  cairo_pattern_t *gradient=cairo_pattern_create_linear(center-30,0,center+30,0);
  cairo_pattern_add_color_stop_rgb(gradient,0,0,0,0);
  cairo_pattern_add_color_stop_rgb(gradient,.5,.48,.48,.48);
  cairo_pattern_add_color_stop_rgb(gradient,1,0,0,0);
  cairo_set_source(cr,gradient); pango_cairo_show_layout(cr,layout);
  cairo_pattern_destroy(gradient); pango_font_description_free(font);
  g_object_unref(layout); cairo_destroy(cr); cairo_surface_destroy(surface);
  return env->intern(env,"t");
}
int emacs_module_init(struct emacs_runtime *runtime) {
  emacs_env *env=runtime->get_environment(runtime);
  if (env->size<(ptrdiff_t)sizeof(*env)) return 1;
  emacs_value args[]={env->intern(env,"lab-canvas-draw"),
    env->make_function(env,2,2,draw,"Draw the Canvas label.",NULL)};
  env->funcall(env,env->intern(env,"fset"),2,args);
  return 0;
}
