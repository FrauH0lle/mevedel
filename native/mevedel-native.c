/* Module entry: each source registers its own Lisp functions. */
#include <emacs-module.h>

int plugin_is_GPL_compatible;

void mevedel_view_native_init(emacs_env *env);
void mevedel_present_init(emacs_env *env);

int emacs_module_init(struct emacs_runtime *runtime) {
  if (runtime->size<(ptrdiff_t)sizeof(*runtime)) return 1;
  emacs_env *env=runtime->get_environment(runtime);
  if (env->size<(ptrdiff_t)sizeof(*env)) return 2;
  mevedel_view_native_init(env);
  mevedel_present_init(env);
  return env->non_local_exit_check(env)!=emacs_funcall_exit_return?3:0;
}
