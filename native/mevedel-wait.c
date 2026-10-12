/* Timed Lisp condition waits without event dispatch.

   sleep-for also dispatches process sentinels in its calling thread.
   condition-wait releases the interpreter lock without dispatching events,
   but has no timeout.  A one-shot, thread-local deadline adapts only that
   wait to pthread_cond_timedwait.  Emacs retains ownership of stack rooting,
   lock restoration, thread signals and nonlocal exits.  Ordinary condition
   waits in Emacs and every library retain their existing behavior. */
#define _GNU_SOURCE
#include <emacs-module.h>
#include <dlfcn.h>
#include <pthread.h>
#include <time.h>
#include <errno.h>
#include <math.h>

int mevedel_native_redirect(const char *name, void *replacement);

static int (*real_wait)(pthread_cond_t *,pthread_mutex_t *);
static _Thread_local const struct timespec *deadline;
static int ready;
static emacs_value condition_wait;

static int timed_wait(pthread_cond_t *condition,pthread_mutex_t *mutex) {
  if (!deadline) return real_wait(condition,mutex);
  struct timespec until=*deadline;
  /* Reacquiring a contended Lisp mutex may wait again; only the condition
     wait itself receives the deadline. */
  deadline=NULL;
  int result=pthread_cond_timedwait(condition,mutex,&until);
  /* Emacs permits spurious wakeups but requires a successful POSIX wait. */
  return result==ETIMEDOUT?0:result;
}

static emacs_value available(emacs_env *env,ptrdiff_t nargs,emacs_value *args,void *data) {
  (void)nargs;(void)args;(void)data;
  if (!ready) {
    emacs_value symbol=env->intern(env,"condition-wait");
    emacs_value primitive=env->funcall(env,env->intern(env,"symbol-function"),1,&symbol);
    emacs_value builtin=env->funcall(env,env->intern(env,"subrp"),1,&primitive);
    /* Refuse an already advised/replaced Lisp entry.  Later advice cannot
       run between arming the deadline and the captured primitive. */
    if (!env->is_not_nil(env,builtin)) {ready=-1;return env->intern(env,"nil");}
    condition_wait=env->make_global_ref(env,primitive);
    real_wait=dlsym(RTLD_DEFAULT,"pthread_cond_wait");
    ready=real_wait && mevedel_native_redirect("pthread_cond_wait",timed_wait)>0?1:-1;
  }
  return env->intern(env,ready==1?"t":"nil");
}

static emacs_value wait_for(emacs_env *env,ptrdiff_t nargs,emacs_value *args,void *data) {
  (void)nargs;(void)data;
  double seconds=env->extract_float(env,args[1]);
  if (env->non_local_exit_check(env)!=emacs_funcall_exit_return) return env->intern(env,"nil");
  if (ready!=1 || !isfinite(seconds) || seconds<0 || seconds>3600) {
    emacs_value message=env->make_string(env,"Timed wait unavailable or invalid delay",38);
    env->non_local_exit_signal(env,env->intern(env,"error"),
                              env->funcall(env,env->intern(env,"list"),1,&message));
    return env->intern(env,"nil");
  }
  struct timespec until;
  clock_gettime(CLOCK_REALTIME,&until);
  until.tv_sec+=(time_t)seconds;
  until.tv_nsec+=(long)((seconds-floor(seconds))*1e9);
  if (until.tv_nsec>=1000000000) {until.tv_nsec-=1000000000;until.tv_sec++;}
  deadline=&until;
  emacs_value result=env->funcall(env,condition_wait,1,args);
  /* condition-wait can signal before reaching pthread_cond_wait. */
  deadline=NULL;
  return result;
}

void mevedel_wait_init(emacs_env *env) {
  emacs_value available_args[]={
    env->intern(env,"mevedel-native--timed-wait-available-p"),
    env->make_function(env,0,0,available,"Enable safe timed condition waits; return non-nil when supported.",NULL)};
  env->funcall(env,env->intern(env,"fset"),2,available_args);
  /* funcall performs GC before entering a primitive.  A GC hook/finalizer
     could wait on an unrelated condition and steal the one-shot deadline.
     Keep the guard inside this module's public function, before the native
     function arms it.  Emacs restores each thread's dynamic bindings when
     switching, so another thread can collect while this one waits. */
  const char wrapper[]=
    "(lambda (condition seconds) "
    "  \"Wait on CONDITION for at most SECONDS (a float, 0 through 3600).\\n"
    "Hold its mutex as for `condition-wait'.  Notifications, thread signals\\n"
    "and spurious wakeups may return earlier.  Dispatch no editor events.\" "
    "  (let ((gc-cons-threshold most-positive-fixnum) "
    "        (debug-on-next-call nil) (inhibit-debugger t) (inhibit-quit t)) "
    "    (funcall primitive condition seconds)))";
  emacs_value source=env->make_string(env,wrapper,sizeof(wrapper)-1);
  emacs_value form=env->funcall(env,env->intern(env,"read"),1,&source);
  emacs_value binding_args[]={env->intern(env,"primitive"),
    env->make_function(env,2,2,wait_for,NULL,NULL)};
  emacs_value binding=env->funcall(env,env->intern(env,"cons"),2,binding_args);
  emacs_value lexical=env->funcall(env,env->intern(env,"list"),1,&binding);
  emacs_value eval_args[]={form,lexical};
  emacs_value wait_args[]={env->intern(env,"mevedel-native--condition-wait"),
    env->funcall(env,env->intern(env,"eval"),2,eval_args)};
  env->funcall(env,env->intern(env,"fset"),2,wait_args);
}
