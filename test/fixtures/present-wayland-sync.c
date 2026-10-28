#include <emacs-module.h>
#include <gtk/gtk.h>
#include <gdk/gdkwayland.h>
#include <errno.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <wayland-client.h>

int plugin_is_GPL_compatible;

static char *copy_string(emacs_env *env, emacs_value value)
{
  ptrdiff_t size = 0;
  if (!env->copy_string_contents(env, value, NULL, &size)
      || size <= 0 || size > 4096)
    return NULL;
  char *string = malloc((size_t) size);
  if (!string || !env->copy_string_contents(env, value, string, &size)
      || strlen(string) + 1 != (size_t) size) {
    free(string);
    return NULL;
  }
  return string;
}

static int safe_runtime(const char *path)
{
  size_t path_length = path ? strlen(path) : 0;
  if (!path || path[0] != '/' || path[1] == '\0'
      || path[path_length - 1] == '/' || strstr(path, "//"))
    return 0;
  for (const char *part = path + 1; *part;) {
    const char *end = strchr(part, '/');
    size_t length = end ? (size_t) (end - part) : strlen(part);
    if ((length == 1 && part[0] == '.')
        || (length == 2 && part[0] == '.' && part[1] == '.'))
      return 0;
    if (!end)
      break;
    part = end + 1;
  }
  return 1;
}

static int safe_target(const char *target)
{
  if (!target || !*target || strcmp(target, ".") == 0
      || strcmp(target, "..") == 0)
    return 0;
  for (const unsigned char *p = (const unsigned char *) target; *p; p++)
    if (!((*p >= 'a' && *p <= 'z') || (*p >= 'A' && *p <= 'Z')
          || (*p >= '0' && *p <= '9') || *p == '_' || *p == '-'
          || *p == '.'))
      return 0;
  return 1;
}

static int peer_matches(GdkDisplay *display, const char *expected)
{
  struct wl_display *wl_display = gdk_wayland_display_get_wl_display(display);
  int fd = wl_display ? wl_display_get_fd(wl_display) : -1;
  struct sockaddr_un address;
  socklen_t length = sizeof(address);
  memset(&address, 0, sizeof(address));
  if (fd < 0 || getpeername(fd, (struct sockaddr *) &address, &length) != 0
      || address.sun_family != AF_UNIX
      || length <= offsetof(struct sockaddr_un, sun_path))
    return 0;

  size_t path_length = (size_t) length - offsetof(struct sockaddr_un, sun_path);
  size_t expected_length = strlen(expected);
  return address.sun_path[0] != '\0'
    && path_length == expected_length + 1
    && address.sun_path[expected_length] == '\0'
    && memcmp(address.sun_path, expected, expected_length) == 0;
}

static GtkWidget *verified_window(emacs_env *env, emacs_value *args,
                                  GdkDisplay **verified_display)
{
  char *frame_id = copy_string(env, args[0]);
  char *target = copy_string(env, args[1]);
  const char *runtime = getenv("XDG_RUNTIME_DIR");
  const char *configured_target = getenv("MEVEDEL_NATIVE_DISPLAY");
  GdkDisplay *display = NULL;
  char expected[sizeof(((struct sockaddr_un *) 0)->sun_path)];
  int safe_paths = safe_runtime(runtime) && safe_target(configured_target)
    && target && strcmp(target, configured_target) == 0;
  int path_length = safe_paths
    ? snprintf(expected, sizeof(expected), "%s/%s", runtime, configured_target) : -1;
  int valid = safe_paths && path_length > 0
    && (size_t) path_length < sizeof(expected);

  GtkWidget *matched_window = NULL;
  unsigned int matches = 0;
  char *end = NULL;
  errno = 0;
  unsigned long long address = frame_id ? strtoull(frame_id, &end, 10) : 0;
  int valid_id = frame_id && !errno && end != frame_id && *end == '\0';
  for (const unsigned char *p = (const unsigned char *) frame_id;
       valid_id && *p; p++)
    valid_id = *p >= '0' && *p <= '9';
  GList *windows = gtk_window_list_toplevels();
  for (GList *item = windows; item; item = item->next) {
    GtkWidget *window = GTK_WIDGET(item->data);
    if (valid_id && (uintptr_t) window == (uintptr_t) address) {
      matched_window = window;
      matches++;
    }
  }
  g_list_free(windows);
  GdkDisplay *frame_display = matched_window
    ? gtk_widget_get_display(matched_window) : NULL;
  GdkWindow *gdk_window = matched_window
    ? gtk_widget_get_window(matched_window) : NULL;
  display = frame_display;
  valid = valid && matches == 1 && display
    && GDK_IS_WAYLAND_DISPLAY(display)
    && gdk_wayland_display_get_wl_display(display) && gdk_window
    && gdk_window_get_display(gdk_window) == display
    && peer_matches(display, expected);

  free(frame_id);
  free(target);
  if (!valid)
    return NULL;
  *verified_display = display;
  return matched_window;
}

static emacs_value mevedel_test_wayland_queue_configure(emacs_env *env,
                                                        ptrdiff_t nargs,
                                                        emacs_value *args,
                                                        void *data)
{
  (void) nargs;
  (void) data;
  GdkDisplay *display = NULL;
  GtkWidget *window = verified_window(env, args, &display);
  if (!window)
    return env->intern(env, "nil");

  gtk_window_maximize(GTK_WINDOW(window));
  gtk_window_unmaximize(GTK_WINDOW(window));
  return env->intern(env, "t");
}

static emacs_value mevedel_test_wayland_sync(emacs_env *env,
                                             ptrdiff_t nargs,
                                             emacs_value *args,
                                             void *data)
{
  (void) nargs;
  (void) data;
  GdkDisplay *display = NULL;
  if (!verified_window(env, args, &display))
    return env->intern(env, "nil");

  gdk_display_sync(display);
  return env->intern(env, "t");
}

int emacs_module_init(struct emacs_runtime *runtime)
{
  emacs_env *env = runtime->get_environment(runtime);
  emacs_value function = env->make_function(env, 2, 2,
                                             mevedel_test_wayland_sync,
                                             "Sync the fixture frame's verified Wayland display.",
                                             NULL);
  emacs_value queue_function = env->make_function(
    env, 2, 2, mevedel_test_wayland_queue_configure,
    "Queue a configure on the fixture's verified Wayland frame.", NULL);
  emacs_value arguments[] = {env->intern(env, "mevedel-test-wayland-sync"),
                             function};
  env->funcall(env, env->intern(env, "fset"), 2, arguments);
  emacs_value queue_arguments[] = {
    env->intern(env, "mevedel-test-wayland-queue-configure"), queue_function};
  env->funcall(env, env->intern(env, "fset"), 2, queue_arguments);
  return env->non_local_exit_check(env) == emacs_funcall_exit_return ? 0 : 1;
}
