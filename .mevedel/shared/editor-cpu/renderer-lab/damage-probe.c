/* Diagnostic interposer: run only in disposable editors, never production.
   Deliberately incomplete clipping proves whether whole-window damage dominates. */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
void gtk_widget_queue_draw(GtkWidget *widget) {
  static void (*original)(GtkWidget *);
  static int reported;
  if (!original) original = dlsym(RTLD_NEXT, "gtk_widget_queue_draw");
  const char *kind = G_OBJECT_TYPE_NAME(widget);
  if (!reported++) fprintf(stderr, "damage-probe widget: %s\n", kind);
  const char *mode = getenv("MEVEDEL_DAMAGE_PROBE");
  if (mode && strcmp(kind, "EmacsFixed") == 0) {
    if (strcmp(mode, "skip") == 0) return;
    if (strcmp(mode, "clip") == 0) {
      gtk_widget_queue_draw_area(widget, 0, 0, 600, 150);
      return;
    }
  }
  original(widget);
}
