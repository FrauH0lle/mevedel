/* Disposable-editor fault injection for native pixel allocation cleanup. */
#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int memfd_create(const char *name, unsigned int flags) {
  const char *path=getenv("MEVEDEL_LAB_FAILURE");
  if (path && !strcmp(name,"mevedel-animation")) {
    FILE *file=fopen(path,"r+");
    if (file) {
      int remaining=0;
      if (fscanf(file,"%d",&remaining)==1 && remaining>0) {
        rewind(file);fprintf(file,"%d",remaining-1);fclose(file);
        if (remaining==1) {errno=EMFILE;return -1;}
      } else fclose(file);
    }
  }
  int (*original)(const char *,unsigned int)=dlsym(RTLD_NEXT,"memfd_create");
  return original(name,flags);
}
