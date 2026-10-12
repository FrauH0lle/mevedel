/* Module entry: each source registers its own Lisp functions. */
#define _GNU_SOURCE
#include <emacs-module.h>
#include <link.h>
#include <elf.h>
#include <sys/mman.h>
#include <unistd.h>
#include <stdint.h>
#include <string.h>

/* The executable's dynamic symbols and relocations. */
static struct {
  int found;
  uintptr_t base, relro_start, relro_end;
  const ElfW(Sym) *symtab;
  const char *strtab;
  const ElfW(Rela) *rela[2];
  size_t relasz[2];
} exe;
static int find_executable(struct dl_phdr_info *info, size_t size, void *data) {
  (void)size;(void)data;
  /* The executable comes first. */
  exe.found=1;exe.base=info->dlpi_addr;
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
int mevedel_native_redirect(const char *name, void *replacement) {
  if (!exe.found) dl_iterate_phdr(find_executable,NULL);
  if (!exe.symtab || !exe.strtab) return 0;
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


int plugin_is_GPL_compatible;

void mevedel_view_native_init(emacs_env *env);
void mevedel_present_init(emacs_env *env);
void mevedel_wait_init(emacs_env *env);

int emacs_module_init(struct emacs_runtime *runtime) {
  if (runtime->size<(ptrdiff_t)sizeof(*runtime)) return 1;
  emacs_env *env=runtime->get_environment(runtime);
  if (env->size<(ptrdiff_t)sizeof(*env)) return 2;
  mevedel_view_native_init(env);
  mevedel_present_init(env);
  mevedel_wait_init(env);
  return env->non_local_exit_check(env)!=emacs_funcall_exit_return?3:0;
}
