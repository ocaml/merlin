#include <caml/mlvalues.h>
#include <dlfcn.h>
#include <stdio.h>

CAMLprim value caml_obj_reachable_words(value v);

static void do_root(value v, void *addr)
{
  long size = Long_val(caml_obj_reachable_words(v));
  if (size)
  {
    Dl_info info;
    if (dladdr(addr, &info))
    {
      fprintf(stderr, "%s[%ld] (%p) has size %ld\n",
              info.dli_sname,
              (void **)addr - (void **)info.dli_saddr,
              addr, size);
    }
  }
}

extern value *caml_globals[];

CAMLprim value ml_scan_roots(value unit)
{
  (void)unit;
  value *glob;
  int i, j;
  for (i = 0; caml_globals[i] != 0; i++)
  {
    for (glob = caml_globals[i]; *glob != 0; glob++)
    {
      for (j = 0; j < Wosize_val(*glob); j++)
        do_root(Field(*glob, j), &Field(*glob, j));
    }
  }
  return Val_unit;
}
