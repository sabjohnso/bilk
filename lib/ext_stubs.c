/* ext_stubs.c — C stubs for dynamic extension loading (dlopen/dlsym).

   Wraps POSIX dynamic linking functions for use from OCaml.
   Used by Extension.load_c to load C extensions at runtime. */

#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/fail.h>
#include <dlfcn.h>
#include <string.h>
#include <stdint.h>

/* wile_ext_dlopen(path : string) -> nativeint handle */
CAMLprim value wile_ext_dlopen(value path_v)
{
    CAMLparam1(path_v);
    const char *path = String_val(path_v);
    void *handle = dlopen(path, RTLD_NOW | RTLD_LOCAL);
    if (!handle) {
        caml_failwith(dlerror());
    }
    CAMLreturn(caml_copy_nativeint((intptr_t)handle));
}

/* wile_ext_dlsym(handle : nativeint, name : string) -> nativeint fn_ptr */
CAMLprim value wile_ext_dlsym(value handle_v, value name_v)
{
    CAMLparam2(handle_v, name_v);
    void *handle = (void *)Nativeint_val(handle_v);
    const char *name = String_val(name_v);
    dlerror();  /* clear any existing error */
    void *sym = dlsym(handle, name);
    char *err = dlerror();
    if (err) {
        caml_failwith(err);
    }
    CAMLreturn(caml_copy_nativeint((intptr_t)sym));
}

/* wile_ext_dlclose(handle : nativeint) -> unit */
CAMLprim value wile_ext_dlclose(value handle_v)
{
    CAMLparam1(handle_v);
    void *handle = (void *)Nativeint_val(handle_v);
    dlclose(handle);
    CAMLreturn(Val_unit);
}

/* wile_ext_call_init(fn_ptr : nativeint, inst_handle : int) -> unit
   Calls: void wile_ext_init(int32_t inst) */
CAMLprim value wile_ext_call_init(value fn_ptr_v, value inst_handle_v)
{
    CAMLparam2(fn_ptr_v, inst_handle_v);
    typedef void (*init_fn_t)(int32_t);
    init_fn_t fn = (init_fn_t)(intptr_t)Nativeint_val(fn_ptr_v);
    int32_t ih = (int32_t)Int_val(inst_handle_v);
    fn(ih);
    CAMLreturn(Val_unit);
}
