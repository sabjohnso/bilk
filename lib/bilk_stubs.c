#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

/* ---- Primitive dispatch table ---- */

typedef int32_t bilk_val_t;
typedef int32_t bilk_inst_t;

typedef bilk_val_t (*bilk_cfunc_t)(bilk_inst_t inst, int argc,
                                    const bilk_val_t *argv, void *data);

#define MAX_PRIMS 1024

static struct {
    bilk_cfunc_t fn;
    void *data;
} prims[MAX_PRIMS];

static int next_prim_id = 0;

/* ---- OCaml-callable: dispatch a C primitive ---- */

CAMLprim value bilk_c_dispatch_primitive(value id_v, value ih_v, value args_v) {
    CAMLparam3(id_v, ih_v, args_v);
    int id = Int_val(id_v);
    int ih = Int_val(ih_v);
    int argc = Wosize_val(args_v);

    if (id < 0 || id >= MAX_PRIMS || prims[id].fn == NULL) {
        CAMLreturn(Val_int(0));
    }

    bilk_val_t *argv = NULL;
    if (argc > 0) {
        argv = (bilk_val_t *)malloc(argc * sizeof(bilk_val_t));
        for (int i = 0; i < argc; i++) {
            argv[i] = (bilk_val_t)Int_val(Field(args_v, i));
        }
    }

    bilk_val_t result = prims[id].fn((bilk_inst_t)ih, argc, argv, prims[id].data);

    if (argv) free(argv);

    CAMLreturn(Val_int(result));
}

/* ---- Lazy callback caching ---- */

static int callbacks_cached = 0;

static const value *cb_create;
static const value *cb_destroy;
static const value *cb_error_message;
static const value *cb_eval_string;
static const value *cb_load_file;
static const value *cb_load_fasl;
static const value *cb_lookup;
static const value *cb_call;
static const value *cb_define_primitive;
static const value *cb_make_nil;
static const value *cb_make_void;
static const value *cb_make_bool;
static const value *cb_make_fixnum;
static const value *cb_make_flonum;
static const value *cb_make_string;
static const value *cb_make_symbol;
static const value *cb_make_cons;
static const value *cb_make_vector;
static const value *cb_make_list;
static const value *cb_is_nil;
static const value *cb_is_bool;
static const value *cb_is_fixnum;
static const value *cb_is_integer;
static const value *cb_is_flonum;
static const value *cb_is_string;
static const value *cb_is_symbol;
static const value *cb_is_pair;
static const value *cb_is_vector;
static const value *cb_is_true;
static const value *cb_get_bool;
static const value *cb_get_fixnum;
static const value *cb_get_integer_string;
static const value *cb_get_flonum;
static const value *cb_get_string;
static const value *cb_get_symbol_name;
static const value *cb_car;
static const value *cb_cdr;
static const value *cb_vector_length;
static const value *cb_vector_ref;
static const value *cb_display_string;
static const value *cb_write_string;
static const value *cb_release;

static void ensure_cached(void) {
    if (callbacks_cached) return;
    cb_create = caml_named_value("bilk_create_instance");
    cb_destroy = caml_named_value("bilk_destroy_instance");
    cb_error_message = caml_named_value("bilk_error_message");
    cb_eval_string = caml_named_value("bilk_eval_string");
    cb_load_file = caml_named_value("bilk_load_file");
    cb_load_fasl = caml_named_value("bilk_load_fasl");
    cb_lookup = caml_named_value("bilk_lookup");
    cb_call = caml_named_value("bilk_call");
    cb_define_primitive = caml_named_value("bilk_define_primitive");
    cb_make_nil = caml_named_value("bilk_make_nil");
    cb_make_void = caml_named_value("bilk_make_void");
    cb_make_bool = caml_named_value("bilk_make_bool");
    cb_make_fixnum = caml_named_value("bilk_make_fixnum");
    cb_make_flonum = caml_named_value("bilk_make_flonum");
    cb_make_string = caml_named_value("bilk_make_string");
    cb_make_symbol = caml_named_value("bilk_make_symbol");
    cb_make_cons = caml_named_value("bilk_make_cons");
    cb_make_vector = caml_named_value("bilk_make_vector");
    cb_make_list = caml_named_value("bilk_make_list");
    cb_is_nil = caml_named_value("bilk_is_nil");
    cb_is_bool = caml_named_value("bilk_is_bool");
    cb_is_fixnum = caml_named_value("bilk_is_fixnum");
    cb_is_integer = caml_named_value("bilk_is_integer");
    cb_is_flonum = caml_named_value("bilk_is_flonum");
    cb_is_string = caml_named_value("bilk_is_string");
    cb_is_symbol = caml_named_value("bilk_is_symbol");
    cb_is_pair = caml_named_value("bilk_is_pair");
    cb_is_vector = caml_named_value("bilk_is_vector");
    cb_is_true = caml_named_value("bilk_is_true");
    cb_get_bool = caml_named_value("bilk_get_bool");
    cb_get_fixnum = caml_named_value("bilk_get_fixnum");
    cb_get_integer_string = caml_named_value("bilk_get_integer_string");
    cb_get_flonum = caml_named_value("bilk_get_flonum");
    cb_get_string = caml_named_value("bilk_get_string");
    cb_get_symbol_name = caml_named_value("bilk_get_symbol_name");
    cb_car = caml_named_value("bilk_car");
    cb_cdr = caml_named_value("bilk_cdr");
    cb_vector_length = caml_named_value("bilk_vector_length");
    cb_vector_ref = caml_named_value("bilk_vector_ref");
    cb_display_string = caml_named_value("bilk_display_string");
    cb_write_string = caml_named_value("bilk_write_string");
    cb_release = caml_named_value("bilk_release");
    callbacks_cached = 1;
}

/* ---- C API functions ---- */

int bilk_init(int *argc, char ***argv) {
    (void)argc; (void)argv;
    /* caml_startup is called by the OCaml runtime when linked as a library.
       For standalone C programs, they must call caml_startup themselves
       before bilk_init. This function just ensures callbacks are cached. */
    ensure_cached();
    return 1;
}

bilk_inst_t bilk_create(void) {
    ensure_cached();
    value r = caml_callback(*cb_create, Val_unit);
    return (bilk_inst_t)Int_val(r);
}

void bilk_destroy(bilk_inst_t inst) {
    ensure_cached();
    caml_callback(*cb_destroy, Val_int(inst));
}

const char *bilk_error_message(bilk_inst_t inst) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback(*cb_error_message, Val_int(inst));
    /* Return pointer into OCaml string — valid until next GC.
       For a real production API this should be copied, but for our
       use case the caller reads it immediately. */
    CAMLreturnT(const char *, String_val(r));
}

bilk_val_t bilk_eval_string(bilk_inst_t inst, const char *src) {
    CAMLparam0(); CAMLlocal1(s);
    ensure_cached();
    s = caml_copy_string(src);
    value r = caml_callback2(*cb_eval_string, Val_int(inst), s);
    CAMLreturnT(bilk_val_t, (bilk_val_t)Int_val(r));
}

int bilk_load_file(bilk_inst_t inst, const char *path) {
    CAMLparam0(); CAMLlocal1(s);
    ensure_cached();
    s = caml_copy_string(path);
    value r = caml_callback2(*cb_load_file, Val_int(inst), s);
    CAMLreturnT(int, Int_val(r));
}

int bilk_load_fasl(bilk_inst_t inst, const char *path) {
    CAMLparam0(); CAMLlocal1(s);
    ensure_cached();
    s = caml_copy_string(path);
    value r = caml_callback2(*cb_load_fasl, Val_int(inst), s);
    CAMLreturnT(int, Int_val(r));
}

bilk_val_t bilk_lookup(bilk_inst_t inst, const char *name) {
    CAMLparam0(); CAMLlocal1(s);
    ensure_cached();
    s = caml_copy_string(name);
    value r = caml_callback2(*cb_lookup, Val_int(inst), s);
    CAMLreturnT(bilk_val_t, (bilk_val_t)Int_val(r));
}

bilk_val_t bilk_call(bilk_inst_t inst, bilk_val_t proc,
                      int argc, const bilk_val_t *argv) {
    CAMLparam0(); CAMLlocal1(arr);
    ensure_cached();
    arr = caml_alloc(argc, 0);
    for (int i = 0; i < argc; i++) {
        Store_field(arr, i, Val_int(argv[i]));
    }
    value r = caml_callback3(*cb_call, Val_int(inst), Val_int(proc), arr);
    CAMLreturnT(bilk_val_t, (bilk_val_t)Int_val(r));
}

void bilk_define_primitive(bilk_inst_t inst, const char *name,
                            bilk_cfunc_t fn, void *data) {
    CAMLparam0(); CAMLlocal1(s);
    ensure_cached();
    if (next_prim_id >= MAX_PRIMS) {
        CAMLreturn0;
    }
    int id = next_prim_id++;
    prims[id].fn = fn;
    prims[id].data = data;
    s = caml_copy_string(name);
    caml_callback3(*cb_define_primitive, Val_int(inst), s, Val_int(id));
    CAMLreturn0;
}

/* ---- Constructors ---- */

bilk_val_t bilk_nil(bilk_inst_t inst) {
    ensure_cached();
    value r = caml_callback(*cb_make_nil, Val_int(inst));
    return (bilk_val_t)Int_val(r);
}

bilk_val_t bilk_void(bilk_inst_t inst) {
    ensure_cached();
    value r = caml_callback(*cb_make_void, Val_int(inst));
    return (bilk_val_t)Int_val(r);
}

bilk_val_t bilk_bool(bilk_inst_t inst, int b) {
    ensure_cached();
    value r = caml_callback2(*cb_make_bool, Val_int(inst), Val_int(b));
    return (bilk_val_t)Int_val(r);
}

bilk_val_t bilk_fixnum(bilk_inst_t inst, long n) {
    ensure_cached();
    value r = caml_callback2(*cb_make_fixnum, Val_int(inst), Val_int((int)n));
    return (bilk_val_t)Int_val(r);
}

bilk_val_t bilk_flonum(bilk_inst_t inst, double d) {
    CAMLparam0(); CAMLlocal1(f);
    ensure_cached();
    f = caml_copy_double(d);
    value r = caml_callback2(*cb_make_flonum, Val_int(inst), f);
    CAMLreturnT(bilk_val_t, (bilk_val_t)Int_val(r));
}

bilk_val_t bilk_string(bilk_inst_t inst, const char *s) {
    CAMLparam0(); CAMLlocal1(str);
    ensure_cached();
    str = caml_copy_string(s);
    value r = caml_callback2(*cb_make_string, Val_int(inst), str);
    CAMLreturnT(bilk_val_t, (bilk_val_t)Int_val(r));
}

bilk_val_t bilk_symbol(bilk_inst_t inst, const char *name) {
    CAMLparam0(); CAMLlocal1(s);
    ensure_cached();
    s = caml_copy_string(name);
    value r = caml_callback2(*cb_make_symbol, Val_int(inst), s);
    CAMLreturnT(bilk_val_t, (bilk_val_t)Int_val(r));
}

bilk_val_t bilk_cons(bilk_inst_t inst, bilk_val_t car, bilk_val_t cdr) {
    ensure_cached();
    value r = caml_callback3(*cb_make_cons, Val_int(inst),
                              Val_int(car), Val_int(cdr));
    return (bilk_val_t)Int_val(r);
}

bilk_val_t bilk_vector(bilk_inst_t inst, int len, const bilk_val_t *e) {
    CAMLparam0(); CAMLlocal1(arr);
    ensure_cached();
    arr = caml_alloc(len, 0);
    for (int i = 0; i < len; i++) {
        Store_field(arr, i, Val_int(e[i]));
    }
    value r = caml_callback2(*cb_make_vector, Val_int(inst), arr);
    CAMLreturnT(bilk_val_t, (bilk_val_t)Int_val(r));
}

bilk_val_t bilk_list(bilk_inst_t inst, int len, const bilk_val_t *e) {
    CAMLparam0(); CAMLlocal1(arr);
    ensure_cached();
    arr = caml_alloc(len, 0);
    for (int i = 0; i < len; i++) {
        Store_field(arr, i, Val_int(e[i]));
    }
    value r = caml_callback2(*cb_make_list, Val_int(inst), arr);
    CAMLreturnT(bilk_val_t, (bilk_val_t)Int_val(r));
}

/* ---- Predicates ---- */

int bilk_is_nil(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_nil, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int bilk_is_bool(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_bool, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int bilk_is_fixnum(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_fixnum, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int bilk_is_integer(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_integer, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int bilk_is_flonum(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_flonum, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int bilk_is_string(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_string, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int bilk_is_symbol(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_symbol, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int bilk_is_pair(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_pair, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int bilk_is_vector(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_vector, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int bilk_is_true(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_true, Val_int(inst), Val_int(v));
    return Int_val(r);
}

/* ---- Extractors ---- */

int bilk_bool_value(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_get_bool, Val_int(inst), Val_int(v));
    return Int_val(r);
}

long bilk_fixnum_value(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_get_fixnum, Val_int(inst), Val_int(v));
    return (long)Int_val(r);
}

char *bilk_integer_string(bilk_inst_t inst, bilk_val_t v) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback2(*cb_get_integer_string, Val_int(inst), Val_int(v));
    char *s = strdup(String_val(r));
    CAMLreturnT(char *, s);
}

double bilk_flonum_value(bilk_inst_t inst, bilk_val_t v) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback2(*cb_get_flonum, Val_int(inst), Val_int(v));
    CAMLreturnT(double, Double_val(r));
}

char *bilk_string_value(bilk_inst_t inst, bilk_val_t v) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback2(*cb_get_string, Val_int(inst), Val_int(v));
    char *s = strdup(String_val(r));
    CAMLreturnT(char *, s);
}

char *bilk_symbol_name(bilk_inst_t inst, bilk_val_t v) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback2(*cb_get_symbol_name, Val_int(inst), Val_int(v));
    char *s = strdup(String_val(r));
    CAMLreturnT(char *, s);
}

bilk_val_t bilk_car(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_car, Val_int(inst), Val_int(v));
    return (bilk_val_t)Int_val(r);
}

bilk_val_t bilk_cdr(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_cdr, Val_int(inst), Val_int(v));
    return (bilk_val_t)Int_val(r);
}

int bilk_vector_length(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_vector_length, Val_int(inst), Val_int(v));
    return Int_val(r);
}

bilk_val_t bilk_vector_ref(bilk_inst_t inst, bilk_val_t v, int i) {
    ensure_cached();
    value r = caml_callback3(*cb_vector_ref, Val_int(inst),
                              Val_int(v), Val_int(i));
    return (bilk_val_t)Int_val(r);
}

/* ---- Display/Write ---- */

char *bilk_display_string(bilk_inst_t inst, bilk_val_t v) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback2(*cb_display_string, Val_int(inst), Val_int(v));
    char *s = strdup(String_val(r));
    CAMLreturnT(char *, s);
}

char *bilk_write_string(bilk_inst_t inst, bilk_val_t v) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback2(*cb_write_string, Val_int(inst), Val_int(v));
    char *s = strdup(String_val(r));
    CAMLreturnT(char *, s);
}

/* ---- Memory ---- */

void bilk_release(bilk_inst_t inst, bilk_val_t v) {
    ensure_cached();
    caml_callback2(*cb_release, Val_int(inst), Val_int(v));
}

/* ---- Environment ---- */

CAMLprim value bilk_unsetenv(value name_v) {
    CAMLparam1(name_v);
    unsetenv(String_val(name_v));
    CAMLreturn(Val_unit);
}
