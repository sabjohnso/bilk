#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/callback.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

/* ---- Primitive dispatch table ---- */

typedef int32_t wile_val_t;
typedef int32_t wile_inst_t;

typedef wile_val_t (*wile_cfunc_t)(wile_inst_t inst, int argc,
                                    const wile_val_t *argv, void *data);

#define MAX_PRIMS 1024

static struct {
    wile_cfunc_t fn;
    void *data;
} prims[MAX_PRIMS];

static int next_prim_id = 0;

/* ---- OCaml-callable: dispatch a C primitive ---- */

CAMLprim value wile_c_dispatch_primitive(value id_v, value ih_v, value args_v) {
    CAMLparam3(id_v, ih_v, args_v);
    int id = Int_val(id_v);
    int ih = Int_val(ih_v);
    int argc = Wosize_val(args_v);

    if (id < 0 || id >= MAX_PRIMS || prims[id].fn == NULL) {
        CAMLreturn(Val_int(0));
    }

    wile_val_t *argv = NULL;
    if (argc > 0) {
        argv = (wile_val_t *)malloc(argc * sizeof(wile_val_t));
        for (int i = 0; i < argc; i++) {
            argv[i] = (wile_val_t)Int_val(Field(args_v, i));
        }
    }

    wile_val_t result = prims[id].fn((wile_inst_t)ih, argc, argv, prims[id].data);

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
    cb_create = caml_named_value("wile_create_instance");
    cb_destroy = caml_named_value("wile_destroy_instance");
    cb_error_message = caml_named_value("wile_error_message");
    cb_eval_string = caml_named_value("wile_eval_string");
    cb_load_file = caml_named_value("wile_load_file");
    cb_load_fasl = caml_named_value("wile_load_fasl");
    cb_lookup = caml_named_value("wile_lookup");
    cb_call = caml_named_value("wile_call");
    cb_define_primitive = caml_named_value("wile_define_primitive");
    cb_make_nil = caml_named_value("wile_make_nil");
    cb_make_void = caml_named_value("wile_make_void");
    cb_make_bool = caml_named_value("wile_make_bool");
    cb_make_fixnum = caml_named_value("wile_make_fixnum");
    cb_make_flonum = caml_named_value("wile_make_flonum");
    cb_make_string = caml_named_value("wile_make_string");
    cb_make_symbol = caml_named_value("wile_make_symbol");
    cb_make_cons = caml_named_value("wile_make_cons");
    cb_make_vector = caml_named_value("wile_make_vector");
    cb_make_list = caml_named_value("wile_make_list");
    cb_is_nil = caml_named_value("wile_is_nil");
    cb_is_bool = caml_named_value("wile_is_bool");
    cb_is_fixnum = caml_named_value("wile_is_fixnum");
    cb_is_integer = caml_named_value("wile_is_integer");
    cb_is_flonum = caml_named_value("wile_is_flonum");
    cb_is_string = caml_named_value("wile_is_string");
    cb_is_symbol = caml_named_value("wile_is_symbol");
    cb_is_pair = caml_named_value("wile_is_pair");
    cb_is_vector = caml_named_value("wile_is_vector");
    cb_is_true = caml_named_value("wile_is_true");
    cb_get_bool = caml_named_value("wile_get_bool");
    cb_get_fixnum = caml_named_value("wile_get_fixnum");
    cb_get_integer_string = caml_named_value("wile_get_integer_string");
    cb_get_flonum = caml_named_value("wile_get_flonum");
    cb_get_string = caml_named_value("wile_get_string");
    cb_get_symbol_name = caml_named_value("wile_get_symbol_name");
    cb_car = caml_named_value("wile_car");
    cb_cdr = caml_named_value("wile_cdr");
    cb_vector_length = caml_named_value("wile_vector_length");
    cb_vector_ref = caml_named_value("wile_vector_ref");
    cb_display_string = caml_named_value("wile_display_string");
    cb_write_string = caml_named_value("wile_write_string");
    cb_release = caml_named_value("wile_release");
    callbacks_cached = 1;
}

/* ---- C API functions ---- */

int wile_init(int *argc, char ***argv) {
    (void)argc; (void)argv;
    /* caml_startup is called by the OCaml runtime when linked as a library.
       For standalone C programs, they must call caml_startup themselves
       before wile_init. This function just ensures callbacks are cached. */
    ensure_cached();
    return 1;
}

wile_inst_t wile_create(void) {
    ensure_cached();
    value r = caml_callback(*cb_create, Val_unit);
    return (wile_inst_t)Int_val(r);
}

void wile_destroy(wile_inst_t inst) {
    ensure_cached();
    caml_callback(*cb_destroy, Val_int(inst));
}

const char *wile_error_message(wile_inst_t inst) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback(*cb_error_message, Val_int(inst));
    /* Return pointer into OCaml string — valid until next GC.
       For a real production API this should be copied, but for our
       use case the caller reads it immediately. */
    CAMLreturnT(const char *, String_val(r));
}

wile_val_t wile_eval_string(wile_inst_t inst, const char *src) {
    CAMLparam0(); CAMLlocal1(s);
    ensure_cached();
    s = caml_copy_string(src);
    value r = caml_callback2(*cb_eval_string, Val_int(inst), s);
    CAMLreturnT(wile_val_t, (wile_val_t)Int_val(r));
}

int wile_load_file(wile_inst_t inst, const char *path) {
    CAMLparam0(); CAMLlocal1(s);
    ensure_cached();
    s = caml_copy_string(path);
    value r = caml_callback2(*cb_load_file, Val_int(inst), s);
    CAMLreturnT(int, Int_val(r));
}

int wile_load_fasl(wile_inst_t inst, const char *path) {
    CAMLparam0(); CAMLlocal1(s);
    ensure_cached();
    s = caml_copy_string(path);
    value r = caml_callback2(*cb_load_fasl, Val_int(inst), s);
    CAMLreturnT(int, Int_val(r));
}

wile_val_t wile_lookup(wile_inst_t inst, const char *name) {
    CAMLparam0(); CAMLlocal1(s);
    ensure_cached();
    s = caml_copy_string(name);
    value r = caml_callback2(*cb_lookup, Val_int(inst), s);
    CAMLreturnT(wile_val_t, (wile_val_t)Int_val(r));
}

wile_val_t wile_call(wile_inst_t inst, wile_val_t proc,
                      int argc, const wile_val_t *argv) {
    CAMLparam0(); CAMLlocal1(arr);
    ensure_cached();
    arr = caml_alloc(argc, 0);
    for (int i = 0; i < argc; i++) {
        Store_field(arr, i, Val_int(argv[i]));
    }
    value r = caml_callback3(*cb_call, Val_int(inst), Val_int(proc), arr);
    CAMLreturnT(wile_val_t, (wile_val_t)Int_val(r));
}

void wile_define_primitive(wile_inst_t inst, const char *name,
                            wile_cfunc_t fn, void *data) {
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

wile_val_t wile_nil(wile_inst_t inst) {
    ensure_cached();
    value r = caml_callback(*cb_make_nil, Val_int(inst));
    return (wile_val_t)Int_val(r);
}

wile_val_t wile_void(wile_inst_t inst) {
    ensure_cached();
    value r = caml_callback(*cb_make_void, Val_int(inst));
    return (wile_val_t)Int_val(r);
}

wile_val_t wile_bool(wile_inst_t inst, int b) {
    ensure_cached();
    value r = caml_callback2(*cb_make_bool, Val_int(inst), Val_int(b));
    return (wile_val_t)Int_val(r);
}

wile_val_t wile_fixnum(wile_inst_t inst, long n) {
    ensure_cached();
    value r = caml_callback2(*cb_make_fixnum, Val_int(inst), Val_int((int)n));
    return (wile_val_t)Int_val(r);
}

wile_val_t wile_flonum(wile_inst_t inst, double d) {
    CAMLparam0(); CAMLlocal1(f);
    ensure_cached();
    f = caml_copy_double(d);
    value r = caml_callback2(*cb_make_flonum, Val_int(inst), f);
    CAMLreturnT(wile_val_t, (wile_val_t)Int_val(r));
}

wile_val_t wile_string(wile_inst_t inst, const char *s) {
    CAMLparam0(); CAMLlocal1(str);
    ensure_cached();
    str = caml_copy_string(s);
    value r = caml_callback2(*cb_make_string, Val_int(inst), str);
    CAMLreturnT(wile_val_t, (wile_val_t)Int_val(r));
}

wile_val_t wile_symbol(wile_inst_t inst, const char *name) {
    CAMLparam0(); CAMLlocal1(s);
    ensure_cached();
    s = caml_copy_string(name);
    value r = caml_callback2(*cb_make_symbol, Val_int(inst), s);
    CAMLreturnT(wile_val_t, (wile_val_t)Int_val(r));
}

wile_val_t wile_cons(wile_inst_t inst, wile_val_t car, wile_val_t cdr) {
    ensure_cached();
    value r = caml_callback3(*cb_make_cons, Val_int(inst),
                              Val_int(car), Val_int(cdr));
    return (wile_val_t)Int_val(r);
}

wile_val_t wile_vector(wile_inst_t inst, int len, const wile_val_t *e) {
    CAMLparam0(); CAMLlocal1(arr);
    ensure_cached();
    arr = caml_alloc(len, 0);
    for (int i = 0; i < len; i++) {
        Store_field(arr, i, Val_int(e[i]));
    }
    value r = caml_callback2(*cb_make_vector, Val_int(inst), arr);
    CAMLreturnT(wile_val_t, (wile_val_t)Int_val(r));
}

wile_val_t wile_list(wile_inst_t inst, int len, const wile_val_t *e) {
    CAMLparam0(); CAMLlocal1(arr);
    ensure_cached();
    arr = caml_alloc(len, 0);
    for (int i = 0; i < len; i++) {
        Store_field(arr, i, Val_int(e[i]));
    }
    value r = caml_callback2(*cb_make_list, Val_int(inst), arr);
    CAMLreturnT(wile_val_t, (wile_val_t)Int_val(r));
}

/* ---- Predicates ---- */

int wile_is_nil(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_nil, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int wile_is_bool(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_bool, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int wile_is_fixnum(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_fixnum, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int wile_is_integer(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_integer, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int wile_is_flonum(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_flonum, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int wile_is_string(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_string, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int wile_is_symbol(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_symbol, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int wile_is_pair(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_pair, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int wile_is_vector(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_vector, Val_int(inst), Val_int(v));
    return Int_val(r);
}

int wile_is_true(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_is_true, Val_int(inst), Val_int(v));
    return Int_val(r);
}

/* ---- Extractors ---- */

int wile_bool_value(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_get_bool, Val_int(inst), Val_int(v));
    return Int_val(r);
}

long wile_fixnum_value(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_get_fixnum, Val_int(inst), Val_int(v));
    return (long)Int_val(r);
}

char *wile_integer_string(wile_inst_t inst, wile_val_t v) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback2(*cb_get_integer_string, Val_int(inst), Val_int(v));
    char *s = strdup(String_val(r));
    CAMLreturnT(char *, s);
}

double wile_flonum_value(wile_inst_t inst, wile_val_t v) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback2(*cb_get_flonum, Val_int(inst), Val_int(v));
    CAMLreturnT(double, Double_val(r));
}

char *wile_string_value(wile_inst_t inst, wile_val_t v) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback2(*cb_get_string, Val_int(inst), Val_int(v));
    char *s = strdup(String_val(r));
    CAMLreturnT(char *, s);
}

char *wile_symbol_name(wile_inst_t inst, wile_val_t v) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback2(*cb_get_symbol_name, Val_int(inst), Val_int(v));
    char *s = strdup(String_val(r));
    CAMLreturnT(char *, s);
}

wile_val_t wile_car(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_car, Val_int(inst), Val_int(v));
    return (wile_val_t)Int_val(r);
}

wile_val_t wile_cdr(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_cdr, Val_int(inst), Val_int(v));
    return (wile_val_t)Int_val(r);
}

int wile_vector_length(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    value r = caml_callback2(*cb_vector_length, Val_int(inst), Val_int(v));
    return Int_val(r);
}

wile_val_t wile_vector_ref(wile_inst_t inst, wile_val_t v, int i) {
    ensure_cached();
    value r = caml_callback3(*cb_vector_ref, Val_int(inst),
                              Val_int(v), Val_int(i));
    return (wile_val_t)Int_val(r);
}

/* ---- Display/Write ---- */

char *wile_display_string(wile_inst_t inst, wile_val_t v) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback2(*cb_display_string, Val_int(inst), Val_int(v));
    char *s = strdup(String_val(r));
    CAMLreturnT(char *, s);
}

char *wile_write_string(wile_inst_t inst, wile_val_t v) {
    CAMLparam0(); CAMLlocal1(r);
    ensure_cached();
    r = caml_callback2(*cb_write_string, Val_int(inst), Val_int(v));
    char *s = strdup(String_val(r));
    CAMLreturnT(char *, s);
}

/* ---- Memory ---- */

void wile_release(wile_inst_t inst, wile_val_t v) {
    ensure_cached();
    caml_callback2(*cb_release, Val_int(inst), Val_int(v));
}
