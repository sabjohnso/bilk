/* wile.h — C embedding API for the Wile Scheme runtime.

   This header provides a handle-based API for embedding Wile in C programs.
   All Scheme values are represented as integer handles (wile_val_t) that
   reference GC-managed OCaml objects.  Handle 0 (WILE_NULL) indicates an
   error or not-found condition.

   Usage:
     1. Link with the wile OCaml library (ocamlfind/dune)
     2. Call wile_init() before any other wile_* functions
     3. Create instances with wile_create()
     4. Use wile_eval_string(), wile_lookup(), wile_call() etc.
     5. Release value handles with wile_release() when done
     6. Destroy instances with wile_destroy() when done

   Error handling:
     Most functions return WILE_NULL (0) on error.  Call
     wile_error_message(inst) to retrieve the error description.
     The error is cleared on the next successful call to that instance. */

#ifndef WILE_H
#define WILE_H

#include <stdint.h>

/* ---- Extension API ---- */

/** Extension API version for compatibility checks. */
#define WILE_EXT_API_VERSION 1

/** Name of the entry point symbol in C extensions. */
#define WILE_EXT_ENTRY "wile_ext_init"

/** Declare the extension entry point function.
    Usage: WILE_EXT_INIT { wile_define_primitive(inst, ...); } */
#define WILE_EXT_INIT \
    void wile_ext_init(wile_inst_t inst)

#ifdef __cplusplus
extern "C" {
#endif

/** Opaque handle to a Scheme value. */
typedef int32_t wile_val_t;

/** Opaque handle to a Scheme instance. */
typedef int32_t wile_inst_t;

/** Sentinel value indicating error or not-found. */
#define WILE_NULL ((wile_val_t)0)

/** Signature for C functions registered as Scheme primitives.
    @param inst    The instance handle
    @param argc    Number of arguments
    @param argv    Array of argument value handles
    @param data    User data pointer passed at registration
    @return        A value handle for the result, or WILE_NULL */
typedef wile_val_t (*wile_cfunc_t)(wile_inst_t inst, int argc,
                                    const wile_val_t *argv, void *data);

/* ---- Lifecycle ---- */

/** Initialize the Wile runtime.  Must be called before any other wile_*
    functions.  The argc/argv parameters are currently unused but reserved
    for future use.
    @return 1 on success, 0 on failure */
int wile_init(int *argc, char ***argv);

/** Create a new Scheme instance with a fresh environment containing the
    full R7RS standard library.
    @return Instance handle, or 0 on error */
wile_inst_t wile_create(void);

/** Destroy an instance and release all associated resources.
    @param inst Instance handle */
void wile_destroy(wile_inst_t inst);

/* ---- Error handling ---- */

/** Retrieve the last error message for an instance.
    Returns an empty string if no error is pending.
    The returned pointer is valid until the next wile_* call on this instance.
    @param inst Instance handle
    @return Error message string (do NOT free) */
const char *wile_error_message(wile_inst_t inst);

/* ---- Evaluation ---- */

/** Evaluate a Scheme expression from a string.
    @param inst Instance handle
    @param src  Scheme source code (one expression)
    @return Value handle for the result, or WILE_NULL on error */
wile_val_t wile_eval_string(wile_inst_t inst, const char *src);

/** Load and execute all expressions from a Scheme source file.
    @param inst Instance handle
    @param path File path
    @return 1 on success, 0 on error */
int wile_load_file(wile_inst_t inst, const char *path);

/** Load and execute a pre-compiled FASL file.
    @param inst Instance handle
    @param path FASL file path
    @return 1 on success, 0 on error */
int wile_load_fasl(wile_inst_t inst, const char *path);

/* ---- Lookup & Call ---- */

/** Look up a global binding by name.
    @param inst Instance handle
    @param name Symbol name
    @return Value handle, or WILE_NULL if not found */
wile_val_t wile_lookup(wile_inst_t inst, const char *name);

/** Call a Scheme procedure with arguments.
    @param inst Instance handle
    @param proc Value handle for the procedure
    @param argc Number of arguments
    @param argv Array of argument value handles
    @return Value handle for the result, or WILE_NULL on error */
wile_val_t wile_call(wile_inst_t inst, wile_val_t proc,
                      int argc, const wile_val_t *argv);

/* ---- Primitive registration ---- */

/** Register a C function as a Scheme primitive.
    @param inst Instance handle
    @param name Scheme name for the primitive
    @param fn   C function pointer
    @param data User data passed to fn on each call */
void wile_define_primitive(wile_inst_t inst, const char *name,
                            wile_cfunc_t fn, void *data);

/* ---- Constructors ---- */

/** Construct the empty list '(). */
wile_val_t wile_nil(wile_inst_t inst);

/** Construct the void value. */
wile_val_t wile_void(wile_inst_t inst);

/** Construct a boolean.  b=0 gives #f, nonzero gives #t. */
wile_val_t wile_bool(wile_inst_t inst, int b);

/** Construct a fixnum (exact integer). */
wile_val_t wile_fixnum(wile_inst_t inst, long n);

/** Construct a flonum (inexact real). */
wile_val_t wile_flonum(wile_inst_t inst, double d);

/** Construct a Scheme string (copied from the C string). */
wile_val_t wile_string(wile_inst_t inst, const char *s);

/** Construct an interned symbol. */
wile_val_t wile_symbol(wile_inst_t inst, const char *name);

/** Construct a pair (cons cell). */
wile_val_t wile_cons(wile_inst_t inst, wile_val_t car, wile_val_t cdr);

/** Construct a vector from an array of value handles. */
wile_val_t wile_vector(wile_inst_t inst, int len, const wile_val_t *e);

/** Construct a proper list from an array of value handles. */
wile_val_t wile_list(wile_inst_t inst, int len, const wile_val_t *e);

/* ---- Predicates (return 0 or 1) ---- */

int wile_is_nil(wile_inst_t inst, wile_val_t v);
int wile_is_bool(wile_inst_t inst, wile_val_t v);
int wile_is_fixnum(wile_inst_t inst, wile_val_t v);
int wile_is_flonum(wile_inst_t inst, wile_val_t v);
int wile_is_string(wile_inst_t inst, wile_val_t v);
int wile_is_symbol(wile_inst_t inst, wile_val_t v);
int wile_is_pair(wile_inst_t inst, wile_val_t v);
int wile_is_vector(wile_inst_t inst, wile_val_t v);

/** R7RS truthiness: only #f is false. */
int wile_is_true(wile_inst_t inst, wile_val_t v);

/* ---- Extractors ---- */

/** Extract boolean value (1 for #t, 0 for #f).
    Sets error if not a boolean. */
int wile_bool_value(wile_inst_t inst, wile_val_t v);

/** Extract fixnum value.  Sets error if not a fixnum. */
long wile_fixnum_value(wile_inst_t inst, wile_val_t v);

/** Extract flonum value.  Sets error if not a flonum. */
double wile_flonum_value(wile_inst_t inst, wile_val_t v);

/** Extract string value.  Caller must free() the result.
    Sets error if not a string. */
char *wile_string_value(wile_inst_t inst, wile_val_t v);

/** Extract symbol name.  Caller must free() the result.
    Sets error if not a symbol. */
char *wile_symbol_name(wile_inst_t inst, wile_val_t v);

/** Extract the car of a pair.  Sets error if not a pair. */
wile_val_t wile_car(wile_inst_t inst, wile_val_t v);

/** Extract the cdr of a pair.  Sets error if not a pair. */
wile_val_t wile_cdr(wile_inst_t inst, wile_val_t v);

/** Get the length of a vector.  Sets error if not a vector. */
int wile_vector_length(wile_inst_t inst, wile_val_t v);

/** Get element i of a vector.  Sets error if not a vector or out of range. */
wile_val_t wile_vector_ref(wile_inst_t inst, wile_val_t v, int i);

/* ---- Display/Write (caller must free() the result) ---- */

/** Return the display representation of a value. */
char *wile_display_string(wile_inst_t inst, wile_val_t v);

/** Return the write (machine-readable) representation of a value. */
char *wile_write_string(wile_inst_t inst, wile_val_t v);

/* ---- Memory management ---- */

/** Release a value handle.  The handle becomes invalid after this call.
    @param inst Instance handle
    @param v    Value handle to release */
void wile_release(wile_inst_t inst, wile_val_t v);

#ifdef __cplusplus
}
#endif

#endif /* WILE_H */
