/* bilk.h — C embedding API for the Bilk Scheme runtime.

   This header provides a handle-based API for embedding Bilk in C programs.
   All Scheme values are represented as integer handles (bilk_val_t) that
   reference GC-managed OCaml objects.  Handle 0 (BILK_NULL) indicates an
   error or not-found condition.

   Usage:
     1. Link with the bilk OCaml library (ocamlfind/dune)
     2. Call bilk_init() before any other bilk_* functions
     3. Create instances with bilk_create()
     4. Use bilk_eval_string(), bilk_lookup(), bilk_call() etc.
     5. Release value handles with bilk_release() when done
     6. Destroy instances with bilk_destroy() when done

   Error handling:
     Most functions return BILK_NULL (0) on error.  Call
     bilk_error_message(inst) to retrieve the error description.
     The error is cleared on the next successful call to that instance. */

#ifndef BILK_H
#define BILK_H

#include <stdint.h>

/* ---- Extension API ---- */

/** Extension API version for compatibility checks. */
#define BILK_EXT_API_VERSION 1

/** Name of the entry point symbol in C extensions. */
#define BILK_EXT_ENTRY "bilk_ext_init"

/** Declare the extension entry point function.
    Usage: BILK_EXT_INIT { bilk_define_primitive(inst, ...); } */
#define BILK_EXT_INIT \
    void bilk_ext_init(bilk_inst_t inst)

#ifdef __cplusplus
extern "C" {
#endif

/** Opaque handle to a Scheme value. */
typedef int32_t bilk_val_t;

/** Opaque handle to a Scheme instance. */
typedef int32_t bilk_inst_t;

/** Sentinel value indicating error or not-found. */
#define BILK_NULL ((bilk_val_t)0)

/** Signature for C functions registered as Scheme primitives.
    @param inst    The instance handle
    @param argc    Number of arguments
    @param argv    Array of argument value handles
    @param data    User data pointer passed at registration
    @return        A value handle for the result, or BILK_NULL */
typedef bilk_val_t (*bilk_cfunc_t)(bilk_inst_t inst, int argc,
                                    const bilk_val_t *argv, void *data);

/* ---- Lifecycle ---- */

/** Initialize the Bilk runtime.  Must be called before any other bilk_*
    functions.  The argc/argv parameters are currently unused but reserved
    for future use.
    @return 1 on success, 0 on failure */
int bilk_init(int *argc, char ***argv);

/** Create a new Scheme instance with a fresh environment containing the
    full R7RS standard library.
    @return Instance handle, or 0 on error */
bilk_inst_t bilk_create(void);

/** Destroy an instance and release all associated resources.
    @param inst Instance handle */
void bilk_destroy(bilk_inst_t inst);

/* ---- Error handling ---- */

/** Retrieve the last error message for an instance.
    Returns an empty string if no error is pending.
    The returned pointer is valid until the next bilk_* call on this instance.
    @param inst Instance handle
    @return Error message string (do NOT free) */
const char *bilk_error_message(bilk_inst_t inst);

/* ---- Evaluation ---- */

/** Evaluate a Scheme expression from a string.
    @param inst Instance handle
    @param src  Scheme source code (one expression)
    @return Value handle for the result, or BILK_NULL on error */
bilk_val_t bilk_eval_string(bilk_inst_t inst, const char *src);

/** Load and execute all expressions from a Scheme source file.
    @param inst Instance handle
    @param path File path
    @return 1 on success, 0 on error */
int bilk_load_file(bilk_inst_t inst, const char *path);

/** Load and execute a pre-compiled FASL file.
    @param inst Instance handle
    @param path FASL file path
    @return 1 on success, 0 on error */
int bilk_load_fasl(bilk_inst_t inst, const char *path);

/* ---- Lookup & Call ---- */

/** Look up a global binding by name.
    @param inst Instance handle
    @param name Symbol name
    @return Value handle, or BILK_NULL if not found */
bilk_val_t bilk_lookup(bilk_inst_t inst, const char *name);

/** Call a Scheme procedure with arguments.
    @param inst Instance handle
    @param proc Value handle for the procedure
    @param argc Number of arguments
    @param argv Array of argument value handles
    @return Value handle for the result, or BILK_NULL on error */
bilk_val_t bilk_call(bilk_inst_t inst, bilk_val_t proc,
                      int argc, const bilk_val_t *argv);

/* ---- Primitive registration ---- */

/** Register a C function as a Scheme primitive.
    @param inst Instance handle
    @param name Scheme name for the primitive
    @param fn   C function pointer
    @param data User data passed to fn on each call */
void bilk_define_primitive(bilk_inst_t inst, const char *name,
                            bilk_cfunc_t fn, void *data);

/* ---- Constructors ---- */

/** Construct the empty list '(). */
bilk_val_t bilk_nil(bilk_inst_t inst);

/** Construct the void value. */
bilk_val_t bilk_void(bilk_inst_t inst);

/** Construct a boolean.  b=0 gives #f, nonzero gives #t. */
bilk_val_t bilk_bool(bilk_inst_t inst, int b);

/** Construct a fixnum (exact integer). */
bilk_val_t bilk_fixnum(bilk_inst_t inst, long n);

/** Construct a flonum (inexact real). */
bilk_val_t bilk_flonum(bilk_inst_t inst, double d);

/** Construct a Scheme string (copied from the C string). */
bilk_val_t bilk_string(bilk_inst_t inst, const char *s);

/** Construct an interned symbol. */
bilk_val_t bilk_symbol(bilk_inst_t inst, const char *name);

/** Construct a pair (cons cell). */
bilk_val_t bilk_cons(bilk_inst_t inst, bilk_val_t car, bilk_val_t cdr);

/** Construct a vector from an array of value handles. */
bilk_val_t bilk_vector(bilk_inst_t inst, int len, const bilk_val_t *e);

/** Construct a proper list from an array of value handles. */
bilk_val_t bilk_list(bilk_inst_t inst, int len, const bilk_val_t *e);

/* ---- Predicates (return 0 or 1) ---- */

int bilk_is_nil(bilk_inst_t inst, bilk_val_t v);
int bilk_is_bool(bilk_inst_t inst, bilk_val_t v);
int bilk_is_fixnum(bilk_inst_t inst, bilk_val_t v);

/** Returns 1 if the value is an exact integer (fixnum or bignum). */
int bilk_is_integer(bilk_inst_t inst, bilk_val_t v);
int bilk_is_flonum(bilk_inst_t inst, bilk_val_t v);
int bilk_is_string(bilk_inst_t inst, bilk_val_t v);
int bilk_is_symbol(bilk_inst_t inst, bilk_val_t v);
int bilk_is_pair(bilk_inst_t inst, bilk_val_t v);
int bilk_is_vector(bilk_inst_t inst, bilk_val_t v);

/** R7RS truthiness: only #f is false. */
int bilk_is_true(bilk_inst_t inst, bilk_val_t v);

/* ---- Extractors ---- */

/** Extract boolean value (1 for #t, 0 for #f).
    Sets error if not a boolean. */
int bilk_bool_value(bilk_inst_t inst, bilk_val_t v);

/** Extract fixnum value.  For bignums that fit in a native int, the
    value is returned.  Sets error if not an integer or too large. */
long bilk_fixnum_value(bilk_inst_t inst, bilk_val_t v);

/** Extract exact integer as decimal string.  Caller must free() the result.
    Works for both fixnums and bignums.  Sets error if not an integer. */
char *bilk_integer_string(bilk_inst_t inst, bilk_val_t v);

/** Extract flonum value.  Sets error if not a flonum. */
double bilk_flonum_value(bilk_inst_t inst, bilk_val_t v);

/** Extract string value.  Caller must free() the result.
    Sets error if not a string. */
char *bilk_string_value(bilk_inst_t inst, bilk_val_t v);

/** Extract symbol name.  Caller must free() the result.
    Sets error if not a symbol. */
char *bilk_symbol_name(bilk_inst_t inst, bilk_val_t v);

/** Extract the car of a pair.  Sets error if not a pair. */
bilk_val_t bilk_car(bilk_inst_t inst, bilk_val_t v);

/** Extract the cdr of a pair.  Sets error if not a pair. */
bilk_val_t bilk_cdr(bilk_inst_t inst, bilk_val_t v);

/** Get the length of a vector.  Sets error if not a vector. */
int bilk_vector_length(bilk_inst_t inst, bilk_val_t v);

/** Get element i of a vector.  Sets error if not a vector or out of range. */
bilk_val_t bilk_vector_ref(bilk_inst_t inst, bilk_val_t v, int i);

/* ---- Display/Write (caller must free() the result) ---- */

/** Return the display representation of a value. */
char *bilk_display_string(bilk_inst_t inst, bilk_val_t v);

/** Return the write (machine-readable) representation of a value. */
char *bilk_write_string(bilk_inst_t inst, bilk_val_t v);

/* ---- Memory management ---- */

/** Release a value handle.  The handle becomes invalid after this call.
    @param inst Instance handle
    @param v    Value handle to release */
void bilk_release(bilk_inst_t inst, bilk_val_t v);

#ifdef __cplusplus
}
#endif

#endif /* BILK_H */
