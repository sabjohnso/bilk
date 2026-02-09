#include <caml/mlvalues.h>

/* Defined in test_c_embed_impl.c */
extern int run_c_tests(void);

CAMLprim value run_c_tests_stub(value unit) {
    (void)unit;
    return Val_int(run_c_tests());
}
