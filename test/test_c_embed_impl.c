#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../lib/bilk.h"

static int tests_run = 0;
static int tests_passed = 0;

#define ASSERT(msg, cond) do { \
    tests_run++; \
    if (!(cond)) { \
        fprintf(stderr, "  FAIL: %s\n", msg); \
    } else { \
        printf("  OK: %s\n", msg); \
        tests_passed++; \
    } \
} while(0)

static void test_create_destroy(void) {
    bilk_inst_t inst = bilk_create();
    ASSERT("create returns handle > 0", inst > 0);
    bilk_destroy(inst);
    ASSERT("destroy completes", 1);
}

static void test_eval_fixnum(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t v = bilk_eval_string(inst, "42");
    ASSERT("eval returns handle", v != BILK_NULL);
    ASSERT("is fixnum", bilk_is_fixnum(inst, v));
    long n = bilk_fixnum_value(inst, v);
    ASSERT("fixnum value is 42", n == 42);
    bilk_release(inst, v);
    bilk_destroy(inst);
}

static void test_eval_expr(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t v = bilk_eval_string(inst, "(+ 10 20 30)");
    ASSERT("eval expr returns handle", v != BILK_NULL);
    long n = bilk_fixnum_value(inst, v);
    ASSERT("(+ 10 20 30) = 60", n == 60);
    bilk_release(inst, v);
    bilk_destroy(inst);
}

static void test_string_roundtrip(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t v = bilk_string(inst, "hello world");
    ASSERT("string handle", v != BILK_NULL);
    ASSERT("is string", bilk_is_string(inst, v));
    char *s = bilk_string_value(inst, v);
    ASSERT("string value", strcmp(s, "hello world") == 0);
    free(s);
    bilk_release(inst, v);
    bilk_destroy(inst);
}

static void test_cons_car_cdr(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t a = bilk_fixnum(inst, 1);
    bilk_val_t b = bilk_fixnum(inst, 2);
    bilk_val_t p = bilk_cons(inst, a, b);
    ASSERT("cons handle", p != BILK_NULL);
    ASSERT("is pair", bilk_is_pair(inst, p));

    bilk_val_t car_v = bilk_car(inst, p);
    bilk_val_t cdr_v = bilk_cdr(inst, p);
    ASSERT("car = 1", bilk_fixnum_value(inst, car_v) == 1);
    ASSERT("cdr = 2", bilk_fixnum_value(inst, cdr_v) == 2);

    bilk_release(inst, a);
    bilk_release(inst, b);
    bilk_release(inst, p);
    bilk_release(inst, car_v);
    bilk_release(inst, cdr_v);
    bilk_destroy(inst);
}

static void test_lookup_call(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t plus = bilk_lookup(inst, "+");
    ASSERT("lookup +", plus != BILK_NULL);

    bilk_val_t args[3];
    args[0] = bilk_fixnum(inst, 1);
    args[1] = bilk_fixnum(inst, 2);
    args[2] = bilk_fixnum(inst, 3);

    bilk_val_t r = bilk_call(inst, plus, 3, args);
    ASSERT("call + returns handle", r != BILK_NULL);
    ASSERT("1+2+3 = 6", bilk_fixnum_value(inst, r) == 6);

    bilk_release(inst, plus);
    bilk_release(inst, args[0]);
    bilk_release(inst, args[1]);
    bilk_release(inst, args[2]);
    bilk_release(inst, r);
    bilk_destroy(inst);
}

static void test_error_message(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t v = bilk_eval_string(inst, "(car 42)");
    ASSERT("error returns NULL", v == BILK_NULL);
    const char *msg = bilk_error_message(inst);
    ASSERT("error message not empty", strlen(msg) > 0);
    bilk_destroy(inst);
}

static void test_vector(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t elts[3];
    elts[0] = bilk_fixnum(inst, 10);
    elts[1] = bilk_fixnum(inst, 20);
    elts[2] = bilk_fixnum(inst, 30);
    bilk_val_t v = bilk_vector(inst, 3, elts);
    ASSERT("vector handle", v != BILK_NULL);
    ASSERT("is vector", bilk_is_vector(inst, v));
    ASSERT("length 3", bilk_vector_length(inst, v) == 3);

    bilk_val_t e1 = bilk_vector_ref(inst, v, 1);
    ASSERT("v[1] = 20", bilk_fixnum_value(inst, e1) == 20);

    bilk_release(inst, elts[0]);
    bilk_release(inst, elts[1]);
    bilk_release(inst, elts[2]);
    bilk_release(inst, v);
    bilk_release(inst, e1);
    bilk_destroy(inst);
}

static void test_bool_predicates(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t t = bilk_bool(inst, 1);
    bilk_val_t f = bilk_bool(inst, 0);
    bilk_val_t n = bilk_fixnum(inst, 0);

    ASSERT("#t is bool", bilk_is_bool(inst, t));
    ASSERT("#f is bool", bilk_is_bool(inst, f));
    ASSERT("0 is not bool", !bilk_is_bool(inst, n));
    ASSERT("#t is true", bilk_is_true(inst, t));
    ASSERT("#f is not true", !bilk_is_true(inst, f));
    ASSERT("0 is true (R7RS)", bilk_is_true(inst, n));

    bilk_release(inst, t);
    bilk_release(inst, f);
    bilk_release(inst, n);
    bilk_destroy(inst);
}

static void test_define_primitive(void) {
    bilk_inst_t inst = bilk_create();
    /* We can't easily test C primitive registration without a full
       C-hosted main.  Instead we test that defining a primitive
       from OCaml-side works through eval. */
    bilk_val_t r = bilk_eval_string(inst,
        "(begin (define (my-double x) (* x 2)) (my-double 21))");
    ASSERT("define+call", r != BILK_NULL);
    ASSERT("my-double 21 = 42", bilk_fixnum_value(inst, r) == 42);
    bilk_release(inst, r);
    bilk_destroy(inst);
}

static void test_multiple_instances(void) {
    bilk_inst_t i1 = bilk_create();
    bilk_inst_t i2 = bilk_create();
    ASSERT("different instances", i1 != i2);

    bilk_eval_string(i1, "(define x 100)");
    bilk_val_t v1 = bilk_lookup(i1, "x");
    bilk_val_t v2 = bilk_lookup(i2, "x");
    ASSERT("x in inst1", v1 != BILK_NULL);
    ASSERT("x not in inst2", v2 == BILK_NULL);
    ASSERT("x = 100", bilk_fixnum_value(i1, v1) == 100);

    bilk_release(i1, v1);
    bilk_destroy(i1);
    bilk_destroy(i2);
}

static void test_display_write(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t v = bilk_string(inst, "hello");
    char *d = bilk_display_string(inst, v);
    char *w = bilk_write_string(inst, v);
    ASSERT("display", strcmp(d, "hello") == 0);
    ASSERT("write", strcmp(w, "\"hello\"") == 0);
    free(d);
    free(w);
    bilk_release(inst, v);
    bilk_destroy(inst);
}

static void test_list(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t elts[3];
    elts[0] = bilk_fixnum(inst, 1);
    elts[1] = bilk_fixnum(inst, 2);
    elts[2] = bilk_fixnum(inst, 3);
    bilk_val_t l = bilk_list(inst, 3, elts);
    ASSERT("list handle", l != BILK_NULL);
    ASSERT("list is pair", bilk_is_pair(inst, l));

    bilk_val_t car_v = bilk_car(inst, l);
    ASSERT("car = 1", bilk_fixnum_value(inst, car_v) == 1);

    char *s = bilk_write_string(inst, l);
    ASSERT("write list", strcmp(s, "(1 2 3)") == 0);
    free(s);

    bilk_release(inst, elts[0]);
    bilk_release(inst, elts[1]);
    bilk_release(inst, elts[2]);
    bilk_release(inst, l);
    bilk_release(inst, car_v);
    bilk_destroy(inst);
}

static void test_flonum(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t v = bilk_flonum(inst, 3.14);
    ASSERT("flonum handle", v != BILK_NULL);
    ASSERT("is flonum", bilk_is_flonum(inst, v));
    double d = bilk_flonum_value(inst, v);
    ASSERT("flonum value", d == 3.14);
    bilk_release(inst, v);
    bilk_destroy(inst);
}

static void test_symbol(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t v = bilk_symbol(inst, "foo");
    ASSERT("symbol handle", v != BILK_NULL);
    ASSERT("is symbol", bilk_is_symbol(inst, v));
    char *n = bilk_symbol_name(inst, v);
    ASSERT("symbol name", strcmp(n, "foo") == 0);
    free(n);
    bilk_release(inst, v);
    bilk_destroy(inst);
}

static void test_nil(void) {
    bilk_inst_t inst = bilk_create();
    bilk_val_t v = bilk_nil(inst);
    ASSERT("nil handle", v != BILK_NULL);
    ASSERT("is nil", bilk_is_nil(inst, v));
    ASSERT("nil is not pair", !bilk_is_pair(inst, v));
    bilk_release(inst, v);
    bilk_destroy(inst);
}

/* Entry point called from OCaml driver */
int run_c_tests(void) {
    printf("Running C embedding tests...\n");

    test_create_destroy();
    test_eval_fixnum();
    test_eval_expr();
    test_string_roundtrip();
    test_cons_car_cdr();
    test_lookup_call();
    test_error_message();
    test_vector();
    test_bool_predicates();
    test_define_primitive();
    test_multiple_instances();
    test_display_write();
    test_list();
    test_flonum();
    test_symbol();
    test_nil();

    printf("\n%d/%d tests passed.\n", tests_passed, tests_run);
    return (tests_passed == tests_run) ? 0 : 1;
}
