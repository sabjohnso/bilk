#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "../lib/wile.h"

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
    wile_inst_t inst = wile_create();
    ASSERT("create returns handle > 0", inst > 0);
    wile_destroy(inst);
    ASSERT("destroy completes", 1);
}

static void test_eval_fixnum(void) {
    wile_inst_t inst = wile_create();
    wile_val_t v = wile_eval_string(inst, "42");
    ASSERT("eval returns handle", v != WILE_NULL);
    ASSERT("is fixnum", wile_is_fixnum(inst, v));
    long n = wile_fixnum_value(inst, v);
    ASSERT("fixnum value is 42", n == 42);
    wile_release(inst, v);
    wile_destroy(inst);
}

static void test_eval_expr(void) {
    wile_inst_t inst = wile_create();
    wile_val_t v = wile_eval_string(inst, "(+ 10 20 30)");
    ASSERT("eval expr returns handle", v != WILE_NULL);
    long n = wile_fixnum_value(inst, v);
    ASSERT("(+ 10 20 30) = 60", n == 60);
    wile_release(inst, v);
    wile_destroy(inst);
}

static void test_string_roundtrip(void) {
    wile_inst_t inst = wile_create();
    wile_val_t v = wile_string(inst, "hello world");
    ASSERT("string handle", v != WILE_NULL);
    ASSERT("is string", wile_is_string(inst, v));
    char *s = wile_string_value(inst, v);
    ASSERT("string value", strcmp(s, "hello world") == 0);
    free(s);
    wile_release(inst, v);
    wile_destroy(inst);
}

static void test_cons_car_cdr(void) {
    wile_inst_t inst = wile_create();
    wile_val_t a = wile_fixnum(inst, 1);
    wile_val_t b = wile_fixnum(inst, 2);
    wile_val_t p = wile_cons(inst, a, b);
    ASSERT("cons handle", p != WILE_NULL);
    ASSERT("is pair", wile_is_pair(inst, p));

    wile_val_t car_v = wile_car(inst, p);
    wile_val_t cdr_v = wile_cdr(inst, p);
    ASSERT("car = 1", wile_fixnum_value(inst, car_v) == 1);
    ASSERT("cdr = 2", wile_fixnum_value(inst, cdr_v) == 2);

    wile_release(inst, a);
    wile_release(inst, b);
    wile_release(inst, p);
    wile_release(inst, car_v);
    wile_release(inst, cdr_v);
    wile_destroy(inst);
}

static void test_lookup_call(void) {
    wile_inst_t inst = wile_create();
    wile_val_t plus = wile_lookup(inst, "+");
    ASSERT("lookup +", plus != WILE_NULL);

    wile_val_t args[3];
    args[0] = wile_fixnum(inst, 1);
    args[1] = wile_fixnum(inst, 2);
    args[2] = wile_fixnum(inst, 3);

    wile_val_t r = wile_call(inst, plus, 3, args);
    ASSERT("call + returns handle", r != WILE_NULL);
    ASSERT("1+2+3 = 6", wile_fixnum_value(inst, r) == 6);

    wile_release(inst, plus);
    wile_release(inst, args[0]);
    wile_release(inst, args[1]);
    wile_release(inst, args[2]);
    wile_release(inst, r);
    wile_destroy(inst);
}

static void test_error_message(void) {
    wile_inst_t inst = wile_create();
    wile_val_t v = wile_eval_string(inst, "(car 42)");
    ASSERT("error returns NULL", v == WILE_NULL);
    const char *msg = wile_error_message(inst);
    ASSERT("error message not empty", strlen(msg) > 0);
    wile_destroy(inst);
}

static void test_vector(void) {
    wile_inst_t inst = wile_create();
    wile_val_t elts[3];
    elts[0] = wile_fixnum(inst, 10);
    elts[1] = wile_fixnum(inst, 20);
    elts[2] = wile_fixnum(inst, 30);
    wile_val_t v = wile_vector(inst, 3, elts);
    ASSERT("vector handle", v != WILE_NULL);
    ASSERT("is vector", wile_is_vector(inst, v));
    ASSERT("length 3", wile_vector_length(inst, v) == 3);

    wile_val_t e1 = wile_vector_ref(inst, v, 1);
    ASSERT("v[1] = 20", wile_fixnum_value(inst, e1) == 20);

    wile_release(inst, elts[0]);
    wile_release(inst, elts[1]);
    wile_release(inst, elts[2]);
    wile_release(inst, v);
    wile_release(inst, e1);
    wile_destroy(inst);
}

static void test_bool_predicates(void) {
    wile_inst_t inst = wile_create();
    wile_val_t t = wile_bool(inst, 1);
    wile_val_t f = wile_bool(inst, 0);
    wile_val_t n = wile_fixnum(inst, 0);

    ASSERT("#t is bool", wile_is_bool(inst, t));
    ASSERT("#f is bool", wile_is_bool(inst, f));
    ASSERT("0 is not bool", !wile_is_bool(inst, n));
    ASSERT("#t is true", wile_is_true(inst, t));
    ASSERT("#f is not true", !wile_is_true(inst, f));
    ASSERT("0 is true (R7RS)", wile_is_true(inst, n));

    wile_release(inst, t);
    wile_release(inst, f);
    wile_release(inst, n);
    wile_destroy(inst);
}

static void test_define_primitive(void) {
    wile_inst_t inst = wile_create();
    /* We can't easily test C primitive registration without a full
       C-hosted main.  Instead we test that defining a primitive
       from OCaml-side works through eval. */
    wile_val_t r = wile_eval_string(inst,
        "(begin (define (my-double x) (* x 2)) (my-double 21))");
    ASSERT("define+call", r != WILE_NULL);
    ASSERT("my-double 21 = 42", wile_fixnum_value(inst, r) == 42);
    wile_release(inst, r);
    wile_destroy(inst);
}

static void test_multiple_instances(void) {
    wile_inst_t i1 = wile_create();
    wile_inst_t i2 = wile_create();
    ASSERT("different instances", i1 != i2);

    wile_eval_string(i1, "(define x 100)");
    wile_val_t v1 = wile_lookup(i1, "x");
    wile_val_t v2 = wile_lookup(i2, "x");
    ASSERT("x in inst1", v1 != WILE_NULL);
    ASSERT("x not in inst2", v2 == WILE_NULL);
    ASSERT("x = 100", wile_fixnum_value(i1, v1) == 100);

    wile_release(i1, v1);
    wile_destroy(i1);
    wile_destroy(i2);
}

static void test_display_write(void) {
    wile_inst_t inst = wile_create();
    wile_val_t v = wile_string(inst, "hello");
    char *d = wile_display_string(inst, v);
    char *w = wile_write_string(inst, v);
    ASSERT("display", strcmp(d, "hello") == 0);
    ASSERT("write", strcmp(w, "\"hello\"") == 0);
    free(d);
    free(w);
    wile_release(inst, v);
    wile_destroy(inst);
}

static void test_list(void) {
    wile_inst_t inst = wile_create();
    wile_val_t elts[3];
    elts[0] = wile_fixnum(inst, 1);
    elts[1] = wile_fixnum(inst, 2);
    elts[2] = wile_fixnum(inst, 3);
    wile_val_t l = wile_list(inst, 3, elts);
    ASSERT("list handle", l != WILE_NULL);
    ASSERT("list is pair", wile_is_pair(inst, l));

    wile_val_t car_v = wile_car(inst, l);
    ASSERT("car = 1", wile_fixnum_value(inst, car_v) == 1);

    char *s = wile_write_string(inst, l);
    ASSERT("write list", strcmp(s, "(1 2 3)") == 0);
    free(s);

    wile_release(inst, elts[0]);
    wile_release(inst, elts[1]);
    wile_release(inst, elts[2]);
    wile_release(inst, l);
    wile_release(inst, car_v);
    wile_destroy(inst);
}

static void test_flonum(void) {
    wile_inst_t inst = wile_create();
    wile_val_t v = wile_flonum(inst, 3.14);
    ASSERT("flonum handle", v != WILE_NULL);
    ASSERT("is flonum", wile_is_flonum(inst, v));
    double d = wile_flonum_value(inst, v);
    ASSERT("flonum value", d == 3.14);
    wile_release(inst, v);
    wile_destroy(inst);
}

static void test_symbol(void) {
    wile_inst_t inst = wile_create();
    wile_val_t v = wile_symbol(inst, "foo");
    ASSERT("symbol handle", v != WILE_NULL);
    ASSERT("is symbol", wile_is_symbol(inst, v));
    char *n = wile_symbol_name(inst, v);
    ASSERT("symbol name", strcmp(n, "foo") == 0);
    free(n);
    wile_release(inst, v);
    wile_destroy(inst);
}

static void test_nil(void) {
    wile_inst_t inst = wile_create();
    wile_val_t v = wile_nil(inst);
    ASSERT("nil handle", v != WILE_NULL);
    ASSERT("is nil", wile_is_nil(inst, v));
    ASSERT("nil is not pair", !wile_is_pair(inst, v));
    wile_release(inst, v);
    wile_destroy(inst);
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
