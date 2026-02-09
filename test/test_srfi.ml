open Wile

let datum_testable =
  Alcotest.testable Datum.pp Datum.equal

let check_datum = Alcotest.check datum_testable

(* Helper: evaluate a string in a fresh instance (single expression) *)
let eval s =
  let inst = Instance.create () in
  Instance.eval_string inst s

(* Helper: evaluate multiple top-level forms (import + expressions) *)
let eval_port s =
  let inst = Instance.create () in
  let port = Port.of_string s in
  Instance.eval_port inst port

(* ===== Infrastructure tests ===== *)

let test_import_bundled_srfi () =
  check_datum "import srfi 8"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 8)) (receive (a b) (values 1 2) (+ a b))")

let test_cond_expand_srfi_feature () =
  check_datum "cond-expand srfi-8"
    (Datum.Bool true)
    (eval "(cond-expand (srfi-8 #t) (else #f))")

let test_unknown_srfi_feature () =
  check_datum "cond-expand unknown srfi"
    (Datum.Bool false)
    (eval "(cond-expand (srfi-999 #t) (else #f))")

let test_bundled_features_nonempty () =
  Alcotest.(check bool) "bundled features nonempty"
    true (Srfi.bundled_features <> [])

let test_srfi_151_feature () =
  check_datum "cond-expand srfi-151"
    (Datum.Bool true)
    (eval "(cond-expand (srfi-151 #t) (else #f))")

(* ===== SRFI 8 — receive ===== *)

let test_srfi8_basic () =
  check_datum "receive basic"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 8)) (receive (a b) (values 1 2) (+ a b))")

let test_srfi8_rest_args () =
  check_datum "receive rest"
    (Datum.Pair { car = Datum.Fixnum 2; cdr =
      Datum.Pair { car = Datum.Fixnum 3; cdr = Datum.Nil } })
    (eval_port "(import (srfi 8)) (receive (a . b) (values 1 2 3) b)")

let test_srfi8_single () =
  check_datum "receive single"
    (Datum.Fixnum 42)
    (eval_port "(import (srfi 8)) (receive (x) (values 42) x)")

(* ===== SRFI 16 — case-lambda ===== *)

let test_srfi16_basic () =
  check_datum "case-lambda"
    (Datum.Fixnum 7)
    (eval_port "(import (srfi 16)) ((case-lambda ((x) x) ((x y) (+ x y))) 3 4)")

let test_srfi16_one_arg () =
  check_datum "case-lambda one arg"
    (Datum.Fixnum 5)
    (eval_port "(import (srfi 16)) ((case-lambda ((x) x) ((x y) (+ x y))) 5)")

let test_srfi16_zero_args () =
  check_datum "case-lambda zero args"
    (Datum.Fixnum 0)
    (eval_port "(import (srfi 16)) ((case-lambda (() 0) ((x) x)) )")

(* ===== SRFI 26 — cut / cute ===== *)

let test_srfi26_cut_basic () =
  check_datum "cut basic"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 26)) ((cut + <> 1) 5)")

let test_srfi26_cut_rest () =
  check_datum "cut rest"
    (Datum.Pair { car = Datum.Fixnum 1; cdr =
      Datum.Pair { car = Datum.Fixnum 2; cdr =
        Datum.Pair { car = Datum.Fixnum 3; cdr = Datum.Nil } } })
    (eval_port "(import (srfi 26)) ((cut list <> <...>) 1 2 3)")

let test_srfi26_cut_no_slots () =
  check_datum "cut no slots"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 26)) ((cut + 1 2))")

(* ===== SRFI 28 — format ===== *)

let test_srfi28_basic () =
  check_datum "format basic"
    (Datum.Str (Bytes.of_string "1 + 2 = 3"))
    (eval_port {|(import (srfi 28)) (format "~a + ~a = ~a" 1 2 3)|})

let test_srfi28_tilde () =
  check_datum "format tilde"
    (Datum.Str (Bytes.of_string "~"))
    (eval_port {|(import (srfi 28)) (format "~~")|})

let test_srfi28_write () =
  check_datum "format write"
    (Datum.Str (Bytes.of_string {|"hello"|}))
    (eval_port {|(import (srfi 28)) (format "~s" "hello")|})

(* ===== SRFI 31 — rec ===== *)

let test_srfi31_basic () =
  check_datum "rec factorial"
    (Datum.Fixnum 120)
    (eval_port "(import (srfi 31)) ((rec f (lambda (n) (if (= n 0) 1 (* n (f (- n 1)))))) 5)")

let test_srfi31_value () =
  check_datum "rec value"
    (Datum.Fixnum 42)
    (eval_port "(import (srfi 31)) (rec x 42)")

let test_srfi31_list () =
  check_datum "rec with list"
    (Datum.Pair { car = Datum.Fixnum 3; cdr =
      Datum.Pair { car = Datum.Fixnum 2; cdr =
        Datum.Pair { car = Datum.Fixnum 1; cdr = Datum.Nil } } })
    (eval_port "(import (srfi 31)) ((rec f (lambda (n) (if (= n 0) '() (cons n (f (- n 1)))))) 3)")

(* ===== SRFI 111 — boxes ===== *)

let test_srfi111_basic () =
  check_datum "box unbox"
    (Datum.Fixnum 42)
    (eval_port "(import (srfi 111)) (let ((b (box 42))) (unbox b))")

let test_srfi111_set () =
  check_datum "set-box!"
    (Datum.Fixnum 99)
    (eval_port "(import (srfi 111)) (let ((b (box 42))) (set-box! b 99) (unbox b))")

let test_srfi111_pred () =
  check_datum "box?"
    (Datum.Bool true)
    (eval_port "(import (srfi 111)) (box? (box 1))")

(* ===== let-values / let*-values ===== *)

let test_let_values_simple () =
  check_datum "let-values simple"
    (Datum.Fixnum 3)
    (eval "(let-values (((a b) (values 1 2))) (+ a b))")

let test_let_values_multiple () =
  check_datum "let-values multiple"
    (Datum.Fixnum 3)
    (eval "(let-values (((a) (values 1)) ((b) (values 2))) (+ a b))")

let test_let_values_rest () =
  check_datum "let-values rest"
    (Datum.Pair { car = Datum.Fixnum 2; cdr =
      Datum.Pair { car = Datum.Fixnum 3; cdr = Datum.Nil } })
    (eval "(let-values (((a . b) (values 1 2 3))) b)")

let test_let_values_single () =
  check_datum "let-values single var"
    (Datum.Fixnum 42)
    (eval "(let-values (((x) (values 42))) x)")

let test_let_star_values () =
  check_datum "let*-values sequential"
    (Datum.Fixnum 2)
    (eval "(let*-values (((a) (values 1)) ((b) (values (+ a 1)))) b)")

let test_let_values_nested () =
  check_datum "let-values nested"
    (Datum.Fixnum 10)
    (eval "(let-values (((a b) (values 1 2))) \
           (let-values (((c d) (values 3 4))) (+ a b c d)))")

let test_let_values_empty () =
  check_datum "let-values empty bindings"
    (Datum.Fixnum 42)
    (eval "(let-values () 42)")

(* SRFI 11 import test *)
let test_srfi11_import () =
  check_datum "srfi 11 import"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 11)) (let-values (((a b) (values 1 2))) (+ a b))")

(* ===== SRFI 2 — and-let* ===== *)

let test_srfi2_basic () =
  check_datum "and-let* basic"
    (Datum.Fixnum 5)
    (eval_port "(import (srfi 2)) (and-let* ((x 5) ((> x 3))) x)")

let test_srfi2_false () =
  check_datum "and-let* false"
    (Datum.Bool false)
    (eval_port "(import (srfi 2)) (and-let* ((x #f)) x)")

let test_srfi2_test_clause () =
  check_datum "and-let* test clause"
    (Datum.Bool false)
    (eval_port "(import (srfi 2)) (and-let* (((> 3 5))) 'yes)")

let test_srfi2_empty () =
  check_datum "and-let* empty"
    (Datum.Bool true)
    (eval_port "(import (srfi 2)) (and-let* ())")

(* ===== SRFI 151 — bitwise ===== *)

let test_srfi151_and () =
  check_datum "bitwise-and"
    (Datum.Fixnum 8)
    (eval_port "(import (srfi 151)) (bitwise-and 12 10)")

let test_srfi151_ior () =
  check_datum "bitwise-ior"
    (Datum.Fixnum 7)
    (eval_port "(import (srfi 151)) (bitwise-ior 3 5)")

let test_srfi151_xor () =
  check_datum "bitwise-xor"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 151)) (bitwise-xor 3 5)")

let test_srfi151_not () =
  check_datum "bitwise-not"
    (Datum.Fixnum (-1))
    (eval_port "(import (srfi 151)) (bitwise-not 0)")

let test_srfi151_shift () =
  check_datum "arithmetic-shift left"
    (Datum.Fixnum 1024)
    (eval_port "(import (srfi 151)) (arithmetic-shift 1 10)")

let test_srfi151_shift_right () =
  check_datum "arithmetic-shift right"
    (Datum.Fixnum 5)
    (eval_port "(import (srfi 151)) (arithmetic-shift 20 -2)")

let test_srfi151_bit_count () =
  check_datum "bit-count"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 151)) (bit-count 13)")

let test_srfi151_integer_length () =
  check_datum "integer-length"
    (Datum.Fixnum 8)
    (eval_port "(import (srfi 151)) (integer-length 255)")

let test_srfi151_bit_set () =
  check_datum "bit-set?"
    (Datum.Bool true)
    (eval_port "(import (srfi 151)) (bit-set? 0 3)")

let test_srfi151_first_set_bit () =
  check_datum "first-set-bit"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 151)) (first-set-bit 12)")

let test_srfi151_bit_field () =
  check_datum "bit-field"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 151)) (bit-field 255 0 2)")

let test_srfi151_bit_field_replace () =
  check_datum "bit-field-replace"
    (Datum.Fixnum 5)
    (eval_port "(import (srfi 151)) (bit-field-replace 1 2 1 3)")

(* ===== SRFI 128 — comparators ===== *)

let test_srfi128_make_default () =
  check_datum "make-default-comparator"
    (Datum.Bool true)
    (eval_port "(import (srfi 128)) (comparator? (make-default-comparator))")

let test_srfi128_equal () =
  check_datum "=? default"
    (Datum.Bool true)
    (eval_port "(import (srfi 128)) (=? (make-default-comparator) 1 1)")

let test_srfi128_less () =
  check_datum "<? default"
    (Datum.Bool true)
    (eval_port "(import (srfi 128)) (<? (make-default-comparator) 1 2)")

let test_srfi128_greater () =
  check_datum ">? default"
    (Datum.Bool true)
    (eval_port "(import (srfi 128)) (>? (make-default-comparator) 2 1)")

let test_srfi128_string_hash () =
  check_datum "string-hash returns integer"
    (Datum.Bool true)
    (eval_port "(import (srfi 128)) (integer? (string-hash \"hello\"))")

(* ===== SRFI 132 — sort ===== *)

let test_srfi132_list_sort () =
  check_datum "list-sort"
    (Datum.Pair { car = Datum.Fixnum 1; cdr =
      Datum.Pair { car = Datum.Fixnum 1; cdr =
        Datum.Pair { car = Datum.Fixnum 3; cdr =
          Datum.Pair { car = Datum.Fixnum 4; cdr =
            Datum.Pair { car = Datum.Fixnum 5; cdr = Datum.Nil } } } } })
    (eval_port "(import (srfi 132)) (list-sort < '(3 1 4 1 5))")

let test_srfi132_list_sorted () =
  check_datum "list-sorted?"
    (Datum.Bool true)
    (eval_port "(import (srfi 132)) (list-sorted? < '(1 2 3))")

let test_srfi132_list_sorted_false () =
  check_datum "list-sorted? false"
    (Datum.Bool false)
    (eval_port "(import (srfi 132)) (list-sorted? < '(1 3 2))")

let test_srfi132_vector_sort () =
  let result = eval_port "(import (srfi 132)) (vector-sort < #(3 1 2))" in
  check_datum "vector-sort"
    (Datum.Vector [| Datum.Fixnum 1; Datum.Fixnum 2; Datum.Fixnum 3 |])
    result

let test_srfi132_list_merge () =
  check_datum "list-merge"
    (Datum.Pair { car = Datum.Fixnum 1; cdr =
      Datum.Pair { car = Datum.Fixnum 2; cdr =
        Datum.Pair { car = Datum.Fixnum 3; cdr =
          Datum.Pair { car = Datum.Fixnum 4; cdr =
            Datum.Pair { car = Datum.Fixnum 5; cdr =
              Datum.Pair { car = Datum.Fixnum 6; cdr = Datum.Nil } } } } } })
    (eval_port "(import (srfi 132)) (list-merge < '(1 3 5) '(2 4 6))")

(* ===== SRFI 133 — vector library ===== *)

let test_srfi133_vector_fold () =
  check_datum "vector-fold"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 133)) (vector-fold + 0 #(1 2 3))")

let test_srfi133_vector_index () =
  check_datum "vector-index"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 133)) (vector-index even? #(1 3 4 5))")

let test_srfi133_vector_any () =
  check_datum "vector-any false"
    (Datum.Bool false)
    (eval_port "(import (srfi 133)) (vector-any odd? #(2 4 6))")

let test_srfi133_vector_any_true () =
  check_datum "vector-any true"
    (Datum.Bool true)
    (eval_port "(import (srfi 133)) (vector-any odd? #(2 3 6))")

let test_srfi133_vector_every () =
  check_datum "vector-every"
    (Datum.Bool true)
    (eval_port "(import (srfi 133)) (vector-every odd? #(1 3 5))")

let test_srfi133_vector_unfold () =
  check_datum "vector-unfold"
    (Datum.Vector [| Datum.Fixnum 1; Datum.Fixnum 2; Datum.Fixnum 3;
                     Datum.Fixnum 4; Datum.Fixnum 5 |])
    (eval_port "(import (srfi 133)) (vector-unfold (lambda (i) (+ i 1)) 5)")

let test_srfi133_vector_swap () =
  check_datum "vector-swap!"
    (Datum.Vector [| Datum.Fixnum 3; Datum.Fixnum 2; Datum.Fixnum 1 |])
    (eval_port "(import (srfi 133)) (let ((v (vector 1 2 3))) (vector-swap! v 0 2) v)")

let test_srfi133_vector_reverse () =
  check_datum "vector-reverse!"
    (Datum.Vector [| Datum.Fixnum 3; Datum.Fixnum 2; Datum.Fixnum 1 |])
    (eval_port "(import (srfi 133)) (let ((v (vector 1 2 3))) (vector-reverse! v) v)")

let test_srfi133_vector_empty () =
  check_datum "vector-empty?"
    (Datum.Bool true)
    (eval_port "(import (srfi 133)) (vector-empty? #())")

let test_srfi133_vector_count () =
  check_datum "vector-count"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 133)) (vector-count even? #(1 2 3 4))")

let test_srfi133_vector_partition () =
  (* returns two values; test first value *)
  check_datum "vector-partition"
    (Datum.Vector [| Datum.Fixnum 2; Datum.Fixnum 4 |])
    (eval_port "(import (srfi 133)) \
     (call-with-values (lambda () (vector-partition even? #(1 2 3 4))) \
       (lambda (yes no) yes))")

let test_srfi133_binary_search () =
  check_datum "vector-binary-search"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 133)) (vector-binary-search #(1 2 3 4 5) 3 (lambda (a b) (- a b)))")

(* ===== SRFI 1 — list library ===== *)

let test_srfi1_iota () =
  check_datum "iota"
    (Datum.Pair { car = Datum.Fixnum 0; cdr =
      Datum.Pair { car = Datum.Fixnum 1; cdr =
        Datum.Pair { car = Datum.Fixnum 2; cdr =
          Datum.Pair { car = Datum.Fixnum 3; cdr =
            Datum.Pair { car = Datum.Fixnum 4; cdr = Datum.Nil } } } } })
    (eval_port "(import (srfi 1)) (iota 5)")

let test_srfi1_iota_start_step () =
  check_datum "iota start step"
    (Datum.Pair { car = Datum.Fixnum 1; cdr =
      Datum.Pair { car = Datum.Fixnum 3; cdr =
        Datum.Pair { car = Datum.Fixnum 5; cdr =
          Datum.Pair { car = Datum.Fixnum 7; cdr =
            Datum.Pair { car = Datum.Fixnum 9; cdr = Datum.Nil } } } } })
    (eval_port "(import (srfi 1)) (iota 5 1 2)")

let test_srfi1_fold () =
  check_datum "fold"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 1)) (fold + 0 '(1 2 3))")

let test_srfi1_unfold () =
  check_datum "unfold"
    (Datum.Pair { car = Datum.Fixnum 1; cdr =
      Datum.Pair { car = Datum.Fixnum 4; cdr =
        Datum.Pair { car = Datum.Fixnum 9; cdr =
          Datum.Pair { car = Datum.Fixnum 16; cdr =
            Datum.Pair { car = Datum.Fixnum 25; cdr = Datum.Nil } } } } })
    (eval_port "(import (srfi 1)) (unfold (lambda (x) (> x 5)) (lambda (x) (* x x)) (lambda (x) (+ x 1)) 1)")

let test_srfi1_filter () =
  check_datum "filter"
    (Datum.Pair { car = Datum.Fixnum 2; cdr =
      Datum.Pair { car = Datum.Fixnum 4; cdr = Datum.Nil } })
    (eval_port "(import (srfi 1)) (filter even? '(1 2 3 4 5))")

let test_srfi1_find () =
  check_datum "find"
    (Datum.Fixnum 4)
    (eval_port "(import (srfi 1)) (find even? '(1 3 4 5))")

let test_srfi1_any () =
  check_datum "any false"
    (Datum.Bool false)
    (eval_port "(import (srfi 1)) (any even? '(1 3 5))")

let test_srfi1_every () =
  check_datum "every true"
    (Datum.Bool true)
    (eval_port "(import (srfi 1)) (every odd? '(1 3 5))")

let test_srfi1_take () =
  check_datum "take"
    (Datum.Pair { car = Datum.Symbol "a"; cdr =
      Datum.Pair { car = Datum.Symbol "b"; cdr = Datum.Nil } })
    (eval_port "(import (srfi 1)) (take '(a b c d) 2)")

let test_srfi1_zip () =
  check_datum "zip"
    (Datum.Pair { car =
      Datum.Pair { car = Datum.Symbol "a"; cdr =
        Datum.Pair { car = Datum.Fixnum 1; cdr = Datum.Nil } }; cdr =
      Datum.Pair { car =
        Datum.Pair { car = Datum.Symbol "b"; cdr =
          Datum.Pair { car = Datum.Fixnum 2; cdr = Datum.Nil } }; cdr =
        Datum.Pair { car =
          Datum.Pair { car = Datum.Symbol "c"; cdr =
            Datum.Pair { car = Datum.Fixnum 3; cdr = Datum.Nil } }; cdr = Datum.Nil } } })
    (eval_port "(import (srfi 1)) (zip '(a b c) '(1 2 3))")

let test_srfi1_delete_duplicates () =
  (* delete-duplicates preserves first occurrence *)
  let result = eval_port "(import (srfi 1)) (length (delete-duplicates '(1 2 1 3 2)))" in
  check_datum "delete-duplicates length"
    (Datum.Fixnum 3)
    result

let test_srfi1_alist_cons () =
  check_datum "alist-cons"
    (Datum.Pair { car =
      Datum.Pair { car = Datum.Symbol "a"; cdr = Datum.Fixnum 1 }; cdr =
      Datum.Pair { car =
        Datum.Pair { car = Datum.Symbol "b"; cdr = Datum.Fixnum 2 }; cdr = Datum.Nil } })
    (eval_port "(import (srfi 1)) (alist-cons 'a 1 '((b . 2)))")

let test_srfi1_lset_union () =
  (* lset-union order is unspecified, just check length *)
  check_datum "lset-union length"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 1)) (length (lset-union eq? '(a b) '(b c)))")

let test_srfi1_partition () =
  (* partition returns two values *)
  check_datum "partition"
    (Datum.Pair { car = Datum.Fixnum 2; cdr =
      Datum.Pair { car = Datum.Fixnum 4; cdr = Datum.Nil } })
    (eval_port "(import (srfi 1)) \
     (call-with-values (lambda () (partition even? '(1 2 3 4 5))) \
       (lambda (yes no) yes))")

let test_srfi1_xcons () =
  check_datum "xcons"
    (Datum.Pair { car = Datum.Fixnum 2; cdr = Datum.Fixnum 1 })
    (eval_port "(import (srfi 1)) (xcons 1 2)")

(* ===== SRFI 69 — Basic Hash Tables ===== *)

let test_srfi69_make_and_predicate () =
  check_datum "make + hash-table?"
    (Datum.Bool true)
    (eval_port "(import (srfi 69)) (hash-table? (make-hash-table))")

let test_srfi69_predicate_false () =
  check_datum "hash-table? false"
    (Datum.Bool false)
    (eval_port "(import (srfi 69)) (hash-table? '())")

let test_srfi69_set_ref () =
  check_datum "set!/ref round-trip"
    (Datum.Fixnum 42)
    (eval_port "(import (srfi 69)) \
     (let ((ht (make-hash-table))) \
       (hash-table-set! ht 'a 42) \
       (hash-table-ref ht 'a))")

let test_srfi69_ref_default () =
  check_datum "ref/default"
    (Datum.Fixnum 99)
    (eval_port "(import (srfi 69)) \
     (let ((ht (make-hash-table))) \
       (hash-table-ref/default ht 'missing 99))")

let test_srfi69_ref_thunk () =
  check_datum "ref with failure thunk"
    (Datum.Symbol "not-found")
    (eval_port "(import (srfi 69)) \
     (let ((ht (make-hash-table))) \
       (hash-table-ref ht 'missing (lambda () 'not-found)))")

let test_srfi69_ref_missing_error () =
  Alcotest.check_raises "ref missing key raises"
    (Vm.Runtime_error "hash-table-ref: key not found")
    (fun () ->
      ignore (eval_port "(import (srfi 69)) \
       (let ((ht (make-hash-table))) \
         (hash-table-ref ht 'missing))"))

let test_srfi69_delete_exists () =
  check_datum "delete! + exists?"
    (Datum.Bool false)
    (eval_port "(import (srfi 69)) \
     (let ((ht (make-hash-table))) \
       (hash-table-set! ht 'a 1) \
       (hash-table-delete! ht 'a) \
       (hash-table-exists? ht 'a))")

let test_srfi69_size () =
  check_datum "size tracking"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 69)) \
     (let ((ht (make-hash-table))) \
       (hash-table-set! ht 'a 1) \
       (hash-table-set! ht 'b 2) \
       (hash-table-set! ht 'a 10) \
       (hash-table-delete! ht 'b) \
       (hash-table-size ht))")

let test_srfi69_update_default () =
  check_datum "update!/default"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 69)) \
     (let ((ht (make-hash-table))) \
       (hash-table-update!/default ht 'count (lambda (n) (+ n 1)) 0) \
       (hash-table-ref ht 'count))")

let test_srfi69_keys_values () =
  check_datum "keys length"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 69)) \
     (let ((ht (make-hash-table))) \
       (hash-table-set! ht 'a 1) \
       (hash-table-set! ht 'b 2) \
       (length (hash-table-keys ht)))")

let test_srfi69_walk () =
  check_datum "walk accumulates"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 69)) \
     (let ((ht (make-hash-table)) (total 0)) \
       (hash-table-set! ht 'a 1) \
       (hash-table-set! ht 'b 2) \
       (hash-table-walk ht (lambda (k v) (set! total (+ total v)))) \
       total)")

let test_srfi69_fold () =
  check_datum "fold sum"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 69)) \
     (let ((ht (make-hash-table))) \
       (hash-table-set! ht 'a 1) \
       (hash-table-set! ht 'b 2) \
       (hash-table-set! ht 'c 3) \
       (hash-table-fold ht (lambda (k v acc) (+ acc v)) 0))")

let test_srfi69_to_alist () =
  check_datum "->alist length"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 69)) \
     (let ((ht (make-hash-table))) \
       (hash-table-set! ht 'a 1) \
       (hash-table-set! ht 'b 2) \
       (length (hash-table->alist ht)))")

let test_srfi69_alist_to_ht () =
  check_datum "alist->hash-table"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 69)) \
     (let ((ht (alist->hash-table '((a . 1) (b . 2) (a . 99))))) \
       (hash-table-ref ht 'a))")

let test_srfi69_copy () =
  check_datum "copy independent"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 69)) \
     (let* ((ht (make-hash-table)) \
            (_ (hash-table-set! ht 'a 1)) \
            (ht2 (hash-table-copy ht))) \
       (hash-table-set! ht2 'a 99) \
       (hash-table-ref ht 'a))")

let test_srfi69_merge () =
  check_datum "merge!"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 69)) \
     (let ((ht1 (make-hash-table)) \
           (ht2 (make-hash-table))) \
       (hash-table-set! ht1 'a 1) \
       (hash-table-set! ht2 'b 2) \
       (hash-table-set! ht2 'c 3) \
       (hash-table-merge! ht1 ht2) \
       (hash-table-size ht1))")

let test_srfi69_hash () =
  check_datum "hash positive"
    (Datum.Bool true)
    (eval_port "(import (srfi 69)) (>= (hash 'hello) 0)")

let test_srfi69_string_hash () =
  check_datum "string-hash deterministic"
    (Datum.Bool true)
    (eval_port "(import (srfi 69)) (= (string-hash \"abc\") (string-hash \"abc\"))")

let test_srfi69_string_ci_hash () =
  check_datum "string-ci-hash case insensitive"
    (Datum.Bool true)
    (eval_port "(import (srfi 69)) (= (string-ci-hash \"ABC\") (string-ci-hash \"abc\"))")

let test_srfi69_hash_by_identity () =
  check_datum "hash-by-identity"
    (Datum.Bool true)
    (eval_port "(import (srfi 69)) (>= (hash-by-identity 42) 0)")

let test_srfi69_custom_equal_hash () =
  check_datum "custom equal?/hash"
    (Datum.Fixnum 10)
    (eval_port "(import (srfi 69)) \
     (let ((ht (make-hash-table string=? string-hash))) \
       (hash-table-set! ht \"key\" 10) \
       (hash-table-ref ht \"key\"))")

let test_srfi69_clear () =
  check_datum "clear!"
    (Datum.Fixnum 0)
    (eval_port "(import (srfi 69)) \
     (let ((ht (make-hash-table))) \
       (hash-table-set! ht 'a 1) \
       (hash-table-set! ht 'b 2) \
       (hash-table-clear! ht) \
       (hash-table-size ht))")

let test_srfi69_mutable () =
  check_datum "mutable?"
    (Datum.Bool true)
    (eval_port "(import (srfi 69)) (hash-table-mutable? (make-hash-table))")

let test_srfi69_cond_expand () =
  check_datum "cond-expand srfi-69"
    (Datum.Bool true)
    (eval "(cond-expand (srfi-69 #t) (else #f))")

(* ===== SRFI 125 — Intermediate Hash Tables ===== *)

let test_srfi125_constructor () =
  check_datum "hash-table constructor"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht (hash-table (make-default-comparator) 'a 1 'b 2))) \
       (hash-table-size ht))")

let test_srfi125_contains () =
  check_datum "hash-table-contains?"
    (Datum.Bool true)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht (hash-table (make-default-comparator) 'x 10))) \
       (hash-table-contains? ht 'x))")

let test_srfi125_empty () =
  check_datum "hash-table-empty?"
    (Datum.Bool true)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (hash-table-empty? (make-hash-table (make-default-comparator)))")

let test_srfi125_intern () =
  check_datum "hash-table-intern!"
    (Datum.Fixnum 42)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht (make-hash-table (make-default-comparator)))) \
       (hash-table-intern! ht 'x (lambda () 42)) \
       (hash-table-intern! ht 'x (lambda () 99)))")

let test_srfi125_pop () =
  check_datum "hash-table-pop! reduces size"
    (Datum.Fixnum 0)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht (hash-table (make-default-comparator) 'a 1))) \
       (hash-table-pop! ht) \
       (hash-table-size ht))")

let test_srfi125_for_each () =
  check_datum "hash-table-for-each"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht (hash-table (make-default-comparator) 'a 1 'b 2)) \
           (total 0)) \
       (hash-table-for-each (lambda (k v) (set! total (+ total v))) ht) \
       total)")

let test_srfi125_map () =
  check_datum "hash-table-map"
    (Datum.Fixnum 20)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let* ((cmp (make-default-comparator)) \
            (ht (hash-table cmp 'a 10)) \
            (ht2 (hash-table-map (lambda (v) (* v 2)) cmp ht))) \
       (hash-table-ref ht2 'a))")

let test_srfi125_map_to_list () =
  check_datum "hash-table-map->list"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht (hash-table (make-default-comparator) 'a 1))) \
       (length (hash-table-map->list (lambda (k v) (cons k v)) ht)))")

let test_srfi125_count () =
  check_datum "hash-table-count"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht (hash-table (make-default-comparator) 'a 1 'b 2 'c 3))) \
       (hash-table-count (lambda (k v) (< v 3)) ht))")

let test_srfi125_union () =
  check_datum "hash-table-union!"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht1 (hash-table (make-default-comparator) 'a 1 'b 2)) \
           (ht2 (hash-table (make-default-comparator) 'b 20 'c 3))) \
       (hash-table-union! ht1 ht2) \
       (hash-table-size ht1))")

let test_srfi125_intersection () =
  check_datum "hash-table-intersection!"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht1 (hash-table (make-default-comparator) 'a 1 'b 2)) \
           (ht2 (hash-table (make-default-comparator) 'b 20 'c 3))) \
       (hash-table-intersection! ht1 ht2) \
       (hash-table-size ht1))")

let test_srfi125_difference () =
  check_datum "hash-table-difference!"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht1 (hash-table (make-default-comparator) 'a 1 'b 2)) \
           (ht2 (hash-table (make-default-comparator) 'b 20 'c 3))) \
       (hash-table-difference! ht1 ht2) \
       (hash-table-size ht1))")

let test_srfi125_entries () =
  check_datum "hash-table-entries"
    (Datum.Fixnum 4)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht (hash-table (make-default-comparator) 'a 1 'b 2))) \
       (call-with-values (lambda () (hash-table-entries ht)) \
         (lambda (keys vals) (+ (length keys) (length vals)))))")

let test_srfi125_unfold () =
  check_datum "hash-table-unfold"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht (hash-table-unfold \
                 (lambda (s) (>= s 3)) \
                 (lambda (s) (values s (* s 10))) \
                 (lambda (s) (+ s 1)) \
                 0 \
                 (make-default-comparator)))) \
       (hash-table-size ht))"
    )

let test_srfi125_equal () =
  check_datum "hash-table=?"
    (Datum.Bool true)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let ((ht1 (hash-table (make-default-comparator) 'a 1 'b 2)) \
           (ht2 (hash-table (make-default-comparator) 'b 2 'a 1))) \
       (hash-table=? equal? ht1 ht2))")

let test_srfi125_empty_copy () =
  check_datum "hash-table-empty-copy"
    (Datum.Fixnum 0)
    (eval_port "(import (srfi 125) (srfi 128)) \
     (let* ((ht (hash-table (make-default-comparator) 'a 1 'b 2)) \
            (ht2 (hash-table-empty-copy ht))) \
       (hash-table-size ht2))")

(* ===== SRFI 14 — char-sets ===== *)

let test_srfi14_predicate () =
  check_datum "char-set?"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) (char-set? (char-set #\\a #\\b))")

let test_srfi14_predicate_false () =
  check_datum "char-set? false"
    (Datum.Bool false)
    (eval_port "(import (srfi 14)) (char-set? 42)")

let test_srfi14_contains () =
  check_datum "char-set-contains?"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) (char-set-contains? (char-set #\\a #\\b #\\c) #\\b)")

let test_srfi14_contains_false () =
  check_datum "char-set-contains? false"
    (Datum.Bool false)
    (eval_port "(import (srfi 14)) (char-set-contains? (char-set #\\a #\\b) #\\z)")

let test_srfi14_equal () =
  check_datum "char-set=?"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) (char-set=? (char-set #\\a #\\b) (char-set #\\b #\\a))")

let test_srfi14_subset () =
  check_datum "char-set<=?"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) (char-set<=? (char-set #\\a) (char-set #\\a #\\b))")

let test_srfi14_adjoin () =
  check_datum "char-set-adjoin"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) \
     (char-set-contains? (char-set-adjoin (char-set #\\a) #\\z) #\\z)")

let test_srfi14_delete () =
  check_datum "char-set-delete"
    (Datum.Bool false)
    (eval_port "(import (srfi 14)) \
     (char-set-contains? (char-set-delete (char-set #\\a #\\b) #\\b) #\\b)")

let test_srfi14_complement () =
  check_datum "char-set-complement"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) \
     (char-set-contains? (char-set-complement (char-set #\\a)) #\\z)")

let test_srfi14_complement_excludes () =
  check_datum "complement excludes"
    (Datum.Bool false)
    (eval_port "(import (srfi 14)) \
     (char-set-contains? (char-set-complement (char-set #\\a)) #\\a)")

let test_srfi14_union () =
  check_datum "char-set-union"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 14)) \
     (char-set-size (char-set-union (char-set #\\a #\\b) (char-set #\\b #\\c)))")

let test_srfi14_intersection () =
  check_datum "char-set-intersection"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 14)) \
     (char-set-size (char-set-intersection (char-set #\\a #\\b) (char-set #\\b #\\c)))")

let test_srfi14_difference () =
  check_datum "char-set-difference"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 14)) \
     (char-set-size (char-set-difference (char-set #\\a #\\b) (char-set #\\b #\\c)))")

let test_srfi14_xor () =
  check_datum "char-set-xor"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 14)) \
     (char-set-size (char-set-xor (char-set #\\a #\\b) (char-set #\\b #\\c)))")

let test_srfi14_list_roundtrip () =
  check_datum "list->char-set->list"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 14)) \
     (length (char-set->list (list->char-set '(#\\a #\\b #\\c))))")

let test_srfi14_string_roundtrip () =
  check_datum "string->char-set->string length"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 14)) \
     (string-length (char-set->string (string->char-set \"abc\")))")

let test_srfi14_ucs_range () =
  check_datum "ucs-range->char-set"
    (Datum.Fixnum 10)
    (eval_port "(import (srfi 14)) \
     (char-set-size (ucs-range->char-set 48 58))")

let test_srfi14_fold () =
  check_datum "char-set-fold"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 14)) \
     (char-set-fold (lambda (c n) (+ n 1)) 0 (char-set #\\a #\\b))")

let test_srfi14_count () =
  check_datum "char-set-count"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 14)) \
     (char-set-count (lambda (c) (char<=? #\\a c #\\a)) (char-set #\\a #\\b #\\c))")

let test_srfi14_every () =
  check_datum "char-set-every"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) \
     (char-set-every char-alphabetic? (char-set #\\a #\\b))")

let test_srfi14_any () =
  check_datum "char-set-any"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) \
     (char-set-any char-numeric? (char-set #\\a #\\1))")

let test_srfi14_filter () =
  check_datum "char-set-filter"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 14)) \
     (char-set-size (char-set-filter char-numeric? (char-set #\\a #\\1)))")

let test_srfi14_cursor () =
  check_datum "char-set-cursor"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) \
     (let ((cs (char-set #\\a #\\b))) \
       (let ((c (char-set-cursor cs))) \
         (and (not (end-of-char-set? c)) \
              (char-alphabetic? (char-set-ref cs c)))))")

let test_srfi14_size () =
  check_datum "char-set-size"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 14)) (char-set-size (char-set #\\x #\\y))")

let test_srfi14_letter () =
  check_datum "char-set:letter"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) \
     (and (char-set-contains? char-set:letter #\\A) \
          (char-set-contains? char-set:letter #\\z))")

let test_srfi14_digit () =
  check_datum "char-set:digit"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) \
     (and (char-set-contains? char-set:digit #\\0) \
          (char-set-contains? char-set:digit #\\9) \
          (not (char-set-contains? char-set:digit #\\a)))")

let test_srfi14_whitespace () =
  check_datum "char-set:whitespace"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) \
     (and (char-set-contains? char-set:whitespace #\\space) \
          (char-set-contains? char-set:whitespace #\\newline))")

let test_srfi14_empty () =
  check_datum "char-set:empty"
    (Datum.Fixnum 0)
    (eval_port "(import (srfi 14)) (char-set-size char-set:empty)")

let test_srfi14_full () =
  check_datum "char-set:full"
    (Datum.Fixnum 256)
    (eval_port "(import (srfi 14)) (char-set-size char-set:full)")

let test_srfi14_copy () =
  check_datum "char-set-copy"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) \
     (let ((a (char-set #\\x)) (b (char-set-copy (char-set #\\x)))) \
       (char-set=? a b))")

let test_srfi14_union_mut () =
  check_datum "char-set-union!"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 14)) \
     (let ((cs (char-set #\\a))) \
       (char-set-union! cs (char-set #\\b #\\c)) \
       (char-set-size cs))")

let test_srfi14_map () =
  check_datum "char-set-map"
    (Datum.Bool true)
    (eval_port "(import (srfi 14)) \
     (char-set-contains? \
       (char-set-map (lambda (c) (integer->char (+ (char->integer c) 1))) \
                     (char-set #\\a)) \
       #\\b)")

let test_srfi14_for_each () =
  check_datum "char-set-for-each"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 14)) \
     (let ((n 0)) \
       (char-set-for-each (lambda (c) (set! n (+ n 1))) (char-set #\\a #\\b)) \
       n)")

let test_srfi14_cond_expand () =
  check_datum "cond-expand srfi-14"
    (Datum.Bool true)
    (eval "(cond-expand (srfi-14 #t) (else #f))")

(* ===== SRFI 41 — streams ===== *)

let test_srfi41_null () =
  check_datum "stream-null?"
    (Datum.Bool true)
    (eval_port "(import (srfi 41)) (stream-null? stream-null)")

let test_srfi41_pair () =
  check_datum "stream-pair?"
    (Datum.Bool true)
    (eval_port "(import (srfi 41)) (stream-pair? (stream-cons 1 stream-null))")

let test_srfi41_car_cdr () =
  check_datum "stream-car/cdr"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 41)) \
     (let ((s (stream-cons 1 (stream-cons 2 stream-null)))) \
       (+ (stream-car s) (stream-car (stream-cdr s))))")

let test_srfi41_stream () =
  check_datum "stream macro"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 41)) \
     (let ((s (stream 1 2 3))) \
       (+ (stream-car s) (stream-car (stream-cdr s)) \
          (stream-car (stream-cdr (stream-cdr s)))))")

let test_srfi41_to_list () =
  check_datum "stream->list"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 41)) \
     (length (stream->list (stream 1 2 3)))")

let test_srfi41_list_to_stream () =
  check_datum "list->stream"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 41)) \
     (stream-car (list->stream '(1 2 3)))")

let test_srfi41_map () =
  check_datum "stream-map"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 41)) \
     (stream-car (stream-map (lambda (x) (* x 2)) (stream 1 2 3)))")

let test_srfi41_filter () =
  check_datum "stream-filter"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 41)) \
     (stream-car (stream-filter even? (stream 1 2 3 4)))")

let test_srfi41_fold () =
  check_datum "stream-fold"
    (Datum.Fixnum 10)
    (eval_port "(import (srfi 41)) \
     (stream-fold + 0 (stream 1 2 3 4))")

let test_srfi41_take () =
  check_datum "stream-take"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 41)) \
     (stream-length (stream-take 2 (stream 1 2 3 4 5)))")

let test_srfi41_drop () =
  check_datum "stream-drop"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 41)) \
     (stream-car (stream-drop 2 (stream 1 2 3 4 5)))")

let test_srfi41_range () =
  check_datum "stream-range"
    (Datum.Fixnum 5)
    (eval_port "(import (srfi 41)) \
     (stream-length (stream-take 5 (stream-range 0 10)))")

let test_srfi41_iterate () =
  check_datum "stream-iterate"
    (Datum.Fixnum 8)
    (eval_port "(import (srfi 41)) \
     (stream-car (stream-drop 3 (stream-iterate (lambda (x) (* x 2)) 1)))")

let test_srfi41_zip () =
  check_datum "stream-zip"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 41)) \
     (length (stream->list (stream-take 2 \
       (stream-zip (stream 1 2 3) (stream 4 5 6)))))")

let test_srfi41_append () =
  check_datum "stream-append"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 41)) \
     (stream-length (stream-append (stream 1 2 3) (stream 4 5 6)))")

let test_srfi41_take_while () =
  check_datum "stream-take-while"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 41)) \
     (stream-length (stream-take-while (lambda (x) (< x 4)) (stream 1 2 3 4 5)))")

let test_srfi41_drop_while () =
  check_datum "stream-drop-while"
    (Datum.Fixnum 4)
    (eval_port "(import (srfi 41)) \
     (stream-car (stream-drop-while (lambda (x) (< x 4)) (stream 1 2 3 4 5)))")

let test_srfi41_length () =
  check_datum "stream-length"
    (Datum.Fixnum 4)
    (eval_port "(import (srfi 41)) (stream-length (stream 1 2 3 4))")

let test_srfi41_scan () =
  check_datum "stream-scan"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 41)) \
     (stream-car (stream-drop 3 (stream-scan + 0 (stream 1 2 3))))")

let test_srfi41_unfold () =
  check_datum "stream-unfold"
    (Datum.Fixnum 5)
    (eval_port "(import (srfi 41)) \
     (stream-length (stream-unfold \
       (lambda (x) (< x 5)) \
       (lambda (x) x) \
       (lambda (x) (+ x 1)) \
       0))")

let test_srfi41_cond_expand () =
  check_datum "cond-expand srfi-41"
    (Datum.Bool true)
    (eval "(cond-expand (srfi-41 #t) (else #f))")

(* ===== SRFI 113 — sets and bags ===== *)

let test_srfi113_set_predicate () =
  check_datum "set?"
    (Datum.Bool true)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set? (set (make-default-comparator) 1 2 3))")

let test_srfi113_set_contains () =
  check_datum "set-contains?"
    (Datum.Bool true)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set-contains? (set (make-default-comparator) 1 2 3) 2)")

let test_srfi113_set_contains_false () =
  check_datum "set-contains? false"
    (Datum.Bool false)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set-contains? (set (make-default-comparator) 1 2 3) 5)")

let test_srfi113_set_empty () =
  check_datum "set-empty?"
    (Datum.Bool true)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set-empty? (set (make-default-comparator)))")

let test_srfi113_set_size () =
  check_datum "set-size"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set-size (set (make-default-comparator) 1 2 3))")

let test_srfi113_set_adjoin () =
  check_datum "set-adjoin"
    (Datum.Fixnum 4)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set-size (set-adjoin (set (make-default-comparator) 1 2 3) 4))")

let test_srfi113_set_delete () =
  check_datum "set-delete"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set-size (set-delete (set (make-default-comparator) 1 2 3) 2))")

let test_srfi113_set_union () =
  check_datum "set-union"
    (Datum.Fixnum 4)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (let ((c (make-default-comparator))) \
       (set-size (set-union (set c 1 2) (set c 3 4))))")

let test_srfi113_set_intersection () =
  check_datum "set-intersection"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (let ((c (make-default-comparator))) \
       (set-size (set-intersection (set c 1 2 3) (set c 2 4))))")

let test_srfi113_set_difference () =
  check_datum "set-difference"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (let ((c (make-default-comparator))) \
       (set-size (set-difference (set c 1 2 3) (set c 2))))")

let test_srfi113_set_xor () =
  check_datum "set-xor"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (let ((c (make-default-comparator))) \
       (set-size (set-xor (set c 1 2) (set c 2 3))))")

let test_srfi113_set_equal () =
  check_datum "set=?"
    (Datum.Bool true)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (let ((c (make-default-comparator))) \
       (set=? (set c 1 2 3) (set c 3 1 2)))")

let test_srfi113_set_subset () =
  check_datum "set<=?"
    (Datum.Bool true)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (let ((c (make-default-comparator))) \
       (set<=? (set c 1 2) (set c 1 2 3)))")

let test_srfi113_set_fold () =
  check_datum "set-fold"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set-fold + 0 (set (make-default-comparator) 1 2 3))")

let test_srfi113_set_map () =
  check_datum "set-map"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (let ((c (make-default-comparator))) \
       (set-size (set-map c (lambda (x) (* x 2)) (set c 1 2 3))))")

let test_srfi113_set_filter () =
  check_datum "set-filter"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set-size (set-filter even? (set (make-default-comparator) 1 2 3)))")

let test_srfi113_set_to_list () =
  check_datum "set->list"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (length (set->list (set (make-default-comparator) 1 2 3)))")

let test_srfi113_list_to_set () =
  check_datum "list->set"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set-size (list->set (make-default-comparator) '(1 2 3)))")

let test_srfi113_set_copy () =
  check_datum "set-copy"
    (Datum.Bool true)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (let ((s (set (make-default-comparator) 1 2 3))) \
       (set=? s (set-copy s)))")

let test_srfi113_set_disjoint () =
  check_datum "set-disjoint?"
    (Datum.Bool true)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (let ((c (make-default-comparator))) \
       (set-disjoint? (set c 1 2) (set c 3 4)))")

let test_srfi113_set_any () =
  check_datum "set-any?"
    (Datum.Bool true)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set-any? even? (set (make-default-comparator) 1 2 3))")

let test_srfi113_set_every () =
  check_datum "set-every?"
    (Datum.Bool false)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set-every? even? (set (make-default-comparator) 1 2 3))")

let test_srfi113_bag_predicate () =
  check_datum "bag?"
    (Datum.Bool true)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (bag? (bag (make-default-comparator) 1 1 2))")

let test_srfi113_bag_count () =
  check_datum "bag-element-count"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (bag-element-count (bag (make-default-comparator) 1 1 1 2) 1)")

let test_srfi113_bag_size () =
  check_datum "bag-size"
    (Datum.Fixnum 5)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (bag-size (bag (make-default-comparator) 1 1 1 2 2))")

let test_srfi113_bag_adjoin () =
  check_datum "bag-adjoin"
    (Datum.Fixnum 4)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (bag-element-count (bag-adjoin (bag (make-default-comparator) 1 1 1) 1) 1)")

let test_srfi113_bag_delete () =
  check_datum "bag-delete"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (bag-element-count (bag-delete (bag (make-default-comparator) 1 1 1) 1) 1)")

let test_srfi113_bag_to_set () =
  check_datum "bag->set"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (set-size (bag->set (bag (make-default-comparator) 1 1 2)))")

let test_srfi113_set_to_bag () =
  check_datum "set->bag"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (bag-element-count (set->bag (set (make-default-comparator) 1 2)) 1)")

let test_srfi113_bag_union () =
  check_datum "bag-union"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (let ((c (make-default-comparator))) \
       (bag-element-count (bag-union (bag c 1 1 1) (bag c 1 1)) 1))")

let test_srfi113_bag_increment () =
  check_datum "bag-increment!"
    (Datum.Fixnum 5)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (let ((b (bag (make-default-comparator) 1 1))) \
       (bag-increment! b 1 3) \
       (bag-element-count b 1))")

let test_srfi113_bag_alist () =
  check_datum "bag->alist length"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 113) (srfi 128)) \
     (length (bag->alist (bag (make-default-comparator) 1 1 2)))")

let test_srfi113_cond_expand () =
  check_datum "cond-expand srfi-113"
    (Datum.Bool true)
    (eval "(cond-expand (srfi-113 #t) (else #f))")

(* ===== SRFI 13 — string library ===== *)

let test_srfi13_null () =
  check_datum "string-null?"
    (Datum.Bool true)
    (eval_port "(import (srfi 13)) (string-null? \"\")")

let test_srfi13_null_false () =
  check_datum "string-null? false"
    (Datum.Bool false)
    (eval_port "(import (srfi 13)) (string-null? \"a\")")

let test_srfi13_every () =
  check_datum "string-every"
    (Datum.Bool true)
    (eval_port "(import (srfi 13)) (string-every char-alphabetic? \"abc\")")

let test_srfi13_every_false () =
  check_datum "string-every false"
    (Datum.Bool false)
    (eval_port "(import (srfi 13)) (string-every char-alphabetic? \"ab1\")")

let test_srfi13_any () =
  check_datum "string-any"
    (Datum.Bool true)
    (eval_port "(import (srfi 13)) (string-any char-numeric? \"ab1\")")

let test_srfi13_tabulate () =
  check_datum "string-tabulate"
    (Datum.Str (Bytes.of_string "abcde"))
    (eval_port "(import (srfi 13)) \
     (string-tabulate (lambda (i) (integer->char (+ i 97))) 5)")

let test_srfi13_take () =
  check_datum "string-take"
    (Datum.Str (Bytes.of_string "hel"))
    (eval_port "(import (srfi 13)) (string-take \"hello\" 3)")

let test_srfi13_take_right () =
  check_datum "string-take-right"
    (Datum.Str (Bytes.of_string "lo"))
    (eval_port "(import (srfi 13)) (string-take-right \"hello\" 2)")

let test_srfi13_drop () =
  check_datum "string-drop"
    (Datum.Str (Bytes.of_string "lo"))
    (eval_port "(import (srfi 13)) (string-drop \"hello\" 3)")

let test_srfi13_drop_right () =
  check_datum "string-drop-right"
    (Datum.Str (Bytes.of_string "hel"))
    (eval_port "(import (srfi 13)) (string-drop-right \"hello\" 2)")

let test_srfi13_pad () =
  check_datum "string-pad"
    (Datum.Str (Bytes.of_string "  hi"))
    (eval_port "(import (srfi 13)) (string-pad \"hi\" 4)")

let test_srfi13_pad_right () =
  check_datum "string-pad-right"
    (Datum.Str (Bytes.of_string "hi  "))
    (eval_port "(import (srfi 13)) (string-pad-right \"hi\" 4)")

let test_srfi13_trim () =
  check_datum "string-trim"
    (Datum.Str (Bytes.of_string "hello  "))
    (eval_port "(import (srfi 13)) (string-trim \"  hello  \")")

let test_srfi13_trim_right () =
  check_datum "string-trim-right"
    (Datum.Str (Bytes.of_string "  hello"))
    (eval_port "(import (srfi 13)) (string-trim-right \"  hello  \")")

let test_srfi13_trim_both () =
  check_datum "string-trim-both"
    (Datum.Str (Bytes.of_string "hello"))
    (eval_port "(import (srfi 13)) (string-trim-both \"  hello  \")")

let test_srfi13_prefix_length () =
  check_datum "string-prefix-length"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 13)) (string-prefix-length \"abcdef\" \"abcxyz\")")

let test_srfi13_suffix_length () =
  check_datum "string-suffix-length"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 13)) (string-suffix-length \"abcde\" \"xxcde\")")

let test_srfi13_prefix () =
  check_datum "string-prefix?"
    (Datum.Bool true)
    (eval_port "(import (srfi 13)) (string-prefix? \"abc\" \"abcdef\")")

let test_srfi13_suffix () =
  check_datum "string-suffix?"
    (Datum.Bool true)
    (eval_port "(import (srfi 13)) (string-suffix? \"def\" \"abcdef\")")

let test_srfi13_index () =
  check_datum "string-index"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 13)) (string-index char-numeric? \"ab3cd\")")

let test_srfi13_index_right () =
  check_datum "string-index-right"
    (Datum.Fixnum 4)
    (eval_port "(import (srfi 13)) (string-index-right char-numeric? \"ab3c4\")")

let test_srfi13_skip () =
  check_datum "string-skip"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 13)) (string-skip char-alphabetic? \"ab3cd\")")

let test_srfi13_contains () =
  check_datum "string-contains"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 13)) (string-contains \"hello world\" \"llo\")")

let test_srfi13_contains_ci () =
  check_datum "string-contains-ci"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 13)) (string-contains-ci \"hello WORLD\" \"LLO\")")

let test_srfi13_count () =
  check_datum "string-count"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 13)) (string-count char-numeric? \"a1b2c3\")")

let test_srfi13_reverse () =
  check_datum "string-reverse"
    (Datum.Str (Bytes.of_string "olleh"))
    (eval_port "(import (srfi 13)) (string-reverse \"hello\")")

let test_srfi13_concatenate () =
  check_datum "string-concatenate"
    (Datum.Str (Bytes.of_string "abcdef"))
    (eval_port "(import (srfi 13)) (string-concatenate '(\"abc\" \"def\"))")

let test_srfi13_fold () =
  check_datum "string-fold"
    (Datum.Fixnum 5)
    (eval_port "(import (srfi 13)) \
     (string-fold (lambda (c n) (+ n 1)) 0 \"hello\")")

let test_srfi13_fold_right () =
  check_datum "string-fold-right"
    (Datum.Fixnum 5)
    (eval_port "(import (srfi 13)) \
     (string-fold-right (lambda (c n) (+ n 1)) 0 \"hello\")")

let test_srfi13_filter () =
  check_datum "string-filter"
    (Datum.Str (Bytes.of_string "ace"))
    (eval_port "(import (srfi 13)) (string-filter char-alphabetic? \"a1c2e3\")")

let test_srfi13_delete () =
  check_datum "string-delete"
    (Datum.Str (Bytes.of_string "123"))
    (eval_port "(import (srfi 13)) (string-delete char-alphabetic? \"a1b2c3\")")

let test_srfi13_replace () =
  check_datum "string-replace"
    (Datum.Str (Bytes.of_string "aXYZef"))
    (eval_port "(import (srfi 13)) (string-replace \"abcdef\" \"XYZ\" 1 4)")

let test_srfi13_titlecase () =
  check_datum "string-titlecase"
    (Datum.Str (Bytes.of_string "Hello World"))
    (eval_port "(import (srfi 13)) (string-titlecase \"hello world\")")

let test_srfi13_xsubstring () =
  check_datum "xsubstring"
    (Datum.Str (Bytes.of_string "cdeab"))
    (eval_port "(import (srfi 13)) (xsubstring \"abcde\" 2 7)")

let test_srfi13_every_charset () =
  check_datum "string-every with char-set"
    (Datum.Bool true)
    (eval_port "(import (srfi 13) (srfi 14)) \
     (string-every char-set:letter \"abc\")")

let test_srfi13_trim_charset () =
  check_datum "string-trim with char-set"
    (Datum.Str (Bytes.of_string "hello"))
    (eval_port "(import (srfi 13) (srfi 14)) \
     (string-trim \"  hello\" char-set:whitespace)")

let test_srfi13_index_charset () =
  check_datum "string-index with char-set"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 13) (srfi 14)) \
     (string-index char-set:digit \"ab3cd\")")

let test_srfi13_cond_expand () =
  check_datum "cond-expand srfi-13"
    (Datum.Bool true)
    (eval "(cond-expand (srfi-13 #t) (else #f))")

let test_srfi13_concatenate_reverse () =
  check_datum "string-concatenate-reverse"
    (Datum.Str (Bytes.of_string "defabc"))
    (eval_port "(import (srfi 13)) (string-concatenate-reverse '(\"abc\" \"def\"))")

(* ===== SRFI 115 — regexp ===== *)

let test_srfi115_predicate () =
  check_datum "regexp?"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) (regexp? (regexp \"abc\"))")

let test_srfi115_predicate_false () =
  check_datum "regexp? false"
    (Datum.Bool false)
    (eval_port "(import (srfi 115)) (regexp? 42)")

let test_srfi115_matches_literal () =
  check_datum "regexp-matches? literal"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) (regexp-matches? \"abc\" \"abc\")")

let test_srfi115_matches_literal_false () =
  check_datum "regexp-matches? literal false"
    (Datum.Bool false)
    (eval_port "(import (srfi 115)) (regexp-matches? \"abc\" \"abcd\")")

let test_srfi115_search () =
  check_datum "regexp-search"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) (regexp-match? (regexp-search \"bc\" \"abcde\"))")

let test_srfi115_search_submatch () =
  check_datum "regexp-search submatch"
    (Datum.Str (Bytes.of_string "bc"))
    (eval_port "(import (srfi 115)) \
     (let ((m (regexp-search (regexp '(submatch \"bc\")) \"abcde\"))) \
       (regexp-match-submatch m 1))")

let test_srfi115_matches_obj () =
  check_datum "regexp-matches obj"
    (Datum.Str (Bytes.of_string "abc"))
    (eval_port "(import (srfi 115)) \
     (let ((m (regexp-matches \"abc\" \"abc\"))) \
       (regexp-match-submatch m 0))")

let test_srfi115_seq () =
  check_datum "SRE seq"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(: \"ab\" \"cd\") \"abcd\")")

let test_srfi115_or () =
  check_datum "SRE or"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(or \"abc\" \"def\") \"def\")")

let test_srfi115_star () =
  check_datum "SRE *"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(* \"a\") \"aaa\")")

let test_srfi115_plus () =
  check_datum "SRE +"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(+ \"a\") \"aaa\")")

let test_srfi115_plus_empty () =
  check_datum "SRE + empty fails"
    (Datum.Bool false)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(+ \"a\") \"\")")

let test_srfi115_question () =
  check_datum "SRE ?"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(: \"a\" (? \"b\") \"c\") \"ac\")")

let test_srfi115_any () =
  check_datum "SRE any"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(: \"a\" any \"c\") \"abc\")")

let test_srfi115_repeat_n () =
  check_datum "SRE = (exact count)"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(= 3 \"a\") \"aaa\")")

let test_srfi115_repeat_n_false () =
  check_datum "SRE = wrong count"
    (Datum.Bool false)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(= 3 \"a\") \"aa\")")

let test_srfi115_range () =
  check_datum "SRE ** (range)"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(** 2 4 \"a\") \"aaa\")")

let test_srfi115_alpha () =
  check_datum "SRE alphabetic"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(+ alphabetic) \"abcXYZ\")")

let test_srfi115_digit () =
  check_datum "SRE numeric"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(+ numeric) \"12345\")")

let test_srfi115_replace () =
  check_datum "regexp-replace"
    (Datum.Str (Bytes.of_string "xy23"))
    (eval_port "(import (srfi 115)) \
     (regexp-replace '(+ alphabetic) \"ab23\" \"xy\")")

let test_srfi115_replace_all () =
  check_datum "regexp-replace-all"
    (Datum.Str (Bytes.of_string "X1X2X3X"))
    (eval_port "(import (srfi 115)) \
     (regexp-replace-all '(+ alphabetic) \"a1bc2d3ef\" \"X\")")

let test_srfi115_extract () =
  check_datum "regexp-extract"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 115)) \
     (length (regexp-extract '(+ numeric) \"a12b34c56\"))")

let test_srfi115_split () =
  check_datum "regexp-split"
    (Datum.Fixnum 4)
    (eval_port "(import (srfi 115)) \
     (length (regexp-split \",\" \"a,b,c,d\"))")

let test_srfi115_match_count () =
  check_datum "regexp-match-count"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 115)) \
     (let ((m (regexp-matches '(: (submatch (+ alphabetic)) (+ numeric)) \"abc123\"))) \
       (regexp-match-count m))")

let test_srfi115_match_start () =
  check_datum "regexp-match-submatch-start"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 115)) \
     (let ((m (regexp-search '(submatch \"bc\") \"abcde\"))) \
       (regexp-match-submatch-start m 1))")

let test_srfi115_match_end () =
  check_datum "regexp-match-submatch-end"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 115)) \
     (let ((m (regexp-search '(submatch \"bc\") \"abcde\"))) \
       (regexp-match-submatch-end m 1))")

let test_srfi115_charset () =
  check_datum "SRE with char-set"
    (Datum.Bool true)
    (eval_port "(import (srfi 115) (srfi 14)) \
     (regexp-matches? (regexp char-set:digit) \"5\")")

let test_srfi115_cond_expand () =
  check_datum "cond-expand srfi-115"
    (Datum.Bool true)
    (eval "(cond-expand (srfi-115 #t) (else #f))")

let test_srfi115_nocase () =
  check_datum "SRE w/nocase"
    (Datum.Bool true)
    (eval_port "(import (srfi 115)) \
     (regexp-matches? '(w/nocase \"abc\") \"ABC\")")

let test_srfi115_fold () =
  check_datum "regexp-fold"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 115)) \
     (regexp-fold '(+ numeric) \
       (lambda (i m acc) (+ acc 1)) \
       0 \"a1b23c456\")")

(* ===== SRFI 145 — Assumptions ===== *)

let test_srfi145_truthy () =
  check_datum "assume truthy returns value"
    (Datum.Fixnum 42)
    (eval_port "(import (srfi 145)) (assume 42)")

let test_srfi145_true () =
  check_datum "assume #t"
    (Datum.Bool true)
    (eval_port "(import (srfi 145)) (assume #t)")

let test_srfi145_expr () =
  check_datum "assume expression"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 145)) (assume (+ 1 2))")

let test_srfi145_false () =
  check_datum "assume #f raises"
    (Datum.Bool true)
    (eval_port "(import (srfi 145)) \
     (guard (e (#t #t)) (assume #f))")

let test_srfi145_message () =
  check_datum "assume false with objs"
    (Datum.Bool true)
    (eval_port "(import (srfi 145)) \
     (guard (e (#t #t)) (assume #f \"should fail\"))")

(* ===== SRFI 156 — Syntactic combiners ===== *)

let test_srfi156_is () =
  check_datum "is predicate"
    (Datum.Bool true)
    (eval_port "(import (srfi 156)) (is 3 odd?)")

let test_srfi156_is_equal () =
  check_datum "is with equal?"
    (Datum.Bool true)
    (eval_port "(import (srfi 156)) (is 5 = 5)")

let test_srfi156_isnt () =
  check_datum "isnt predicate"
    (Datum.Bool true)
    (eval_port "(import (srfi 156)) (isnt 4 odd?)")

let test_srfi156_is_binary () =
  check_datum "is binary comparison"
    (Datum.Bool true)
    (eval_port "(import (srfi 156)) (is 3 < 10)")

let test_srfi156_filter () =
  check_datum "is with filter"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 156) (srfi 1)) \
     (length (filter (is _ even?) '(1 2 3 4)))")

(* ===== SRFI 219 — Define higher-order lambda ===== *)

let test_srfi219_basic () =
  check_datum "define-curried basic"
    (Datum.Fixnum 7)
    (eval_port "(import (srfi 219)) \
     (define-curried ((adder x) y) (+ x y)) \
     ((adder 3) 4)")

let test_srfi219_triple () =
  check_datum "define-curried triple nesting"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 219)) \
     (define-curried (((f a) b) c) (+ a b c)) \
     (((f 1) 2) 3)")

let test_srfi219_normal () =
  check_datum "define-curried normal define still works"
    (Datum.Fixnum 10)
    (eval_port "(import (srfi 219)) \
     (define (double x) (* 2 x)) \
     (double 5)")

(* ===== SRFI 223 — Generalized binary search ===== *)

let test_srfi223_bisect_left () =
  check_datum "bisect-left"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 223)) \
     (bisect-left #(1 3 5 7 9) 5 vector-ref < 0 5)")

let test_srfi223_bisect_right () =
  check_datum "bisect-right"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 223)) \
     (bisect-right #(1 3 5 7 9) 5 vector-ref < 0 5)")

let test_srfi223_vector_bisect_left () =
  check_datum "vector-bisect-left"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 223)) \
     (vector-bisect-left #(1 3 5 7 9) 5 <)")

let test_srfi223_vector_bisect_right () =
  check_datum "vector-bisect-right"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 223)) \
     (vector-bisect-right #(1 3 5 7 9) 5 <)")

(* ===== SRFI 175 — ASCII character library ===== *)

let test_srfi175_upper () =
  check_datum "ascii-upper-case?"
    (Datum.Bool true)
    (eval_port "(import (srfi 175)) (ascii-upper-case? #\\A)")

let test_srfi175_lower () =
  check_datum "ascii-lower-case?"
    (Datum.Bool true)
    (eval_port "(import (srfi 175)) (ascii-lower-case? #\\z)")

let test_srfi175_digit () =
  check_datum "ascii-numeric?"
    (Datum.Bool true)
    (eval_port "(import (srfi 175)) (ascii-numeric? #\\5)")

let test_srfi175_upcase () =
  check_datum "ascii-upcase"
    (Datum.Fixnum 65)
    (eval_port "(import (srfi 175)) \
     (char->integer (ascii-upcase #\\a))")

let test_srfi175_downcase () =
  check_datum "ascii-downcase"
    (Datum.Fixnum 97)
    (eval_port "(import (srfi 175)) \
     (char->integer (ascii-downcase #\\A))")

let test_srfi175_ci_eq () =
  check_datum "ascii-ci=?"
    (Datum.Bool true)
    (eval_port "(import (srfi 175)) (ascii-ci=? #\\a #\\A)")

let test_srfi175_alphanumeric () =
  check_datum "ascii-alphanumeric?"
    (Datum.Bool true)
    (eval_port "(import (srfi 175)) (ascii-alphanumeric? #\\a)")

let test_srfi175_non_alphanumeric () =
  check_datum "ascii-alphanumeric? space"
    (Datum.Bool false)
    (eval_port "(import (srfi 175)) (ascii-alphanumeric? #\\space)")

(* ===== SRFI 162 — Comparators sublibrary ===== *)

let test_srfi162_default () =
  check_datum "default-comparator via 162"
    (Datum.Bool true)
    (eval_port "(import (srfi 162)) \
     (comparator? default-comparator)")

let test_srfi162_boolean () =
  check_datum "boolean-comparator"
    (Datum.Bool true)
    (eval_port "(import (srfi 162)) \
     (comparator? boolean-comparator)")

let test_srfi162_char () =
  check_datum "char-comparator ordering"
    (Datum.Bool true)
    (eval_port "(import (srfi 162) (srfi 128)) \
     (<? char-comparator #\\a #\\b)")

let test_srfi162_string () =
  check_datum "string-comparator ordering"
    (Datum.Bool true)
    (eval_port "(import (srfi 162) (srfi 128)) \
     (<? string-comparator \"abc\" \"abd\")")

let test_srfi162_real () =
  check_datum "real-comparator"
    (Datum.Bool true)
    (eval_port "(import (srfi 162) (srfi 128)) \
     (=? real-comparator 3 3)")

(* ===== SRFI 228 — Composing Comparators ===== *)

let test_srfi228_wrapper () =
  check_datum "make-wrapper-comparator"
    (Datum.Bool true)
    (eval_port "(import (srfi 228) (srfi 128) (srfi 162)) \
     (let ((c (make-wrapper-comparator number? \
                number->string string-comparator))) \
       (=? c 42 42))")

let test_srfi228_product () =
  check_datum "make-product-comparator"
    (Datum.Bool true)
    (eval_port "(import (srfi 228) (srfi 128) (srfi 162)) \
     (let ((c (make-product-comparator \
                (make-wrapper-comparator pair? car real-comparator) \
                (make-wrapper-comparator pair? cdr real-comparator)))) \
       (=? c (cons 1 2) (cons 1 2)))")

let test_srfi228_sum () =
  check_datum "make-sum-comparator"
    (Datum.Bool true)
    (eval_port "(import (srfi 228) (srfi 128) (srfi 162)) \
     (let ((c (make-sum-comparator \
                real-comparator \
                string-comparator))) \
       (and (=? c 3 3) (=? c \"a\" \"a\")))")

(* ===== SRFI 195 — Multiple-value boxes ===== *)

let test_srfi195_box () =
  check_datum "box and unbox"
    (Datum.Fixnum 42)
    (eval_port "(import (srfi 195)) (unbox (box 42))")

let test_srfi195_arity () =
  check_datum "box-arity"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 195)) (box-arity (box 1 2 3))")

let test_srfi195_unbox_value () =
  check_datum "unbox-value"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 195)) (unbox-value (box 1 2 3) 1)")

let test_srfi195_set_box () =
  check_datum "set-box!"
    (Datum.Fixnum 99)
    (eval_port "(import (srfi 195)) \
     (let ((b (box 1))) (set-box! b 99) (unbox b))")

let test_srfi195_predicate () =
  check_datum "box?"
    (Datum.Bool true)
    (eval_port "(import (srfi 195)) (box? (box 1))")

(* ===== SRFI 48 — Intermediate format strings ===== *)

let test_srfi48_basic () =
  check_datum "format ~a"
    (Datum.Str (Bytes.of_string "hello world"))
    (eval_port "(import (srfi 48)) (format \"~a ~a\" \"hello\" \"world\")")

let test_srfi48_decimal () =
  check_datum "format ~d"
    (Datum.Str (Bytes.of_string "42"))
    (eval_port "(import (srfi 48)) (format \"~d\" 42)")

let test_srfi48_hex () =
  check_datum "format ~x"
    (Datum.Str (Bytes.of_string "ff"))
    (eval_port "(import (srfi 48)) (format \"~x\" 255)")

let test_srfi48_octal () =
  check_datum "format ~o"
    (Datum.Str (Bytes.of_string "77"))
    (eval_port "(import (srfi 48)) (format \"~o\" 63)")

let test_srfi48_binary () =
  check_datum "format ~b"
    (Datum.Str (Bytes.of_string "1010"))
    (eval_port "(import (srfi 48)) (format \"~b\" 10)")

let test_srfi48_tilde () =
  check_datum "format ~~"
    (Datum.Str (Bytes.of_string "a~b"))
    (eval_port "(import (srfi 48)) (format \"a~~b\")")

let test_srfi48_newline () =
  check_datum "format ~%"
    (Datum.Str (Bytes.of_string "a\nb"))
    (eval_port "(import (srfi 48)) (format \"a~%b\")")

(* ===== SRFI 117 — Queues based on lists ===== *)

let test_srfi117_make () =
  check_datum "list-queue-front"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 117)) \
     (list-queue-front (list-queue 1 2 3))")

let test_srfi117_back () =
  check_datum "list-queue-back"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 117)) \
     (list-queue-back (list-queue 1 2 3))")

let test_srfi117_empty () =
  check_datum "list-queue-empty?"
    (Datum.Bool true)
    (eval_port "(import (srfi 117)) \
     (list-queue-empty? (make-list-queue '()))")

let test_srfi117_add_front () =
  check_datum "list-queue-add-front!"
    (Datum.Fixnum 0)
    (eval_port "(import (srfi 117)) \
     (let ((q (list-queue 1 2))) \
       (list-queue-add-front! q 0) \
       (list-queue-front q))")

let test_srfi117_add_back () =
  check_datum "list-queue-add-back!"
    (Datum.Fixnum 9)
    (eval_port "(import (srfi 117)) \
     (let ((q (list-queue 1 2))) \
       (list-queue-add-back! q 9) \
       (list-queue-back q))")

let test_srfi117_remove_front () =
  check_datum "list-queue-remove-front!"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 117)) \
     (let ((q (list-queue 1 2 3))) \
       (list-queue-remove-front! q))")

let test_srfi117_list () =
  check_datum "list-queue-list"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 117)) \
     (apply + (list-queue-list (list-queue 1 2 3)))")

(* ===== SRFI 234 — Topological sorting ===== *)

let test_srfi234_basic () =
  check_datum "topological-sort basic"
    (Datum.Str (Bytes.of_string "(a b c)"))
    (eval_port "(import (srfi 234) (scheme write)) \
     (let ((graph '((a b c) (b c) (c)))) \
       (let ((p (open-output-string))) \
         (write (topological-sort graph) p) \
         (get-output-string p)))")

let test_srfi234_edgelist () =
  check_datum "edgelist->graph"
    (Datum.Bool true)
    (eval_port "(import (srfi 234)) \
     (let ((g (edgelist->graph '((a b) (b c))))) \
       (pair? g))")

let test_srfi234_graph_edgelist () =
  check_datum "graph->edgelist"
    (Datum.Bool true)
    (eval_port "(import (srfi 234)) \
     (let ((el (graph->edgelist '((a b c) (b c))))) \
       (list? el))")

let test_srfi234_connected () =
  check_datum "connected-components"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 234)) \
     (length (connected-components \
       '((a b) (b) (c d) (d))))")

(* ===== SRFI 235 — Combinators ===== *)

let test_srfi235_constantly () =
  check_datum "constantly"
    (Datum.Fixnum 5)
    (eval_port "(import (srfi 235)) ((constantly 5) 1 2 3)")

let test_srfi235_complement () =
  check_datum "complement"
    (Datum.Bool true)
    (eval_port "(import (srfi 235)) ((complement even?) 3)")

let test_srfi235_apply_chain () =
  check_datum "apply-chain"
    (Datum.Fixnum 8)
    (eval_port "(import (srfi 235)) \
     ((apply-chain (lambda (x) (+ x 2)) (lambda (x) (* x 2))) 3)")

let test_srfi235_flip () =
  check_datum "flip"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 235)) ((flip -) 3 5)")

let test_srfi235_on () =
  check_datum "on"
    (Datum.Bool true)
    (eval_port "(import (srfi 235)) \
     ((on = string-length string-length) \"abc\" \"xyz\")")

let test_srfi235_conjoin () =
  check_datum "conjoin"
    (Datum.Bool true)
    (eval_port "(import (srfi 235)) \
     ((conjoin positive? odd?) 3)")

let test_srfi235_disjoin () =
  check_datum "disjoin"
    (Datum.Bool true)
    (eval_port "(import (srfi 235)) \
     ((disjoin even? negative?) -3)")

let test_srfi235_each_of () =
  check_datum "each-of"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 235)) \
     (let ((a 0)) \
       ((each-of (lambda (x) (set! a (+ a x))) \
                 (lambda (x) (set! a (+ a x))) \
                 (lambda (x) (set! a (+ a x)))) 1) \
       a)")

(* ===== SRFI 158 — Generators and accumulators ===== *)

let test_srfi158_generator () =
  check_datum "generator basics"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 158)) \
     (let ((g (generator 1 2 3))) (g))")

let test_srfi158_eof () =
  check_datum "generator eof"
    (Datum.Bool true)
    (eval_port "(import (srfi 158)) \
     (let ((g (generator 1))) (g) (eof-object? (g)))")

let test_srfi158_circular () =
  check_datum "circular-generator"
    (Datum.Fixnum 1)
    (eval_port "(import (srfi 158)) \
     (let ((g (circular-generator 1 2 3))) (g) (g) (g) (g))")

let test_srfi158_make_iota () =
  check_datum "make-iota-generator"
    (Datum.Fixnum 15)
    (eval_port "(import (srfi 158)) \
     (let ((g (make-iota-generator 6))) \
       (let loop ((sum 0)) \
         (let ((v (g))) \
           (if (eof-object? v) sum \
               (loop (+ sum v))))))")

let test_srfi158_gmap () =
  check_datum "gmap"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 158)) \
     (let ((g (gmap (lambda (x) (* x 2)) (generator 1 2 3)))) (g))")

let test_srfi158_gfilter () =
  check_datum "gfilter"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 158)) \
     (let ((g (gfilter even? (generator 1 2 3 4)))) (g))")

let test_srfi158_gtake () =
  check_datum "generator->list with gtake"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 158)) \
     (length (generator->list (gtake (generator 1 2 3 4 5) 3)))")

let test_srfi158_accumulator () =
  check_datum "count-accumulator"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 158)) \
     (let ((a (count-accumulator))) (a 1) (a 2) (a 3) (a (eof-object)))")

let test_srfi158_list_accumulator () =
  check_datum "list-accumulator"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 158)) \
     (let ((a (list-accumulator))) (a 1) (a 2) (a 3) \
       (apply + (a (eof-object))))")

(* ===== SRFI 214 — Flexvectors ===== *)

let test_srfi214_make () =
  check_datum "flexvector-ref"
    (Datum.Fixnum 20)
    (eval_port "(import (srfi 214)) \
     (flexvector-ref (flexvector 10 20 30) 1)")

let test_srfi214_length () =
  check_datum "flexvector-length"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 214)) \
     (flexvector-length (flexvector 1 2 3))")

let test_srfi214_add () =
  check_datum "flexvector-add-back!"
    (Datum.Fixnum 4)
    (eval_port "(import (srfi 214)) \
     (let ((fv (flexvector 1 2 3))) \
       (flexvector-add-back! fv 4) \
       (flexvector-length fv))")

let test_srfi214_set () =
  check_datum "flexvector-set!"
    (Datum.Fixnum 99)
    (eval_port "(import (srfi 214)) \
     (let ((fv (flexvector 1 2 3))) \
       (flexvector-set! fv 0 99) \
       (flexvector-ref fv 0))")

let test_srfi214_predicate () =
  check_datum "flexvector?"
    (Datum.Bool true)
    (eval_port "(import (srfi 214)) (flexvector? (flexvector))")

let test_srfi214_remove () =
  check_datum "flexvector-remove!"
    (Datum.Fixnum 2)
    (eval_port "(import (srfi 214)) \
     (let ((fv (flexvector 1 2 3))) \
       (flexvector-remove! fv 0) \
       (flexvector-length fv))")

let test_srfi214_to_vector () =
  check_datum "flexvector->vector"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 214)) \
     (let ((v (flexvector->vector (flexvector 1 2 3)))) \
       (+ (vector-ref v 0) (vector-ref v 1) (vector-ref v 2)))")

(* ===== SRFI 189 — Maybe and Either ===== *)

let test_srfi189_just () =
  check_datum "just?"
    (Datum.Bool true)
    (eval_port "(import (srfi 189)) (just? (just 42))")

let test_srfi189_nothing () =
  check_datum "nothing?"
    (Datum.Bool true)
    (eval_port "(import (srfi 189)) (nothing? (nothing))")

let test_srfi189_maybe_ref () =
  check_datum "maybe-ref"
    (Datum.Fixnum 42)
    (eval_port "(import (srfi 189)) \
     (maybe-ref (just 42) (lambda () 0) (lambda (x) x))")

let test_srfi189_maybe_ref_nothing () =
  check_datum "maybe-ref nothing"
    (Datum.Fixnum 0)
    (eval_port "(import (srfi 189)) \
     (maybe-ref (nothing) (lambda () 0) (lambda (x) x))")

let test_srfi189_right () =
  check_datum "right?"
    (Datum.Bool true)
    (eval_port "(import (srfi 189)) (right? (right 1))")

let test_srfi189_left () =
  check_datum "left?"
    (Datum.Bool true)
    (eval_port "(import (srfi 189)) (left? (left 1))")

let test_srfi189_maybe_map () =
  check_datum "maybe-map"
    (Datum.Fixnum 6)
    (eval_port "(import (srfi 189)) \
     (maybe-ref (maybe-map (lambda (x) (* x 2)) (just 3)) \
       (lambda () 0) (lambda (x) x))")

let test_srfi189_maybe_map_nothing () =
  check_datum "maybe-map nothing"
    (Datum.Bool true)
    (eval_port "(import (srfi 189)) \
     (nothing? (maybe-map (lambda (x) (* x 2)) (nothing)))")

(* ===== SRFI 210 — Procedures and syntax for multiple values ===== *)

let test_srfi210_coarity () =
  check_datum "coarity"
    (Datum.Fixnum 3)
    (eval_port "(import (srfi 210)) \
     (coarity (lambda () (values 1 2 3)))")

let test_srfi210_identity () =
  check_datum "identity"
    (Datum.Fixnum 42)
    (eval_port "(import (srfi 210)) (identity 42)")

let test_srfi210_compose_left () =
  check_datum "compose-left"
    (Datum.Fixnum 10)
    (eval_port "(import (srfi 210)) \
     ((compose-left (lambda (x) (+ x 2)) (lambda (x) (* x 2))) 3)")

let test_srfi210_compose_right () =
  check_datum "compose-right"
    (Datum.Fixnum 8)
    (eval_port "(import (srfi 210)) \
     ((compose-right (lambda (x) (+ x 2)) (lambda (x) (* x 2))) 3)")

let test_srfi210_with_values () =
  check_datum "with-values"
    (Datum.Fixnum 5)
    (eval_port "(import (srfi 210)) \
     (with-values (lambda () (values 2 3)) +)")

let () =
  Alcotest.run "SRFI"
    [ ("infrastructure",
       [ Alcotest.test_case "import bundled SRFI" `Quick test_import_bundled_srfi
       ; Alcotest.test_case "cond-expand srfi feature" `Quick test_cond_expand_srfi_feature
       ; Alcotest.test_case "unknown SRFI feature" `Quick test_unknown_srfi_feature
       ; Alcotest.test_case "bundled features nonempty" `Quick test_bundled_features_nonempty
       ; Alcotest.test_case "srfi-151 feature" `Quick test_srfi_151_feature
       ])
    ; ("srfi-8",
       [ Alcotest.test_case "basic" `Quick test_srfi8_basic
       ; Alcotest.test_case "rest args" `Quick test_srfi8_rest_args
       ; Alcotest.test_case "single" `Quick test_srfi8_single
       ])
    ; ("srfi-16",
       [ Alcotest.test_case "two args" `Quick test_srfi16_basic
       ; Alcotest.test_case "one arg" `Quick test_srfi16_one_arg
       ; Alcotest.test_case "zero args" `Quick test_srfi16_zero_args
       ])
    ; ("srfi-26",
       [ Alcotest.test_case "cut basic" `Quick test_srfi26_cut_basic
       ; Alcotest.test_case "cut rest" `Quick test_srfi26_cut_rest
       ; Alcotest.test_case "cut no slots" `Quick test_srfi26_cut_no_slots
       ])
    ; ("srfi-28",
       [ Alcotest.test_case "format basic" `Quick test_srfi28_basic
       ; Alcotest.test_case "format tilde" `Quick test_srfi28_tilde
       ; Alcotest.test_case "format write" `Quick test_srfi28_write
       ])
    ; ("srfi-31",
       [ Alcotest.test_case "factorial" `Quick test_srfi31_basic
       ; Alcotest.test_case "value" `Quick test_srfi31_value
       ; Alcotest.test_case "list" `Quick test_srfi31_list
       ])
    ; ("srfi-111",
       [ Alcotest.test_case "unbox" `Quick test_srfi111_basic
       ; Alcotest.test_case "set-box!" `Quick test_srfi111_set
       ; Alcotest.test_case "box?" `Quick test_srfi111_pred
       ])
    ; ("let-values",
       [ Alcotest.test_case "simple" `Quick test_let_values_simple
       ; Alcotest.test_case "multiple" `Quick test_let_values_multiple
       ; Alcotest.test_case "rest" `Quick test_let_values_rest
       ; Alcotest.test_case "single var" `Quick test_let_values_single
       ; Alcotest.test_case "let*-values" `Quick test_let_star_values
       ; Alcotest.test_case "nested" `Quick test_let_values_nested
       ; Alcotest.test_case "empty" `Quick test_let_values_empty
       ; Alcotest.test_case "srfi 11 import" `Quick test_srfi11_import
       ])
    ; ("srfi-2",
       [ Alcotest.test_case "basic" `Quick test_srfi2_basic
       ; Alcotest.test_case "false" `Quick test_srfi2_false
       ; Alcotest.test_case "test clause" `Quick test_srfi2_test_clause
       ; Alcotest.test_case "empty" `Quick test_srfi2_empty
       ])
    ; ("srfi-151",
       [ Alcotest.test_case "bitwise-and" `Quick test_srfi151_and
       ; Alcotest.test_case "bitwise-ior" `Quick test_srfi151_ior
       ; Alcotest.test_case "bitwise-xor" `Quick test_srfi151_xor
       ; Alcotest.test_case "bitwise-not" `Quick test_srfi151_not
       ; Alcotest.test_case "arithmetic-shift left" `Quick test_srfi151_shift
       ; Alcotest.test_case "arithmetic-shift right" `Quick test_srfi151_shift_right
       ; Alcotest.test_case "bit-count" `Quick test_srfi151_bit_count
       ; Alcotest.test_case "integer-length" `Quick test_srfi151_integer_length
       ; Alcotest.test_case "bit-set?" `Quick test_srfi151_bit_set
       ; Alcotest.test_case "first-set-bit" `Quick test_srfi151_first_set_bit
       ; Alcotest.test_case "bit-field" `Quick test_srfi151_bit_field
       ; Alcotest.test_case "bit-field-replace" `Quick test_srfi151_bit_field_replace
       ])
    ; ("srfi-128",
       [ Alcotest.test_case "make-default-comparator" `Quick test_srfi128_make_default
       ; Alcotest.test_case "=?" `Quick test_srfi128_equal
       ; Alcotest.test_case "<?" `Quick test_srfi128_less
       ; Alcotest.test_case ">?" `Quick test_srfi128_greater
       ; Alcotest.test_case "string-hash" `Quick test_srfi128_string_hash
       ])
    ; ("srfi-132",
       [ Alcotest.test_case "list-sort" `Quick test_srfi132_list_sort
       ; Alcotest.test_case "list-sorted?" `Quick test_srfi132_list_sorted
       ; Alcotest.test_case "list-sorted? false" `Quick test_srfi132_list_sorted_false
       ; Alcotest.test_case "vector-sort" `Quick test_srfi132_vector_sort
       ; Alcotest.test_case "list-merge" `Quick test_srfi132_list_merge
       ])
    ; ("srfi-133",
       [ Alcotest.test_case "vector-fold" `Quick test_srfi133_vector_fold
       ; Alcotest.test_case "vector-index" `Quick test_srfi133_vector_index
       ; Alcotest.test_case "vector-any false" `Quick test_srfi133_vector_any
       ; Alcotest.test_case "vector-any true" `Quick test_srfi133_vector_any_true
       ; Alcotest.test_case "vector-every" `Quick test_srfi133_vector_every
       ; Alcotest.test_case "vector-unfold" `Quick test_srfi133_vector_unfold
       ; Alcotest.test_case "vector-swap!" `Quick test_srfi133_vector_swap
       ; Alcotest.test_case "vector-reverse!" `Quick test_srfi133_vector_reverse
       ; Alcotest.test_case "vector-empty?" `Quick test_srfi133_vector_empty
       ; Alcotest.test_case "vector-count" `Quick test_srfi133_vector_count
       ; Alcotest.test_case "vector-partition" `Quick test_srfi133_vector_partition
       ; Alcotest.test_case "vector-binary-search" `Quick test_srfi133_binary_search
       ])
    ; ("srfi-1",
       [ Alcotest.test_case "iota" `Quick test_srfi1_iota
       ; Alcotest.test_case "iota start step" `Quick test_srfi1_iota_start_step
       ; Alcotest.test_case "fold" `Quick test_srfi1_fold
       ; Alcotest.test_case "unfold" `Quick test_srfi1_unfold
       ; Alcotest.test_case "filter" `Quick test_srfi1_filter
       ; Alcotest.test_case "find" `Quick test_srfi1_find
       ; Alcotest.test_case "any" `Quick test_srfi1_any
       ; Alcotest.test_case "every" `Quick test_srfi1_every
       ; Alcotest.test_case "take" `Quick test_srfi1_take
       ; Alcotest.test_case "zip" `Quick test_srfi1_zip
       ; Alcotest.test_case "delete-duplicates" `Quick test_srfi1_delete_duplicates
       ; Alcotest.test_case "alist-cons" `Quick test_srfi1_alist_cons
       ; Alcotest.test_case "lset-union" `Quick test_srfi1_lset_union
       ; Alcotest.test_case "partition" `Quick test_srfi1_partition
       ; Alcotest.test_case "xcons" `Quick test_srfi1_xcons
       ])
    ; ("srfi-69",
       [ Alcotest.test_case "make + hash-table?" `Quick test_srfi69_make_and_predicate
       ; Alcotest.test_case "hash-table? false" `Quick test_srfi69_predicate_false
       ; Alcotest.test_case "set!/ref" `Quick test_srfi69_set_ref
       ; Alcotest.test_case "ref/default" `Quick test_srfi69_ref_default
       ; Alcotest.test_case "ref thunk" `Quick test_srfi69_ref_thunk
       ; Alcotest.test_case "ref missing error" `Quick test_srfi69_ref_missing_error
       ; Alcotest.test_case "delete! + exists?" `Quick test_srfi69_delete_exists
       ; Alcotest.test_case "size tracking" `Quick test_srfi69_size
       ; Alcotest.test_case "update!/default" `Quick test_srfi69_update_default
       ; Alcotest.test_case "keys/values" `Quick test_srfi69_keys_values
       ; Alcotest.test_case "walk" `Quick test_srfi69_walk
       ; Alcotest.test_case "fold" `Quick test_srfi69_fold
       ; Alcotest.test_case "->alist" `Quick test_srfi69_to_alist
       ; Alcotest.test_case "alist->hash-table" `Quick test_srfi69_alist_to_ht
       ; Alcotest.test_case "copy" `Quick test_srfi69_copy
       ; Alcotest.test_case "merge!" `Quick test_srfi69_merge
       ; Alcotest.test_case "hash" `Quick test_srfi69_hash
       ; Alcotest.test_case "string-hash" `Quick test_srfi69_string_hash
       ; Alcotest.test_case "string-ci-hash" `Quick test_srfi69_string_ci_hash
       ; Alcotest.test_case "hash-by-identity" `Quick test_srfi69_hash_by_identity
       ; Alcotest.test_case "custom equal/hash" `Quick test_srfi69_custom_equal_hash
       ; Alcotest.test_case "clear!" `Quick test_srfi69_clear
       ; Alcotest.test_case "mutable?" `Quick test_srfi69_mutable
       ; Alcotest.test_case "cond-expand" `Quick test_srfi69_cond_expand
       ])
    ; ("srfi-14",
       [ Alcotest.test_case "char-set?" `Quick test_srfi14_predicate
       ; Alcotest.test_case "char-set? false" `Quick test_srfi14_predicate_false
       ; Alcotest.test_case "contains?" `Quick test_srfi14_contains
       ; Alcotest.test_case "contains? false" `Quick test_srfi14_contains_false
       ; Alcotest.test_case "char-set=?" `Quick test_srfi14_equal
       ; Alcotest.test_case "char-set<=?" `Quick test_srfi14_subset
       ; Alcotest.test_case "adjoin" `Quick test_srfi14_adjoin
       ; Alcotest.test_case "delete" `Quick test_srfi14_delete
       ; Alcotest.test_case "complement" `Quick test_srfi14_complement
       ; Alcotest.test_case "complement excludes" `Quick test_srfi14_complement_excludes
       ; Alcotest.test_case "union" `Quick test_srfi14_union
       ; Alcotest.test_case "intersection" `Quick test_srfi14_intersection
       ; Alcotest.test_case "difference" `Quick test_srfi14_difference
       ; Alcotest.test_case "xor" `Quick test_srfi14_xor
       ; Alcotest.test_case "list roundtrip" `Quick test_srfi14_list_roundtrip
       ; Alcotest.test_case "string roundtrip" `Quick test_srfi14_string_roundtrip
       ; Alcotest.test_case "ucs-range" `Quick test_srfi14_ucs_range
       ; Alcotest.test_case "fold" `Quick test_srfi14_fold
       ; Alcotest.test_case "count" `Quick test_srfi14_count
       ; Alcotest.test_case "every" `Quick test_srfi14_every
       ; Alcotest.test_case "any" `Quick test_srfi14_any
       ; Alcotest.test_case "filter" `Quick test_srfi14_filter
       ; Alcotest.test_case "cursor" `Quick test_srfi14_cursor
       ; Alcotest.test_case "size" `Quick test_srfi14_size
       ; Alcotest.test_case "char-set:letter" `Quick test_srfi14_letter
       ; Alcotest.test_case "char-set:digit" `Quick test_srfi14_digit
       ; Alcotest.test_case "char-set:whitespace" `Quick test_srfi14_whitespace
       ; Alcotest.test_case "char-set:empty" `Quick test_srfi14_empty
       ; Alcotest.test_case "char-set:full" `Quick test_srfi14_full
       ; Alcotest.test_case "copy" `Quick test_srfi14_copy
       ; Alcotest.test_case "union!" `Quick test_srfi14_union_mut
       ; Alcotest.test_case "map" `Quick test_srfi14_map
       ; Alcotest.test_case "for-each" `Quick test_srfi14_for_each
       ; Alcotest.test_case "cond-expand" `Quick test_srfi14_cond_expand
       ])
    ; ("srfi-41",
       [ Alcotest.test_case "stream-null?" `Quick test_srfi41_null
       ; Alcotest.test_case "stream-pair?" `Quick test_srfi41_pair
       ; Alcotest.test_case "car/cdr" `Quick test_srfi41_car_cdr
       ; Alcotest.test_case "stream macro" `Quick test_srfi41_stream
       ; Alcotest.test_case "stream->list" `Quick test_srfi41_to_list
       ; Alcotest.test_case "list->stream" `Quick test_srfi41_list_to_stream
       ; Alcotest.test_case "stream-map" `Quick test_srfi41_map
       ; Alcotest.test_case "stream-filter" `Quick test_srfi41_filter
       ; Alcotest.test_case "stream-fold" `Quick test_srfi41_fold
       ; Alcotest.test_case "stream-take" `Quick test_srfi41_take
       ; Alcotest.test_case "stream-drop" `Quick test_srfi41_drop
       ; Alcotest.test_case "stream-range" `Quick test_srfi41_range
       ; Alcotest.test_case "stream-iterate" `Quick test_srfi41_iterate
       ; Alcotest.test_case "stream-zip" `Quick test_srfi41_zip
       ; Alcotest.test_case "stream-append" `Quick test_srfi41_append
       ; Alcotest.test_case "stream-take-while" `Quick test_srfi41_take_while
       ; Alcotest.test_case "stream-drop-while" `Quick test_srfi41_drop_while
       ; Alcotest.test_case "stream-length" `Quick test_srfi41_length
       ; Alcotest.test_case "stream-scan" `Quick test_srfi41_scan
       ; Alcotest.test_case "stream-unfold" `Quick test_srfi41_unfold
       ; Alcotest.test_case "cond-expand" `Quick test_srfi41_cond_expand
       ])
    ; ("srfi-113",
       [ Alcotest.test_case "set?" `Quick test_srfi113_set_predicate
       ; Alcotest.test_case "set-contains?" `Quick test_srfi113_set_contains
       ; Alcotest.test_case "set-contains? false" `Quick test_srfi113_set_contains_false
       ; Alcotest.test_case "set-empty?" `Quick test_srfi113_set_empty
       ; Alcotest.test_case "set-size" `Quick test_srfi113_set_size
       ; Alcotest.test_case "set-adjoin" `Quick test_srfi113_set_adjoin
       ; Alcotest.test_case "set-delete" `Quick test_srfi113_set_delete
       ; Alcotest.test_case "set-union" `Quick test_srfi113_set_union
       ; Alcotest.test_case "set-intersection" `Quick test_srfi113_set_intersection
       ; Alcotest.test_case "set-difference" `Quick test_srfi113_set_difference
       ; Alcotest.test_case "set-xor" `Quick test_srfi113_set_xor
       ; Alcotest.test_case "set=?" `Quick test_srfi113_set_equal
       ; Alcotest.test_case "set<=?" `Quick test_srfi113_set_subset
       ; Alcotest.test_case "set-fold" `Quick test_srfi113_set_fold
       ; Alcotest.test_case "set-map" `Quick test_srfi113_set_map
       ; Alcotest.test_case "set-filter" `Quick test_srfi113_set_filter
       ; Alcotest.test_case "set->list" `Quick test_srfi113_set_to_list
       ; Alcotest.test_case "list->set" `Quick test_srfi113_list_to_set
       ; Alcotest.test_case "set-copy" `Quick test_srfi113_set_copy
       ; Alcotest.test_case "set-disjoint?" `Quick test_srfi113_set_disjoint
       ; Alcotest.test_case "set-any?" `Quick test_srfi113_set_any
       ; Alcotest.test_case "set-every?" `Quick test_srfi113_set_every
       ; Alcotest.test_case "bag?" `Quick test_srfi113_bag_predicate
       ; Alcotest.test_case "bag-element-count" `Quick test_srfi113_bag_count
       ; Alcotest.test_case "bag-size" `Quick test_srfi113_bag_size
       ; Alcotest.test_case "bag-adjoin" `Quick test_srfi113_bag_adjoin
       ; Alcotest.test_case "bag-delete" `Quick test_srfi113_bag_delete
       ; Alcotest.test_case "bag->set" `Quick test_srfi113_bag_to_set
       ; Alcotest.test_case "set->bag" `Quick test_srfi113_set_to_bag
       ; Alcotest.test_case "bag-union" `Quick test_srfi113_bag_union
       ; Alcotest.test_case "bag-increment!" `Quick test_srfi113_bag_increment
       ; Alcotest.test_case "bag->alist" `Quick test_srfi113_bag_alist
       ; Alcotest.test_case "cond-expand" `Quick test_srfi113_cond_expand
       ])
    ; ("srfi-13",
       [ Alcotest.test_case "string-null?" `Quick test_srfi13_null
       ; Alcotest.test_case "string-null? false" `Quick test_srfi13_null_false
       ; Alcotest.test_case "string-every" `Quick test_srfi13_every
       ; Alcotest.test_case "string-every false" `Quick test_srfi13_every_false
       ; Alcotest.test_case "string-any" `Quick test_srfi13_any
       ; Alcotest.test_case "string-tabulate" `Quick test_srfi13_tabulate
       ; Alcotest.test_case "string-take" `Quick test_srfi13_take
       ; Alcotest.test_case "string-take-right" `Quick test_srfi13_take_right
       ; Alcotest.test_case "string-drop" `Quick test_srfi13_drop
       ; Alcotest.test_case "string-drop-right" `Quick test_srfi13_drop_right
       ; Alcotest.test_case "string-pad" `Quick test_srfi13_pad
       ; Alcotest.test_case "string-pad-right" `Quick test_srfi13_pad_right
       ; Alcotest.test_case "string-trim" `Quick test_srfi13_trim
       ; Alcotest.test_case "string-trim-right" `Quick test_srfi13_trim_right
       ; Alcotest.test_case "string-trim-both" `Quick test_srfi13_trim_both
       ; Alcotest.test_case "string-prefix-length" `Quick test_srfi13_prefix_length
       ; Alcotest.test_case "string-suffix-length" `Quick test_srfi13_suffix_length
       ; Alcotest.test_case "string-prefix?" `Quick test_srfi13_prefix
       ; Alcotest.test_case "string-suffix?" `Quick test_srfi13_suffix
       ; Alcotest.test_case "string-index" `Quick test_srfi13_index
       ; Alcotest.test_case "string-index-right" `Quick test_srfi13_index_right
       ; Alcotest.test_case "string-skip" `Quick test_srfi13_skip
       ; Alcotest.test_case "string-contains" `Quick test_srfi13_contains
       ; Alcotest.test_case "string-contains-ci" `Quick test_srfi13_contains_ci
       ; Alcotest.test_case "string-count" `Quick test_srfi13_count
       ; Alcotest.test_case "string-reverse" `Quick test_srfi13_reverse
       ; Alcotest.test_case "string-concatenate" `Quick test_srfi13_concatenate
       ; Alcotest.test_case "string-fold" `Quick test_srfi13_fold
       ; Alcotest.test_case "string-fold-right" `Quick test_srfi13_fold_right
       ; Alcotest.test_case "string-filter" `Quick test_srfi13_filter
       ; Alcotest.test_case "string-delete" `Quick test_srfi13_delete
       ; Alcotest.test_case "string-replace" `Quick test_srfi13_replace
       ; Alcotest.test_case "string-titlecase" `Quick test_srfi13_titlecase
       ; Alcotest.test_case "xsubstring" `Quick test_srfi13_xsubstring
       ; Alcotest.test_case "string-every char-set" `Quick test_srfi13_every_charset
       ; Alcotest.test_case "string-trim char-set" `Quick test_srfi13_trim_charset
       ; Alcotest.test_case "string-index char-set" `Quick test_srfi13_index_charset
       ; Alcotest.test_case "cond-expand" `Quick test_srfi13_cond_expand
       ; Alcotest.test_case "string-concatenate-reverse" `Quick test_srfi13_concatenate_reverse
       ])
    ; ("srfi-115",
       [ Alcotest.test_case "regexp?" `Quick test_srfi115_predicate
       ; Alcotest.test_case "regexp? false" `Quick test_srfi115_predicate_false
       ; Alcotest.test_case "matches literal" `Quick test_srfi115_matches_literal
       ; Alcotest.test_case "matches literal false" `Quick test_srfi115_matches_literal_false
       ; Alcotest.test_case "search" `Quick test_srfi115_search
       ; Alcotest.test_case "search submatch" `Quick test_srfi115_search_submatch
       ; Alcotest.test_case "matches obj" `Quick test_srfi115_matches_obj
       ; Alcotest.test_case "SRE seq" `Quick test_srfi115_seq
       ; Alcotest.test_case "SRE or" `Quick test_srfi115_or
       ; Alcotest.test_case "SRE *" `Quick test_srfi115_star
       ; Alcotest.test_case "SRE +" `Quick test_srfi115_plus
       ; Alcotest.test_case "SRE + empty" `Quick test_srfi115_plus_empty
       ; Alcotest.test_case "SRE ?" `Quick test_srfi115_question
       ; Alcotest.test_case "SRE any" `Quick test_srfi115_any
       ; Alcotest.test_case "SRE = count" `Quick test_srfi115_repeat_n
       ; Alcotest.test_case "SRE = wrong count" `Quick test_srfi115_repeat_n_false
       ; Alcotest.test_case "SRE ** range" `Quick test_srfi115_range
       ; Alcotest.test_case "SRE alphabetic" `Quick test_srfi115_alpha
       ; Alcotest.test_case "SRE numeric" `Quick test_srfi115_digit
       ; Alcotest.test_case "regexp-replace" `Quick test_srfi115_replace
       ; Alcotest.test_case "regexp-replace-all" `Quick test_srfi115_replace_all
       ; Alcotest.test_case "regexp-extract" `Quick test_srfi115_extract
       ; Alcotest.test_case "regexp-split" `Quick test_srfi115_split
       ; Alcotest.test_case "match-count" `Quick test_srfi115_match_count
       ; Alcotest.test_case "submatch-start" `Quick test_srfi115_match_start
       ; Alcotest.test_case "submatch-end" `Quick test_srfi115_match_end
       ; Alcotest.test_case "char-set" `Quick test_srfi115_charset
       ; Alcotest.test_case "cond-expand" `Quick test_srfi115_cond_expand
       ; Alcotest.test_case "w/nocase" `Quick test_srfi115_nocase
       ; Alcotest.test_case "regexp-fold" `Quick test_srfi115_fold
       ])
    ; ("srfi-125",
       [ Alcotest.test_case "constructor" `Quick test_srfi125_constructor
       ; Alcotest.test_case "contains?" `Quick test_srfi125_contains
       ; Alcotest.test_case "empty?" `Quick test_srfi125_empty
       ; Alcotest.test_case "intern!" `Quick test_srfi125_intern
       ; Alcotest.test_case "pop!" `Quick test_srfi125_pop
       ; Alcotest.test_case "for-each" `Quick test_srfi125_for_each
       ; Alcotest.test_case "map" `Quick test_srfi125_map
       ; Alcotest.test_case "map->list" `Quick test_srfi125_map_to_list
       ; Alcotest.test_case "count" `Quick test_srfi125_count
       ; Alcotest.test_case "union!" `Quick test_srfi125_union
       ; Alcotest.test_case "intersection!" `Quick test_srfi125_intersection
       ; Alcotest.test_case "difference!" `Quick test_srfi125_difference
       ; Alcotest.test_case "entries" `Quick test_srfi125_entries
       ; Alcotest.test_case "unfold" `Quick test_srfi125_unfold
       ; Alcotest.test_case "hash-table=?" `Quick test_srfi125_equal
       ; Alcotest.test_case "empty-copy" `Quick test_srfi125_empty_copy
       ])
    ; ("srfi-145",
       [ Alcotest.test_case "truthy" `Quick test_srfi145_truthy
       ; Alcotest.test_case "true" `Quick test_srfi145_true
       ; Alcotest.test_case "expr" `Quick test_srfi145_expr
       ; Alcotest.test_case "false raises" `Quick test_srfi145_false
       ; Alcotest.test_case "message raises" `Quick test_srfi145_message
       ])
    ; ("srfi-156",
       [ Alcotest.test_case "is predicate" `Quick test_srfi156_is
       ; Alcotest.test_case "is equal" `Quick test_srfi156_is_equal
       ; Alcotest.test_case "isnt" `Quick test_srfi156_isnt
       ; Alcotest.test_case "is binary" `Quick test_srfi156_is_binary
       ; Alcotest.test_case "filter" `Quick test_srfi156_filter
       ])
    ; ("srfi-219",
       [ Alcotest.test_case "basic" `Quick test_srfi219_basic
       ; Alcotest.test_case "triple" `Quick test_srfi219_triple
       ; Alcotest.test_case "normal define" `Quick test_srfi219_normal
       ])
    ; ("srfi-223",
       [ Alcotest.test_case "bisect-left" `Quick test_srfi223_bisect_left
       ; Alcotest.test_case "bisect-right" `Quick test_srfi223_bisect_right
       ; Alcotest.test_case "vector-bisect-left" `Quick test_srfi223_vector_bisect_left
       ; Alcotest.test_case "vector-bisect-right" `Quick test_srfi223_vector_bisect_right
       ])
    ; ("srfi-175",
       [ Alcotest.test_case "upper-case?" `Quick test_srfi175_upper
       ; Alcotest.test_case "lower-case?" `Quick test_srfi175_lower
       ; Alcotest.test_case "digit?" `Quick test_srfi175_digit
       ; Alcotest.test_case "upcase" `Quick test_srfi175_upcase
       ; Alcotest.test_case "downcase" `Quick test_srfi175_downcase
       ; Alcotest.test_case "ci=?" `Quick test_srfi175_ci_eq
       ; Alcotest.test_case "alphanumeric?" `Quick test_srfi175_alphanumeric
       ; Alcotest.test_case "non-alphanumeric" `Quick test_srfi175_non_alphanumeric
       ])
    ; ("srfi-162",
       [ Alcotest.test_case "default" `Quick test_srfi162_default
       ; Alcotest.test_case "boolean" `Quick test_srfi162_boolean
       ; Alcotest.test_case "char ordering" `Quick test_srfi162_char
       ; Alcotest.test_case "string ordering" `Quick test_srfi162_string
       ; Alcotest.test_case "real" `Quick test_srfi162_real
       ])
    ; ("srfi-228",
       [ Alcotest.test_case "wrapper" `Quick test_srfi228_wrapper
       ; Alcotest.test_case "product" `Quick test_srfi228_product
       ; Alcotest.test_case "sum" `Quick test_srfi228_sum
       ])
    ; ("srfi-195",
       [ Alcotest.test_case "box/unbox" `Quick test_srfi195_box
       ; Alcotest.test_case "arity" `Quick test_srfi195_arity
       ; Alcotest.test_case "unbox-value" `Quick test_srfi195_unbox_value
       ; Alcotest.test_case "set-box!" `Quick test_srfi195_set_box
       ; Alcotest.test_case "box?" `Quick test_srfi195_predicate
       ])
    ; ("srfi-48",
       [ Alcotest.test_case "basic ~a" `Quick test_srfi48_basic
       ; Alcotest.test_case "~d" `Quick test_srfi48_decimal
       ; Alcotest.test_case "~x" `Quick test_srfi48_hex
       ; Alcotest.test_case "~o" `Quick test_srfi48_octal
       ; Alcotest.test_case "~b" `Quick test_srfi48_binary
       ; Alcotest.test_case "~~" `Quick test_srfi48_tilde
       ; Alcotest.test_case "~%" `Quick test_srfi48_newline
       ])
    ; ("srfi-117",
       [ Alcotest.test_case "front" `Quick test_srfi117_make
       ; Alcotest.test_case "back" `Quick test_srfi117_back
       ; Alcotest.test_case "empty?" `Quick test_srfi117_empty
       ; Alcotest.test_case "add-front!" `Quick test_srfi117_add_front
       ; Alcotest.test_case "add-back!" `Quick test_srfi117_add_back
       ; Alcotest.test_case "remove-front!" `Quick test_srfi117_remove_front
       ; Alcotest.test_case "list" `Quick test_srfi117_list
       ])
    ; ("srfi-234",
       [ Alcotest.test_case "basic sort" `Quick test_srfi234_basic
       ; Alcotest.test_case "edgelist->graph" `Quick test_srfi234_edgelist
       ; Alcotest.test_case "graph->edgelist" `Quick test_srfi234_graph_edgelist
       ; Alcotest.test_case "connected-components" `Quick test_srfi234_connected
       ])
    ; ("srfi-235",
       [ Alcotest.test_case "constantly" `Quick test_srfi235_constantly
       ; Alcotest.test_case "complement" `Quick test_srfi235_complement
       ; Alcotest.test_case "apply-chain" `Quick test_srfi235_apply_chain
       ; Alcotest.test_case "flip" `Quick test_srfi235_flip
       ; Alcotest.test_case "on" `Quick test_srfi235_on
       ; Alcotest.test_case "conjoin" `Quick test_srfi235_conjoin
       ; Alcotest.test_case "disjoin" `Quick test_srfi235_disjoin
       ; Alcotest.test_case "each-of" `Quick test_srfi235_each_of
       ])
    ; ("srfi-158",
       [ Alcotest.test_case "generator" `Quick test_srfi158_generator
       ; Alcotest.test_case "eof" `Quick test_srfi158_eof
       ; Alcotest.test_case "circular" `Quick test_srfi158_circular
       ; Alcotest.test_case "make-iota" `Quick test_srfi158_make_iota
       ; Alcotest.test_case "gmap" `Quick test_srfi158_gmap
       ; Alcotest.test_case "gfilter" `Quick test_srfi158_gfilter
       ; Alcotest.test_case "gtake" `Quick test_srfi158_gtake
       ; Alcotest.test_case "count-accumulator" `Quick test_srfi158_accumulator
       ; Alcotest.test_case "list-accumulator" `Quick test_srfi158_list_accumulator
       ])
    ; ("srfi-214",
       [ Alcotest.test_case "ref" `Quick test_srfi214_make
       ; Alcotest.test_case "length" `Quick test_srfi214_length
       ; Alcotest.test_case "add-back!" `Quick test_srfi214_add
       ; Alcotest.test_case "set!" `Quick test_srfi214_set
       ; Alcotest.test_case "flexvector?" `Quick test_srfi214_predicate
       ; Alcotest.test_case "remove!" `Quick test_srfi214_remove
       ; Alcotest.test_case "->vector" `Quick test_srfi214_to_vector
       ])
    ; ("srfi-189",
       [ Alcotest.test_case "just?" `Quick test_srfi189_just
       ; Alcotest.test_case "nothing?" `Quick test_srfi189_nothing
       ; Alcotest.test_case "maybe-ref just" `Quick test_srfi189_maybe_ref
       ; Alcotest.test_case "maybe-ref nothing" `Quick test_srfi189_maybe_ref_nothing
       ; Alcotest.test_case "right?" `Quick test_srfi189_right
       ; Alcotest.test_case "left?" `Quick test_srfi189_left
       ; Alcotest.test_case "maybe-map just" `Quick test_srfi189_maybe_map
       ; Alcotest.test_case "maybe-map nothing" `Quick test_srfi189_maybe_map_nothing
       ])
    ; ("srfi-210",
       [ Alcotest.test_case "coarity" `Quick test_srfi210_coarity
       ; Alcotest.test_case "identity" `Quick test_srfi210_identity
       ; Alcotest.test_case "compose-left" `Quick test_srfi210_compose_left
       ; Alcotest.test_case "compose-right" `Quick test_srfi210_compose_right
       ; Alcotest.test_case "with-values" `Quick test_srfi210_with_values
       ])
    ]
