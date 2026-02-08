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
    ]
