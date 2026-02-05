type t =
  | Constituent
  | Whitespace
  | Terminating_macro
  | Non_terminating_macro
  | Single_escape
  | Multiple_escape

let equal a b =
  match (a, b) with
  | Constituent, Constituent
  | Whitespace, Whitespace
  | Terminating_macro, Terminating_macro
  | Non_terminating_macro, Non_terminating_macro
  | Single_escape, Single_escape
  | Multiple_escape, Multiple_escape ->
    true
  | _ -> false

let to_string = function
  | Constituent -> "constituent"
  | Whitespace -> "whitespace"
  | Terminating_macro -> "terminating-macro"
  | Non_terminating_macro -> "non-terminating-macro"
  | Single_escape -> "single-escape"
  | Multiple_escape -> "multiple-escape"

let all =
  [ Constituent; Whitespace; Terminating_macro; Non_terminating_macro;
    Single_escape; Multiple_escape ]
