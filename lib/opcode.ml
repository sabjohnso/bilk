type t =
  | Halt
  | Const of int
  | Lookup of int
  | Define of int
  | SetBang of int
  | Push
  | Jump of int
  | JumpFalse of int
  | Call of int
  | TailCall of int
  | Return
  | MakeClosure of int

let pp fmt = function
  | Halt -> Format.fprintf fmt "HALT"
  | Const i -> Format.fprintf fmt "CONST %d" i
  | Lookup i -> Format.fprintf fmt "LOOKUP %d" i
  | Define i -> Format.fprintf fmt "DEFINE %d" i
  | SetBang i -> Format.fprintf fmt "SET! %d" i
  | Push -> Format.fprintf fmt "PUSH"
  | Jump i -> Format.fprintf fmt "JUMP %d" i
  | JumpFalse i -> Format.fprintf fmt "JUMP_FALSE %d" i
  | Call n -> Format.fprintf fmt "CALL %d" n
  | TailCall n -> Format.fprintf fmt "TAIL_CALL %d" n
  | Return -> Format.fprintf fmt "RETURN"
  | MakeClosure i -> Format.fprintf fmt "MAKE_CLOSURE %d" i

let to_string op =
  Format.asprintf "%a" pp op
