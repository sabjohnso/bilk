type t = {
  symbols : Symbol.table;
  global_env : Env.t;
  readtable : Readtable.t;
}

let create ?(readtable = Readtable.default) () = {
  symbols = Symbol.create_table ();
  global_env = Env.empty ();
  readtable;
}

let intern inst name = Symbol.intern inst.symbols name
