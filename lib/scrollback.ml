type t = {
  max_bytes : int;
  buf : Buffer.t;
}

let create ~max_bytes =
  { max_bytes; buf = Buffer.create (min max_bytes 4096) }

let trim t =
  let len = Buffer.length t.buf in
  if len > t.max_bytes then begin
    let excess = len - t.max_bytes in
    let kept = Buffer.sub t.buf excess (len - excess) in
    Buffer.clear t.buf;
    Buffer.add_string t.buf kept
  end

let append t s =
  Buffer.add_string t.buf s;
  trim t

let contents t = Buffer.contents t.buf

let clear t = Buffer.clear t.buf

let byte_count t = Buffer.length t.buf
