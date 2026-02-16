type client_cmd =
  | Quit
  | Help
  | Paredit
  | Theme of string
  | Clear

type classification =
  | Client_local of client_cmd
  | Server_side
  | Scheme_input

let classify input =
  let trimmed = String.trim input in
  if String.length trimmed = 0 || trimmed.[0] <> ',' then
    Scheme_input
  else
    let cmd = String.sub trimmed 1 (String.length trimmed - 1) in
    let word, rest =
      match String.index_opt cmd ' ' with
      | Some i ->
        (String.sub cmd 0 i,
         String.trim (String.sub cmd (i + 1) (String.length cmd - i - 1)))
      | None -> (cmd, "")
    in
    match word with
    | "quit" | "q" -> Client_local Quit
    | "help" | "h" -> Client_local Help
    | "paredit" -> Client_local Paredit
    | "theme" -> Client_local (Theme rest)
    | "clear" -> Client_local Clear
    | _ -> Server_side
