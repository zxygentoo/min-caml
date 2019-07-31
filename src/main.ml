let max_opt_iter = ref 1000
let inline_threshold = Inline.threshold


let rec optimize_pass n ast =
  if n = 0 then
    ast
  else
    let ast_new =
      ast
      |> Beta.f
      |> Assoc.f
      |> Inline.f
      |> Constfold.f
      |> Elim.f
    in if ast = ast_new then
      ast
    else
      optimize_pass (n - 1) ast_new


let compile oc buf =
  Id.counter := 0 ;
  Typing.extenv := M.empty ;
  buf
  |> Parser.exp Lexer.token
  |> Typing.infer
  |> Knormal.normalize
  |> Alpha.convert
  |> optimize_pass !max_opt_iter
  |> Closure.flattern
  |> Emit.emitcode oc


let compile_string str =
  compile stdout (Lexing.from_string str)


let compile_file filename =
  let ic = open_in (filename ^ ".ml") in
  let oc = open_out (filename ^ ".wat") in
  try
    compile oc (Lexing.from_channel ic) ;
    close_in ic ;
    close_out oc ;
  with e ->
    close_in ic ;
    close_out oc ;
    raise e


let () =
  let files = ref [] in
  Arg.parse
    [
      ( "-inline"
      , Arg.Int(fun n -> inline_threshold := n)
      , "maximum size of functions to inline"
      ) ;
      ( "-iter"
      , Arg.Int(fun m -> max_opt_iter := m)
      , "maximum number of optimization iterations"
      )
    ]
    (fun file -> files := !files @ [file])
    ( "Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
      Printf.sprintf
        "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..."
        Sys.argv.(0)
    ) ;
  List.iter (fun file -> ignore (compile_file file)) !files
