let max_opt_iter = ref 1000

let rec optimize_pass n ast =
  Format.eprintf "==> iteration: %d@." n;
  if n = 0 then
    ast
  else
    let ast_new = ast
                  |> Beta.f
                  |> Assoc.f
                  |> Inline.f
                  |> Constfold.f
                  |> Elim.f
    in if ast = ast_new
    then ast
    else optimize_pass (n - 1) ast_new

let compile outchan buf =
  Id.counter := 0;
  Typing.extenv := M.empty;
  buf
  |> Parser.exp Lexer.token
  |> Typing.infer
  |> Knormal.normalize
  |> Alpha.f
  |> optimize_pass !max_opt_iter
  |> Closure.f
  |> Virtual.f
  |> Simm.f
  |> Regalloc.f
  |> Emit.f outchan

let compile_string str =
  compile stdout (Lexing.from_string str)

let compile_file filename =
  let inchan = open_in (filename ^ ".ml") in
  let outchan = open_out (filename ^ ".s") in
  try
    compile outchan (Lexing.from_channel inchan);
    close_in inchan;
    close_out outchan;
  with e ->
    (close_in inchan; close_out outchan; raise e)

let () =
  let files = ref [] in
  Arg.parse
    [
      (
        "-inline",
        Arg.Int(fun n -> Inline.threshold := n),
        "maximum size of functions to inline"
      );
      (
        "-iter",
        Arg.Int(fun n -> max_opt_iter := n),
        "maximum number of optimization iterations"
      )
    ]
    (fun file -> files := !files @ [file])
    (
      "Mitou Min-Caml Compiler (C) Eijiro Sumii\n" ^
      Printf.sprintf
        "usage: %s [-inline m] [-iter n] ...filenames without \".ml\"..."
        Sys.argv.(0)
    );

  List.iter (fun file -> ignore (compile_file file)) !files
