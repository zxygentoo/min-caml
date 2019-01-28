let max_opt_iter = ref 1000
let inline_threshold = Inline.threshold

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
    in
    if ast = ast_new then ast
    else optimize_pass (n - 1) ast_new

let compile oc buf =
  Id.counter := 0;
  Typing.extenv := M.empty;
  let cls_ast = buf
    |> Parser.exp Lexer.token
    |> Typing.infer
    |> Knormal.normalize
    |> Alpha.convert
    |> optimize_pass !max_opt_iter
    |> Closure.flattern
  in
  (* x86 assembly code emission *)
(*   begin
    cls_ast |> Virtual.gencode |> Emit.emitcode oc
  end
 *)
  (* WebAssembly code emission *)
  begin
    ignore (Virtual.gencode cls_ast) ;
    (* cls_ast |> Wasmit.emitcode oc *)
    cls_ast |> Wasm.emitcode oc
  end

let compile_string str =
  compile stdout (Lexing.from_string str)

let compile_file filename =
  let ic = open_in (filename ^ ".ml") in
  let oc = open_out (filename ^ ".wat") in
  try
    compile oc (Lexing.from_channel ic);
    close_in ic;
    close_out oc;
  with e ->
    close_in ic;
    close_out oc;
    raise e
