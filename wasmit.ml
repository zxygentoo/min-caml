(* open Wasm *)

let eln = Printf.fprintf

(* 
let func_name (funcdef: C.fundef) : string =
  let Id.Label(name), _ = funcdef.name in
  List.hd (String.split_on_char '.' name)

let func_body (funcdef: C.fundef) : C.t =
  funcdef.body

let func_export oc funcdef =
  let name = func_name funcdef in
  eln oc " (export \"%s\" (func $%s))\n" name name

let rec g oc = function
  | C.Unit ->
    eln oc ";; C.Unit\n"

  | C.Int(i) ->
    eln oc ";; C.Int(%d)\n" i

  | C.Add(_x, _y) ->
    eln oc "  (i64.add x y)\n"

  | C.Let((_x, _t1), e1, e2) ->
    g oc e1;
    g oc e2

  | _ ->
    eln oc ";; \n"

let func_body oc n funcdef =
  let name = func_name funcdef in
  eln oc " (func $%s (; %d ;) (result i64)\n" name n;
  g oc (func_body funcdef);
  eln oc "  (i64.const 42)\n";
  eln oc " )\n"
 *)

let emit oc _prog =
  Format.eprintf "==> generating WebAssembly...@.";
  eln oc "(module\n";
  eln oc " (table 0 anyfunc)\n";
  eln oc " (memory $0 1)\n";
  eln oc " (export \"memory\" (memory $0))\n";
  (* func exports *)
  (* func bodies *)
  eln oc ")\n";
