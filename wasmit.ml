open Closure
module T = Type


let eln =
  Printf.fprintf

(* now we only dealing with i32 consts and memory address, so all 4 bytes *)
let offset_unit = 4

let id_to_offset id =
  let uid = id
    |> String.split_on_char '.'
    |> List.rev
    |> List.hd
    |> int_of_string
  in
  uid * offset_unit

let seperate_fundefs fundefs =
  let rec f fds clss fns =
  match fds with
  | [] ->
    (List.rev clss), (List.rev fns)

  | x :: xs ->
    if List.length x.formal_fv = 0
    then f xs clss (x :: fns)
    else f xs (x :: clss) fns
in
f fundefs [] []

let rec g _oc _e =
  ()

let t_to_s = function
| T.Int -> "i32"
| T.Fun(_) -> "i32"
| _ -> failwith "don't know how to deal with this yet..."


let emit_result oc ty =
  eln oc "(result %s)" (t_to_s ty)

let emit_param oc with_label (label, ty) =
  if with_label
  then eln oc "(param $%s %s) " label (t_to_s ty)
  else eln oc "(param %s) " (t_to_s ty)

let emit_sig oc with_label ret_ty args =
  List.iter (emit_param oc with_label) args ;
  (
    match ret_ty with
    | T.Fun(_, ret) ->
      emit_result oc ret
    | _ ->
      failwith "fundef doesn't have Fun type."
  )

let emit_func oc {
  name = (Id.Label(label), ret_ty);
  args;
  formal_fv;
  body
} =
  eln oc "(func $%s " label ;
  emit_sig oc true ret_ty (formal_fv @ args) ;
  eln oc "\n" ;
  g oc body ;
  eln oc ")\n"

let emit_type oc {
  name = (Id.Label(label), ret_ty);
  args;
  formal_fv;
  body = _body
} =
  eln oc "(type $%s (func " label ;
  emit_sig oc false ret_ty (formal_fv @ args);
  eln oc "))\n"

let emit_table oc clss =
  List.iter (emit_type oc) clss ;
  List.iter (emit_func oc) clss ;
  eln oc "(table %d anyfunc)\n" (List.length clss) ;
  eln oc "(elem (i32.const 0) 0)\n"

let emit oc (Prog(fundefs, e)) =
  Format.eprintf "==> generating WebAssembly...@." ;

  eln oc "(module\n" ;

  eln oc "\n;; memory section\n" ;
  eln oc "(memory $0 1)\n" ;
  eln oc "(export \"memory\" (memory $0))\n" ;

  let clss, fns = seperate_fundefs fundefs in

  eln oc "\n;; table section\n" ;
  emit_table oc clss ;

  eln oc "\n;; function section\n" ;
  List.iter (emit_func oc) fns ;

  eln oc "\n;; start function\n" ;
  eln oc "(func $start (result i32)\n" ;
  g oc e;
  eln oc ")\n" ;

  eln oc "\n;; export start\n" ;
  eln oc "(export \"start\" (func $start))\n" ;

  eln oc ")\n";
