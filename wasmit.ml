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

let get_local_or_load oc bvs var_ty_env id =
  if S.mem id bvs
  then eln oc "get_local $%s\n" id
  else if M.mem id var_ty_env
  then begin
    let ty = M.find id var_ty_env in
    match ty with
    | T.Int ->
      eln oc "i32.const %d\n" (id_to_offset id) ;
      eln oc "i32.load\n"
    | T.Fun(args, _) ->
      List.iter
        (fun i ->
          eln oc "i32.const %d\n" (id_to_offset id) ;
          if i > 0 then begin
            eln oc "i32.const %d\n" (i * offset_unit) ;
            eln oc "i32.add\n" ;
          end ;
          eln oc "i32.load\n" ;
          eln oc "i32.load\n"
        )
        (List.init (List.length args + 1) (fun i -> i)) ;
    | _ ->
      Printf.eprintf "~~> don't know how to do this yet...\n"
  end
  else failwith "~~> don't know var ty..."


let find_cls_sig x var_ty_env funcidx =
  let module TM = Map.Make(struct
    type t = Type.t
    let compare = compare
  end
  ) in
  let pairs = 
    M.fold (fun label (idx, ty) acc -> (ty, label) :: acc) funcidx []
  in
  let tm = 
    List.fold_left (fun acc (ty, label) -> TM.add ty label acc) TM.empty pairs
  in
  let ty = M.find x var_ty_env in
  TM.find ty tm

let rec g oc bvs funcidx var_ty_env = function
  | Unit ->
    ()

  | Int(i) ->
    eln oc "i32.const %d\n" i

  | Add(x, y) ->
    List.iter (get_local_or_load oc bvs var_ty_env) [x; y] ;
    eln oc "i32.add\n"

  | Let((x, t), e1, e2) ->
    let var_ty_env' = M.add x t var_ty_env in
    eln oc "i32.const %d\n" (id_to_offset x) ;
    g oc bvs funcidx var_ty_env' e1 ;
    eln oc "i32.store\n" ;
    eln oc "\n" ;
    g oc bvs funcidx var_ty_env' e2

  | Var(x) ->
      eln oc "i32.const %d\n" (id_to_offset x)

  | MakeCls((x, _), { entry; actual_fv }, e) ->
    let base = id_to_offset x in
    (* store all fv *)
    eln oc "i32.const %d\n" base ;
    List.iter (get_local_or_load oc bvs var_ty_env) (List.rev actual_fv) ;
    eln oc "i32.store\n" ;
    (* store funaddr *)
    eln oc "i32.const %d\n" (base + offset_unit * (List.length actual_fv)) ;
    let Id.Label(label) = entry in
    let idx, _ty = (M.find label funcidx) in
    eln oc "i32.const %d\n" idx ;
    eln oc "i32.store\n" ;
    (* body *)
    g oc bvs funcidx var_ty_env e

  | AppDir(Id.Label(f), args) ->
    List.iter (get_local_or_load oc bvs var_ty_env) args ;
    eln oc "call $%s\n" f

  | AppCls(x, args) ->
    List.iter (get_local_or_load oc bvs var_ty_env) args ;
    get_local_or_load oc bvs var_ty_env x ;
    eln oc "call_indirect (type $%s)\n" (find_cls_sig x var_ty_env funcidx)

  | _ ->
    Printf.eprintf "~~> don't know how to do this yet...\n"

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

let emit_func oc funcidx clss {
  name = (Id.Label(label), ret_ty);
  args;
  formal_fv;
  body
} =
  let all_args = formal_fv @ args in
  eln oc "(func $%s " label ;
  emit_sig oc true ret_ty all_args ;
  eln oc "\n" ;
  g oc (
    S.of_list (List.map (fun (label, _) -> label) all_args)
  ) funcidx clss body ;
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

let emit_table oc funcidx clss var_ty_env =
  List.iter (emit_type oc) clss ;
  List.iter (emit_func oc funcidx var_ty_env) clss ;
  eln oc "(table %d anyfunc)\n" (List.length clss) ;
  eln oc "(elem (i32.const 0) 0)\n"

let funcindex fns =
  M.empty
  |> M.add_list (
    List.mapi
      (fun i fn -> let Id.Label(label), ty = fn.name in (label, (i, ty)))
      fns
  )

let emit oc (Prog(fundefs, e)) =
  Format.eprintf "==> generating WebAssembly...@." ;

  eln oc "(module\n" ;

  eln oc "\n;; memory section\n" ;
  eln oc "(memory $0 1)\n" ;
  eln oc "(export \"memory\" (memory $0))\n" ;

  let clss, fns = seperate_fundefs fundefs in
  let funcidx = funcindex clss in
  let var_ty_env = M.empty in

  eln oc "\n;; table section\n" ;
  emit_table oc funcidx clss var_ty_env ;

  eln oc "\n;; function section\n" ;
  List.iter (emit_func oc funcidx var_ty_env) fns ;

  eln oc "\n;; start function\n" ;
  eln oc "(func $start (result i32)\n" ;
  g oc S.empty funcidx var_ty_env e ;
  eln oc ")\n" ;

  eln oc "\n;; export start\n" ;
  eln oc "(export \"start\" (func $start))\n" ;

  eln oc ")\n";
