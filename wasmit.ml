open Closure

module T = Type

module TM = Map.Make(
  struct
    type t = Type.t
    let compare = compare
  end
)


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

let get_local_or_load oc bvs var_ty_env base _i id =
  (* let hp = base + (i * offset_unit) in *)
  if S.mem id bvs
  then begin
    (* eln oc "i32.const %d\n" hp ; *)
    eln oc "get_local $%s\n" id ;
    (* eln oc "i32.store\n" ; *)
  end
  else if M.mem id var_ty_env
  then let ty = M.find id var_ty_env in
    (
      match ty with
      | T.Int ->
        eln oc "i32.const %d\n" base ;
        eln oc "i32.load\n"

      | T.Fun(args, _) ->
        List.iter
          (
            fun j ->
              eln oc "(i32.load (i32.load (i32.const %d)))\n"
                (base + (j * offset_unit)) ;
          )
          (List.init (List.length args) (fun i -> i)) ;

      | _ ->
        Printf.eprintf "~~> don't know how to do this yet...\n"
    )
  else failwith "~~> don't know var ty..."

let rec g oc bvs labidx tyidx var_ty_env = function
  | Unit ->
    ()

  | Int(i) ->
    eln oc "i32.const %d ;; Int %d\n" i i

  | Add(x, y) ->
    eln oc ";; Let %s %s\n" x y ;
    get_local_or_load oc bvs var_ty_env (id_to_offset x) 0 x ;
    get_local_or_load oc bvs var_ty_env (id_to_offset y) 0 y ;
    eln oc "i32.add\n"

  | Let((x, t), e1, e2) ->
    eln oc ";; Let %s\n" x ;
    let var_ty_env' = M.add x t var_ty_env in
    eln oc ";; %s\n" x ;
    eln oc "i32.const %d\n" (id_to_offset x) ;
    g oc bvs labidx tyidx var_ty_env e1 ;
    eln oc "i32.store\n" ;
    g oc bvs labidx tyidx var_ty_env' e2

  | Var(x) ->
      if S.mem x bvs
      then eln oc "get_local $%s\n" x
      else eln oc "i32.const %d\n" (id_to_offset x)

  | MakeCls((x, t), { entry; actual_fv }, e) ->
    eln oc ";; MakeCls: %s\n" x ;
    let var_ty_env' = M.add x t var_ty_env in
    let base = id_to_offset x in

    (* store fv *)
    eln oc ";; %s: fv (total: %d) \n" x (List.length actual_fv) ;
    List.iteri
      (
        fun _i fv ->
          (* let ofst = base + offset_unit * i in *)

          if S.mem fv bvs
          then
(*             eln oc "(i32.store (i32.const %d) (get_local $%s))\n"
              ofst fv

 *)     
             eln oc "get_local $%s\n" fv  
          else
(*             eln oc "(i32.store (i32.const %d) (i32.load (i32.const %d)))\n"
              ofst (id_to_offset fv)
 *)      
              eln oc "(i32.load (i32.const %d))\n" (id_to_offset fv)
         ;
      )
      (List.rev actual_fv) ;

    (* store funaddr *)
    eln oc ";; %s: funcaddr\n" x ;
    let Id.Label(label) = entry in
    let idx, _ty = (M.find label labidx) in
    eln oc "(i32.store (i32.const %d) (i32.const %d))\n" 
      (base + offset_unit * (List.length actual_fv))
      idx ;
      (* eln oc "i32.const %d\n" idx ; *)

    (* body *)
    eln oc ";; %s: body\n" x ;
    (* g oc (S.add x bvs) labidx tyidx var_ty_env' e *)
    g oc bvs labidx tyidx var_ty_env' e

  | AppDir(Id.Label(x), args) ->
    eln oc ";; AppDir %s\n" x ;
    (* load bvs *)
    List.iteri
      (fun i arg ->
        get_local_or_load oc bvs var_ty_env (id_to_offset arg) i arg)
      args ;
    eln oc "call $%s\n" x

  | AppCls(x, bargs) ->
    eln oc ";; AppCls %s\n" x ;

    (* load fv if necessary *)
    eln oc ";; --- fvs (if necesarry)\n" ;
    let ty = M.find x var_ty_env in
    let clsidx = (TM.find (M.find x var_ty_env) tyidx) in

    (
      match ty with
      | Fun(args, _ret) ->

(*         List.iteri
        (
          fun i arg ->
          match ret with
          | T.Fun(_, _) ->
              eln oc "(i32.load (i32.const %d))\n"
                ((id_to_offset x) + (offset_unit * i))
          | T.Int ->
              eln oc "(i32.load (i32.load (i32.const %d)))\n"
                ((id_to_offset x) + (offset_unit * i))
          | _ ->
              failwith "AppCls don't know have to deal with: Tx"
        )
        args ;
 *)
        (* laod bvs *)
        eln oc ";; --- all bvs\n" ;
        List.iter
         (fun fv -> eln oc "(i32.load (i32.const %d))\n" (id_to_offset fv))
         bargs ;

        eln oc ";; --- funcaddr\n" ;
        eln oc "(call_indirect (type $%s) (i32.load (i32.const %d)))\n"
          clsidx
          ((id_to_offset x) + (offset_unit * (List.length args)))
          (* (id_to_offset x) *)
        ;


      | _ ->
        failwith "AppCls return has not Fun type..."
    )

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

let emit_type oc {
  name = (Id.Label(label), ret_ty);
  args;
  formal_fv;
  body = _body
} =
  eln oc "(type $%s (func " label ;
  emit_sig oc false ret_ty (formal_fv @ args);
  eln oc "))\n"

let emit_func oc labidx tyidx clss {
  name = (Id.Label(label), ret_ty);
  args;
  formal_fv;
  body
} =
  let all_args = formal_fv @ args in
  (* sig *)
  eln oc "(type $%s (func " label ;
  emit_sig oc false ret_ty (formal_fv @ args) ;
  eln oc "))\n" ;
  (* body *)
  eln oc "(func $%s " label ;
  emit_sig oc true ret_ty all_args ;
  eln oc "\n" ;
  g 
    oc (S.of_list (List.map (fun (label, _) -> label) all_args))
    labidx tyidx clss body ;
  eln oc ")\n"

let emit_table oc labidx tyidx fns var_ty_env =
  (* List.iter (emit_type oc) clss ; *)
  List.iter (emit_func oc labidx tyidx var_ty_env) fns ;
  eln oc "(table %d anyfunc)\n" (List.length fns) ;
  eln oc "(elem (i32.const 0)" ;
  List.iter
    (fun i -> eln oc " %s" (string_of_int i))
    (List.init (List.length fns) (fun i -> i)) ;
  eln oc ")\n"

let labelindex fns =
  let f = fun i fn -> let Id.Label(lab), ty = fn.name in (lab, (i, ty)) in
  M.empty |> M.add_list (List.mapi f fns)

let typeindex fns =
  List.fold_left
    (fun acc fd -> let Id.Label(lab), ty = fd.name in TM.add ty lab acc)
    TM.empty fns

let emitcode oc (Prog(fundefs, e)) =
  Format.eprintf "==> generating WebAssembly...@." ;

  eln oc "(module\n" ;

  eln oc "\n;; memory section\n" ;
  eln oc "(memory $0 1)\n" ;
  eln oc "(export \"memory\" (memory $0))\n" ;

  (* let clss, fns = seperate_fundefs fundefs in *)
  (* let labidx = labelindex clss in *)
  (* let tyidx = typeindex clss in *)
  let labidx = labelindex fundefs in
  let tyidx = typeindex fundefs in
  (* let var_ty_env = M.empty in *)
  let var_ty_env = M.empty
  |> M.add_list (
    List.map (fun fd -> let (Label(label), ty) = fd.name in label, ty) fundefs
  ) in

  eln oc "\n;; table section\n" ;
  emit_table oc labidx tyidx fundefs var_ty_env ;

  (* eln oc "\n;; function section\n" ; *)
  (* List.iter (emit_func oc labidx tyidx var_ty_env) fns ; *)

  eln oc "\n;; start function\n" ;
  eln oc "(func $start (result i32)\n" ;
  g oc S.empty labidx tyidx var_ty_env e ;
  eln oc ")\n" ;

  eln oc "\n;; export start\n" ;
  eln oc "(export \"start\" (func $start))\n" ;

  eln oc ")\n";
