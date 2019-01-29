open Closure

module T = Type

module TM = Map.Make(
  struct
    type t = Type.t
    let compare = compare
  end
)

let allfns = ref M.empty
let fnindex = ref M.empty
let tyindex = ref TM.empty

let t2s = function
  | T.Int -> "i32"
  | T.Fun(_) -> "i32"
  | _ -> failwith "don't know how to deal with this yet..."

let rec local_vars = function
  | Let((x, t), e1, e2) ->
      (x, t) :: (local_vars e1) @ (local_vars e2)

  | MakeCls((x, t), _, e) ->
    (x, t) :: (local_vars e)

  | Unit | Int(_) | Float(_)
  | Neg(_) | FNeg(_)
  | Add(_) | Sub(_) | FAdd(_) | FSub(_)
  | FMul(_) | FDiv(_) | Get(_)
  | IfEq(_)| IfLE(_)
  | Var(_) | AppCls(_) | AppDir(_) | Tuple(_)
  | ExtArray(_) | Put(_) ->
    []

  | LetTuple(_) ->
    failwith "don't know how to gather locals"

let t2ofst = function
  | T.Unit -> 0
  | T.Bool -> 4
  | T.Int -> 4
  | T.Float -> 4
  | T.Fun(_) -> 4
  | T.Tuple(_) | T.Array(_) | T.Var(_) -> failwith "don't know offset"

let emit = Printf.fprintf

let local_or_fvs oc known fvars id =
  if S.mem id known
  then emit oc "get_local $%s\n" id
  else begin
    Format.eprintf "--> id not in known, assuming env fv@." ;
    ignore (List.find (fun (x, _) -> x = id) fvars) ;
    List.iteri
      (
        fun i (x, _t) ->
          if x = id then
            emit oc "(i32.load (i32.add (i32.const %d) (get_local 0)))\n" (i*4)
      )
      fvars
  end


let rec g oc env known fvars = function
  | Unit ->
    ()

  | Int(i) ->
    emit oc ";; Int(%d)\n" i ;
    emit oc "i32.const %d\n" i

  | Add(x, y) ->
    emit oc ";; Add %s %s\n" x y ;
    List.iter (local_or_fvs oc known fvars) [x; y] ;
    emit oc "i32.add\n"

  | Sub(x, y) ->
    emit oc ";; Sub %s %s\n" x y ;
    List.iter (local_or_fvs oc known fvars) [x; y] ;
    emit oc "i32.sub\n"

  | Let((x, t), e1, e2) ->
    emit oc ";; Let %s\n" x ;
    let env' = M.add x t env in
    let known' = S.add x known in
    g oc env known fvars e1 ;
    emit oc "set_local $%s\n" x ;
    g oc env' known' fvars e2

  | Var(x) ->
    emit oc ";; Var(%s)\n" x ;
    local_or_fvs oc known fvars x


  | MakeCls((x, t), { entry = Id.Label(fn_lab) ; actual_fv }, e) ->
    emit oc ";; MakeCls %s\n" x ;

    let fn = M.find fn_lab !allfns in
    let offest_list =
      List.map t2ofst (List.map (fun (_, t) -> t) fn.formal_fv) in
    let env' = M.add x t env in
    let known' = S.add x known in

    emit oc ";; MakeCls %s --- codeptr\n" x ;
    emit oc "(i32.store (get_global $HP) (i32.const %d))\n"
      (M.find fn_lab !fnindex) ;

    emit oc ";; MakeCls %s -- fvs\n" x ;
    List.iteri
      (
        fun i fv ->
          emit oc ";; MakeCls %s --- fv: %s\n" x fv ;
          emit oc "(i32.add (i32.const %d) (get_global $HP))\n" ((i+1)*4) ;
          local_or_fvs oc known fvars fv ;
          emit oc "i32.store\n"
      )
      actual_fv ;

    emit oc ";; -- %s: codeptr\n" x ;
    emit oc "(set_local $%s (get_global $HP))\n" x ;

    let alloc = List.fold_left (fun acc x -> acc + x) 0 offest_list + 4 in
    emit oc "(set_global $HP (i32.add (i32.const %d) (get_global $HP)))\n"
      alloc ;

    emit oc ";; -- %s: body\n" x ;
    g oc env' known' fvars e

  | AppDir(Id.Label(x), bvs) ->
    emit oc ";; AppDir %s\n" x ;
    (* fake env *)
    emit oc "i32.const -10000" ;
    List.iter
      (fun bv ->
        emit oc ";; AppDir %s --- bv: %s\n" x bv ;
        local_or_fvs oc known fvars bv
      )
      bvs ;
    emit oc "(call $%s)\n" x

  | AppCls(x, bvs) ->
    emit oc ";; AppCls %s\n" x ;

    emit oc ";; AppCls %s --- fvs env\n" x ;
    emit oc "i32.const 4\n" ;
    local_or_fvs oc known fvars x ;
    emit oc "i32.add\n" ;

    emit oc ";; AppCls %s --- bvs\n" x ;
    List.iter
      (
        fun bv ->
          emit oc ";; AppCls %s --- bv: %s\n" x bv ;
          local_or_fvs oc known fvars bv
      ) bvs ;

    emit oc ";; AppCls %s --- codeptr\n" x ;
    local_or_fvs oc known fvars x ;
    emit oc "i32.load\n" ;
    emit oc ";; AppCls %s -- indirect call\n" x;
    emit oc ";; find ty\n" ;
    let ty = M.find x env in
    emit oc ";; find ty label\n" ;    
    let ty_lab = TM.find ty !tyindex in
    emit oc "(call_indirect (type $%s))\n" ty_lab

  | _ ->
    failwith "~~> don't know how to compile this yet...\n"

let emit_result oc ty =
  emit oc "(result %s)" (t2s ty)

let emit_param oc with_label (label, ty) =
  if with_label
  then emit oc "(param $%s %s) " label (t2s ty)
  else emit oc "(param %s) " (t2s ty)

let emit_sig oc with_label ty args =
  List.iter (emit_param oc with_label) args ;
  begin match ty with
  | T.Fun(_, ty) ->
    emit_result oc ty

  | _ ->
    failwith "fundef doesn't have Fun type."
  end

let emit_locals oc e =
  List.iter
    (
      fun (x, t) ->
        emit oc "(local $%s %s)\n" x (t2s t)
    )
    (local_vars e)

let emit_func oc { name = (Id.Label(label), ty); args; formal_fv; body } =
  emit oc "(type $%s (func " label ;
  (* env *)
  emit oc "(param i32) " ;
  emit_sig oc false ty args ;
  emit oc "))\n" ;
  emit oc "(func $%s " label ;
  (* env *)
  emit oc "(param i32) " ;
  emit_sig oc true ty args ;
  emit oc "\n" ;
  emit_locals oc body ;
  g oc
    (M.add_list (args @ formal_fv) M.empty)
    (S.of_list (List.map (fun (label, _) -> label) args))
    formal_fv
    body ;
  emit oc ")\n"

let emit_funcs oc fns =
    List.iter (fun fn -> emit_func oc fn ; emit oc "\n") fns

let emit_table oc fns =
  emit oc "(table %d anyfunc)\n" (List.length fns) ;
  emit oc "(elem (i32.const 0) %s)\n"
    (Id.pp_list (List.init (List.length fns) (fun i -> string_of_int i)))

let emitcode oc (Prog(fundefs, e)) =
  Format.eprintf "==> generating WebAssembly...@." ;

  allfns := M.add_list
    (
      List.map (fun fd -> let (Id.Label(label), _) = fd.name in label, fd)
      fundefs
    )
    !allfns ;

  fnindex := M.add_list
    (List.mapi
        (fun i fd -> let (Id.Label(label), _) = fd.name in label, i)
        fundefs)
    !fnindex ;

  tyindex := List.fold_left
    (fun idx fd -> let (Id.Label(label), t) = fd.name in TM.add t label idx)
    !tyindex fundefs ;

  emit oc "(module\n" ;
  emit oc "\n;; memory section\n" ;
  emit oc "(memory $0 1)\n" ;
  emit oc "(export \"memory\" (memory $0))\n" ;
  emit oc "\n;; heap pointer\n" ;
  emit oc "(global $HP (mut i32) (i32.const 0))\n" ;
  emit oc "\n;; functions\n" ;
  emit_funcs oc fundefs ;
  emit oc "\n;; table section\n" ;
  emit_table oc fundefs ;
  emit oc "\n;; start function\n" ;
  emit oc "(func $start (result i32)\n" ;
  emit_locals oc e ;
  g oc M.empty S.empty [] e ;
  emit oc ")\n" ;
  emit oc "\n;; export start function\n" ;
  emit oc "(export \"start\" (func $start))\n" ;
  emit oc ")\n";
