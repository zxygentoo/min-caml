open Closure


module TM = Map.Make(
  struct
    type t = Type.t
    let compare = compare
  end
  )

let fnindex = ref M.empty
let allfns = ref M.empty
let tyindex = ref TM.empty

let emit =
  Printf.fprintf


let rec local_vars = function
  | Let((x, t), e1, e2) ->
    (x, t) :: (local_vars e1) @ (local_vars e2)

  | MakeCls((x, t), _, e) ->
    (x, t) :: (local_vars e)

  | Unit | Int _ | Float _ | Neg _ | FNeg _ | Add _ | Sub _
  | FAdd _ | FSub _ | FMul _ | FDiv _ | Get _ | IfEq _ | IfLE _
  | Var _ | AppCls _ | AppDir _ | Tuple _ | ExtArray _ | Put _ ->
    []

  | LetTuple _ ->
    failwith "don't know how to gather locals"


let t2ofst = function
  | Type.Unit -> 0
  | Type.Bool -> 4
  | Type.Int -> 4
  | Type.Float -> 8
  | Type.Fun(_) -> 4
  | Type.Tuple(_)
  | Type.Array(_)
  | Type.Var(_) -> failwith "don't know offset"


let str_of_ty = function
  | Type.Unit ->
    (* failwith "Unit" *)
    ""

  | Type.Int
  | Type.Fun _
    -> "i32"

  | Type.Float
    -> "f64"

  | _ ->
    failwith "WebAssembly only have i32/i64/f32/f64."


let arg_is_fvar arg fvars =
  List.find_opt ((=) arg) (List.map (fun (x, _) -> x) fvars) = (Some arg)


let emit_var oc _env fvars name =
  let rec emit_var' ofst = function
    | [] ->
      Printf.eprintf "==> can't find %s in fvars, assuming local\n" name ;
      emit oc "(get_local $%s)\n" name ;

    | (x, t) :: _ when x = name ->
      emit oc "(%s.load (i32.add (i32.const %i) (get_global $CL)))\n"
        (str_of_ty t) (ofst + (t2ofst t)) ;

    | (_, t) :: xs  ->
      emit_var' (ofst + (t2ofst t)) xs
  in
  emit_var' 0 fvars


let rec g oc env fvars = function
  | Unit ->
    ()

  | Int i ->
    emit oc "(i32.const %d)\n" i

  | Float a ->
    emit oc "(f64.const %f)" a

  | Add(x, y) ->
    emit oc "(i32.add \n" ;
    emit_var oc env fvars x ;
    emit_var oc env fvars y ;
    emit oc ")\n" ;

  | Sub(x, y) ->
    emit oc "(i32.sub \n" ;
    emit_var oc env fvars x ;
    emit_var oc env fvars y ;
    emit oc ")\n" ;

  | Var v ->
    emit_var oc env fvars v

  | Let((id, t), e1, e2) ->
    let env' = M.add id t env in
    emit oc "(set_local $%s\n" id ;
    g oc env fvars e1 ;
    emit oc ")\n" ;
    g oc env' fvars e2

  | MakeCls((x, t), { entry = Id.Label(fn_lab) ; actual_fv }, e) ->    
    let env' = M.add x t env in
    let fn = M.find fn_lab !allfns in
    let offest_list =
      List.map t2ofst (List.map (fun (_, t) -> t) fn.formal_fv) in

    emit oc "(set_local $%s (get_global $HP))\n" x ;

    emit oc "(set_global $HP (i32.add (i32.const %i) (get_global $HP)))\n"
      ((List.fold_left (+) 0 offest_list) + 4) ;

    emit oc "(i32.store (get_local $%s) (i32.const %i))\n"
      x (M.find fn_lab !fnindex) ;

    let current_offset = ref 0 in
    List.iter2
      (fun offset fv ->
         current_offset := !current_offset + offset ;
         emit oc "(i32.store " ;
         emit oc "(i32.add (i32.const %i) (get_local $%s)) " !current_offset x ;
         emit oc "(get_local $%s))\n" fv ;
      )
      offest_list
      actual_fv ;

    g oc env' fvars e

  | AppDir(Id.Label x, args) ->
    emit oc "(call $%s\n" x ;
    List.iter (emit_var oc env fvars) args ;
    emit oc ")\n" 

  | AppCls(name, args) ->
    let fun_sig = TM.find (M.find name env) !tyindex in
    (* backup CL *)
    emit oc "(set_local $cl_back (get_global $CL))\n" ;
    (* move CL *)
    emit oc "(set_global $CL " ;
    emit_var oc env fvars name ;
    emit oc ")\n" ;
    emit oc "(call_indirect (type $%s)\n" fun_sig ;
    List.iter (emit_var oc env fvars) args ;
    (* can't just emit_var here because CL moving *)
    if arg_is_fvar name fvars then
      emit oc "(i32.load (get_global $CL)))\n"
    else
      emit oc "(i32.load (get_local $%s)))\n" name ;
    (* restore CL *)
    emit oc "(set_global $CL (get_local $cl_back))\n" ;

  | _ ->
    emit oc "\n(; --- TODO --- ;)\n"


let emit_param oc with_label (label, t) =
  let ty = str_of_ty t in
  if with_label then
    emit oc " (param $%s %s)" label ty
  else
    emit oc " (param %s)" ty


let emit_result oc ret_ty =
  emit oc " (result %s)" (str_of_ty ret_ty)


let emit_fun_sig oc = function
  | { name = (Id.Label name, Type.Fun(_, ret_ty)) ; args ; _ } ->
    emit oc "(type $%s (func" name ;
    List.iter (emit_param oc false) args ;
    emit_result oc ret_ty ;
    emit oc "))\n"

  | _ ->
    failwith "emit_fun_sig: argument is not a function."


let emit_locals oc e =
  List.iter
    (fun (x, t) -> emit oc "(local $%s %s)\n" x (str_of_ty t))
    (local_vars e) ;
  (* additional CL backup *)
  emit oc "(local $cl_back i32)\n"


let emit_fun_def oc = function
  | { name = (Id.Label name, Type.Fun(_, ret_ty)) ;
      args ; formal_fv ; body ; _
    } ->
    emit oc "(func $%s" name ;
    List.iter (emit_param oc true) args ;
    emit_result oc ret_ty ;
    emit oc "\n" ;
    emit_locals oc body ;
    g oc (M.add_list (args @ formal_fv) M.empty) formal_fv body ;
    emit oc ")\n\n"

  | _ ->
    failwith "argument is not a function."    


let emit_fun oc fundef =
  emit_fun_sig oc fundef ;
  emit_fun_def oc fundef


let emit_fundefs oc fundefs =
  List.iter (emit_fun oc) fundefs


let emit_table oc fundefs =
  emit oc "(table %d anyfunc)\n" (List.length fundefs) ;
  emit oc "(elem (i32.const 0) %s)\n"
    (Id.pp_list
       (List.map
          (fun { name = Id.Label(name), _ ; _} -> "$" ^ name)
          fundefs))


let emitcode oc (Prog(fundefs, start)) =
  allfns := M.add_list
      (List.map
         (fun fd -> let (Id.Label(label), _) = fd.name in label, fd)
         fundefs)
      !allfns ;

  tyindex := List.fold_left
      (fun idx fd -> let (Id.Label(label), t) = fd.name in TM.add t label idx)
      !tyindex fundefs ;

  fnindex := M.add_list
      (List.mapi
         (fun i fd -> let (Id.Label(label), _) = fd.name in label, i)
         fundefs)
      !fnindex ;

  emit oc "(module\n" ;
  emit oc "(func $min_caml_print_int (import \"js\" \"print_int\") (param i32))\n" ;
  emit oc "(memory $0 1)\n" ;
  emit oc "(export \"memory\" (memory $0))\n" ;
  emit oc "\n"  ;
  emit oc "(global $HP (mut i32) (i32.const 0))\n" ;
  emit oc "(global $CL (mut i32) (i32.const 0))\n" ;
  emit oc "\n"  ;
  emit_table oc fundefs ;
  emit oc "\n" ;
  emit_fundefs oc fundefs ;
  emit oc "\n" ;
  (* emit oc "(func (export \"start\") (result i32)\n" ; *)
  emit oc "(func (export \"start\")\n" ;
  emit_locals oc start ;
  g oc M.empty [] start ;
  emit oc "))" ;
