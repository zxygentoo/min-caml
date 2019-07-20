open Closure


module TM = Map.Make(
  struct
    type t = Type.t
    let compare = compare
  end
  )


type funindex_entry =
  { ty : Type.t      (* function type *)
  ; idx : int        (* function index *)
  ; ty_idx : string  (* function type index *)
  }


let fnindex = ref M.empty
let allfns = ref M.empty
let tyindex = ref TM.empty


let emit = Printf.fprintf


let rec local_vars = function
  | Let(xt, e1, e2) ->
    xt :: local_vars e1 @ local_vars e2

  | MakeCls(xt, _, e) ->
    xt :: local_vars e

  | IfEq(_, _, e1, e2)
  | IfLE(_, _, e1, e2) ->
    local_vars e1 @ local_vars e2

  | LetTuple(xts, _, e) ->
    xts @ local_vars e

  | Unit
  | Int _
  | Float _
  | Neg _
  | FNeg _
  | Add _
  | Sub _
  | FAdd _
  | FSub _
  | FMul _
  | FDiv _
  | Get _
  | Var _
  | AppCls _
  | AppDir _
  | Tuple _
  | ExtArray _
  | Put _ ->
    []


let ofst_of_ty = function
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
    ""

  | Type.Float ->
    "f64"

  | Type.Int
  | Type.Fun _
  | Type.Array _
  | Type.Tuple _ ->
    "i32"

  | _ ->
    failwith "WebAssembly only have i32/i64/f32/f64."


let arg_is_fvar arg fvars =
  List.find_opt ((=) arg) (List.map (fun (x, _) -> x) fvars) = (Some arg)


let emit_var oc env fvars name =
  let rec emit_var' ofst = function
    | [] ->
      if M.find name env = Type.Unit then
        ()
      else
        emit oc "(get_local $%s)\n" name

    | (x, t) :: _ when x = name ->
      (* free var *)
      if t = Type.Unit then
        ()
      else
        emit oc "(%s.load (i32.add (i32.const %i) (get_global $CL)))\n"
          (str_of_ty t) (ofst + (ofst_of_ty t)) ;

    | (_, t) :: xs  ->
      emit_var' (ofst + (ofst_of_ty t)) xs
  in
  emit_var' 0 fvars


let rec g oc env fvars = function
  | Unit ->
    ()

  | Int i ->
    emit oc "(i32.const %d)\n" i

  | Float a ->
    emit oc "(f64.const %f)\n" a

  | Neg x ->
    emit oc "(i32.sub\n(i32.const 0)\n" ;
    emit_var oc env fvars x ;
    emit oc ")\n"

  | Add(x, y) ->
    emit oc "(i32.add\n" ;
    emit_var oc env fvars x ;
    emit_var oc env fvars y ;
    emit oc ")\n"

  | Sub(x, y) ->
    emit oc "(i32.sub\n" ;
    emit_var oc env fvars x ;
    emit_var oc env fvars y ;
    emit oc ")\n"

  | FNeg x ->
    emit oc "(f64.sub\n(f64.const 0)\n" ;
    emit_var oc env fvars x ;
    emit oc ")\n"

  | FAdd(x, y) ->
    emit oc "(f64.add\n" ;
    emit_var oc env fvars x ;
    emit_var oc env fvars y ;
    emit oc ")\n"

  | FSub(x, y) ->
    emit oc "(f64.sub\n" ;
    emit_var oc env fvars x ;
    emit_var oc env fvars y ;
    emit oc ")\n"

  | FMul(x, y) ->
    emit oc "(f64.mul\n" ;
    emit_var oc env fvars x ;
    emit_var oc env fvars y ;
    emit oc ")\n"

  | FDiv(x, y)->
    emit oc "(f64.div\n" ;
    emit_var oc env fvars x ;
    emit_var oc env fvars y ;
    emit oc ")\n"

  | IfEq(x, y, e1, e2) ->
    let st = str_of_ty (M.find x env) in
    emit oc "(if (result %s)\n(%s.eq\n" st st ;
    emit_var oc env fvars x ;
    emit_var oc env fvars y ;
    emit oc ")\n(then\n" ;
    g oc env fvars e1 ;
    emit oc ")\n(else\n" ;
    g oc env fvars e2 ;
    emit oc "))\n"

  | IfLE(x, y, e1, e2) ->
    let st = str_of_ty (M.find x env) in
    emit oc "(if (result %s)\n(%s.le_s\n" st st ;
    emit_var oc env fvars x ;
    emit_var oc env fvars y ;
    emit oc ")\n(then\n" ;
    g oc env fvars e1 ;
    emit oc ")\n(else\n" ;
    g oc env fvars e2 ;
    emit oc "))\n"

  | Let((id, Type.Unit), e1, e2) ->
    g oc env fvars e1 ;
    g oc (M.add id Type.Unit env) fvars e2

  | Let((id, t), e1, e2) ->
    emit oc "(set_local $%s\n" id ;
    g oc env fvars e1 ;
    emit oc ")\n" ;
    g oc (M.add id t env) fvars e2

  | Var v ->
    emit_var oc env fvars v

  | MakeCls((name, t), { entry = Id.Label(fn_lab) ; actual_fv }, e) ->    
    let env' = M.add name t env in
    let fn = M.find fn_lab !allfns in
    let offsets = List.map (fun (_, t) -> ofst_of_ty t) fn.formal_fv in
    (* get current HP *)
    emit oc "(set_local $%s (get_global $HP))\n" name ;
    (* allocate space for free vars and move HP *)
    emit oc "(set_global $HP (i32.add (i32.const %i) (get_global $HP)))\n"
      (List.fold_left (+) 4 offsets) ;
    (* store function pointer *)
    emit oc "(i32.store (get_local $%s) (i32.const %i))\n"
      name (M.find fn_lab !fnindex) ;
    (* store free vars *)
    let cur_ofst = ref 0 in
    List.iter2
      (fun offset fv ->
         cur_ofst := !cur_ofst + offset ;
         emit oc "(i32.store " ;
         emit oc "(i32.add (i32.const %i) (get_local $%s)) " !cur_ofst name ;
         emit oc "(get_local $%s))\n" fv ;
      )
      offsets
      actual_fv ;
    g oc env' fvars e

  | AppCls(name, args) when M.mem name env ->
    let fun_sig = TM.find (M.find name env) !tyindex in
    (* backup CL *)
    emit oc "(set_local $$cl_back (get_global $CL))\n" ;
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
      emit oc "(i32.load (get_local $%s)))\n" name
    ;
    (* restore CL *)
    emit oc "(set_global $CL (get_local $$cl_back))\n"

  | AppCls(name, args) when M.mem name !fnindex ->
    (* indirect recursive call *)
    let fun_idx = M.find name !fnindex in
    let _, fun_ty = (M.find name !allfns).name in
    let fun_sig = TM.find fun_ty !tyindex in
    emit oc "(set_local $$cl_back (get_global $CL))\n" ;
    emit oc "(set_global $CL (i32.const %i))" fun_idx ;
    emit oc "(call_indirect (type $%s)\n" fun_sig ;
    List.iter (emit_var oc env fvars) args ;
    emit oc "(i32.const %i))\n" fun_idx ;
    emit oc "(set_global $CL (get_local $$cl_back))\n"

  | AppCls(name, _)  ->
    failwith ("'" ^ name ^ "' is neither local or function.")

  | AppDir(Id.Label name, args) ->
    emit oc "(call $%s\n" name ;
    List.iter (emit_var oc env fvars) args ;
    emit oc ")\n"

  | Tuple xs ->
    List.iter (fun x -> M.find x env |> ignore) xs ;
    emit oc "(; -- TODO: Tuple %s -- ;)" (Id.pp_list xs)

  | LetTuple _ ->
    emit oc "(; -- TODO: LetTuple -- ;)"

  | Get(arr, i) ->
    begin match M.find arr env with
      | Type.Array(Type.Unit) ->
        ()

      | Type.Array(Type.Float) ->
        emit oc "(f64.load\n(i32.add\n" ;
        emit_var oc env fvars i ;
        emit_var oc env fvars arr ;
        emit oc "))\n"

      | Type.Array(_) ->
        emit oc "(i32.load\n(i32.add\n" ;
        emit_var oc env fvars i ;
        emit_var oc env fvars arr ;
        emit oc "))\n"

      | _ ->
        failwith "Get argument not Array."
    end

  | Put _ ->
    emit oc "(; -- TODO: Put -- ;)"

  | ExtArray _ ->
    emit oc "(; -- TODO: ExtArray -- ;)"


let emit_param oc with_label = function
  | _, Type.Unit ->
    ()

  | label, t ->
    let ty = str_of_ty t in
    if with_label then
      emit oc " (param $%s %s)" label ty
    else
      emit oc " (param %s)" ty


let emit_result oc = function
  | Type.Unit ->
    ()

  | ret_ty ->
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
    (function
      | _, Type.Unit -> ()
      | x, t -> emit oc "(local $%s %s)\n" x (str_of_ty t))
    (local_vars e) ;
  (* additional CL backup *)
  emit oc "(local $$cl_back i32)\n"


let emit_fun_def oc = function
  | { name = (Id.Label name, Type.Fun(_, ret_ty)) ;
      args ; formal_fv ; body ; _
    } ->
    emit oc "(func $%s" name ;
    List.iter (emit_param oc true) args ;
    emit_result oc ret_ty ;
    emit oc "\n" ;
    emit_locals oc body ;
    g
      oc
      (M.add_list (args @ formal_fv) M.empty)
      formal_fv
      body ;
    emit oc ")\n"

  | _ ->
    failwith "argument is not a function."    


let emit_fun oc fundef =
  emit_fun_sig oc fundef ;
  emit_fun_def oc fundef ;
  emit oc "\n"


let emit_fundefs oc fundefs =
  List.iter (emit_fun oc) fundefs


let emit_table oc fundefs =
  emit oc "(table %d anyfunc)\n" (List.length fundefs) ;
  emit oc "(elem (i32.const 0) %s)\n"
    (Id.pp_list
       (List.map
          (fun { name = Id.Label(name), _ ; _ } -> "$" ^ name)
          fundefs))


let emit_imports oc () =
  let f n t =
    emit oc "(func $min_caml_%s (import \"core\" \"%s\") %s)\n" n n t
  in
  f "print_newline" "" ;
  f "print_int" "(param i32)" ;
  f "abs_float" "(param f64) (result f64)" ;
  f "sqrt" "(param f64) (result f64)" ;
  f "cos" "(param f64) (result f64)" ;
  f "sin" "(param f64) (result f64)" ;
  f "float_of_int" "(param i32) (result f64)" ;
  f "int_of_float" "(param f64) (result i32)"


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

  let funtyindex =
    M.add_list
      (fundefs
      |> List.map (fun { name = (_, t) ; _ } -> t)
      |> S.of_list
      |> S.to_seq
      |> List.of_seq
      |> List.mapi (fun t -> t, "$" ^ string_of_int i)
      M.empty
  in

  let _funindex =
    M.add_list
      (List.mapi
        (fun i { name = (Id.Label(x), t) ; _ } ->
          x, { ty = t ; idx = i ; ty_idx = M.find t funtyindex })
        fundefs)
      M.empty
  in

  emit oc "(module\n" ;
  emit_imports oc () ;
  emit oc "(memory (export \"memory\") 1)\n" ;
  emit oc "\n" ;
  emit oc "(global $HP (mut i32) (i32.const 0))\n" ;
  emit oc "(global $CL (mut i32) (i32.const 0))\n" ;
  emit oc "\n" ;
  emit_table oc fundefs ;
  emit oc "\n" ;
  emit_fundefs oc fundefs ;
  emit oc "\n" ;
  emit oc "(func (export \"start\")\n" ;
  emit_locals oc start ;
  g oc M.empty [] start ;
  emit oc "))" ;
