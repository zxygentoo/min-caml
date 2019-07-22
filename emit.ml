open Closure


module T_ = struct
  type t = Type.t
  let compare = compare
end

module TM = Map.Make(T_)
module TS = Set.Make(T_)


(* holding various information about fundef *)
type fun_info =
  { id : Id.t
  ; ty : Type.t
  ; idx : int
  ; ty_idx : string
  ; fn : Closure.fundef
  }

(* fun_info lookup by name *)
let funindex = ref M.empty

(* fun_info lookup by typ *)
let funtyindex = ref TM.empty


let emit = Printf.fprintf


let rec local_vars = function
  | Let(xt, e1, e2) -> xt :: local_vars e1 @ local_vars e2
  | MakeCls(xt, _, e) -> xt :: local_vars e
  | IfEq(_, _, e1, e2)
  | IfLE(_, _, e1, e2) -> local_vars e1 @ local_vars e2
  | LetTuple(xts, _, e) -> xts @ local_vars e
  | _ -> []


let ofst_of_ty = function
  | Type.Unit -> 0
  | Type.Float -> 8
  | _ -> 4


(* convert Type.t to i32/i64/f32/f64 in string *)
let rec primt_of_ty env = function
  | Type.Unit -> failwith "primt_of_ty Unit"
  | Type.Float -> "f64"
  | Type.Int
  | Type.Bool
  | Type.Fun _
  | Type.Tuple _
  | Type.Array _ -> "i32"
  | Type.Var { contents = None } -> failwith "primt_of_ty Var(ref(None))"
  | Type.Var { contents = Some t } -> primt_of_ty env t


let emit_var oc env fvs name =
  let rec emit_var' ofst = function
    | [] ->
      if M.find name env <> Type.Unit then
        emit oc "(get_local $%s)\n" name

    | (x, t) :: _ when x = name ->
      if t <> Type.Unit then
        emit oc "(%s.load (i32.add (i32.const %i) (get_global $CL)))\n"
          (primt_of_ty env t) (ofst + (ofst_of_ty t)) ;

    | (_, t) :: xs  ->
      emit_var' (ofst + (ofst_of_ty t)) xs
  in
  emit_var' 0 fvs


let rec g oc env fvs = function
  | Unit ->
    ()

  | Int i ->
    emit oc "(i32.const %d)\n" i

  | Float a ->
    emit oc "(f64.const %f)\n" a

  | Neg x ->
    emit oc "(i32.sub\n(i32.const 0)\n" ;
    emit_var oc env fvs x ;
    emit oc ")\n"

  | Add(x, y) ->
    emit oc "(i32.add\n" ;
    emit_var oc env fvs x ;
    emit_var oc env fvs y ;
    emit oc ")\n"

  | Sub(x, y) ->
    emit oc "(i32.sub\n" ;
    emit_var oc env fvs x ;
    emit_var oc env fvs y ;
    emit oc ")\n"

  | FNeg x ->
    emit oc "(f64.sub\n(f64.const 0)\n" ;
    emit_var oc env fvs x ;
    emit oc ")\n"

  | FAdd(x, y) ->
    emit oc "(f64.add\n" ;
    emit_var oc env fvs x ;
    emit_var oc env fvs y ;
    emit oc ")\n"

  | FSub(x, y) ->
    emit oc "(f64.sub\n" ;
    emit_var oc env fvs x ;
    emit_var oc env fvs y ;
    emit oc ")\n"

  | FMul(x, y) ->
    emit oc "(f64.mul\n" ;
    emit_var oc env fvs x ;
    emit_var oc env fvs y ;
    emit oc ")\n"

  | FDiv(x, y)->
    emit oc "(f64.div\n" ;
    emit_var oc env fvs x ;
    emit_var oc env fvs y ;
    emit oc ")\n"

  | IfEq(x, _, e1, _) when M.find x env = Type.Unit ->
    g oc env fvs e1

  | IfEq(x, y, e1, e2) ->
    let st = primt_of_ty env (M.find x env) in
    emit oc "(if (result %s)\n(%s.eq\n" st st ;
    emit_var oc env fvs x ;
    emit_var oc env fvs y ;
    emit oc ")\n(then\n" ;
    g oc env fvs e1 ;
    emit oc ")\n(else\n" ;
    g oc env fvs e2 ;
    emit oc "))\n"

  | IfLE(x, _, e1, _) when M.find x env = Type.Unit ->
    g oc env fvs e1

  | IfLE(x, y, e1, e2) ->
    let st = primt_of_ty env (M.find x env) in
    emit oc "(if (result %s)\n(%s.le_s\n" st st ;
    emit_var oc env fvs x ;
    emit_var oc env fvs y ;
    emit oc ")\n(then\n" ;
    g oc env fvs e1 ;
    emit oc ")\n(else\n" ;
    g oc env fvs e2 ;
    emit oc "))\n"

  | Let((id, Type.Unit), e1, e2) ->
    g oc env fvs e1 ;
    g oc (M.add id Type.Unit env) fvs e2

  | Let((id, t), e1, e2) ->
    emit oc "(set_local $%s\n" id ; g oc env fvs e1 ; emit oc ")\n" ;
    g oc (M.add id t env) fvs e2

  | Var v ->
    emit_var oc env fvs v

  | MakeCls((name, t), { entry = Id.Label(n) ; actual_fv }, e) ->
    let info = M.find n !funindex in
    let offsets = List.map (fun (_, t) -> ofst_of_ty t) info.fn.formal_fv in
    (* get HP *)
    emit oc "(set_local $%s (get_global $HP))\n" name ;
    (* allocate memory *)
    emit oc "(set_global $HP (i32.add (i32.const %i) (get_global $HP)))\n"
      (List.fold_left (+) 4 offsets) ;
    (* store function pointer *)
    emit oc "(i32.store (get_local $%s) (i32.const %i))\n" name info.idx ;
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
    g oc (M.add name t env) fvs e

  | AppCls(name, args) when M.mem name env ->
    (* backup CL *)
    emit oc "(set_local $$cl_back (get_global $CL))\n" ;
    (* move CL *)
    emit oc "(set_global $CL " ; emit_var oc env fvs name ; emit oc ")\n" ;
    emit oc "(call_indirect (type %s)\n"
      (TM.find (M.find name env) !funtyindex).ty_idx ;
    List.iter (emit_var oc env fvs) args ;
    emit oc "(i32.load (get_global $CL)))\n" ;
    (* restore CL *)
    emit oc "(set_global $CL (get_local $$cl_back))\n"

  | AppCls(name, args) when M.mem name !funindex ->
    (* for indirect recursive call *)
    let fun_info = (M.find name !funindex) in
    emit oc "(set_local $$cl_back (get_global $CL))\n" ;
    emit oc "(set_global $CL (i32.const %i))" fun_info.idx ;
    emit oc "(call_indirect (type %s)\n" fun_info.ty_idx ;
    List.iter (emit_var oc env fvs) args ;
    emit oc "(i32.const %i))\n" fun_info.idx ;
    emit oc "(set_global $CL (get_local $$cl_back))\n"

  | AppCls(name, _)  ->
    failwith ("'CLs " ^ name ^ "' is neither local or function.")

  | AppDir(Id.Label name, args) ->
    emit oc "(call $%s\n" name ;
    List.iter (emit_var oc env fvs) args ;
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
        emit_var oc env fvs i ;
        emit_var oc env fvs arr ;
        emit oc "))\n"

      | Type.Array(_) ->
        emit oc "(i32.load\n(i32.add\n" ;
        emit_var oc env fvs i ;
        emit_var oc env fvs arr ;
        emit oc "))\n"

      | _ ->
        failwith "Get argument not Array."
    end

  | Put _ ->
    emit oc "(; -- TODO: Put -- ;)"

  | ExtArray _ ->
    emit oc "(; -- TODO: ExtArray -- ;)"


(* function index building *)

let funsig_index fundefs =
  List.fold_left
    (fun tm (t, idx) -> TM.add t idx tm)
    TM.empty
    (fundefs
     |> List.map (fun { name = (_, t) ; _ } -> t)
     |> TS.of_list |> TS.to_seq |> List.of_seq
     |> List.mapi (fun i t -> t, "$" ^ string_of_int i))


let infos_of_fundefs fundefs sigidx =
  List.mapi
    (fun i ({ name = (Id.Label n, t) ; _ } as fundef) ->
       { id = n
       ; ty = t
       ; idx = i
       ; ty_idx = TM.find t sigidx
       ; fn = fundef })
    fundefs


let funinfo_name_index fun_infos =
  M.add_list (List.map (fun e -> e.id, e) fun_infos) M.empty


let funinfo_ty_index fun_infos =
  List.fold_left (fun tm e -> TM.add e.ty e tm) TM.empty fun_infos


(* emit helpers *)

let emit_local oc = function
  | _, Type.Unit -> ()
  | x, t -> emit oc "(local $%s %s)\n" x (primt_of_ty M.empty t)


let emit_locals oc e =
  List.iter (emit_local oc) (local_vars e) ;
  (* additional CL backup *)
  emit oc "(local $$cl_back i32)\n"


let emit_label_param oc = function
  | _, Type.Unit -> ()
  | label, t -> emit oc " (param $%s %s)" label (primt_of_ty M.empty t)


let emit_param oc = function
  | Type.Unit -> ()
  | t -> emit oc " (param %s)" (primt_of_ty M.empty t)


let emit_result oc = function
  | Type.Unit -> ()
  | t -> emit oc " (result %s)" (primt_of_ty M.empty t)


(* emit module sections *)

let emit_imports oc () =
  List.iter
    (fun (n, s) ->
       emit oc "(func $min_caml_%s (import \"core\" \"%s\") %s)\n" n n s)
    [
      ("print_newline", "") ;
      ("print_int",     "(param i32)") ;
      ("abs_float",     "(param f64) (result f64)") ;
      ("sqrt",          "(param f64) (result f64)") ;
      ("cos",           "(param f64) (result f64)") ;
      ("sin",           "(param f64) (result f64)") ;
      ("float_of_int",  "(param i32) (result f64)") ;
      ("int_of_float",  "(param f64) (result i32)")
    ]


let emit_table oc fundefs =
  emit oc "(table %d anyfunc)\n" (List.length fundefs) ;
  emit oc "(elem (i32.const 0) %s)\n"
    (Id.pp_list
       (List.map
          (fun { name = Id.Label n, _ ; _ } -> "$" ^ n)
          fundefs))


let emit_types oc sigs =
  List.iter
    (function
      | Type.Fun(args, ret_t), idx ->
        emit oc "(type %s (func" idx ;
        List.iter (emit_param oc) args ;
        emit_result oc ret_t ;
        emit oc "))\n"

      | _ ->
        raise (Invalid_argument "emit_funtype"))
    (TM.bindings sigs)


let emit_fundefs oc fundefs =
  List.iter
    (function
      | { name = (Id.Label n, Type.Fun(_, ret_t)) ;
          args ; formal_fv ; body ; _
        } ->
        emit oc "(func $%s" n ;
        List.iter (emit_label_param oc) args ;
        emit_result oc ret_t ;
        emit oc "\n" ;
        emit_locals oc body ;
        g oc (M.add_list (args @ formal_fv) M.empty) formal_fv body ;
        emit oc ")\n"

      | _ ->
        raise (Invalid_argument "emit_fundef"))
    fundefs


let emit_start oc start =
  emit oc "(func (export \"start\")\n" ;
  emit_locals oc start ;
  g oc M.empty [] start ;
  emit oc ")"


(* emit module *)

let emitcode oc (Prog(fundefs, start)) =
  let sigs = funsig_index fundefs in
  let info = infos_of_fundefs fundefs sigs in
  funindex := funinfo_name_index info ;
  funtyindex := funinfo_ty_index info ;
  emit oc "(module\n\n" ;
  emit_imports oc () ; emit oc "\n" ;
  emit oc "(memory (export \"memory\") 1)\n\n" ;
  emit oc "(global $HP (mut i32) (i32.const 0))\n" ;
  emit oc "(global $CL (mut i32) (i32.const 0))\n\n" ;
  emit_table oc fundefs ; emit oc "\n" ;
  emit_types oc sigs ; emit oc "\n" ;
  emit_fundefs oc fundefs ; emit oc "\n" ;
  emit_start oc start ;
  emit oc ")"
