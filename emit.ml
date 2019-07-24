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
let smit = Printf.sprintf


let rec local_vars = function
  | Let(xt, e1, e2) -> xt :: local_vars e1 @ local_vars e2
  | MakeCls(xt, _, e) -> xt :: local_vars e
  | IfEq(_, _, e1, e2, _)
  | IfLE(_, _, e1, e2, _) -> local_vars e1 @ local_vars e2
  | LetTuple(xts, _, e) -> xts @ local_vars e
  | _ -> []


let ofst_of_ty = function
  | Type.Unit -> 0
  | Type.Float -> 8
  | _ -> 4


(* convert Type.t to wasm type i32/i64/f32/f64 in string *)
let rec wt_of_ty env = function
  | Type.Unit -> ""
  | Type.Float -> "f64"
  | Type.Int
  | Type.Bool
  | Type.Fun _
  | Type.Tuple _
  | Type.Array _ -> "i32"
  | Type.Var { contents = None } -> failwith "wt_of_ty Var(ref(None))"
  | Type.Var { contents = Some t } -> wt_of_ty env t


let smit_var env fvs id =
  let rec smit_var' ofst = function
    | [] ->
      if M.find id env <> Type.Unit then smit "(get_local $%s)\n" id else ""

    | (x, t) :: _ when x = id ->
      if t <> Type.Unit then
        smit "(%s.load (i32.add (i32.const %i) (get_global $CL)))\n"
          (wt_of_ty env t) (ofst + (ofst_of_ty t))
      else
        ""

    | (_, t) :: xs  ->
      smit_var' (ofst + (ofst_of_ty t)) xs
  in
  smit_var' 0 fvs


let smit_vars env fvs args =
  Id.pp_list (List.map (smit_var env fvs) args)


let emit_var oc env fvs id =
  emit oc "%s" (smit_var env fvs id)


let rec g oc env fvs = function
  | Unit ->
    ()

  | Int i ->
    emit oc "(i32.const %d)\n" i

  | Float a ->
    emit oc "(f64.const %f)\n" a

  | Neg x ->
    emit oc "(i32.sub (i32.const 0) %s )\n" (smit_var env fvs x)

  | Add(x, y) ->
    emit oc "(i32.add %s %s)\n" (smit_var env fvs x) (smit_var env fvs y)

  | Sub(x, y) ->
    emit oc "(i32.sub %s %s)\n" (smit_var env fvs x) (smit_var env fvs y)

  | FNeg x ->
    emit oc "(f64.sub (f64.const 0) %s)\n" (smit_var env fvs x)

  | FAdd(x, y) ->
    emit oc "(f64.add %s %s)\n" (smit_var env fvs x) (smit_var env fvs y)

  | FSub(x, y) ->
    emit oc "(f64.sub %s %s)\n" (smit_var env fvs x) (smit_var env fvs y)

  | FMul(x, y) ->
    emit oc "(f64.mul %s %s)\n" (smit_var env fvs x) (smit_var env fvs y)

  | FDiv(x, y)->
    emit oc "(f64.div %s %s)\n" (smit_var env fvs x) (smit_var env fvs y)

  | IfEq(x, _, e1, _, _) when M.find x env = Type.Unit ->
    g oc env fvs e1

  | IfEq(x, y, e1, e2, t) ->
    emit oc "(if (result %s) (%s.eq %s %s)\n"
      (wt_of_ty env t)
      (wt_of_ty env (M.find x env))
      (smit_var env fvs x)
      (smit_var env fvs y) ;
    emit oc "(then\n" ; g oc env fvs e1 ; emit oc ")\n" ;
    emit oc "(else\n" ; g oc env fvs e2 ; emit oc "))\n"

  | IfLE(x, _, e1, _, _) when M.find x env = Type.Unit ->
    g oc env fvs e1

  | IfLE(x, y, e1, e2, t) ->
    emit oc "(if (result %s) (%s.le_s %s %s)\n"
      (wt_of_ty env t)
      (wt_of_ty env (M.find x env))
      (smit_var env fvs x)
      (smit_var env fvs y) ;
    emit oc "(then\n" ; g oc env fvs e1 ; emit oc ")\n" ;
    emit oc "(else\n" ; g oc env fvs e2 ; emit oc "))\n"

  | Let((id, Type.Unit), e1, e2) ->
    g oc env fvs e1 ;
    g oc (M.add id Type.Unit env) fvs e2

  | Let((id, t), e1, e2) ->
    emit oc "(set_local $%s " id ; g oc env fvs e1 ; emit oc ")\n" ;
    g oc (M.add id t env) fvs e2

  | Var v ->
    emit_var oc env fvs v

  | MakeCls((id, t), { entry = Id.Label(n) ; actual_fv }, e) ->
    let info = M.find n !funindex in
    let fvos = List.map (fun (_, t) -> ofst_of_ty t) info.fn.formal_fv in
    let size = (List.fold_left (+) 4 fvos) in
    emit oc
      "(; get HP ;)\n\
       (set_local $%s (get_global $HP))\n\
       (; allocate memory ;)\n\
       (set_global $HP (i32.add (i32.const %i) (get_global $HP)))\n\
       (; store func pointer ;)\n\
       (i32.store (get_local $%s) (i32.const %i))\n\
       (; store free vars ;)\n"
      id size id info.idx ;
    let cur = ref 0 in
    List.iter2
      (fun offset fv ->
         cur := !cur + offset ;
         emit oc "(i32.store (i32.add (i32.const %i) (get_local $%s)) %s)\n"
           !cur id (smit_var env fvs fv))
      fvos
      actual_fv ;
    g oc (M.add id t env) fvs e

  | AppCls(name, args) when M.mem name env ->
    emit oc
      "(; backup CL ;)\n\
       (set_local $$cl_bak (get_global $CL))\n\
       (; register cls address to CL ;)\n\
       (set_global $CL %s)\n\
       (call_indirect (type %s)\n\
       (; free vars ;)\n\
       %s\n\
       (; func pointer ;)\n\
       (i32.load (get_global $CL)))\n\
       (; restore CL ;)\n\
       (set_global $CL (get_local $$cl_bak))\n"
      (smit_var env fvs name)
      (TM.find (M.find name env) !funtyindex).ty_idx
      (smit_vars env fvs args)

  | AppCls(name, args) when M.mem name !funindex ->
    (* for indirect recursive call *)
    let info = (M.find name !funindex) in
    emit oc 
      "(; backup CL ;)\n\
       (set_local $$cl_bak (get_global $CL))\n\
       (; register fun_idx to CL ;)\n\
       (set_global $CL (i32.const %i))\n\
       (call_indirect (type %s)\n\
       (; free vars ;)\n\
       %s\
       (; func pointer ;)\n\
       (i32.const %i))\n\
       (; restore CL ;)\n\
       (set_global $CL (get_local $$cl_bak))\n"
      info.idx
      info.ty_idx
      (smit_vars env fvs args)
      info.idx

  | AppCls(name, _)  ->
    failwith ("'AppCls: " ^ name ^ "' is neither local or function.")

  | AppDir(Id.Label "min_caml_make_array", [_; a])
    when M.mem a env && M.find a env = Type.Unit ->
    ()

  | AppDir(Id.Label "min_caml_make_array", [n; a]) when M.mem a !funindex ->
    (* function array *)
    emit oc "(set_local $$counter (i32.const 0))\n" ;
    emit oc "(block\n(loop\n" ;
    emit oc "(br_if 1 (i32.eq (get_local $$counter)\n" ;
    emit_var oc env fvs n ;
    emit oc "))\n" ;
    emit oc "(i32.store\n(get_global $HP)\n" ;
    emit oc "(i32.const %i)\n" (M.find a !funindex).idx ;
    emit oc ")\n" ;
    emit oc "(set_global $HP (i32.add (i32.const 4) (get_global $HP)))\n" ;
    emit oc "(set_local $$counter\n" ;
    emit oc "(i32.add (get_local $$counter) (i32.const 1)))\n" ;
    emit oc "(br 0)\n" ;
    emit oc "))\n" ;
    emit oc "(i32.sub (get_global $HP) (i32.mul (i32.const 4)\n" ;
    emit_var oc env fvs n ;
    emit oc "))\n" ;

  | AppDir(Id.Label "min_caml_make_array", [n; a]) ->
    emit oc "(set_local $$counter (i32.const 0))\n" ;
    emit oc "(block\n(loop\n" ;
    emit oc "(br_if 1 (i32.eq (get_local $$counter)\n" ;
    emit_var oc env fvs n ;
    emit oc "))\n" ;
    emit oc "(i32.store\n(get_global $HP)\n" ;
    emit_var oc env fvs a ;
    emit oc ")\n" ;
    emit oc "(set_global $HP (i32.add (i32.const 4) (get_global $HP)))\n" ;
    emit oc "(set_local $$counter\n" ;
    emit oc "(i32.add (get_local $$counter) (i32.const 1)))\n" ;
    emit oc "(br 0)\n" ;
    emit oc "))\n" ;
    emit oc "(i32.sub (get_global $HP) (i32.mul (i32.const 4)\n" ;
    emit_var oc env fvs n ;
    emit oc "))\n" ;

  | AppDir(Id.Label "min_caml_make_float_array", [n; a]) ->
    emit oc "(set_local $$counter (i32.const 0))\n" ;
    emit oc "(block\n(loop\n" ;
    emit oc "(br_if 1 (i32.eq (get_local $$counter)\n" ;
    emit_var oc env fvs n ;
    emit oc "))\n" ;
    emit oc "(f64.store\n(get_global $HP)\n" ;
    emit_var oc env fvs a ;
    emit oc ")\n" ;
    emit oc "(set_global $HP (i32.add (i32.const 8) (get_global $HP)))\n" ;
    emit oc "(set_local $$counter\n" ;
    emit oc "(i32.add (get_local $$counter) (i32.const 1)))\n" ;
    emit oc "(br 0)\n" ;
    emit oc "))\n" ;
    emit oc "(i32.sub (get_global $HP) " ;
    emit oc "(i32.mul (i32.const 8) " ;
    emit_var oc env fvs n ;
    emit oc "))\n" ;

  | AppDir(Id.Label "min_caml_make_array", _)
  | AppDir(Id.Label "min_caml_make_float_array", _) ->
    failwith "wrong number of arguments for array creation."

  | AppDir(Id.Label name, args) ->
    emit oc "(call $%s %s)\n" name (smit_vars env fvs args)

  | Tuple xs ->
    (* reverse order *)
    let xs' = List.rev xs in
    let ts = List.map (fun x -> M.find x env) xs' in
    let offsets = List.map ofst_of_ty ts in
    let tos = List.map2 (fun t o -> (t, o)) ts offsets in
    emit oc "(set_global $HP (i32.add (i32.const %i) (get_global $HP)))\n"
      (List.fold_left (+) 0 offsets) ;
    let cur = ref 0 in
    List.iter2
      (fun x (t, o) ->
         cur := !cur + o ;
         emit oc "(%s.store (i32.sub (get_global $HP) (i32.const %i)) %s)\n"
           (wt_of_ty env t) !cur (smit_var env fvs x))
      xs'
      tos ;
    emit oc "(i32.sub (get_global $HP) (i32.const %i))\n" !cur ;

  | LetTuple(xts, y, e) ->
    let cur = ref 0 in
    List.iter
      (fun (x, t) ->
         emit oc "(set_local $%s (%s.load (i32.add (i32.const %i) %s)))\n"
           x (wt_of_ty env t) !cur (smit_var env fvs y) ;
         cur := !cur + (ofst_of_ty t))
      xts ;
    g oc (M.add_list xts env) fvs e

  | Get(x, y) ->
    begin match M.find x env with
      | Type.Array Type.Unit ->
        ()

      | Type.Array t ->
        emit oc
          "(%s.load (i32.add (i32.mul (i32.const %i) %s) %s))\n"
          (wt_of_ty env t)
          (ofst_of_ty t)
          (smit_var env fvs y)
          (smit_var env fvs x)

      | _ ->
        failwith "Get: first argument is not Array."
    end

  | Put(x, y, z) ->
    begin match M.find x env with
      | Type.Array Type.Unit ->
        ()

      | Type.Array t ->
        emit oc
          "(%s.store (i32.add (i32.mul (i32.const %i) %s) %s) %s)\n"
          (wt_of_ty env t)
          (ofst_of_ty t)
          (smit_var env fvs y)
          (smit_var env fvs x)
          (smit_var env fvs z)

      | _ ->
        failwith "Put: first argument is not Array."
    end

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


let infos_of_fundefs fundefs sigs =
  List.mapi
    (fun i ({ name = (Id.Label n, t) ; _ } as fundef) ->
       { id = n ; ty = t ; idx = i ; ty_idx = TM.find t sigs ; fn = fundef })
    fundefs


let funinfo_name_index fun_infos =
  M.add_list (List.map (fun e -> e.id, e) fun_infos) M.empty


let funinfo_ty_index fun_infos =
  List.fold_left (fun tm e -> TM.add e.ty e tm) TM.empty fun_infos


(* emit helpers *)

let emit_local oc = function
  | _, Type.Unit -> ()
  | x, t -> emit oc "(local $%s %s)\n" x (wt_of_ty M.empty t)


let emit_locals oc e =
  List.iter (emit_local oc) (local_vars e) ;
  (* additional CL backup and counter
     we can eliminate these when unnecessary, but the additional check
     just seems not worth it, and in a real production system,
     you will most certainly have a backend optimization for that anyway. *)
  emit oc "(local $$cl_bak i32)\n" ;
  emit oc "(local $$counter i32)\n"


let emit_label_param oc = function
  | _, Type.Unit -> ()
  | label, t -> emit oc " (param $%s %s)" label (wt_of_ty M.empty t)


let emit_param oc = function
  | Type.Unit -> ()
  | t -> emit oc " (param %s)" (wt_of_ty M.empty t)


let emit_result oc t =
  emit oc " (result %s)" (wt_of_ty M.empty t)


(* emit module sections *)

let emit_imports oc =
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
      ("int_of_float",  "(param f64) (result i32)") ;
      ("truncate",      "(param f64) (result i32)")
    ]


let emit_memory oc =
  emit oc "(memory (export \"memory\") 1)\n"


let emit_globals oc =
  (* heap pointer *)
  emit oc "(global $HP (mut i32) (i32.const 0))\n" ;
  (* closure pointer *)
  emit oc "(global $CL (mut i32) (i32.const 0))\n" ;
  (* generic 32-bit register, mainly use for looping *)
  emit oc "(global $GI (mut i32) (i32.const 0))\n"


let emit_table oc fds =
  emit oc
    "(table %d anyfunc)\n\
     (elem (i32.const 0) %s)\n"
    (List.length fds)
    (Id.pp_list (List.map (fun { name = Id.Label n, _ ; _ } -> "$" ^ n) fds))


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
      | { name = (Id.Label n, Type.Fun(_, t)) ; args ; formal_fv ; body } ->
        emit oc "(func $%s" n ;
        List.iter (emit_label_param oc) args ;
        emit_result oc t ;
        emit oc "\n" ;
        emit_locals oc body ;
        g oc (M.add_list (args @ formal_fv) M.empty) formal_fv body ;
        emit oc ")\n"

      | _ ->
        raise (Invalid_argument "emit_fundef"))
    fundefs


let emit_start oc start =
  emit_fundefs oc [
    { name = (Id.Label "start", Type.Fun([], Type.Unit))
    ; args = [] ; formal_fv = [] ; body = start }
  ] ;
  emit oc "(export \"start\" (func $start))"


(* emit module *)

let emitcode oc (Prog(fundefs, start)) =
  let sigs = funsig_index fundefs in
  let infos = infos_of_fundefs fundefs sigs in
  funindex := funinfo_name_index infos ;
  funtyindex := funinfo_ty_index infos ;
  emit oc "(module\n" ;
  emit_imports oc ;
  emit_memory oc ;
  emit_globals oc ;
  emit_table oc fundefs ;
  emit_types oc sigs ;
  emit_fundefs oc fundefs ;
  emit_start oc start ;
  emit oc ")"
