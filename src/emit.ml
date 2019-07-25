open Closure


module T_ = struct
  type t = Type.t
  let compare = compare
end

module TM = Map.Make(T_)
module TS = Set.Make(T_)


(* holding various information about functions *)
type fun_info =
  { id : Id.t
  ; ty : Type.t
  ; idx : int
  ; ty_idx : string
  ; fn : Closure.fundef
  }

(* function information lookup by name *)
let funindex = ref M.empty

(* function type index lookup by type *)
let funtyindex = ref TM.empty


(* general helpers *)

let emit = Printf.fprintf
let smit = Printf.sprintf


let sep_pairs xs =
  let rec sep_pairs' acc_a acc_b = function
    | [] -> acc_a, acc_b
    | (a, b) :: xs -> sep_pairs' (a :: acc_a) (b :: acc_b) xs
  in
  sep_pairs' [] [] xs


let fold_sums f xs =
  let rec fold_sums' acc = function
    | [] -> []
    | x :: xs -> let acc' = acc + f x in acc' :: (fold_sums' acc' xs)
  in
  fold_sums' 0 xs


let hd_based xs =
  0 :: (xs |> List.rev |> List.tl |> List.rev)


(* types and sizes *)

let size_of_t = function
  | Type.Unit -> 0
  | Type.Float -> 8
  | _ -> 4


let rec wt_of_t env = function
  | Type.Unit -> ""
  | Type.Float -> "f64"
  | Type.Int
  | Type.Bool
  | Type.Fun _
  | Type.Tuple _
  | Type.Array _ -> "i32"
  | Type.Var { contents = Some t } -> wt_of_t env t
  | Type.Var { contents = None } -> failwith "wt_of_t"


(* emit var *)

let smit_var env fvs id =
  if M.mem id fvs then
    (* free vars *)
    begin
      match M.find id fvs with
      | Type.Unit, _ -> ""
      | t, o -> smit "(%s.load (i32.add (i32.const %i) (global.get $CL)))\n"
                  (wt_of_t env t) o
    end
  else
    (* locals *)
    begin match M.find id env with
      | Type.Unit -> ""
      | _ -> smit "(local.get $%s)\n" id
    end


let smit_vars env fvs args =
  Id.pp_list_sep "" (List.map (smit_var env fvs) args)


(* Currently NodeJS doesn't support WebAssembly.Global API,
   so these array making functions will be quite annoying to write as
   JavaScript externals, for now we just do it in WebAssembly. *)
let emit_make_array oc env fvs = function
  | AppDir(Id.Label "min_caml_make_array", [n; a]) ->
    emit oc
      "(global.set $GA (i32.const 0))\n\
       (global.set $GB (global.get $HP))\n\
       (block\n\
       (loop\n\
       (br_if 1 (i32.eq (global.get $GA) %s))\n\
       (i32.store\n(global.get $HP) %s)\n\
       (global.set $HP (i32.add (i32.const 4) (global.get $HP)))\n\
       (global.set $GA (i32.add (global.get $GA) (i32.const 1)))\n\
       (br 0)))\n\
       (global.get $GB)\n"
      (smit_var env fvs n)
      (if M.mem a !funindex then
         (* immediate function array *)
         smit "(i32.const %i)" (M.find a !funindex).idx
       else
         smit_var env fvs a)

  | AppDir(Id.Label "min_caml_make_float_array", [n; a]) ->
    emit oc
      "(global.set $GA (i32.const 0))\n\
       (global.set $GB (global.get $HP))\n\
       (block\n\
       (loop\n\
       (br_if 1 (i32.eq (global.get $GA) %s))\n\
       (f64.store\n(global.get $HP) %s)\n\
       (global.set $HP (i32.add (i32.const 8) (global.get $HP)))\n\
       (global.set $GA (i32.add (global.get $GA) (i32.const 1)))\n\
       (br 0)))\n\
       (global.get $GB)\n"
      (smit_var env fvs n)
      (smit_var env fvs a)

  | _ ->
    failwith "emit_make_array"


(* emit expression *)

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
      (wt_of_t env t)
      (wt_of_t env (M.find x env))
      (smit_var env fvs x)
      (smit_var env fvs y) ;
    emit oc "(then\n" ; g oc env fvs e1 ; emit oc ")\n" ;
    emit oc "(else\n" ; g oc env fvs e2 ; emit oc "))\n"

  | IfLE(x, _, e1, _, _) when M.find x env = Type.Unit ->
    g oc env fvs e1

  | IfLE(x, y, e1, e2, t) ->
    emit oc "(if (result %s) (%s.le_s %s %s)\n"
      (wt_of_t env t)
      (wt_of_t env (M.find x env))
      (smit_var env fvs x)
      (smit_var env fvs y) ;
    emit oc "(then\n" ; g oc env fvs e1 ; emit oc ")\n" ;
    emit oc "(else\n" ; g oc env fvs e2 ; emit oc "))\n"

  | Let((id, Type.Unit), e1, e2) ->
    g oc env fvs e1 ;
    g oc (M.add id Type.Unit env) fvs e2

  | Let((id, t), e1, e2) ->
    emit oc "(set_local $%s " id ;
    g oc env fvs e1 ;
    emit oc ")\n" ;
    g oc (M.add id t env) fvs e2

  | Var id ->
    emit oc "%s" (smit_var env fvs id)

  | MakeCls((id, t), { entry = Id.Label fname ; actual_fv }, e) ->
    let ts = List.map (fun x -> M.find x env) actual_fv in
    let ss = List.map size_of_t ts in
    let os = fold_sums (fun x -> x) ss in
    let total_size = (List.fold_left (+) 4 ss) in
    emit oc
      "(; get HP ;)\n\
       (set_local $%s (global.get $HP))\n\
       (; allocate memory ;)\n\
       (global.set $HP (i32.add (i32.const %i) (global.get $HP)))\n\
       (; store func pointer ;)\n\
       (i32.store (local.get $%s) (i32.const %i))\n\
       (; fvs ;)\n"
      id total_size id (M.find fname !funindex).idx ;
    List.iter2
      (fun fv o ->
         emit oc "(i32.store (i32.add (i32.const %i) (local.get $%s)) %s)\n"
           o id (smit_var env fvs fv))
      actual_fv 
      os ;
    g oc (M.add id t env) fvs e

  | AppCls(id, args) when M.mem id env ->
    emit oc
      "(; backup CL ;)\n\
       (set_local $$cl_bak (global.get $CL))\n\
       (; register cls address to CL ;)\n\
       (global.set $CL %s)\n\
       (call_indirect (type %s)\n\
       (; bvs ;)\n\
       %s\n\
       (; func pointer ;)\n\
       (i32.load (global.get $CL)))\n\
       (; restore CL ;)\n\
       (global.set $CL (local.get $$cl_bak))\n"
      (smit_var env fvs id)
      (TM.find (M.find id env) !funtyindex)
      (smit_vars env fvs args)

  | AppCls(id, args) ->
    (* For indirect recursive self-calls,
       no need to actually make the closre (and backup/restore $CL),
       because someone must have done it. *)
    let info = (M.find id !funindex) in
    emit oc 
      "(call_indirect (type %s)\n\
       (; bvs ;)\n\
       %s\
       (; func pointer ;)\n\
       (i32.const %i))\n"
      info.ty_idx
      (smit_vars env fvs args)
      info.idx

  | AppDir(Id.Label "min_caml_make_array", [_; a])
    when M.mem a env && M.find a env = Type.Unit ->
    ()

  | AppDir(Id.Label "min_caml_make_array", _)
  | AppDir(Id.Label "min_caml_make_float_array", _) as e ->
    emit_make_array oc env fvs e

  | AppDir(Id.Label label, args) ->
    emit oc "(call $%s %s)\n" label (smit_vars env fvs args)

  | Tuple xs ->
    let ts = List.map (fun x -> M.find x env) xs in
    let ss = List.map size_of_t ts in
    let total_size = List.fold_left (+) 0 ss in
    let os = hd_based (fold_sums (fun x -> x) ss) in
    emit oc
      "(global.set $GA (global.get $HP))\n\
       (global.set $HP (i32.add (i32.const %i) (global.get $HP)))\n"
      total_size ;
    List.iter2
      (fun x (t, o) ->
         emit oc "(%s.store (i32.add (global.get $GA) (i32.const %i)) %s)\n"
           (wt_of_t env t) o (smit_var env fvs x))
      xs
      (List.map2 (fun t o -> (t, o)) ts os) ;
    emit oc "(global.get $GA)\n"

  | LetTuple(xts, y, e) ->
    let _, ts = sep_pairs xts in
    let os = hd_based (fold_sums size_of_t ts) in
    List.iter2
      (fun (x, t) o ->
         emit oc "(set_local $%s (%s.load (i32.add (i32.const %i) %s)))\n"
           x (wt_of_t env t) o (smit_var env fvs y))
      xts
      os ;
    g oc (M.add_list xts env) fvs e

  | Get(x, y) ->
    begin match M.find x env with
      | Type.Array Type.Unit ->
        ()

      | Type.Array Type.Float ->
        emit oc
          "(f64.load (i32.add (i32.shl %s (i32.const 3)) %s))\n"
          (smit_var env fvs y)
          (smit_var env fvs x)

      | Type.Array _ ->
        emit oc
          "(i32.load (i32.add (i32.shl %s (i32.const 2)) %s))\n"
          (smit_var env fvs y)
          (smit_var env fvs x)

      | _ ->
        failwith "Get"
    end

  | Put(x, y, z) ->
    begin match M.find x env with
      | Type.Array Type.Unit ->
        ()

      | Type.Array Type.Float ->
        emit oc
          "(f64.store (i32.add (i32.shl %s (i32.const 3)) %s) %s)\n"
          (smit_var env fvs y)
          (smit_var env fvs x)
          (smit_var env fvs z)

      | Type.Array _ ->
        emit oc
          "(i32.store (i32.add (i32.shl %s (i32.const 2)) %s) %s)\n"
          (smit_var env fvs y)
          (smit_var env fvs x)
          (smit_var env fvs z)

      | _ ->
        failwith "Put"
    end

  | ExtArray Id.Label label ->
    emit oc "(global.get $min_caml_%s)" label


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


let funinfo_index fun_infos =
  M.add_list (List.map (fun e -> e.id, e) fun_infos) M.empty


(* emit helpers *)

let emit_label_param oc = function
  | _, Type.Unit -> ()
  | label, t -> emit oc " (param $%s %s)" label (wt_of_t M.empty t)


let emit_param oc = function
  | Type.Unit -> ()
  | _ as t -> emit oc " (param %s)" (wt_of_t M.empty t)


let emit_result oc = function
  | Type.Unit -> ()
  | _ as t -> emit oc " (result %s)" (wt_of_t M.empty t)


let rec gather_locals = function
  | Let(xt, e1, e2) ->
    xt :: gather_locals e1 @ gather_locals e2

  | MakeCls(xt, _, e) ->
    xt :: gather_locals e

  | IfEq(_, _, e1, e2, _)
  | IfLE(_, _, e1, e2, _) ->
    gather_locals e1 @ gather_locals e2

  | LetTuple(xts, _, e) ->
    xts @ gather_locals e

  | _ ->
    []


let emit_local oc = function
  | _, Type.Unit -> ()
  | name, t -> emit oc "(local $%s %s)\n" name (wt_of_t M.empty t)


let emit_locals oc e =
  List.iter (emit_local oc) (gather_locals e) ;
  (* additional CL backup,
     we can eliminate this when `e` doesn't contain MakeCLS,
     but the additional check just seems not worth it,
     and in a real production system, 
     you will most certainly have a backend optimization for that anyway. *)
  emit oc "(local $$cl_bak i32)\n"


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
  emit oc "(memory (export \"memory\") 1)\n\n"


let emit_globals oc =
  (* heap pointer *)
  emit oc "(global $HP (mut i32) (i32.const 0))\n" ;
  (* closure pointer *)
  emit oc "(global $CL (mut i32) (i32.const 0))\n" ;
  (* generic 32-bit registers *)
  emit oc "(global $GA (mut i32) (i32.const 0))\n\
           (global $GB (mut i32) (i32.const 0))\n\n"

let emit_table oc fds =
  emit oc
    "(table %d anyfunc)\n(elem (i32.const 0) %s)\n\n"
    (List.length fds)
    (Id.pp_list (List.map (fun { name = Id.Label n, _ ; _ } -> "$" ^ n) fds))


let emit_types oc sigs =
  List.iter
    (function
      | Type.Fun(args, ret_t), idx ->
        emit oc "(type %s (func" idx ;
        List.iter (emit_param oc) args ;
        emit_result oc ret_t ;
        emit oc "))\n\n"

      | _ ->
        failwith "emit_types")
    (TM.bindings sigs)


let emit_func oc = function
  | { name = (Id.Label n, Type.Fun(_, t)) ; args ; formal_fv ; body } ->
    let fvindex formal_fv =
      let _, ts = sep_pairs formal_fv in
      let os = fold_sums size_of_t ts in
      (List.map2 (fun (id, t) o -> id, (t, o)) formal_fv os)
    in
    emit oc "(func $%s" n ;
    List.iter (emit_label_param oc) args ;
    emit_result oc t ;
    emit oc "\n" ;
    emit_locals oc body ;
    (
      let env = M.add_list (args @ formal_fv) M.empty in
      let fvs = M.add_list (fvindex formal_fv) M.empty in
      g oc env fvs body
    ) ;
    emit oc ")\n\n"

  | _ ->
    failwith "emit_func"


let emit_funcs oc fundefs =
  List.iter (emit_func oc) fundefs


let emit_start oc start =
  emit_func oc
    { name = (Id.Label "$start", Type.Fun([], Type.Unit))
    ; args = [] ; formal_fv = [] ; body = start } ;
  emit oc "(start $$start)"


(* emit module *)

let emitcode oc (Prog(fundefs, start)) =
  funtyindex := funsig_index fundefs ;
  funindex := funinfo_index (infos_of_fundefs fundefs !funtyindex) ;
  emit oc "(module\n" ;
  emit_imports oc ;
  emit_memory oc ;
  emit_globals oc ;
  emit_table oc fundefs ;
  emit_types oc !funtyindex ;
  emit_funcs oc fundefs ;
  emit_start oc start ;
  emit oc ")"
