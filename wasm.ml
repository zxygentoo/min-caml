open Closure

module T = Type

module TM = Map.Make(
  struct
    type t = Type.t
    let compare = compare
  end
)


let fnindex = ref M.empty

let tyindex = ref TM.empty

let emit = Printf.fprintf

(* now we only dealing with i32 consts and memory address, so all 4 bytes *)
let ofst_unit = 4

let id2ofst id =
  let uid = id
    |> String.split_on_char '.'
    |> List.rev
    |> List.hd
    |> int_of_string
  in
  ofst_unit * uid

let rec g oc env known = function
  | Unit ->
    ()

  | Int(i) ->
    emit oc ";; Int(%d)\n" i ;
    emit oc "i32.const %d\n" i

  | Add(x, y) ->
    emit oc ";; Add %s %s\n" x y ;
    List.iter
      (
        fun v ->
          if S.mem v known
          then emit oc "get_local $%s\n" v
          else emit oc "(i32.load (i32.const %d))\n" (id2ofst v)
      )
      [x; y]
    ;
    emit oc "i32.add\n"

  | Sub(x, y) ->
    emit oc ";; Sub %s %s\n" x y ;
    List.iter
      (
        fun v ->
          if S.mem v known
          then emit oc "get_local $%s\n" v
          else emit oc "(i32.load (i32.const %d))\n" (id2ofst v)
      )
      [x; y] ;
    emit oc "i32.sub\n"

  | Let((x, t), e1, e2) ->
    emit oc ";; Let %s\n" x ;
    let env' = M.add x t env in
    emit oc "i32.const %d\n" (id2ofst x) ;
    g oc env known e1 ;
    emit oc "i32.store\n" ;
    g oc env' known e2

  | Var(x) ->
    emit oc ";; Var(%s)\n" x ;
    if S.mem x known
    then emit oc "get_local $%s\n" x
    else emit oc "(i32.load (i32.const %d))\n" (id2ofst x)

  | MakeCls((x, t), closure, e) ->
    emit oc ";; MakeCls: %s\n" x ;

    emit oc ";; -- %s: funcaddr\n" x ;
    emit oc "(i32.store (i32.const %d) (i32.const %d))\n"
      (id2ofst x) (M.find x !fnindex) ;

    emit oc ";; -- %s: fvs\n" x ;
    let { entry = _entry ; actual_fv } = closure in
    List.iter
      (
        fun fv ->
          let ofst = id2ofst fv in
          if S.mem fv known then
            emit oc "(i32.store (i32.const %d) (get_local $%s))\n" ofst fv
      )
      (List.rev actual_fv) ;

    emit oc ";; -- %s: body\n" x ;
    g oc (M.add x t env) known e

  | AppDir(Id.Label(x), bvs) ->
    emit oc ";; AppDir %s\n" x ;
    emit oc "(call $%s\n" x ;
    List.iter
      (fun bv ->
        if S.mem bv known
        then emit oc "    get_local $%s\n" x
        else emit oc "    (i32.load (i32.const %d))\n" (id2ofst bv)
      )
      bvs ;
    emit oc ")\n"

  | AppCls(x, bvs) ->
    emit oc ";; AppCls %s\n" x ;

    emit oc ";; AppCls %s --- bvs\n" x ;
    List.iter
      (fun bv -> emit oc "(i32.load (i32.const %d))\n" (id2ofst bv)) bvs ;
    emit oc "(call_indirect (type $%s) (i32.load (i32.const %d)))\n"
      (TM.find (M.find x env) !tyindex) (id2ofst x)

  | _ ->
    failwith "~~> don't know how to compile this yet...\n"

let t2s = function
  | T.Int -> "i32"
  | T.Fun(_) -> "i32"
  | _ -> failwith "don't know how to deal with this yet..."

let emit_result oc ty =
  emit oc "(result %s)" (t2s ty)

let emit_param oc with_label (label, ty) =
  if with_label
  then emit oc "(param $%s %s) " label (t2s ty)
  else emit oc "(param %s) " (t2s ty)

let emit_sig oc with_label ret args =
  List.iter (emit_param oc with_label) args ;
  (
    match ret with
    | T.Fun(_, ret) ->
      emit_result oc ret

    | _ ->
      failwith "fundef doesn't have Fun type."
  )

let emit_func oc { name = (Id.Label(label), ret); args; formal_fv; body } =
  emit oc "(type $%s (func " label ;
  emit_sig oc false ret args ;
  emit oc "))\n" ;
  emit oc "(func $%s " label ;
  emit_sig oc true ret args ;
  emit oc "\n" ;
  g oc
    (M.add_list (args @ formal_fv) M.empty)
    (S.of_list (List.map (fun (label, _) -> label) args))
    body ;
  emit oc ")\n"

let emit_funcs oc fns =
    List.iter (emit_func oc) fns

let emit_table oc fns =
  emit oc "(table %d anyfunc)\n" (List.length fns) ;
  emit oc "(elem (i32.const 0)" ;
  List.iter
    (fun i -> emit oc " %s" (string_of_int i))
    (List.init (List.length fns) (fun i -> i)) ;
  emit oc ")\n"

let emitcode oc (Prog(fundefs, e)) =
  Format.eprintf "==> generating WebAssembly...@." ;

  fnindex := M.add_list
    (List.mapi
        (fun i fd -> let (Id.Label(label), _) = fd.name in label, i)
        fundefs)
    !fnindex ;

  tyindex := List.fold_left
    (fun idx fd -> let (Id.Label(label), t) = fd.name in TM.add t label idx)
    !tyindex fundefs ;

  let env = M.add_list
    (List.map (fun fd -> let Id.Label(label), t = fd.name in (label, t)) fundefs)
    M.empty
  in

  emit oc "(module\n" ;
  emit oc ";; memory section\n" ;
  emit oc "(memory $0 1)\n" ;
  emit oc "(export \"memory\" (memory $0))\n" ;
  emit oc ";; functions\n" ;
  emit_funcs oc fns ;
  emit oc ";; table section\n" ;
  emit_table oc fundefs ;
  emit oc ";; start function\n" ;
  emit oc "(func $start (result i32)\n" ;
  g oc env S.empty e ;
  emit oc ")\n" ;
  emit oc "\n;; export start function\n" ;
  emit oc "(export \"start\" (func $start))\n" ;
  emit oc ")\n";
