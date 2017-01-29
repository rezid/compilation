open Position
open Error
open HopixAST

(** [error pos msg] reports runtime error messages. *)
let error positions msg =
  errorN "execution" positions msg

exception MatchError

(** Every expression of hopix evaluates into a [value]. *)
type 'e gvalue =
  | VBool         of bool
  | VInt          of Int32.t
  | VChar         of char
  | VString       of string
  | VUnit
  | VAddress      of Memory.location
  | VTaggedValues of constructor * 'e gvalue list
  | VPrimitive    of string * ('e gvalue Memory.t -> 'e gvalue list -> 'e gvalue)
  | VFun          of pattern located list * expression located * 'e

type ('a, 'e) coercion = 'e gvalue -> 'a option
let value_as_int      = function VInt x -> Some x | _ -> None
let value_as_bool     = function VBool x -> Some x | _ -> None
let value_as_char     = function VChar c -> Some c | _ -> None

type ('a, 'e) wrapper = 'a -> 'e gvalue
let int_as_value x  = VInt x

let primitive name ?(error = fun () -> assert false) coercion wrapper f =
  VPrimitive (name, fun x ->
      match coercion x with
      | None -> error ()
      | Some x -> wrapper (f x)
    )

let print_value m v =
  let max_depth = 5 in

  let rec print_value d v =
    if d >= max_depth then "..." else
      match v with
      | VInt x                    -> Int32.to_string x
      | VBool true                -> "true"
      | VBool false               -> "false"
      | VChar c                   -> "'" ^ Char.escaped c ^ "'"
      | VString s                 -> "\"" ^ String.escaped s ^ "\""
      | VUnit                     -> "()"
      | VAddress a                -> print_array_value d (Memory.dereference m a)
      | VTaggedValues (KId k, []) -> k
      | VTaggedValues (KId k, vs) -> k ^ "(" ^ String.concat ", " (List.map (print_value (d + 1)) vs) ^ ")"
      | VFun _                    -> "<fun>"
      | VPrimitive (s, _)         -> Printf.sprintf "<primitive: %s>" s
  and print_array_value d block =
    let r = Memory.read block in
    let n = Memory.size block in
    "[ " ^ String.concat ", " (
      List.(map (fun i -> print_value (d + 1) (r i)) (ExtStd.List.range 0 (n - 1))
           )) ^ " ]"
  in
  print_value 0 v

module Environment : sig
  (** Evaluation environments map identifiers to values. *)
  type t

  (** The empty environment. *)
  val empty : t

  (** [bind env x v] extends [env] with a binding from [x] to [v]. *)
  val bind    : t -> identifier -> t gvalue -> t

  (** [update pos x env v] modifies the binding of [x] in [env] so
      that [x ↦ v] ∈ [env]. *)
  val update  : Position.t -> identifier -> t -> t gvalue -> unit

  (** [lookup pos x env] returns [v] such that [x ↦ v] ∈ env. *)
  val lookup  : Position.t -> identifier -> t -> t gvalue

  (** [UnboundIdentifier (x, pos)] is raised when [update] or
      [lookup] assume that there is a binding for [x] in [env],
      where there is no such binding. *)
  exception UnboundIdentifier of identifier * Position.t

  (** [last env] returns the latest binding in [env] if it exists. *)
  val last    : t -> (identifier * t gvalue * t) option

  (** [print env] returns a human readable representation of [env]. *)
  val print   : t gvalue Memory.t -> t -> string
end = struct

  type t =
    | EEmpty
    | EBind of identifier * t gvalue ref * t

  let empty = EEmpty

  let bind e x v =
    EBind (x, ref v, e)

  exception UnboundIdentifier of identifier * Position.t

  let lookup' pos x =
    let rec aux = function
      | EEmpty -> raise (UnboundIdentifier (x, pos))
      | EBind (y, v, e) ->
        if x = y then v else aux e
    in
    aux

  let lookup pos x e = !(lookup' pos x e)

  let update pos x e v =
    lookup' pos x e := v

  let last = function
    | EBind (x, v, e) -> Some (x, !v, e)
    | EEmpty -> None

  let print_binding m (Id x, v) =
    x ^ " = " ^ print_value m !v

  let print m e =
    let b = Buffer.create 13 in
    let push x v = Buffer.add_string b (print_binding m (x, v)) in
    let rec aux = function
      | EEmpty -> Buffer.contents b
      | EBind (x, v, EEmpty) -> push x v; aux EEmpty
      | EBind (x, v, e) -> push x v; Buffer.add_string b "\n"; aux e
    in
    aux e

end

type value = Environment.t gvalue

type formals = identifier list

type runtime = {
  memory      : value Memory.t;
  environment : Environment.t;
}

type observable = {
  new_memory      : value Memory.t;
  new_environment : Environment.t;
}

(** [primitives] is an environment that contains the implementation
    of all primitives (+, <, ...). *)
let primitives =
  let intbin name out op =
    VPrimitive (name, fun _ -> function
        | [VInt x; VInt y] -> out (op x y)
        | _ -> assert false (* By typing. *)
      )
  in
  let bind_all what l x =
    List.fold_left (fun env (x, v) -> Environment.bind env (Id x) (what x v)) x l
  in
  (* Define arithmetic binary operators. *)
  let binarith name =
    intbin name (fun x -> VInt x) in
  let binarithops = Int32.(
      [ ("`+", add); ("`-", sub); ("`*", mul); ("`/", div) ]
    ) in
  (* Define arithmetic comparison operators. *)
  let cmparith name = intbin name (fun x -> VBool x) in
  let cmparithops =
    [ ("`=", ( = )); ("`<", ( < )); ("`>", ( > )); ("`>=", ( >= )); ("`<=", ( <= )) ]
  in
  let boolbin name out op =
    VPrimitive (name, fun m -> function
        | [VBool x; VBool y] -> out (op x y)
        | _ -> assert false (* By typing. *)
      )
  in
  let boolarith name = boolbin name (fun x -> VBool x) in
  let boolarithops =
    [ ("`||", ( || )); ("`&&", ( && )) ]
  in
  let generic_printer =
    VPrimitive ("print", fun m vs ->
        let repr = String.concat ", " (List.map (print_value m) vs) in
        output_string stdout repr;
        flush stdout;
        VUnit
      )
  in
  let print s =
    output_string stdout s;
    flush stdout;
    VUnit
  in
  let print_int =
    VPrimitive  ("print_int", fun m -> function
        | [ VInt x ] -> print (Int32.to_string x)
        | _ -> assert false (* By typing. *)
      )
  in
  let print_string =
    VPrimitive  ("print_string", fun m -> function
        | [ VString x ] -> print x
        | _ -> assert false (* By typing. *)
      )
  in
  let bind' x w env = Environment.bind env (Id x) w in
  Environment.empty
  |> bind_all binarith binarithops
  |> bind_all cmparith cmparithops
  |> bind_all boolarith boolarithops
  |> bind' "print"        generic_printer
  |> bind' "print_int"    print_int
  |> bind' "print_string" print_string
  |> bind' "true"         (VBool true)
  |> bind' "false"        (VBool false)

let initial_runtime () = {
  memory      = Memory.create (640 * 1024 (* should be enough. -- B.Gates *));
  environment = primitives;
}

let rec evaluate runtime ast =
  try
    let runtime' = List.fold_left definition runtime ast in
    (runtime', extract_observable runtime runtime')
  with Environment.UnboundIdentifier (Id x, pos) ->
    Error.error "interpretation" pos (Printf.sprintf "`%s' is unbound." x)

(* [definition pos runtime d] evaluates the new definition [d]
   into a new runtime [runtime']. In the specification, this
   is the judgment:

   E, M ⊢ dᵥ ⇒ E', M'

*)
and definition runtime d =
  match Position.value d with
  | DefineValue (x, e) ->
    let v, memory = expression' runtime.environment runtime.memory e in
    { 
      environment = bind_identifier runtime.environment x v;
      memory
    }

  | DefineType _ -> 
    runtime

  | DeclareExtern _ ->
    runtime

  | DefineRecFuns fs -> 
    let fs = List.map (fun (x, e) -> (Position.value x, Position.value e)) fs in
    let fs = List.map (fun (x, FunctionDefinition(_,mBar,e)) -> (x,mBar,e)) fs in
    let rec environment1 = List.fold_left (fun env (f, mBar, e) -> 
        let e' = Environment.bind env f (VFun(mBar,e,env)) in
        Environment.update (Position.position e) f e' (VFun(mBar,e,e'));
        e'
      ) runtime.environment fs in
    {
      environment = environment1;
      memory = runtime.memory
    } 



and expression' environment memory e =
  expression (position e) environment memory (value e)

(* [expression pos runtime e] evaluates into a value [v] if

                          E, M ⊢ e ⇓ v, M'

   and E = [runtime.environment], M = [runtime.memory].
*)
and expression position environment memory = function
  | Literal l -> 
    literal (value l), memory

  | Variable x ->
    Environment.lookup (Position.position x) (Position.value x) environment, memory

  | Tagged (k, _, elist) ->
    let last_memory = ref memory in
    let rec calcule_one elist m0 =
      match elist  with
      | [] -> last_memory := m0; []
      | e0::rest -> 
        let v0,m1 = expression' environment m0 e0 in
        v0 :: (calcule_one rest m1) 
    in
    VTaggedValues(Position.value k,calcule_one elist memory), !last_memory

  | TypeAnnotation(e,_) ->
    expression' environment memory e

  | Define(id, dv, e) ->
    let v, memory = expression' environment memory dv in
    let environment = bind_identifier environment id v in
    expression' environment memory e

  | Apply (e, _, eBar) ->
    let val_e, m0 = expression' environment memory e in
    begin
      match val_e with
      | VFun(m_list,e,e0) ->
        let last_memory = ref memory in
        let rec calcule_v eBar m0 =
          match eBar with
          | [] -> last_memory := m0; []
          | e1::rest -> let v1,m1 = expression' environment m0 e1 in
            v1::(calcule_v rest m1) in
        let vBar = calcule_v eBar m0 in
        let rec calcule_envirenement vBar mBar e0 =
          begin
            match vBar, mBar with
            | [],[] -> e0
            | v1::rest1, m1::rest2 -> let e1 = bind_pattern e0 m1 v1 in
              calcule_envirenement rest1 rest2 e1
            | _ -> failwith("error partial application ??")
          end
        in
        let last_environment = calcule_envirenement vBar m_list e0 in
        expression' last_environment !last_memory e
      | VPrimitive(_, f) ->
        let last_memory = ref memory in
        let rec calcule_v eBar m0 =
          match eBar with
          | [] -> last_memory := m0; []
          | e1::rest -> let v1,m1 = expression' environment m0 e1 in
            v1::(calcule_v rest m1) in
        let vBar = calcule_v eBar m0 in
        f !last_memory vBar, !last_memory
      | _ -> 
        failwith("assert false because typing system ?")
    end

  | Fun (FunctionDefinition (_, mBar, e)) ->
    VFun(mBar, e, environment),memory

  | If (ifList,elseOption) ->
    begin
      let rec findTrue ifList =
        match ifList with
        | [] -> begin match elseOption with
            | None -> None
            | Some e -> Some e
          end
        | (c,t)::rest ->
          let v, memory = expression' environment memory c in
          begin match value_as_bool v with
            | None -> failwith("not possible!")
            | Some true -> Some t
            | Some false -> findTrue rest
          end
      in
      let eTrue = findTrue ifList in
      begin match eTrue with
        | None -> VUnit,memory
        | Some eTrue -> expression' environment memory eTrue
      end
    end

  | Ref(e) -> 
    let v, m' = expression' environment memory e in
    VAddress (Memory.allocate m' 1 v), m'


  | Read (e) ->
    let a, m' = expression' environment memory e in
    begin match a with
      | VAddress a -> 
        Memory.read (Memory.dereference memory a) 0,m'
      | _ -> 
        failwith("not possible on Read (e)")
    end

  | Write (e,e') ->
    let a, m' = expression' environment memory e in 
    let v, m'' = expression' environment m' e' in
    begin match a with
      | VAddress a -> 
        Memory.write (Memory.dereference m'' a) 0 v;
        VUnit, m''
      | _ -> failwith("not possible on Write (e,e')")
    end

  | Case (e, branchBar) ->
    let vs, m' = expression' environment memory e in 
    let rec calcule_expression bBar =
      begin match bBar with
        | b::rest -> 
          begin match Position.value b with
            | Branch(p,e) ->
              begin try 
                  let environment = bind_pattern environment p vs in
                  expression' environment m' e
                with 
                | MatchError -> calcule_expression rest
              end
            | _ -> failwith("not possible on  Case (e, branchBar)")
          end
        | _ -> failwith("not possible on  Case (e, branchBar)")
      end
    in
    calcule_expression branchBar

  | While(eb,e) ->
    let b, m' = expression' environment memory eb in
    begin match b with
      | VBool false -> VUnit,m'
      | VBool true -> 
        let  b, m'' = expression' environment m' e in
        begin match b with
          | VUnit -> VUnit, m''
          | _ -> failwith("TO DO on While1")

        end
      | _ ->  failwith("TO DO on While2")
    end

  | DefineRec (fs,e) -> 
    failwith("to do DefineRec (fs,e)")


and expressions environment memory es =
  let rec aux vs memory = function
    | [] ->
      List.rev vs
    | e :: es ->
      let v = expression' environment memory e in
      aux (v :: vs) memory es
  in
  aux [] memory es


and bind_identifier environment x v =
  Environment.bind environment (Position.value x) v

and bind_pattern environment pat v = 
  match Position.value pat, v with
  | PWildcard, _ -> 
    environment

  | PVariable id, _ ->
    Environment.bind environment (Position.value id) v

  | PTaggedValue (k, pBar), VTaggedValues (k', vBar) ->
    if (Position.value k) = k' then List.fold_left2 bind_pattern environment pBar vBar 
    else raise MatchError

  | _, _ -> failwith("TO DO on bind_pattern")

and literal = function
  | LInt x -> VInt x
  | LChar c -> VChar c
  | LBool b -> VBool b
  | LString s -> VString s 

and extract_observable runtime runtime' =
  let rec substract new_environment env env' =
    if env == env' then new_environment
    else
      match Environment.last env' with
      | None -> assert false (* Absurd. *)
      | Some (x, v, env') ->
        let new_environment = Environment.bind new_environment x v in
        substract new_environment env env'
  in
  {
    new_environment =
      substract Environment.empty runtime.environment runtime'.environment;
    new_memory =
      runtime'.memory
  }

let print_observable runtime observation =
  Environment.print observation.new_memory observation.new_environment
