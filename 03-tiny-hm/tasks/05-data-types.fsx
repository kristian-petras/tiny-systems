// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match ty with
  | TyVariable s -> s = vcheck
  | TyList t -> occursCheck vcheck t
  | TyBool | TyNumber -> false
  | TyFunction(a, r) -> occursCheck vcheck a || occursCheck vcheck r
  | TyTuple(fst, scd) -> occursCheck vcheck fst || occursCheck vcheck scd

let rec substType (subst:Map<_, _>) t1 = 
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match t1 with
  | TyVariable s ->
    match subst.TryFind s with
    | None -> t1
    | Some value -> value
  | TyList t -> TyList(substType subst t)
  | TyBool | TyNumber -> t1
  | TyFunction(a, r) -> TyFunction((substType subst a), (substType subst r))
  | TyTuple(l, r) -> TyTuple((substType subst l), (substType subst r))

let substConstrs subst cs = 
  cs |> List.map (fun (left, right) -> (substType subst left, substType subst right))
 
let rec solve constraints =
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match constraints with 
  | [] -> []
  | (TyNumber, TyNumber)::cs -> solve cs
  | (TyBool, TyBool)::cs -> solve cs
  | (TyList l, TyList r)::cs -> solve ((l, r)::cs)
  | (TyFunction(ta1, tb1), TyFunction(ta2, tb2))::cs -> solve ((ta1, ta2)::(tb1, tb2)::cs)
  | (TyTuple(ta1, tb1), TyTuple(ta2, tb2))::cs -> solve ((ta1, ta2)::(tb1, tb2)::cs)
  | (TyVariable v, n)::cs | (n, TyVariable v)::cs ->
    if occursCheck v n then failwith "Cannot be solved (occurs check)"
    let constraints = substConstrs (Map.ofList [v, n]) cs
    let subst = solve constraints
    let n = substType (Map.ofList subst) n
    (v, n)::subst
  | (_, _)::_ -> failwith "Cannot be solved"


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
    match e with 
      | Constant _ -> 
          TyNumber, []

      | Binary("+", e1, e2) ->
          let t1, s1 = generate ctx e1
          let t2, s2 = generate ctx e2
          TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]

      | Binary("*", e1, e2) ->
        let t1, s1 = generate ctx e1
        let t2, s2 = generate ctx e2
        TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
      
      | Binary("=", e1, e2) ->
          let t1, s1 = generate ctx e1
          let t2, s2 = generate ctx e2
          TyBool, s1 @ s2 @ [ t1, t2; t2, t1 ]

      | Binary(op, _, _) ->
          failwithf $"Binary operator '%s{op}' not supported."

      | Variable v ->
          ctx[v], []

      | If(econd, etrue, efalse) ->
          let tc, sc = generate ctx econd
          let tt, st = generate ctx etrue 
          let tf, sf = generate ctx efalse
          tt, sc @ st @ sf @ [ tc, TyBool; tt, tf; tf, tt ]

      | Let(v, e1, e2) ->
          let t1, s1 = generate ctx e1
          let t2, s2 = generate (ctx.Add(v, t1)) e2
          t2, s1 @ s2
      
      | Lambda(v, e) ->
          let targ = newTyVariable()
          let t1, s1 = generate (ctx.Add(v, targ)) e
          TyFunction(targ, t1), s1

      | Application(e1, e2) -> 
          let t1, s1 = generate ctx e1
          let t2, s2 = generate ctx e2
          let tret = newTyVariable()
          tret, s1 @ s2 @ [t1, TyFunction(t2, tret)]

      | Tuple(e1, e2) ->
          let t1, s1 = generate ctx e1
          let t2, s2 = generate ctx e2
          TyTuple(t1, t2), s1 @ s2

      | TupleGet(b, e) ->
          let t1 = newTyVariable()
          let t2 = newTyVariable()
          let t, s = generate ctx e
          let constraints = s @ [t, TyTuple(t1, t2)]
          let returnType = if b then t1 else t2
          returnType, constraints

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer
TupleGet(false, etup) |> infer

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer
