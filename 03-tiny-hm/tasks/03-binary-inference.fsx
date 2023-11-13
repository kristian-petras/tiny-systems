// ----------------------------------------------------------------------------
// 03 - Type inference for binary operators and conditionals
// ----------------------------------------------------------------------------

// NOTE: Start with some basic expressions from TinyML
// This time, If requires a real Boolean argument and we have
// operators '+' (int -> int -> int) and '=' (int -> int -> bool)
type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  match ty with
  | TyVariable s -> s = vcheck
  | TyList t -> occursCheck vcheck t
  | TyBool | TyNumber -> false
  
let rec substType (subst:Map<_, _>) t1 = 
  match t1 with
  | TyVariable s ->
    match subst.TryFind s with
    | None -> t1
    | Some value -> value
  | TyList t -> TyList(substType subst t)
  | TyBool | TyNumber -> t1

let substConstrs subst cs = 
  cs |> List.map (fun (left, right) -> (substType subst left, substType subst right))
 
let rec solve constraints =
  match constraints with 
  | [] -> []
  | (TyNumber, TyNumber)::cs -> solve cs
  | (TyBool, TyBool)::cs -> solve cs
  | (TyList l, TyList r)::cs -> solve ((l, r)::cs)
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

// Variable context to keep types of declared variables
// (those will typically be TyVariable cases, but don't have to)
type TypingContext = Map<string, Type>

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> 
      // NOTE: If the expression is a constant number, we return
      // its type (number) and generate no further constraints.
      TyNumber, []

  | Binary("+", e1, e2) ->
      // NOTE: Recursively process sub-expressions, collect all the 
      // constraints and ensure the types of 'e1' and 'e2' are 'TyNumber'
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
      // collect all constraints and add a constraint that (i) the type
      // of 'econd' is 'TyBool' and (ii) types of 'etrue' and 'efalse' match.
      tt, sc @ st @ sf @ [ tc, TyBool; tt, tf; tf, tt ]


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------


// Simple expressions: x = 10 + x
// Assuming x:'a, infers that 'a = int
let e1 = 
  Binary("=",   
    Variable("x"), 
    Binary("+", Constant(10), Variable("x")))

let t1, cs1 = 
  generate (Map.ofList ["x", TyVariable "a"]) e1

solve cs1

// Simple expressions: if x then 2 + 1 else y
// Assuming x:'a, y:'b, infers 'a = bool, 'b = int
let e2 = 
  If(Variable("x"), 
    Binary("+", Constant(2), Constant(1)),
    Variable("y"))

let t2, cs2 = 
  generate (Map.ofList ["x", TyVariable "a"; "y", TyVariable "b"]) e2

solve cs2

// Simple expressions: if x then 2 + 1 else x
// Cannot be solved, because 'x' used as 'int' and 'bool'
let e3 = 
  If(Variable("x"), 
    Binary("+", Constant(2), Constant(1)),
    Variable("x"))

let t3, cs3 = 
  generate (Map.ofList ["x", TyVariable "a"; "y", TyVariable "b"]) e3

solve cs3
