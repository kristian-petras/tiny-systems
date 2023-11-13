// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise 
// a bit more interesting, we will implement constraint resolution 
// for lists here already. This will help you in the next steps!
type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

let rec occursCheck (vcheck:string) (ty:Type) =
  match ty with
  | TyVariable s -> s = vcheck
  | TyList t -> occursCheck vcheck t
  | TyBool | TyNumber -> false
 
let rec substType (subst:Map<string, Type>) (ty:Type) =
  match ty with
  | TyVariable s ->
    match subst.TryFind s with
    | None -> ty
    | Some value -> value
  | TyList t -> TyList(substType subst t)
  | TyBool | TyNumber -> ty

let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) =
  cs |> List.map (fun (left, right) -> (substType subst left, substType subst right))
  
let rec solve cs =
  match cs with 
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
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyList(TyNumber)
    TyVariable("b"), TyList(TyVariable("a")) ]

// Cannot be solved (list<'a> <> bool)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyList(TyNumber) ]

// Cannot be solved ('a <> list<'a>)
solve  
  [ TyList(TyVariable("a")), TyVariable("a") ]
