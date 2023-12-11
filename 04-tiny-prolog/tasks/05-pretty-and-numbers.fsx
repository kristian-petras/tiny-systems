// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to TinyProlog
// ----------------------------------------------------------------------------

open System

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

let rec substitute (subst:Map<string, Term>) term = 
  match term with
  | Atom _ -> term
  | Variable var when subst.ContainsKey var -> subst[var]
  | Predicate(s, terms) -> Predicate(s, terms |> List.map (substitute subst))
  | Variable _ -> term

let substituteSubst (newSubst:Map<string, Term>) (subst:list<string * Term>) = 
  subst |> List.map (fun (name, term) -> (name, substitute newSubst term))

let substituteTerms subst (terms:list<Term>) = 
  terms |> List.map (substitute subst)

let rec unifyLists l1 l2 = 
  match l1, l2 with 
  | [], [] ->
      Some(list.Empty)
  | h1::t1, h2::t2 -> 
      let l1 = unify h1 h2
      match l1 with
      | None -> None
      | Some subst ->
        let newTerms1 = substituteTerms (Map.ofList subst) t1
        let newTerms2 = substituteTerms (Map.ofList subst) t2
        let l2 = unifyLists newTerms1 newTerms2
        match l2 with
        | None -> None
        | Some secondSubst ->
          let x = substituteSubst (Map.ofList secondSubst) subst
          Some(x @ secondSubst)
  | _ -> None

and unify t1 t2 = 
  match t1, t2 with
  | Atom a, Atom b -> if a = b then Some(list.Empty) else None
  | Variable s, t | t, Variable s -> Some([s, t])
  | Predicate(s1, terms1), Predicate(s2, terms2) -> if s1 = s2 then unifyLists terms1 terms2 else None
  | _ -> None

// ----------------------------------------------------------------------------
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term = 
    // TODO: Write an active pattern to recognize numbers in the form used below.
    // If the term is 'Atom("zero")' return Some(0). 
    // If the term is 'Predicate("succ", [n])' where 'n' is itself
    // a term representing number, return the number value +1. 
  match term with 
  | Atom("zero") -> Some(0)
  | Predicate("succ", [Number n]) -> Some(n + 1)
  | _ -> None


let rec formatTerm term = 
  match term with 
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) ->
      // TODO: format all arguments recursively using 'formatTerm'
      // You can then concatenate the arguments using 'String.concat'
      let items = items |> List.map formatTerm |> String.concat ","
      $"%s{p}: %s{items}"

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  match term with
  | Atom _ -> []
  | Variable s -> [s]
  | Predicate(_, terms) -> List.collect freeVariables terms

let withFreshVariables (clause:Clause) : Clause =
  let head = freeVariables clause.Head
  let tail = clause.Body |> List.map freeVariables |> List.collect id
  let combined = List.distinct(head @ tail)
  let variableNumber = nextNumber().ToString()
  let subst = combined
              |> List.map (fun name -> (name, Variable (name + variableNumber)))
              |> Map.ofList
  let newHead = substitute subst clause.Head
  let newTail = substituteTerms subst clause.Body
  rule newHead newTail

let query (program:list<Clause>) (query:Term) =
  program
    |> List.map withFreshVariables
    |> List.map (fun f -> f.Head)
    |> List.map (unify query)
    |> List.zip program
    |> List.choose (fun (a, b) -> match b with
                                  | None -> None
                                  | Some value -> Some(a, value))

let rec solve program subst goals =
  // TODO: When printing the computed substitution 'subst', print
  // the terms nicely using 'formatTerm'. You can use 'for' loop like:
  // 'for var, term in subst do printfn ...'
  match goals with 
  | g::goals -> 
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = substituteTerms (Map.ofList newSubst) (clause.Body @ goals)
        let subst = substituteSubst (Map.ofList newSubst) subst
        solve program (subst @ newSubst) (newGoals)
  | [] -> 
    for var, term in subst do printfn $"%s{var} -> %s{formatTerm term}"

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// Queries from previous step (now with readable output)
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n = 
  // TODO: Write a helper that generates a term representing number.
  // This should return Atom("zero") when n is 0 and otherwise
  // succ(succ(...(zero))) with appropriate number of 'succ's.
  if n = 0 then Atom("zero") else Predicate("succ", [num (n - 1)])


// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// Query: add(2, 3, X)
// Output should include: 'X = 5' 
//   (and other variables resulting from recursive calls)
solve nums [] [ Predicate("add", [num 2; num 3; Variable("X")]) ]

// Query: add(2, X, 5)
// Output should include: 'X = 3' 
//   (we can use 'add' to calculate subtraction too!)
solve nums [] [ Predicate("add", [num 2; Variable("X"); num 5]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))' 
//   (with some number for ?? - indicating that this can be any term)
solve nums [] [ Predicate("add", [num 2; Variable("Y"); Variable("X")]) ]
