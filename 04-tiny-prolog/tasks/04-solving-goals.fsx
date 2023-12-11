// ----------------------------------------------------------------------------
// 04 - Generating and solving goals recursively
// ----------------------------------------------------------------------------

open System
open Microsoft.FSharp.Collections

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

let rec solve (program: list<Clause>) (subst:list<string*Term>) (goals: list<Term>) = 
  match goals with 
  | g::goals -> 
      // TODO: We need to solve the goal (term) 'g'. To do so, find all 
      // matching clauses in the 'program' using 'query' and iterate over
      // the returned list using 'for clause, newSubst in matches do'.
      // For each possible solution, we need to add the 'clause.Body' to 
      // the list of 'goals' and apply the substitution 'newSubst' to the
      // new concatentated list of 'goals'. Then we need to apply the 
      // substitution 'newSubst' to the substitution 'subst' we have so far,
      // append the two and call 'solve' recursively with this new substitution
      // to solve the new goals.
      let matches = query program g
      Console.WriteLine("matches")
      Console.WriteLine(matches)
      for clause, newSubst in matches do
        let newGoals = substituteTerms (Map.ofList newSubst) (clause.Body @ goals)
        let subst = substituteSubst (Map.ofList newSubst) subst
        solve program (subst @ newSubst) (newGoals)

  | [] -> 
    // TODO: We solved all goals, which means 'subst' is a possible solution!
    // Print 'subst' (either using printfn "%A" or in some nicer way).
    printfn $"%A{subst}"

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Some information about the British royal family 
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

// Query: father(X, William)
// Result #1: [ X -> Charles, ... ]
solve family [] [ Predicate("father", [Variable("X"); Atom("William")]) ]

// Query: father(X, Y)
// Result #1: [ X -> Charles, Y -> William, ... ]
// Result #2: [ X -> William, Y -> George, ... ]
solve family [] [ Predicate("father", [Variable("X"); Variable("Y")]) ]

