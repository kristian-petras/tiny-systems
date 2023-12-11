// ----------------------------------------------------------------------------
// 03 - Searching for clauses & variable renaming
// ----------------------------------------------------------------------------

open System
open System.Collections.Generic

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
  // TODO: Return a list of all variables that appear in 'term'
  // (this may contain duplicates, we will eliminate them below)
  // HINT: Use List.collect: ('a -> list<'b>) -> list<'a> -> list<'b>
  match term with
  | Atom _ -> []
  | Variable s -> [s]
  | Predicate(_, terms) -> List.collect freeVariables terms

let withFreshVariables (clause:Clause) : Clause =
  // TODO: Get a list of distinct variables in the clause (using 
  // 'freeVariables' and 'List.distinct'), generate a substitution 
  // that append a number 'n' obtained by 'nextNumber()' to the end
  // of all the variable names, and apply the substitutions to the 
  // head and body of the clause.
  //
  // For example, 'grandparent(X,Y) :- parent(X,Z), parent(Z,Y)' may
  // become 'grandparent(X3,Y3) :- parent(X3,Z3), parent(Z3,Y3)'
  //
  // This may not be correct if the user-provided names of variables
  // had numbers in them in a certain format, but that's OK for now! 
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


let query (program:list<Clause>) (query:Term) 
    : list<Clause * list<string * Term>> =
  // TODO: Return all clauses from 'program' whose 'Head' can be
  // unified with the specified 'query' and return the resulting
  // substitutions. Before unifying, rename variables in the program
  // rule using 'withFreshVariables'. You can do this using 'List.choose' 
  // or by using list comprehension.
  // 
  // The return type of this is a list of tuples consisting of the matching
  // clause and a substitution (list<string * Term>). Calling 'unify'
  // gives you 'option<list<string * Term>>', so you need to pattern match
  // on this and if it is 'Some(subst)' return 'Some(clause, subst)'.
  program
    |> List.map withFreshVariables
    |> List.map (fun f -> f.Head)
    |> List.map (unify query)
    |> List.zip program
    |> List.choose (fun (clause, subst) -> match subst with
                                            | None -> None
                                            | Some value -> Some(clause, value))


// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Generating fresh variables - repeated calls
// should append new number to all variable names
rule (Predicate("grandparent", [Variable("X"); Variable("Y")])) [
  Predicate("parent", [Variable("X"); Variable("Z")])
  Predicate("parent", [Variable("Z"); Variable("Y")]) ]
|> withFreshVariables

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

// Query: male(X)
// Match #1: male(William)
// Match #2: male(Charles)
// Match #3: male(George)
query family (Predicate("male", [Variable("X")]))

// Query: father(X, William)
// Match #1: father(X, Y) :- parent(X, Y), male(X)
query family (Predicate("father", [Variable("X"); Atom("William")]))
