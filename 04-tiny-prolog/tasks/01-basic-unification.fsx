// ----------------------------------------------------------------------------
// 01 - Implementing basic unication of terms
// ----------------------------------------------------------------------------

// A term is a recursive type, which can be either an atom (a known fact 
// like 'socrates'), a variable (to which we want to asign a term by 
// unification), or a predicate (with predicate name and a list of arguments). 
type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list
  //| Call of Term * Term list

// A clause is for example 'mortal(X) :- human(X)' or 'human(socrates)'. It
// consists of a head and body (head :- body). Body is a sequence of terms.
type Clause =
  { Head : Term
    Body : Term list }

// A program is just a list of clauses
type Program = Clause list

// Create a clause that states a fact
let fact p = { Head = p; Body = [] }

// Create a clause that defines a rule  
let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Unification of terms and lists
// ----------------------------------------------------------------------------

let rec unifyLists l1 l2 : option<list<string * Term>> = 
  match l1, l2 with 
  | [], [] ->
      // TODO: Succeeds, but returns an empty substitution
      Some(list.Empty)
  | h1::t1, h2::t2 -> 
      // TODO: Unify 'h1' with 'h2' using 'unify' and
      // 't1' with 't2' using 'unifyLists'. If both 
      // succeed, return the generated joint substitution!
      let l1 = unify h1 h2
      let l2 = unifyLists t1 t2
      match l1, l2 with
      | Some value1, Some value2 -> Some(value1 @ value2)
      | _ -> None
  | _ -> 
    // TODO: Lists cannot be unified
    None

and unify (t1: Term) (t2: Term): (string * Term) list option = 
  match t1, t2 with 
      // TODO: Add all the necessary cases here!
      // * For matching atoms, return empty substitution
      // * For matching predicates, return the result of 'unifyLists'
      // * For variable and any term, return a new substitution
      // * For anything else, return None (failed to unify)
  | Atom a, Atom b -> if a = b then Some(list.Empty) else None
  | Variable s, t | t, Variable s -> Some([s, t])
  | Predicate(s1, terms1), Predicate(s2, terms2) -> if s1 = s2 then unifyLists terms1 terms2 else None
  | _ -> None

// ----------------------------------------------------------------------------
// Basic unification tests 
// ----------------------------------------------------------------------------

// Example: human(socrates) ~ human(X) 
// Returns: [X -> socrates]
unify
  (Predicate("human", [Atom("socrates")]))
  (Predicate("human", [Variable("X")]))

// Example: human(socrates) ~ mortal(X) 
// Returns: None (fail)
unify
  (Predicate("human", [Atom("socrates")]))
  (Predicate("mortal", [Variable("X")]))

// Example: parent(charles, harry) ~ parent(charles, X)
// Returns: [X -> harry]
unify
  (Predicate("parent", [Atom("charles"); Atom("harry")]))
  (Predicate("parent", [Atom("charles"); Variable("X")]))

// Example: parent(X, harry) ~ parent(charles, Y)
// Returns: [X -> charles; Y -> harry]
unify
  (Predicate("parent", [Variable("X"); Atom("harry")]))
  (Predicate("parent", [Atom("charles"); Variable("Y")]))

// Example: succ(succ(succ(zero))) ~ succ(X)
// Returns: [X -> succ(succ(zero))]
unify
  (Predicate("succ", [Predicate("succ", [Predicate("succ", [Atom("zero")])])]))
  (Predicate("succ", [Variable("X")]))

// Example: succ(succ(zero)) ~ succ(zero)
// Returns: None (fail)
unify
  (Predicate("succ", [Predicate("succ", [Atom("zero")])]))
  (Predicate("succ", [Atom("zero")]))

