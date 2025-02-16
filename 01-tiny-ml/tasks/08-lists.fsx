// ----------------------------------------------------------------------------
// 08 - Add unit and create a list value
// ----------------------------------------------------------------------------

open System.Reflection.Metadata

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  | ValCase of bool * Value
  // NOTE: A value that represents "empty value" and is
  // useful as the value for representing the empty list.
  | ValUnit 

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression
  | Recursive of string * Expression * Expression
  // NOTE: An expression that evaluates to a unit value.
  // This exists in F# too and it is written as '()'
  | Unit 

and VariableContext = 
  Map<string, Lazy<Value>>

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evaluate (ctx:VariableContext) e =
  match e with 
  | Constant n -> ValNum n
  | Binary(op, e1, e2) ->
      let v1 = evaluate ctx e1
      let v2 = evaluate ctx e2
      match v1, v2 with 
      | ValNum n1, ValNum n2 -> 
          match op with 
          | "+" -> ValNum(n1 + n2)
          | "*" -> ValNum(n1 * n2)
          | _ -> failwith "unsupported binary operator"
      | _ -> failwith "invalid argument of binary operator"
  | Variable(v) ->
      match ctx.TryFind v with 
      | Some res -> res.Value
      | _ -> failwith ("unbound variable: " + v)

  // NOTE: You have the following from before
  | Unary(op, e) ->
      let v = evaluate ctx e
      match v with
      | ValNum n ->
        match op with
        | "-" -> ValNum(-n)
        | _ -> failwith "unsupported unary operator"
      | _ -> failwith "invalid argument of unary operator"
      
  | If(conditional, on_true, on_false) ->
    let result = evaluate ctx conditional 
    match result with
    | ValNum n ->
      match n with
      | x when x = 1 -> evaluate ctx on_true
      | _ -> evaluate ctx on_false
    | _ -> failwith "invalid argument of if conditional expression"
  
  | Lambda(v, e) ->
      ValClosure(v, e, ctx)

  | Application(e1, e2) ->
      let lambda = evaluate ctx e1
      match lambda with
      | ValClosure(s, expression, context) ->
        let value = evaluate ctx e2
        let newContext = context.Add(s, lazy value)
        evaluate newContext expression
      | _ -> failwith "first argument should be a function"

  | Let(v, e1, e2) ->
    let left = evaluate ctx e1
    let newContext = ctx.Add(v, lazy left)
    evaluate newContext e2

  | Tuple(e1, e2) ->
      ValTuple(evaluate ctx e1, evaluate ctx e2)
  | TupleGet(b, e) ->
      let tuple = evaluate ctx e
      match tuple with
      | ValTuple(first, second) -> if b then first else second
      | _ -> failwith "Argument must be a tuple"

  | Match(e, v, e1, e2) ->
      let matchCase = evaluate ctx e
      match matchCase with
      | ValCase(i, j) ->
        let newContext = ctx.Add(v, lazy j)
        match i with
        | true -> evaluate newContext e1
        | false -> evaluate newContext e2
      | _ -> failwith "Matcher should be a case."

  | Case(b, e) ->
      ValCase(b, evaluate ctx e)

  | Recursive(v, e1, e2) ->
      let rec newContext = ctx.Add(v, lazy evaluate newContext e1)
      evaluate newContext e2

  // NOTE: This is so uninteresting I did this for you :-)
  | Unit -> ValUnit


// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Ultimate functional programming - lists and List.map!
// We represent lists as cons cells using tuples, so [1,2,3]
//
// = Case(true, Tuple(Constant(1), Case(true, Tuple(Constant(2), 
//     Case(true, Tuple(Constant(3), Case(false, Unit) ))))))

// Helper function to construct lists, so that we 
// do not need to write them by hand!
let rec makeListExpr l = 
  match l with
  | x::xs -> Case(true, Tuple(x, makeListExpr xs))
  | [] -> Case(false, Unit)

let el = makeListExpr [ for i in 1 .. 5 -> Constant i ]

// List.map function in TinyML:
//
//   let rec map = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> Case1(f x#1, (map f) x#2) 
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y * 10) l
//
let em = 
  Recursive("map",
    Lambda("f", Lambda("l", 
      Match(
        Variable("l"), "x",
        Case(true, Tuple(
          Application(Variable "f", TupleGet(true, Variable "x")),
          Application(Application(Variable "map", Variable "f"), 
            TupleGet(false, Variable "x"))
        )),
        Case(false, Unit)
      )
    )),
    Application(Application(Variable "map", 
      Lambda("y", Binary("*", Variable "y", Constant 10))), el)
  )
evaluate Map.empty em

// The somewhat silly example removes 3 from the list.
// Add '%' binary operator and you can remove odd/even numbers!
//
//   let rec filter = (fun f -> fun l -> 
//     match l with 
//     | Case1 t -> 
//          if f x#1 then Case1(x#1, (map f) x#2) 
//          else (map f) x#2
//     | Case2(Unit) -> Case2(Unit))
//   in map (fun y -> y + (-2)) l
//
let ef = 
  Recursive("filter",
    Lambda("f", Lambda("l", 
      Match(
        Variable("l"),
        "x",
        If(Application(Variable "f", TupleGet(true, Variable "x")),
          Tuple(
            TupleGet(true, Variable "x"),
            Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x"))
          ),
          Application(Application(Variable "filter", Variable "f"), TupleGet(false, Variable "x"))),
        Case(false, Unit)
      )
    )),
    Application(Application(Variable "filter", 
      Lambda("y", Binary("+", Variable "y", Constant -2))), el)
  )
evaluate Map.empty el
evaluate Map.empty ef