// ----------------------------------------------------------------------------
// 07 - Add support for recursion
// ----------------------------------------------------------------------------

open System

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  | ValCase of bool * Value

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
  // NOTE: A recursive definition. You can think of 
  // 'Let(v, e1, e2)' as 'let rec v = e1 in e2'. 
  | Recursive of string * Expression * Expression

and VariableContext = 
  // NOTE: For recursive calls, we need to add the function
  // being defined to the variable context when defining it.
  // This can be done using 'let rec', but we need to store
  // the variables as lazy values.
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
      | Some res ->
          // NOTE: As 'res' is now 'Lazy<Value>' we need to get its value here.
          res.Value
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
     

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Recursion and conditionals - implementing factorial!
//   let rec factorial = fun x -> 
//     if x then 1 else x*(factorial (-1 + x))
//   in factorial 5
let er = 
  Recursive("factorial", 
    Lambda("x", If(
      Variable("x"),
      Constant(1),
      Binary(
        "*", Variable("x"), 
        Application(Variable("factorial"), 
          Binary("+", Constant(-1), Variable("x")))
      )
    )),  
    Application(Variable "factorial", Constant 5)
  )
evaluate Map.empty er
