// ----------------------------------------------------------------------------
// 04 - Let binding as syntactic sugar
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext

and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  | Application of Expression * Expression
  | Lambda of string * Expression
  // NOTE: Added. Let(v, e1, e2) stands for 'let v = e1 in e2'
  | Let of string * Expression * Expression

and VariableContext = 
  Map<string, Value>

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
      | Some res -> res
      | _ -> failwith ("unbound variable: " + v)

  | Unary(op, e) ->
      let v = evaluate ctx e
      match v with
      | ValNum n ->
        match op with
        | "-" -> ValNum(-n)
        | _ -> failwith "unsupported unary operator"
      | _ -> failwith "invalid upsie"
      
  | If(conditional, on_true, on_false) ->
    let result = evaluate ctx conditional 
    match result with
    | ValNum n ->
      match n with
      | x when x > 0 -> evaluate ctx on_true
      | x when x <= 0 -> evaluate ctx on_false
      | _ -> failwith "not supported result in expression"
    | _ -> failwith "invalid upsie"
  
  | Lambda(v, e) ->
      ValClosure(v, e, VariableContext([]))

  | Application(e1, e2) ->
      let lambda = evaluate ctx e1
      match lambda with
      | ValNum _ -> failwith "This should be a function!!!"
      | ValClosure(s, expression, context) ->
        let value = evaluate ctx e2
        let newContext = context.Add(s, value)
        evaluate newContext expression

  | Let(v, e1, e2) ->
    let left = evaluate ctx e1
    let newContext = ctx.Add(v, left)
    evaluate newContext e2

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Simple let binding
//   let x = 2 in (20*x) + x
let el1 = 
  Let("x", 
    Constant(2), 
    Binary("+", Variable("x"), 
      Binary("*", Variable("x"), Constant(20)))
  )
evaluate Map.empty el1

// Function calls with let binding
//   let f = fun x -> x*2 in (f 20) + (f 1)
//
// In F#, you would write this as follows
//   let f x = x*2
//   (f 20) + (f 1)
let el2 = 
  Let("f",
    Lambda("x", Binary("*", Variable("x"), Constant(2))),
    Binary("+", 
      Application(Variable("f"), Constant(20)),
      Application(Variable("f"), Constant(1)) 
    )    
  )
evaluate Map.empty el2
