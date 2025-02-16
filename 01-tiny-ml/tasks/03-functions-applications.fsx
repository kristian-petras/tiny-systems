// ----------------------------------------------------------------------------
// 03 - Functions and application
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  // NOTE: In ML functions are "first-class values" meaning that
  // you can pass them around. A closure is a value that represents
  // a function at run-time. We store the variable name of the lambda,
  // the body of the lambda and captured variable context.
  // (This is the trick to get lexical and not dynamic scoping!)
  | ValClosure of string * Expression * VariableContext

// NOTE: 'ValClosure' above needs to refer to 'Expression' and also
// 'VariableContext'. To make such recursive references, we define 
// the types using 'type .. and .. and' from now on!
and Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | Variable of string
  | Unary of string * Expression 
  | If of Expression * Expression * Expression
  // NOTE: Added application 'e1 e2' and lambda 'fun v -> e'
  | Application of Expression * Expression
  | Lambda of string * Expression

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
      | _ -> failwith "invalid upsie"
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
     
// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Basic function declaration (should return closure)
//   (fun x -> x * 2) 
let ef1 = 
  Lambda("x", Binary("*", Variable("x"), Constant(2)))
evaluate Map.empty ef1

// Basic function calls (should return number)
//   (fun x -> x * 2) 21
let ef2 = 
  Application(
    Lambda("x", Binary("*", Variable("x"), Constant(2))),
    Constant(21)
  )
evaluate Map.empty ef2

// Wrong function call (the first argument is not a function)
//   21 (fun x -> x * 2)
let ef3 = 
  Application(
    Constant(21),
    Lambda("x", Binary("*", Variable("x"), Constant(2)))
  )
evaluate Map.empty ef3

// Wrong binary operator (it is now possible to apply '+'
// to functions; this makes no sense and should fail!)
//   21 + (fun x -> x * 2)
let ef4 = 
  Binary("+",
    Constant(21),
    Lambda("x", Binary("*", Variable("x"), Constant(2)))  
  )
evaluate Map.empty ef4
