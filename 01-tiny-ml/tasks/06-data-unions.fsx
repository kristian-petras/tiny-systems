// ----------------------------------------------------------------------------
// 06 - Add more data types - unions
// ----------------------------------------------------------------------------

type Value = 
  | ValNum of int 
  | ValClosure of string * Expression * VariableContext
  | ValTuple of Value * Value
  // NOTE: Value representing a union case. Again, we use 'bool':
  // 'true' for 'Case1' and 'false' for 'Case2'
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
  // NOTE: 'Case' represents creating a union value and 'Match' pattern 
  // matching. You can read 'Match(e, v, e1, e2)' as F# pattern matching 
  // of the form: 'match e with v -> e1 | v -> e2'
  | Case of bool * Expression
  | Match of Expression * string * Expression * Expression

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
      | x when x > 0 -> evaluate ctx on_true
      | x when x <= 0 -> evaluate ctx on_false
      | _ -> failwith "not supported result in expression"
    | _ -> failwith "invalid argument of if conditional expression"
  
  | Lambda(v, e) ->
      ValClosure(v, e, VariableContext([]))

  | Application(e1, e2) ->
      let lambda = evaluate ctx e1
      match lambda with
      | ValClosure(s, expression, context) ->
        let value = evaluate ctx e2
        let newContext = context.Add(s, value)
        evaluate newContext expression
      | _ -> failwith "first argument should be a function"

  | Let(v, e1, e2) ->
    let left = evaluate ctx e1
    let newContext = ctx.Add(v, left)
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
        let newContext = ctx.Add(v, j)
        match i with
        | true -> evaluate newContext e1
        | false -> evaluate newContext e2
      | _ -> failwith "Matcher should be a case."

  | Case(b, e) ->
      ValCase(b, evaluate ctx e)

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

// Data types - creating a union value
let ec1 =
  Case(true, Binary("*", Constant(21), Constant(2)))
evaluate Map.empty ec1

// Data types - working with union cases
//   match Case1(21) with Case1(x) -> x*2 | Case2(x) -> x*100
//   match Case2(21) with Case1(x) -> x*2 | Case2(x) -> x*100
let ec2 = 
  Match(Case(true, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec2

let ec3 = 
  Match(Case(false, Constant(21)), "x", 
    Binary("*", Variable("x"), Constant(2)),
    Binary("*", Variable("x"), Constant(100))
  )
evaluate Map.empty ec3
