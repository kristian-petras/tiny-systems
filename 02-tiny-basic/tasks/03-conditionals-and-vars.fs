// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

open System

type Value =
  | StringValue of string
  // NOTE: Added numerical and Boolean values
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  // NOTE: Added functions and variables. Functions  are used for both 
  // functions (later) and binary operators (in this step). We use only
  // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  // NOTE: Assign expression to a given variable and conditional that 
  // runs a given Command only if the expression evaluates to 'BoolValue(true)'
  | Assign of string * Expression
  | If of Expression * Command

type State = 
  { Program : list<int * Command>
    Variables: Map<String, Value>
  }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  match value with
  | StringValue s -> Console.WriteLine(s)
  | NumberValue i -> Console.WriteLine(i)
  | BoolValue b -> Console.WriteLine(b)

let getLine state line =
  let equals (l, _) = l = line
  List.find equals state.Program
  
let addLine state (line, cmd) =
  let newState = (line, cmd)::List.filter (fun (l, _) -> l <> line) state.Program |> List.sortBy fst
  { Program = newState
    Variables = state.Variables }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression state expr = 
  match expr with
    | Const c -> c
    | Function (name, expr) ->
      let first, second =
        match expr with
        | [e1; e2] -> (evalExpression state e1, evalExpression state e2)
        | _ -> failwith "Expression should contain 2 elements as it is a binary function."
      match name with
      | "-" ->
        match (first, second) with
        | NumberValue f, NumberValue s -> NumberValue(f - s)
        | _, _ -> failwith "Type mismatch!"
      | "=" ->
        match (first, second) with
        | BoolValue f, BoolValue s -> BoolValue(f = s)
        | NumberValue f, NumberValue s -> BoolValue(f = s)
        | StringValue f, StringValue s -> BoolValue(f = s)
        | _, _ -> failwith "Type mismatch!"
      | _ -> failwith "Unsupported function name"
    | Variable v -> state.Variables[v]

let rec runCommand state (line, cmd) =
  match cmd with 
  | Print(expr) ->
      let value = evalExpression state expr
      printValue value
      runNextLine state line
  | Run ->
      let first = List.head state.Program
      runCommand state first
  | Goto(line) ->
      let command = getLine state line
      runCommand state command

  | Assign (s, expr) ->
    let value = evalExpression state expr
    let newState = { Program = state.Program; Variables = state.Variables.Add(s, value) }
    runNextLine newState line
  
  | If (expr, command) ->
    let value = evalExpression state expr
    match value with
    | BoolValue b -> if b then runCommand state (line, command) else runNextLine state line
    | _ -> failwith "Result of if expression has to be a boolean."

and runNextLine state line =
  let isGreater (l, _) = l > line
  let newLine = List.tryFind isGreater state.Program
  match newLine with
  | None -> state
  | Some command ->
      runCommand state command

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) =
  match line with
  | None -> runCommand state (Int32.MaxValue, cmd)
  | Some l -> addLine state (l, cmd)
  
let runInputs state cmds = List.fold runInput state cmds 

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let empty = { Program = []
              Variables = Map.empty }

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let testVariables = 
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S") 
    Some 50, Print(Variable "I") 
    Some 60, Print(Variable "B")
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function) 
runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
