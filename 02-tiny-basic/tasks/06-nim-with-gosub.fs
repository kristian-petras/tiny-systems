// ----------------------------------------------------------------------------
// 06 - Add support for more elegant programs with GOSUB
// ----------------------------------------------------------------------------
module TinyBASIC

open System
open System.Collections.Generic

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Clear
  | Poke of Expression * Expression * Expression
  | Print of Expression list
  | Input of string 
  | Stop
  // NOTE: Add the GOSUB jump and RETURN commands
  | GoSub of int
  | Return

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random
    Stack: list<int>
    }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
  match value with
  | StringValue s -> Console.Write(s)
  | NumberValue i -> Console.Write(i)
  | BoolValue b -> Console.Write(b)

let getLine state line =
  let equals (l, _) = l = line
  List.find equals state.Program
let addLine state (line, cmd) =
  let newState = (line, cmd)::List.filter (fun (l, _) -> l <> line) state.Program |> List.sortBy fst
  { Program = newState
    Variables = state.Variables
    Random = Random()
    Stack = List.empty }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr = 
  match expr with
    | Const c -> c
    | Function (name, expr) ->
      let first, second =
        match expr with
        | [e1; e2] -> (evalExpression state e1, Some(evalExpression state e2))
        | [e1] -> (evalExpression state e1, None)
        | _ -> failwith "Expression should contain 2 elements as it is a binary function."
      match name with
      | "RND" ->
        match (first) with
        | NumberValue f -> NumberValue(state.Random.Next(f))
        | _ -> failwith "Type mismatch!"
      | ">" -> binaryRelOp (fun f s -> f > s) [first; second.Value]
      | "<" ->  binaryRelOp (fun f s -> f < s) [first; second.Value]
      | "||" ->
        match (first, second.Value) with
        | BoolValue f, BoolValue s -> BoolValue(f || s)
        | _, _ -> failwith "Type mismatch!"
      | "-" -> 
        match (first, second.Value) with
        | NumberValue f, NumberValue s -> NumberValue(f - s)
        | _, _ -> failwith "Type mismatch!"
      | "=" ->
        match (first, second.Value) with
        | BoolValue f, BoolValue s -> BoolValue(f = s)
        | NumberValue f, NumberValue s -> BoolValue(f = s)
        | StringValue f, StringValue s -> BoolValue(f = s)
        | _, _ -> failwith "Type mismatch!"
      | "MIN" ->
        match (first, second.Value) with
        | NumberValue f, NumberValue s -> NumberValue(min f s)
        | _, _ -> failwith "Type mismatch!"
      | _ -> failwith "Unsupported function name"
    | Variable v -> state.Variables[v]

let rec runCommand state (line, cmd) =
  match cmd with 
  | Print(expr) ->
      let values = List.map (evalExpression state) expr
      for value in values do
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
    let newState = {
      Program = state.Program
      Variables = state.Variables.Add(s, value)
      Random = state.Random
      Stack = state.Stack 
    }
    runNextLine newState line
  | If (expr, command) ->
    let value = evalExpression state expr
    match value with
    | BoolValue b -> if b then runCommand state (line, command) else runNextLine state line
    | _ -> failwith "Result of if expression has to be a boolean."
  | Clear ->
    Console.Clear()
    runNextLine state line
  | Poke (x, y, char) ->
    let first = evalExpression state x
    let second = evalExpression state y
    let character = evalExpression state char
    match (first, second, character) with
    | NumberValue f, NumberValue s, StringValue c ->
      Console.CursorLeft <- f
      Console.CursorTop <- s
      Console.Write(c)
      runNextLine state line
    | _, _, _ -> failwith "Invalid x, y"
  | Input variableName ->
    let input = Console.ReadLine()
    match Int32.TryParse input with
    | true, v ->
      runCommand state (line, Assign(variableName, Const(NumberValue(v))))
    | _ ->
      Console.WriteLine "Could not parse, try again"
      runCommand state (line, cmd)
  | Stop -> state
  | GoSub l ->
    let newState = {
      Program = state.Program
      Variables = state.Variables
      Random = state.Random
      Stack = line::state.Stack
    }
    runCommand newState (line, Goto(l))
  | Return ->
    let returnLine = state.Stack.Head
    let newState = {
      Program = state.Program
      Variables = state.Variables
      Random = state.Random
      Stack = state.Stack.Tail
    }
    runNextLine newState returnLine

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

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

// TODO: Add empty stack of return line numbers here
let empty = { Program = []; Variables = Map.empty; Random = System.Random(); Stack = List.empty }

let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Assign("U", num 1)
    Some 30, GoSub(100)
    Some 40, Assign("U", num 2)
    Some 50, GoSub(100)
    Some 60, Goto(20) 
    Some 100, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 110, Print [ str "PLAYER "; var "U"; str ": YOU CAN TAKE BETWEEN 1 AND "; 
      Function("MIN", [num 5; var "M"]); str " MATCHES\n" ]
    Some 120, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 130, Input("P")
    Some 140, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 120)
    Some 150, Assign("M", var "M" .- var "P")
    Some 160, If(var "M" .= num 0, Goto 200)
    Some 170, Return    
    Some 200, Print [str "PLAYER "; var "U"; str " WINS!"]
    None, Run
  ]

runInputs empty nim |> ignore
