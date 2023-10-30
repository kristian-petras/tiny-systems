// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
// ----------------------------------------------------------------------------
module TinyBASIC

open System

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
  // NOTE: Input("X") reads a number from console and assigns it to X;
  // Stop terminates the program; I also modified Print to take a list of
  // expressions instead of just one (which is what C64 supports too).
  | Print of Expression list
  | Input of string 
  | Stop

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : System.Random }

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
    Random = Random() }
// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr = 
  // TODO: We need an extra function 'MIN' that returns the smaller of
  // the two given numbers (in F#, the function 'min' does exactly this.)
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

let rec runCommand (state: State) ((line, cmd): int * Command): State =
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

let empty = { Program = []; Variables = Map.empty; Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

runInputs empty nim |> ignore
