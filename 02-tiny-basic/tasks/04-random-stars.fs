// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
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
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at 
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random: Random
    // TODO: You will need to include random number generator in the state!
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
    Random = Random() }

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression state expr = 
  // TODO: Add support for 'RND(N)' which returns a random number in range 0..N-1
  // and for binary operators ||, <, > (and the ones you have already, i.e., - and =).
  // To add < and >, you can use the 'binaryRelOp' helper above. You can similarly
  // add helpers for numerical operators and binary Boolean operators to make
  // your code a bit nicer.
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

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []
              Variables = Map.empty
              Random = Random() 
               } // TODO: Add random number generator!

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num 60], "RND" @ [num 20], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num 60], "RND" @ [num 20], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40)) 
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
