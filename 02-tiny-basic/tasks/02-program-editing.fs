// ----------------------------------------------------------------------------
// 02 - Implement interactive program editing
// ----------------------------------------------------------------------------
module TinyBASIC

open System

type Value =
  | StringValue of string

type Expression = 
  | Const of Value

type Command = 
  | Print of Expression
  | Run 
  | Goto of int

type State = 
  { Program : list<int * Command> }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value =
  match value with
  | StringValue s -> Console.WriteLine(s)

let getLine state line =
    let equals (l, _) = l = line
    List.find equals state.Program

let addLine state (line, cmd) =
  let newState = (line, cmd)::List.filter (fun (l, _) -> l <> line) state.Program |> List.sortBy fst
  { Program = newState }
  
// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let rec evalExpression expr =
    match expr with
    | Const c -> c

let rec runCommand state (line, cmd) =
  match cmd with 
    | Print(expr) ->
        let value = evalExpression expr
        printValue value
        runNextLine state line
    | Run ->
        let first = List.head state.Program
        runCommand state first
    | Goto(line) ->
        let command = getLine state line
        runCommand state command

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
      

let runInputs state cmds =
  List.fold runInput state cmds 

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let empty = { Program = [] }


runInputs empty helloOnce |> ignore
runInputs empty helloInf |> ignore
