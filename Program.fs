module Main

open Expr
open AbstractSyntaxTree
// open Expr.Lexer
// open Expr.Parser 
open FSharp.Text.Lexing

let parse txt =
  txt
  |> LexBuffer<_>.FromString
  |> Parser.Expr Lexer.token
 
let ctx = [ "a", 2.0
            "b", 10.0 ] |> Map

let eval expr ctx = 
    let rec eval expr =
        match expr with
        | Number f              -> f
        | Symbol s              -> ctx |> Map.find s
        | Field (s,f)           -> 55.0
        | Function (Plus,a,b)   -> eval a + eval b
        | Function (Minus,a,b)  -> eval a - eval b
        | Function (Times,a,b)  -> eval a * eval b
        | Function (Divide,a,b) -> eval a / eval b
    eval expr


let dependencies expr =
    let rec dependencies expr =
        seq {
            match expr with
            | Symbol s -> s
            | Field (s,f) -> f
            | Function (_,a,b)   -> yield! dependencies a
                                    yield! dependencies b
            | _ -> ()
        }
    dependencies expr |> Set.ofSeq


[<EntryPoint>]
let main argv =
    let expr = "a - b.z"
    let ast = expr |> parse
    let deps = ast |> dependencies
    printfn "dependencies: %A" deps
    let result = eval ast ctx
    result |> printfn "%A"
    0
