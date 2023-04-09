﻿// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012 Eric Taucher                                           //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

module FSharpx.Books.AutomatedReasoning.Tests.rewrite

open FSharpx.Books.AutomatedReasoning.lib
open FSharpx.Books.AutomatedReasoning.formulas
open FSharpx.Books.AutomatedReasoning.fol
open FSharpx.Books.AutomatedReasoning.resolution
open FSharpx.Books.AutomatedReasoning.rewrite

open NUnit.Framework
open FsUnit


let private rewriteValues : (formula<fol> list * term * term * string)[] = [|
    (
        // idx 0
        // rewrite.p001
        [parse @"0 + x = x";
        parse @"S(x) + y = S(x + y)";
        parse @"0 * x = 0";
        parse @"S(x) * y = y + x * y"],
        (parset @"S(S(S(0))) * S(S(0)) + S(S(S(S(0))))"),
        Fn
          ("S",
           [Fn
              ("S",
               [Fn
                  ("S",
                   [Fn
                      ("S",
                       [Fn
                          ("S",
                           [Fn
                              ("S",
                               [Fn
                                  ("S",
                                   [Fn
                                      ("S",[Fn ("S",[Fn ("S",[Fn ("0",[])])])])])])])])])])]),
        @"<<|S(S(S(S(S(S(S(S(S(S(0))))))))))|>>"
    )
    |]

[<Test>]
[<TestCase(0, TestName = "rewrite.p001")>]
let ``rewrite tests`` idx =
    let (eqs, _, _, _) = rewriteValues.[idx]
    let (_, tm, _, _) = rewriteValues.[idx]
    let (_, _, astResult, _) = rewriteValues.[idx]
    let (_, _, _, stringResult) = rewriteValues.[idx]
    let result = rewrite eqs tm
    result
    |> should equal result
    sprint_term result
    |> should equal stringResult
