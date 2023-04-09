﻿// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012 Eric Taucher, Jack Pappas, Anh-Dung Phan               //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

module FSharpx.Books.AutomatedReasoning.prolog

open intro
open formulas
open prop
open fol
open skolem
open unif
open tableaux

// ========================================================================= //
// Backchaining procedure for Horn clauses, and toy Prolog implementation.   //
// ========================================================================= //

// pg. 207
// ------------------------------------------------------------------------- //
// Rename a rule.                                                            //
// ------------------------------------------------------------------------- //

let renamerule k (asm, c) =
    let fvs = fv (list_conj (c :: asm))
    let n = List.length fvs
    let vvs = List.map (fun i -> "_" + string i) (k -- (k + n - 1))
    let inst = subst (fpf fvs (List.map (fun x -> Var x) vvs))
    (List.map inst asm, inst c), k + n
        
// pg. 207
// ------------------------------------------------------------------------- //
// Basic prover for Horn clauses based on backchaining with unification.     //
// ------------------------------------------------------------------------- //

let rec backchain rules n k env goals =
    match goals with
    | [] -> env
    | g :: gs ->
        if n = 0 then failwith "Too deep" 
        else
            let rec tryfind f l =
                match l with
                | [] -> failwith "tryfind"
                | h :: t ->
                    try f h
                    with _ -> tryfind f t
            tryfind (
                fun rule ->
                    let (a, c), k' =
                        renamerule k rule
                    backchain rules (n - 1) k' (unify_literals env (c, g)) (a @ gs))
                    rules

let hornify cls =
    let pos, neg = List.partition positive cls
    if List.length pos > 1 then
        failwith "non-Horn clause"
    else
        List.map negate neg, (if pos = [] then False else List.head pos)

let hornprove fm =
    let rules = List.map hornify (simpcnf (skolemize (Not (generalize fm))))
    deepen (fun n -> backchain rules n 0 undefined [False], n) 0

// pg. 210
// ------------------------------------------------------------------------- //
// Parsing rules in a Prolog-like syntax.                                    //
// ------------------------------------------------------------------------- //

let parserule s =
    let c, rest =
        parse_formula (parse_infix_atom, parse_atom) [] (lex (explode s))
    let asm, rest1 =
        if rest <> [] && List.head rest = ":-"
        then parse_list ","
                (parse_formula (parse_infix_atom, parse_atom) []) (List.tail rest)
        else [], rest
    if rest1 = [] then asm, c
    else failwith "Extra material after rule"

// pg. 210
// ------------------------------------------------------------------------- //
// Prolog interpreter: just use depth-first search not iterative deepening.  //
// ------------------------------------------------------------------------- //

let simpleprolog rules gl =
    backchain (List.map parserule rules) -1 0 undefined [parse gl]

// ------------------------------------------------------------------------- //
// With instantiation collection to produce a more readable result.          //
// ------------------------------------------------------------------------- //

let prolog rules gl =
    let i = solve (simpleprolog rules gl)
    mapfilter (fun x -> Atom (R ("=", [Var x; apply i x]))) (fv (parse gl))                      
