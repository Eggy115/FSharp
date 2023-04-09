﻿// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012 Eric Taucher, Jack Pappas, Anh-Dung Phan               //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

module FSharpx.Books.AutomatedReasoning.unif

open intro
open fol

// pg. 167
// ========================================================================= //
// Unification for first order terms.                                        //
// ========================================================================= //

let rec istriv env x t =
    match t with
    | Var y -> 
        y = x
        || defined env y
        && istriv env x (apply env y)
    | Fn(f,args) ->
        List.exists (istriv env x) args 
        && failwith "cyclic"
        
// ------------------------------------------------------------------------- //
// Main unification procedure                                                //
// ------------------------------------------------------------------------- //

let rec unify (env : func<string, term>) eqs =
    match eqs with
    | [] -> env
    | (Fn (f, fargs), Fn (g, gargs)) :: oth ->
        if f = g && List.length fargs = List.length gargs then
            unify env (List.zip fargs gargs @ oth)
        else
            failwith "impossible unification"
    | (Var x, t) :: oth
    | (t, Var x) :: oth ->
        if defined env x then
            unify env ((apply env x,t) :: oth)
        else
            unify (if istriv env x t then env else (x |-> t) env) oth

// pg. 169
// ------------------------------------------------------------------------- //
// Solve to obtain a single instantiation.                                   //
// ------------------------------------------------------------------------- //

let rec solve env =
    let env' = mapf (tsubst env) env
    if env' = env then env 
    else solve env'

// pg. 171
// ------------------------------------------------------------------------- //
// Unification reaching a final solved form (often this isn't needed).       //
// ------------------------------------------------------------------------- //

let fullunify eqs = solve (unify undefined eqs)

// pg. 171
// ------------------------------------------------------------------------- //
// Examples.                                                                 //
// ------------------------------------------------------------------------- //

let unify_and_apply eqs =
    let i = fullunify eqs
    let apply (t1, t2) =
        tsubst i t1, tsubst i t2
    List.map apply eqs
