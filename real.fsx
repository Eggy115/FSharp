﻿// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012 Eric Taucher, Jack Pappas, Anh-Dung Phan               //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

#load "initialization.fsx"

open FSharpx.Books.AutomatedReasoning.initialization
open FSharpx.Books.AutomatedReasoning.lib
open FSharpx.Books.AutomatedReasoning.intro
open FSharpx.Books.AutomatedReasoning.formulas
open FSharpx.Books.AutomatedReasoning.prop
open FSharpx.Books.AutomatedReasoning.fol
open FSharpx.Books.AutomatedReasoning.skolem
open FSharpx.Books.AutomatedReasoning.completion
open FSharpx.Books.AutomatedReasoning.qelim
open FSharpx.Books.AutomatedReasoning.cooper
open FSharpx.Books.AutomatedReasoning.complex
open FSharpx.Books.AutomatedReasoning.real

fsi.AddPrinter sprint_fol_formula

// pg. 375
// ------------------------------------------------------------------------- //
// First examples.                                                           //
// ------------------------------------------------------------------------- //

// real.p001
real_qelim (parse @"exists x. x^4 + x^2 + 1 = 0");;

// real.p002
real_qelim (parse @"exists x. x^3 - x^2 + x - 1 = 0");;

// real.p003
real_qelim (parse @"exists x y. x^3 - x^2 + x - 1 = 0 /\ y^3 - y^2 + y - 1 = 0 /\ ~(x = y)");;

// real.p004
real_qelim (parse @"exists x. x^2 - 3 * x + 2 = 0 /\ 2 * x - 3 = 0");;

// real.p005
real_qelim (parse @"forall a f k. (forall e. k < e ==> f < a * e) ==> f <= a * k");;

// real.p006
real_qelim (parse @"exists x. a * x^2 + b * x + c = 0");;

// real.p007
real_qelim (parse @"forall a b c. (exists x. a * x^2 + b * x + c = 0) <=> b^2 >= 4 * a * c");;

// real.p008
real_qelim (parse @"forall a b c. (exists x. a * x^2 + b * x + c = 0) <=> a = 0 /\ (b = 0 ==> c = 0) \/ ~(a = 0) /\ b^2 >= 4 * a * c");;

// pg. 377
// ------------------------------------------------------------------------- //
// Termination ordering for group theory completion.                         //
// ------------------------------------------------------------------------- //

// real.p009
real_qelim (parse @"1 < 2 /\ (forall x. 1 < x ==> 1 < x^2) /\ (forall x y. 1 < x /\ 1 < y ==> 1 < x * (1 + 2 * y))");;

// real.p010
// Real: 00:00:31.254, CPU: 00:00:31.109, GC gen0: 169, gen1: 168, gen2: 1
let eqs = complete_and_simplify ["1"; "*"; "i"] [(parse @"1 * x = x"); (parse @"i(x) * x = 1"); (parse @"(x * y) * z = x * y * z")];;

// real.p011
let fm = list_conj (List.map grpform eqs);;

// real.p012
runWithEnlargedStack (fun () ->
    real_qelim fm);;

// real.p013
real_qelim' (parse @"forall d. (exists c. forall a b. (a = d /\ b = c) \/ (a = c /\ b = 1) ==> a^2 = b) <=> d^4 = 1");;

// Not in book.
// ------------------------------------------------------------------------- //
// Didn't seem worth it in the book, but monicization can help a lot.        //
// Now this is just set as an exercise.                                      //
// ------------------------------------------------------------------------- //

let rec casesplit vars dun pols cont sgns =
    match pols with
    | []     -> monicize vars dun cont sgns
    | p::ops -> split_trichotomy sgns (head vars p) ( if is_constant vars p then delconst vars dun p ops cont else casesplit vars dun (behead vars p :: ops) cont ) ( if is_constant vars p then delconst vars dun p ops cont else casesplit vars (dun@[p]) ops cont )

and delconst vars dun p ops cont sgns =
    let cont' m = cont(List.map (insertat (List.length dun) (findsign sgns p)) m)
    casesplit vars dun ops cont' sgns

and matrix vars pols cont sgns =
    if pols = [] then try cont [[]] with Failure _ -> False else
    let p = List.head(sort(decreasing (degree vars)) pols)
    let p' = poly_diff vars p 
    let i = index p pols
    let qs = 
      let p1,p2 = chop_list i pols
      p'::p1 @ List.tail p2
    let gs = List.map (pdivide_pos vars sgns p) qs
    let cont' m = cont(List.map (fun l -> insertat i (List.head l) (List.tail l)) m)
    casesplit vars [] (qs@gs) (dedmatrix cont') sgns

and monicize vars pols cont sgns =
    let mols,swaps = List.unzip(List.map monic pols)
    let sols = setify mols
    let indices = List.map (fun p -> index p sols) mols
    let transform m = List.map2 (fun sw i -> swap sw (List.nth m i)) swaps indices
    let cont' mat = cont(List.map transform mat)
    matrix vars sols cont' sgns;;

let basic_real_qelim vars = function 
    | (Exists(x,p)) ->
        let pols = atom_union (function (R(a,[t;Fn("0",[])])) -> [t] | _ -> []) p
        let cont mat = if List.exists (fun m -> testform (List.zip pols m) p) mat
                        then True else False
        casesplit (x::vars) [] pols cont init_sgns
    | _ -> failwith "malformed input";;

let real_qelim = simplify << evalc << lift_qelim polyatom (simplify << evalc) basic_real_qelim;;

let real_qelim' = simplify << evalc << lift_qelim polyatom (dnf << cnnf id << evalc) basic_real_qelim;;
