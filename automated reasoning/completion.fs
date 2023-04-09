﻿// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012 Eric Taucher, Jack Pappas, Anh-Dung Phan               //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

module FSharpx.Books.AutomatedReasoning.completion

open intro
open formulas
open prop
open defcnf
open dp
open stal
open bdd
open fol
open skolem
open herbrand
open unif
open tableaux
open resolution
open prolog
open meson
open skolems
open equal
open cong
open rewrite
open order

// pg. 276
// ========================================================================= //
// Knuth-Bendix completion.                                                  //
// ========================================================================= //

let renamepair (fm1,fm2) =
    let fvs1 = fv fm1
    let fvs2 = fv fm2
    let nms1, nms2 = 
        chop_list (List.length fvs1)
            (List.map (fun n -> Var ("x" + string n))
                (0 -- (List.length fvs1 + List.length fvs2 - 1)))
    subst (fpf fvs1 nms1) fm1, subst (fpf fvs2 nms2) fm2
  
// pg. 276
// ------------------------------------------------------------------------- //
// Rewrite (using unification) with l = r inside tm to give a critical pair. //
// ------------------------------------------------------------------------- //

let rec listcases fn rfn lis acc =
    match lis with
    | [] -> acc
    | h :: t ->
        fn h (fun i h' ->
            rfn i (h' :: t))
        @ listcases fn (fun i t' ->
            rfn i (h :: t'))
            t acc

let rec overlaps (l, r) tm rfn =
    match tm with
    | Var x -> []
    | Fn (f, args) ->
        listcases (overlaps (l, r)) (fun i a -> rfn i (Fn (f, a))) args
            (try [rfn (fullunify [l, tm]) r] with Failure _ -> [])
  
// pg. 277
// ------------------------------------------------------------------------- //
// Generate all critical pairs between two equations.                        //
// ------------------------------------------------------------------------- //

let crit1 (Atom (R ("=", [l1;r1]))) (Atom (R ("=", [l2;r2]))) =
    overlaps (l1,r1) l2 (fun i t -> subst i (mk_eq t r2))

let critical_pairs fma fmb =
    let fm1, fm2 = renamepair (fma, fmb)
    if fma = fmb then crit1 fm1 fm2
    else union (crit1 fm1 fm2) (crit1 fm2 fm1)

// pg. 278
// ------------------------------------------------------------------------- //
// Orienting an equation.                                                    //
// ------------------------------------------------------------------------- //

let normalize_and_orient ord eqs (Atom (R ("=", [s;t]))) =
    let s' = rewrite eqs s
    let t' = rewrite eqs t
    if ord s' t' then s', t'
    elif ord t' s' then t', s'
    else failwith "Can't orient equation"
  
// pg. 279
// ------------------------------------------------------------------------- //
// Status report so the user doesn't get too bored.                          //
// ------------------------------------------------------------------------- //

let status (eqs, def, crs) eqs0 =
    if not (eqs = eqs0 && (List.length crs) % 1000 <> 0) then
        printfn "%i equations and %i pending critical pairs + %i deferred"
            (List.length eqs) (List.length crs) (List.length def)
   
// pg. 279
// ------------------------------------------------------------------------- //
// Completion main loop (deferring non-orientable equations).                //
// ------------------------------------------------------------------------- //

let rec complete ord (eqs,def,crits) =
    match crits with
    | eq :: ocrits ->
        let trip =
            try
                let s', t' = normalize_and_orient ord eqs eq
                if s' = t' then
                    eqs, def, ocrits
                else
                    let eq' = Atom (R ("=", [s'; t']))
                    let eqs' = eq' :: eqs
                    eqs', def, ocrits @ List.foldBack ((@) << critical_pairs eq') eqs' []
            with Failure _ ->
                eqs, eq :: def, ocrits
        status trip eqs
        complete ord trip
    | _ -> 
        if def = [] then eqs
        else
            let e = List.find (can (normalize_and_orient ord eqs)) def
            complete ord (eqs, subtract def [e], [e])

// pg. 283
// ------------------------------------------------------------------------- //
// Interreduction.                                                           //
// ------------------------------------------------------------------------- //

let rec interreduce dun eqs =
    match eqs with
    | [] -> List.rev dun
    | (Atom (R ("=", [l; r]))) :: oeqs ->
        let dun' =
            if rewrite (dun @ oeqs) l <> l then dun
            else mk_eq l (rewrite (dun @ eqs) r) :: dun
        interreduce dun' oeqs

// pg. 283
// ------------------------------------------------------------------------- //
// Overall function with post-simplification (but not dynamically).          //
// ------------------------------------------------------------------------- //

let complete_and_simplify wts eqs =
    let ord = lpo_ge (weight wts)
    let eqs' = 
        List.map (fun e -> 
            let l, r = normalize_and_orient ord [] e
            mk_eq l r) eqs
    (interreduce [] << complete ord) (eqs', [], unions (allpairs critical_pairs eqs' eqs'))

// Not in book.
// ------------------------------------------------------------------------- //
// Step-by-step; note that we *do* deduce commutativity, deferred of course. //
// ------------------------------------------------------------------------- //

let eqs = [
    parse "(x * y) * z = x * (y * z)";
    parse "1 * x = x";
    parse "x * 1 = x";
    parse "x * x = 1"; ]
let wts = ["1"; "*"; "i"]

let ord = lpo_ge (weight wts)

let def = []
let crits = unions (allpairs critical_pairs eqs eqs)
let complete1 ord (eqs, def, crits) =
    match crits with
    | eq :: ocrits ->
        let trip =
            try
                let s', t' = normalize_and_orient ord eqs eq
                if s' = t' then
                    eqs, def, ocrits
                else
                    let eq' = Atom (R ("=", [s';t']))
                    let eqs' = eq' :: eqs
                    eqs', def,
                        ocrits @ List.foldBack ((@) << critical_pairs eq') eqs' []
            with Failure _ ->
                eqs, eq :: def, ocrits

        status trip eqs
        trip
    | _ ->
        if def = [] then
            eqs, def, crits
        else
            let e = List.find (can (normalize_and_orient ord eqs)) def
            eqs, subtract def [e], [e]
