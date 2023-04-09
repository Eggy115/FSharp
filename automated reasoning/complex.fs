﻿// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012 Eric Taucher, Jack Pappas, Anh-Dung Phan               //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

module FSharpx.Books.AutomatedReasoning.complex

open LanguagePrimitives

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
open completion
open eqelim
open paramodulation
open decidable
open qelim
open cooper

// ========================================================================= //
// Complex quantifier elimination (by simple divisibility a la Tarski).      //
// ========================================================================= //

// pg. 353
// ------------------------------------------------------------------------- //
// Basic arithmetic operations on canonical polynomials.                     //
// ------------------------------------------------------------------------- //

let rec poly_add vars pol1 pol2 =
    match pol1, pol2 with
    | Fn ("+", [c; Fn ("*", [Var x; p])]), Fn ("+", [d; Fn ("*", [Var y; q])]) ->
        if earlier vars x y then
            poly_ladd vars pol2 pol1
        elif earlier vars y x then
            poly_ladd vars pol1 pol2
        else
            let e = poly_add vars c d 
            let r = poly_add vars p q
            if r = zero then e
            else
                Fn ("+", [e; Fn ("*", [Var x; r])])
    | pol1, Fn ("+", _) ->
        poly_ladd vars pol1 pol2
    | Fn ("+", _), pol2 ->
        poly_ladd vars pol2 pol1
    | _ ->
        numeral2 (+) pol1 pol2

and poly_ladd vars =
    fun pol1 (Fn ("+", [d; Fn ("*", [Var y; q])])) ->
        Fn ("+", [poly_add vars pol1 d; Fn ("*", [Var y; q])])

let rec poly_neg = function 
    | Fn ("+", [c; Fn ("*", [Var x; p])]) ->
        Fn ("+", [poly_neg c; Fn ("*", [Var x; poly_neg p])])
    | n ->
        numeral1 (~-) n

let poly_sub vars p q =
    poly_add vars p (poly_neg q)

let rec poly_mul vars pol1 pol2 =
    match pol1, pol2 with
    | Fn ("+", [c; Fn ("*", [Var x; p])]), Fn ("+", [d; Fn ("*", [Var y; q])]) ->
        if earlier vars x y then poly_lmul vars pol2 pol1
        else poly_lmul vars pol1 pol2
    | Fn ("0", []), _
    | _, Fn ("0", []) ->
        zero
    | _, Fn ("+", _) ->
        poly_lmul vars pol1 pol2
    | Fn ("+", _), _ ->
        poly_lmul vars pol2 pol1
    | _ ->
        numeral2 ( * ) pol1 pol2

and poly_lmul vars =
    fun pol1 (Fn ("+", [d; Fn ("*", [Var y; q])])) ->
        poly_add vars (poly_mul vars pol1 d)
            (Fn ("+", [zero; Fn ("*", [Var y; poly_mul vars pol1 q])]))

let poly_pow vars p n =
    funpow n (poly_mul vars p) (Fn ("1", []))

let poly_div vars p q =
    poly_mul vars p (numeral1 ((/) GenericOne) q)

let poly_var x =
    Fn ("+", [zero; Fn ("*", [Var x; Fn ("1", [])])])

//  pg. 354
//  ------------------------------------------------------------------------- //
//  Convert term into canonical polynomial representative.                    //
//  ------------------------------------------------------------------------- //

let rec polynate vars tm =
    match tm with
    | Var x ->
        poly_var x
    | Fn ("-", [t]) ->
        poly_neg (polynate vars t)
    | Fn ("+", [s; t]) ->
        poly_add vars (polynate vars s) (polynate vars t)
    | Fn ("-", [s; t]) ->
        poly_sub vars (polynate vars s) (polynate vars t)
    | Fn ("*", [s; t]) ->
        poly_mul vars (polynate vars s) (polynate vars t)
    | Fn ("/", [s; t]) ->
        poly_div vars (polynate vars s) (polynate vars t)
    | Fn ("^", [p; Fn (n, [])]) ->
        poly_pow vars (polynate vars p)  (System.Int32.Parse n)
    | _ ->
        if is_numeral tm then tm
        else failwith "lint: unknown term"

//  pg. 355
//  ------------------------------------------------------------------------- //
//  Do likewise for atom so the RHS is zero.                                  //
//  ------------------------------------------------------------------------- //

let polyatom vars fm =
    match fm with
    | Atom (R (a, [s; t])) ->
        Atom (R (a, [polynate vars (Fn ("-", [s; t])); zero]))
    | _ ->
        failwith "polyatom: not an atom"

//  pg. 358
//  ------------------------------------------------------------------------- //
//  Useful utility functions for polynomial terms.                            //
//  ------------------------------------------------------------------------- //

let rec coefficients vars = function 
    | Fn ("+", [c; Fn ("*", [Var x; q])])
        when x = List.head vars ->
        c :: (coefficients vars q)
    | p -> [p]

let degree vars p =
    List.length (coefficients vars p) - 1

let is_constant vars p =
    degree vars p = 0

let head vars p =
    last (coefficients vars p)

let rec behead vars = function
    | Fn ("+", [c; Fn ("*", [Var x; p])])
        when x = List.head vars ->
        let p' = behead vars p
        if p' = zero then c
        else Fn ("+", [c; Fn ("*", [Var x; p'])])
    | _ -> zero

//  pg. 359
//  ------------------------------------------------------------------------- //
//  Get the constant multiple of the "maximal" monomial (implicit lex order)  //
//  ------------------------------------------------------------------------- //

let rec poly_cmul k p =
    match p with
    | Fn ("+", [c; Fn ("*", [Var x; q])]) ->
        Fn ("+", [poly_cmul k c; Fn ("*", [Var x; poly_cmul k q])])
    | _ ->
        numeral1 (fun m -> k * m) p

let rec headconst p =
    match p with
    | Fn ("+", [c; Fn ("*", [Var x; q])]) ->
        headconst q
    | Fn (n, []) ->
        dest_numeral p

//  pg. 359
//  ------------------------------------------------------------------------- //
//  Make a polynomial monic and return negativity flag for head constant      //
//  ------------------------------------------------------------------------- //

let monic p =
    let h = headconst p
    if h = GenericZero then
        p, false
    else
        poly_cmul (GenericOne / h) p, h < GenericZero

//  pg. 361
//  ------------------------------------------------------------------------- //
//  Pseudo-division of s by p; head coefficient of p assumed nonzero.         //
//  Returns (k,r) so that a^k s = p q + r for some q, deg(r) < deg(p).        //
//  Optimized only for the trivial case of equal head coefficients; no GCDs.  //
//  ------------------------------------------------------------------------- //

let pdivide =
    let shift1 x p =
        Fn ("+", [zero; Fn ("*", [Var x; p])])

    let rec pdivide_aux vars a n p k s =
        if s = zero then k, s
        else
            let b = head vars s 
            let m = degree vars s
            if m < n then k, s 
            else
                let p' = funpow (m - n) (shift1 (List.head vars)) p
                if a = b then pdivide_aux vars a n p k (poly_sub vars s p')
                else pdivide_aux vars a n p (k+1) (poly_sub vars (poly_mul vars a s) (poly_mul vars b p'))

    fun vars s p ->
        pdivide_aux vars (head vars p) (degree vars p) p 0 s

//  pg. 362
//  ------------------------------------------------------------------------- //
//  Datatype of signs.                                                        //
//  ------------------------------------------------------------------------- //

type sign =
    | Zero
    | Nonzero
    | Positive
    | Negative

let swap swf s =
    if not swf then s
    else
        match s with
        | Positive -> Negative
        | Negative -> Positive
        | s -> s

//  pg. 362
//  ------------------------------------------------------------------------- //
//  Lookup and asserting of polynomial sign, modulo constant multiples.       //
//  Note that we are building in a characteristic-zero assumption here.       //
//  ------------------------------------------------------------------------- //

let findsign sgns p =
    try
        let p', swf = monic p
        swap swf (assoc p' sgns)
    with Failure _ ->
        failwith "findsign"

let assertsign sgns (p, s) =
    if p = zero then
        if s = Zero then sgns
        else failwith "assertsign"
    else
        let p',swf = monic p
        let s' = swap swf s
        let s0 = try assoc p' sgns with Failure _ -> s'
        if s' = s0 || s0 = Nonzero && (s' = Positive || s' = Negative) then
            (p', s') :: (subtract sgns [p', s0])
        else
            failwith "assertsign"

//  pg. 363
//  ------------------------------------------------------------------------- //
//  Deduce or case-split over zero status of polynomial.                      //
//  ------------------------------------------------------------------------- //

let split_zero sgns pol cont_z cont_n =
    try
        let z = findsign sgns pol
        (if z = Zero then cont_z else cont_n) sgns
    with Failure "findsign" ->
        let eq = Atom (R ("=", [pol; zero]))
        Or (And (eq, cont_z (assertsign sgns (pol, Zero))),
            And (Not eq, cont_n (assertsign sgns (pol, Nonzero))))

//  pg. 364
//  ------------------------------------------------------------------------- //
//  Whether a polynomial is nonzero in a context.                             //
//  ------------------------------------------------------------------------- //

let poly_nonzero vars sgns pol =
    let dcs, ucs =
        let cs = coefficients vars pol
        List.partition (can (findsign sgns)) cs

    if List.exists (fun p -> findsign sgns p <> Zero) dcs then True
    elif ucs = [] then False
    else
        List.reduceBack mk_or (List.map (fun p -> Not (mk_eq p zero)) ucs)

//  pg. 364
//  ------------------------------------------------------------------------- //
//  Non-divisibility of q by p.                                               //
//  ------------------------------------------------------------------------- //

let rec poly_nondiv vars sgns p s =
    let _, r = pdivide vars s p
    poly_nonzero vars sgns r

//  pg. 365
//  ------------------------------------------------------------------------- //
//  Main reduction for exists x. all eqs = 0 and all neqs =/= 0, in context.  //
//  ------------------------------------------------------------------------- //
                      
let rec cqelim vars (eqs,neqs) sgns =
    try 
        let c = List.find (is_constant vars) eqs
        try 
            let sgns' = assertsign sgns (c, Zero)
            let eqs' = subtract eqs [c]
            And (mk_eq c zero, cqelim vars (eqs', neqs) sgns')
        with 
            Failure "assertsign" -> False
    with Failure _ ->
        if eqs = [] then list_conj(List.map (poly_nonzero vars sgns) neqs) 
        else
            let n = List.reduceBack min (List.map (degree vars) eqs)
            let p = List.find (fun p -> degree vars p = n) eqs
            let oeqs = subtract eqs [p]
            split_zero sgns (head vars p)
                (cqelim vars (behead vars p :: oeqs, neqs))
                (fun sgns' ->
                    let cfn s = snd(pdivide vars s p)
                    if oeqs <> [] then cqelim vars (p::(List.map cfn oeqs),neqs) sgns'
                    elif neqs = [] then True 
                    else
                        let q = List.reduceBack (poly_mul vars) neqs
                        poly_nondiv vars sgns' p (poly_pow vars q (degree vars p)))        

//  pg. 365
//  ------------------------------------------------------------------------- //
//  Basic complex quantifier elimination on actual existential formula.       //
//  ------------------------------------------------------------------------- //

let init_sgns = [
    Fn ("1", []), Positive;
    Fn ("0", []), Zero; ]

let basic_complex_qelim vars (Exists (x, p)) =
    let eqs, neqs = List.partition (non negative) (conjuncts p)
    cqelim (x :: vars) (List.map lhs eqs,List.map (lhs << negate) neqs) init_sgns

//  pg. 366
//  ------------------------------------------------------------------------- //
//  Full quantifier elimination.                                              //
//  ------------------------------------------------------------------------- //

let complex_qelim =
    simplify << evalc << lift_qelim polyatom (dnf << cnnf id << evalc) basic_complex_qelim




