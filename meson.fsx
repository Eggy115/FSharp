﻿// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012 Eric Taucher, Jack Pappas, Anh-Dung Phan               //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

#load "initialization.fsx"

open FSharpx.Books.AutomatedReasoning.lib
open FSharpx.Books.AutomatedReasoning.formulas
open FSharpx.Books.AutomatedReasoning.prop
open FSharpx.Books.AutomatedReasoning.fol
open FSharpx.Books.AutomatedReasoning.skolem
open FSharpx.Books.AutomatedReasoning.tableaux
open FSharpx.Books.AutomatedReasoning.prolog
open FSharpx.Books.AutomatedReasoning.meson

// pg. 215
// ------------------------------------------------------------------------- //
// Example of naivety of tableau prover.                                     //
// ------------------------------------------------------------------------- //

// meson.p001
tab (parse @"forall a. ~(P(a) /\ (forall y z. Q(y) \/ R(z)) /\ ~P(a))");;

// meson.p002
tab (parse @"forall a. ~(P(a) /\ ~P(a) /\ (forall y z. Q(y) \/ R(z)))");;

// pg. 218
// ------------------------------------------------------------------------- //
// The interesting example where tableaux connections make the proof longer. //
// Unfortuntely this gets hammered by normalization first...                 //
// ------------------------------------------------------------------------- //

// meson.p003
// Harrison #09
tab (parse @"
    ~p /\ (p \/ q) /\ (r \/ s) /\ (~q \/ t \/ u) /\ 
    (~r \/ ~t) /\ (~r \/ ~u) /\ (~q \/ v \/ w) /\ 
    (~s \/ ~v) /\ (~s \/ ~w) ==> false");;
      
// pg. 220
// ------------------------------------------------------------------------- //
// Example.                                                                  //
// ------------------------------------------------------------------------- //

// meson.p004
// Davis Putnam #1
let davis_putnam_example = time meson002 (parse @"
    exists x. exists y. forall z. 
    (F(x,y) ==> (F(y,z) /\ F(z,z))) /\ 
    ((F(x,y) /\ G(x,y)) ==> (G(x,z) /\ G(z,z)))");;

// pg. 223
// ------------------------------------------------------------------------- //
// The Los problem (depth 20) and the Steamroller (depth 53) --- lengthier.  //
// ------------------------------------------------------------------------- //

// meson.p005
// Los #1
//Real: 00:04:45.014, CPU: 00:04:44.828, GC gen0: 2875, gen1: 2873, gen2: 2
let los = time meson002 (parse @"
    (forall x y z. P(x,y) ==> P(y,z) ==> P(x,z)) /\ 
    (forall x y z. Q(x,y) ==> Q(y,z) ==> Q(x,z)) /\ 
    (forall x y. Q(x,y) ==> Q(y,x)) /\ 
    (forall x y. P(x,y) \/ Q(x,y)) 
    ==> (forall x y. P(x,y)) \/ (forall x y. Q(x,y))");;

// meson.p006
// Pelletier #47
// long running. Quit at depth limit 51
let steamroller = time meson002 (parse @"
    ((forall x. P1(x) ==> P0(x)) /\ (exists x. P1(x))) /\ 
    ((forall x. P2(x) ==> P0(x)) /\ (exists x. P2(x))) /\ 
    ((forall x. P3(x) ==> P0(x)) /\ (exists x. P3(x))) /\ 
    ((forall x. P4(x) ==> P0(x)) /\ (exists x. P4(x))) /\ 
    ((forall x. P5(x) ==> P0(x)) /\ (exists x. P5(x))) /\ 
    ((exists x. Q1(x)) /\ (forall x. Q1(x) ==> Q0(x))) /\ 
    (forall x. P0(x) 
                ==> (forall y. Q0(y) ==> R(x,y)) \/ 
                    ((forall y. P0(y) /\ S0(y,x) /\ 
                            (exists z. Q0(z) /\ R(y,z)) 
                                ==> R(x,y)))) /\ 
    (forall x y. P3(y) /\ (P5(x) \/ P4(x)) ==> S0(x,y)) /\ 
    (forall x y. P3(x) /\ P2(y) ==> S0(x,y)) /\ 
    (forall x y. P2(x) /\ P1(y) ==> S0(x,y)) /\ 
    (forall x y. P1(x) /\ (P2(y) \/ Q1(y)) ==> ~(R(x,y))) /\ 
    (forall x y. P3(x) /\ P4(y) ==> R(x,y)) /\ 
    (forall x y. P3(x) /\ P5(y) ==> ~(R(x,y))) /\ 
    (forall x. (P4(x) \/ P5(x)) ==> exists y. Q0(y) /\ R(x,y)) 
    ==> exists x y. P0(x) /\ P0(y) /\ 
                exists z. Q1(z) /\ R(y,z) /\ R(x,y)");;

// ------------------------------------------------------------------------- //
// Test it.                                                                  //
// ------------------------------------------------------------------------- //

// meson.p007
// Pelletier #01
let prop_1 =
     time meson002 (parse @"
    p ==> q <=> ~q ==> ~p");;

// meson.p008
// Pelletier #02
let prop_2 =
    time meson002 (parse @"
    ~ ~p <=> p");;

// meson.p009
// Pelletier #03
let prop_3 =
    time meson002 (parse @"
    ~(p ==> q) ==> q ==> p");;

// meson.p010 
// Pelletier #04
let prop_4 =
    time meson002 (parse @"
    ~p ==> q <=> ~q ==> p");;

// meson.p011
// Pelletier #05
let prop_5 =
    time meson002 (parse @"
    (p \/ q ==> p \/ r) ==> p \/ (q ==> r)");;

// meson.p012
// Pelletier #06
let prop_6 =
    time meson002 (parse @"
    p \/ ~p");;

// meson.p013
// Pelletier #07
let prop_7 =
    time meson002 (parse @"
    p \/ ~ ~ ~p");;

// meson.p014
// Pelletier #08
let prop_8 =
    time meson002 (parse @"
    ((p ==> q) ==> p) ==> p");;

// meson.p015
// Pelletier #09
let prop_9 =
    time meson002 (parse @"
    (p \/ q) /\ (~p \/ q) /\ (p \/ ~q) ==> ~(~q \/ ~q)");;

// meson.p016
// Pelletier #10
let prop_10 =
    time meson002 (parse @"
    (q ==> r) /\ (r ==> p /\ q) /\ (p ==> q /\ r) ==> (p <=> q)");;

// meson.p017
// Pelletier #11
let prop_11 =
    time meson002 (parse @"
    p <=> p");;

// meson.p018
// Pelletier #12
let prop_12 =
    time meson002 (parse @"
    ((p <=> q) <=> r) <=> (p <=> (q <=> r))");;

// meson.p019
// Pelletier #13
let prop_13 =
    time meson002 (parse @"
    p \/ q /\ r <=> (p \/ q) /\ (p \/ r)");;

// meson.p020
// Pelletier #14
let prop_14 =
    time meson002 (parse @"
    (p <=> q) <=> (q \/ ~p) /\ (~q \/ p)");;

// meson.p021
// Pelletier #15
let prop_15 =
    time meson002 (parse @"
    p ==> q <=> ~p \/ q");;

// meson.p022
// Pelletier #16
let prop_16 =
    time meson002 (parse @"
    (p ==> q) \/ (q ==> p)");;

// meson.p023
// Pelletier #17
let prop_17 =
    time meson002 (parse @"
    p /\ (q ==> r) ==> s <=> (~p \/ q \/ s) /\ (~p \/ ~r \/ s)");;

// ------------------------------------------------------------------------- //
// Monadic Predicate Logic.                                                  //
// ------------------------------------------------------------------------- //

// meson.p024
// Pelletier #18
let p18 = 
    time meson002 (parse @"
    exists y. forall x. P(y) ==> P(x)");;

// meson.p025
// Pelletier #19
let p19 = 
    time meson002 (parse @"
    exists x. forall y z. (P(y) ==> Q(z)) ==> P(x) ==> Q(x)");;

// meson.p026
// Pelletier #20
let p20 =
    time meson002 (parse @"
    (forall x y. exists z. forall w. P(x) /\ Q(y) ==> R(z) /\ U(w)) ==> 
    (exists x y. P(x) /\ Q(y)) ==> 
    (exists z. R(z))");;

// meson.p027
// Pelletier #21
let p21 = 
    time meson002 (parse @"
    (exists x. P ==> Q(x)) /\ (exists x. Q(x) ==> P)
   ==> (exists x. P <=> Q(x))");;

// meson.p028
// Pelletier #22
let p22 = 
    time meson002 (parse @"(forall x. P <=> Q(x)) ==> (P <=> (forall x. Q(x)))");;

// meson.p029
// Pelletier #23
let p23 = 
    time meson002 (parse @"(forall x. P \/ Q(x)) <=> P \/ (forall x. Q(x))");;

// meson.p030
// Pelletier #24
let p24 = 
    time meson002 (parse @"
    ~(exists x. U(x) /\ Q(x)) /\ 
    (forall x. P(x) ==> Q(x) \/ R(x)) /\ 
    ~(exists x. P(x) ==> (exists x. Q(x))) /\ 
    (forall x. Q(x) /\ R(x) ==> U(x)) ==> 
    (exists x. P(x) /\ R(x))");;

// meson.p031
// Pelletier #25
let p25 = 
    time meson002 (parse @"
    (exists x. P(x)) /\ 
    (forall x. U(x) ==> ~G(x) /\ R(x)) /\ 
    (forall x. P(x) ==> G(x) /\ U(x)) /\ 
    ((forall x. P(x) ==> Q(x)) \/ (exists x. Q(x) /\ P(x))) ==> 
    (exists x. Q(x) /\ P(x))");;

// meson.p032
// Pelletier #26
let p26 = 
    time meson002 (parse @"
    ((exists x. P(x)) <=> (exists x. Q(x))) /\ 
    (forall x y. P(x) /\ Q(y) ==> (R(x) <=> U(y))) ==> 
    ((forall x. P(x) ==> R(x)) <=> (forall x. Q(x) ==> U(x)))");;

// meson.p033
// Pelletier #27
let p27 = 
    time meson002 (parse @"
        (exists x. P(x) /\ ~Q(x)) /\
        (forall x. P(x) ==> R(x)) /\
        (forall x. U(x) /\ V(x) ==> P(x)) /\
        (exists x. R(x) /\ ~Q(x))
        ==> (forall x. V(x) ==> ~R(x))
            ==> (forall x. U(x) ==> ~V(x))");;

// meson.p034
// Pelletier #28
let p28 = 
    time meson002 (parse @"
    (forall x. P(x) ==> (forall x. Q(x))) /\ 
    ((forall x. Q(x) \/ R(x)) ==> (exists x. Q(x) /\ R(x))) /\ 
    ((exists x. R(x)) ==> (forall x. L(x) ==> M(x))) ==> 
    (forall x. P(x) /\ L(x) ==> M(x))");;

// meson.p035
// Pelletier #29
let p29 = 
    time meson002 (parse @"
    (exists x. P(x)) /\ (exists x. G(x)) ==>
    ((forall x. P(x) ==> H(x)) /\ (forall x. G(x) ==> J(x)) <=>
    (forall x y. P(x) /\ G(y) ==> H(x) /\ J(y)))");;

// meson.p036
// Pelletier #30
let p30 = 
    time meson002 (parse @"
    (forall x. P(x) \/ G(x) ==> ~H(x)) /\ (forall x. (G(x) ==> ~U(x)) ==> 
    P(x) /\ H(x)) ==> 
    (forall x. U(x))");;

// meson.p037
// Pelletier #31
let p31 = 
    time meson002 (parse @"
    ~(exists x. P(x) /\ (G(x) \/ H(x))) /\ (exists x. Q(x) /\ P(x)) /\ 
    (forall x. ~H(x) ==> J(x)) ==> 
    (exists x. Q(x) /\ J(x))");;

// meson.p038
// Pelletier #32
let p32 = 
    time meson002 (parse @"
    (forall x. P(x) /\ (G(x) \/ H(x)) ==> Q(x)) /\ 
    (forall x. Q(x) /\ H(x) ==> J(x)) /\ 
    (forall x. R(x) ==> H(x)) ==> 
    (forall x. P(x) /\ R(x) ==> J(x))");;

// meson.p039
// Pelletier #33
let p33 = 
    time meson002 (parse @"
    (forall x. P(a) /\ (P(x) ==> P(b)) ==> P(c)) <=> 
    (forall x. P(a) ==> P(x) \/ P(c)) /\ (P(a) ==> P(b) ==> P(c))");;

// meson.p040
// Pelletier #34
let p34 = 
    time meson002 (parse @"
    ((exists x. forall y. P(x) <=> P(y)) <=> 
    ((exists x. Q(x)) <=> (forall y. Q(y)))) <=> 
    ((exists x. forall y. Q(x) <=> Q(y)) <=> 
    ((exists x. P(x)) <=> (forall y. P(y))))");;

// meson.p041
// Pelletier #35
let p35 = 
    time meson002 (parse @"
    exists x y. P(x,y) ==> (forall x y. P(x,y))");;

// ------------------------------------------------------------------------- //
//  Full predicate logic (without Identity and Functions)                    //
// ------------------------------------------------------------------------- //

// meson.p042
// Pelletier #36
let p36 = 
    time meson002 (parse @"
    (forall x. exists y. P(x,y)) /\ 
    (forall x. exists y. G(x,y)) /\ 
    (forall x y. P(x,y) \/ G(x,y) 
    ==> (forall z. P(y,z) \/ G(y,z) ==> H(x,z))) 
        ==> (forall x. exists y. H(x,y))");;

// meson.p043
// Pelletier #37
let p37 = 
    time meson002 (parse @"
    (forall z. 
        exists w. forall x. exists y. (P(x,z) ==> P(y,w)) /\ P(y,z) /\ 
        (P(y,w) ==> (exists u. Q(u,w)))) /\ 
    (forall x z. ~P(x,z) ==> (exists y. Q(y,z))) /\ 
    ((exists x y. Q(x,y)) ==> (forall x. R(x,x))) ==> 
    (forall x. exists y. R(x,y))");;

// meson.p044
// Pelletier #38
let p38 = 
    time meson002 (parse @"
    (forall x. 
        P(a) /\ (P(x) ==> (exists y. P(y) /\ R(x,y))) ==> 
        (exists z w. P(z) /\ R(x,w) /\ R(w,z))) <=> 
    (forall x. 
        (~P(a) \/ P(x) \/ (exists z w. P(z) /\ R(x,w) /\ R(w,z))) /\ 
        (~P(a) \/ ~(exists y. P(y) /\ R(x,y)) \/ 
        (exists z w. P(z) /\ R(x,w) /\ R(w,z))))");;

// meson.p045
// Pelletier #39
let p39 = 
    time meson002 (parse @"
    ~(exists x. forall y. P(y,x) <=> ~P(y,y))");;

// meson.p046
// Pelletier #40
let p40 = 
    time meson002 (parse @"
    (exists y. forall x. P(x,y) <=> P(x,x)) 
    ==> ~(forall x. exists y. forall z. P(z,y) <=> ~P(z,x))");;

// meson.p047
// Pelletier #41
let p41 = 
    time meson002 (parse @"
    (forall z. exists y. forall x. P(x,y) <=> P(x,z) /\ ~P(x,x)) 
    ==> ~(exists z. forall x. P(x,z))");;

// meson.p048
// Pelletier #42
let p42 = 
    time meson002 (parse @"
    ~(exists y. forall x. P(x,y) <=> ~(exists z. P(x,z) /\ P(z,x)))");;

// meson.p049
// Pelletier #43
let p43 = 
    time meson002 (parse @"
    (forall x y. Q(x,y) <=> forall z. P(z,x) <=> P(z,y)) 
    ==> forall x y. Q(x,y) <=> Q(y,x)");;

// meson.p050
// Pelletier #44
let p44 = 
    time meson002 (parse @"
    (forall x. P(x) ==> (exists y. G(y) /\ H(x,y)) /\ 
    (exists y. G(y) /\ ~H(x,y))) /\ 
    (exists x. J(x) /\ (forall y. G(y) ==> H(x,y))) ==> 
    (exists x. J(x) /\ ~P(x))");;

// meson.p051
// Pelletier #45
let p45 = 
    time meson002 (parse @"
    (forall x. 
        P(x) /\ (forall y. G(y) /\ H(x,y) ==> J(x,y)) ==> 
            (forall y. G(y) /\ H(x,y) ==> R(y))) /\ 
    ~(exists y. L(y) /\ R(y)) /\ 
        (exists x. P(x) /\ (forall y. H(x,y) ==> 
            L(y)) /\ (forall y. G(y) /\ H(x,y) ==> J(x,y))) ==> 
    (exists x. P(x) /\ ~(exists y. G(y) /\ H(x,y)))");;

// meson.p052
// Pelletier #46
let p46 = 
    time meson002 (parse @"
    (forall x. P(x) /\ (forall y. P(y) /\ H(y,x) ==> G(y)) ==> G(x)) /\ 
    ((exists x. P(x) /\ ~G(x)) ==> 
        (exists x. P(x) /\ ~G(x) /\ 
            (forall y. P(y) /\ ~G(y) ==> J(x,y)))) /\ 
    (forall x y. P(x) /\ P(y) /\ H(x,y) ==> ~J(y,x)) ==> 
    (forall x. P(x) ==> G(x))");;

// ------------------------------------------------------------------------- //
// Example from Manthey and Bry, CADE-9.                                     //
// ------------------------------------------------------------------------- //

// meson.p053
// Pelletier #55
let p55 = 
    time meson002 (parse @"
    lives(agatha) /\ lives(butler) /\ lives(charles) /\ 
    (killed(agatha,agatha) \/ killed(butler,agatha) \/ 
        killed(charles,agatha)) /\ 
    (forall x y. killed(x,y) ==> hates(x,y) /\ ~richer(x,y)) /\ 
    (forall x. hates(agatha,x) ==> ~hates(charles,x)) /\ 
    (hates(agatha,agatha) /\ hates(agatha,charles)) /\ 
    (forall x. lives(x) /\ ~richer(x,agatha) ==> hates(butler,x)) /\ 
    (forall x. hates(agatha,x) ==> hates(butler,x)) /\ 
    (forall x. ~hates(x,agatha) \/ ~hates(x,butler) \/ ~hates(x,charles)) 
    ==> killed(agatha,agatha) /\ 
        ~killed(butler,agatha) /\ 
        ~killed(charles,agatha)");;

// meson.p054
// Pelletier #57
let p57 = 
    time meson002 (parse @"
    P(f((a),b),f(b,c)) /\ 
    P(f(b,c),f(a,c)) /\ 
    (forall (x) y z. P(x,y) /\ P(y,z) ==> P(x,z)) 
    ==> P(f(a,b),f(a,c))");;

// ------------------------------------------------------------------------- //
// See info-hol, circa 1500.                                                 //
// ------------------------------------------------------------------------- //

// meson.p055
// Pelletier #58
// TODO: Is this a conrrect translation from Pelletier #58?
let p58 = 
    time meson002 (parse @"
    forall P Q R. forall x. exists v. exists w. forall y. forall z. 
    ((P(x) /\ Q(y)) ==> ((P(v) \/ R(w))  /\ (R(z) ==> Q(v))))");;

// meson.p056
// Pelletier #59
let p59 = 
    time meson002 (parse @"
    (forall x. P(x) <=> ~P(f(x))) ==> (exists x. P(x) /\ ~P(f(x)))");;

// meson.p057
// Pelletier #60
let p60 = 
    time meson002 (parse @"
    forall x. P(x,f(x)) <=> 
        exists y. (forall z. P(z,y) ==> P(z,f(x))) /\ P(x,y)");;

// ------------------------------------------------------------------------- //
// From Gilmore's classic paper.                                             //
// ------------------------------------------------------------------------- //

// meson.p058
// Gilmore #1
// long running
//** Amazingly, this still seems non-trivial... in HOL it works at depth 45!
let gilmore_1 = 
    time meson002 (parse @"
    exists x. forall y z. 
        ((F(y) ==> G(y)) <=> F(x)) /\ 
        ((F(y) ==> H(y)) <=> G(x)) /\ 
        (((F(y) ==> G(y)) ==> H(y)) <=> H(x)) 
        ==> F(z) /\ G(z) /\ H(z)");;

// meson.p059
// Gilmore #2
// long running
//** This is not valid, according to Gilmore
let gilmore_2 = 
    time meson002 (parse @"
    exists x y. forall z. 
        (F(x,z) <=> F(z,y)) /\ (F(z,y) <=> F(z,z)) /\ (F(x,y) <=> F(y,x)) 
        ==> (F(x,y) <=> F(x,z))");;

// meson.p060
// Gilmore #3
let gilmore_3 = 
    time meson002 (parse @"
    exists x. forall y z. 
        ((F(y,z) ==> (G(y) ==> H(x))) ==> F(x,x)) /\ 
        ((F(z,x) ==> G(x)) ==> H(z)) /\ 
        F(x,y) 
        ==> F(z,z)");;

// meson.p061
// Gilmore #4
let gilmore_4 = 
    time meson002 (parse @"
    exists x y. forall z. 
        (F(x,y) ==> F(y,z) /\ F(z,z)) /\ 
        (F(x,y) /\ G(x,y) ==> G(x,z) /\ G(z,z))");;

// meson.p062
// Gilmore #5
let gilmore_5 = 
    time meson002 (parse @"
    (forall x. exists y. F(x,y) \/ F(y,x)) /\ 
    (forall x y. F(y,x) ==> F(y,y)) 
    ==> exists z. F(z,z)");;

// meson.p063
// Gilmore #6
let gilmore_6 = 
    time meson002 (parse @"
    forall x. exists y. 
        (exists u. forall v. F(u,x) ==> G(v,u) /\ G(u,x)) 
        ==> (exists u. forall v. F(u,y) ==> G(v,u) /\ G(u,y)) \/ 
            (forall u v. exists w. G(v,u) \/ H(w,y,u) ==> G(u,w))");;

// meson.p064
// Gilmore #7
let gilmore_7 = 
    time meson002 (parse @"
    (forall x. K(x) ==> exists y. L(y) /\ (F(x,y) ==> G(x,y))) /\ 
    (exists z. K(z) /\ forall u. L(u) ==> F(z,u)) 
    ==> exists v w. K(v) /\ L(w) /\ G(v,w)");;

// meson.p065
// Gilmore #8
let gilmore_8 = 
    time meson002 (parse @"
    exists x. forall y z. 
        ((F(y,z) ==> (G(y) ==> (forall u. exists v. H(u,v,x)))) ==> F(x,x)) /\ 
        ((F(z,x) ==> G(x)) ==> (forall u. exists v. H(u,v,z))) /\ 
        F(x,y) 
        ==> F(z,z)");;

// meson.p066
// Gilmore #9
// long running
//** This is still a very hard problem
let gilmore_9 = 
    time meson002 (parse @"
    forall x. exists y. forall z. 
        ((forall u. exists v. F(y,u,v) /\ G(y,u) /\ ~H(y,x)) 
            ==> (forall u. exists v. F(x,u,v) /\ G(z,u) /\ ~H(x,z)) 
                ==> (forall u. exists v. F(x,u,v) /\ G(y,u) /\ ~H(x,y))) /\ 
        ((forall u. exists v. F(x,u,v) /\ G(y,u) /\ ~H(x,y)) 
        ==> ~(forall u. exists v. F(x,u,v) /\ G(z,u) /\ ~H(x,z)) 
            ==> (forall u. exists v. F(y,u,v) /\ G(y,u) /\ ~H(y,x)) /\ 
                (forall u. exists v. F(z,u,v) /\ G(y,u) /\ ~H(z,y)))");;

// ------------------------------------------------------------------------- //
// Translation of Gilmore procedure using separate definitions.              //
// ------------------------------------------------------------------------- //

// meson.p067
let gilmore_9a = 
    time meson002 (parse @"
    (forall x y. P(x,y) <=> 
        forall u. exists v. F(x,u,v) /\ G(y,u) /\ ~H(x,y)) 
    ==> forall x. exists y. forall z. 
        (P(y,x) ==> (P(x,z) ==> P(x,y))) /\ 
        (P(x,y) ==> (~P(x,z) ==> P(y,x) /\ P(z,y)))");;

// ------------------------------------------------------------------------- //
// Example from Davis-Putnam papers where Gilmore procedure is poor.         //
// ------------------------------------------------------------------------- //

// meson.p068
// Davis Putnam #1
let davis_putnam_example_t = 
    time meson002 (parse @"
    exists x. exists y. forall z. 
    (F(x,y) ==> (F(y,z) /\ F(z,z))) /\ 
    ((F(x,y) /\ G(x,y)) ==> (G(x,z) /\ G(z,z)))");;

// ------------------------------------------------------------------------- //
// The "connections make things worse" example once again.                   //
// ------------------------------------------------------------------------- //

// meson.p069
// Harrison #09
time meson002 (parse @" 
    ~p /\ (p \/ q) /\ (r \/ s) /\ (~q \/ t \/ u) /\ 
        (~r \/ ~t) /\ (~r \/ ~u) /\ (~q \/ v \/ w) /\ 
        (~s \/ ~v) /\ (~s \/ ~w) ==> false");;
