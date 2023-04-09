﻿// ========================================================================= //
// Copyright (c) 2003-2007, John Harrison.                                   //
// Copyright (c) 2012 Eric Taucher, Jack Pappas, Anh-Dung Phan               //
// (See "LICENSE.txt" for details.)                                          //
// ========================================================================= //

#load "initialization.fsx"

open FSharpx.Books.AutomatedReasoning.lib
open FSharpx.Books.AutomatedReasoning.intro

// pg. 14
// ------------------------------------------------------------------------- //
// Trivial example of using the type constructors.                           //
// ------------------------------------------------------------------------- //

// intro.p001
Add (Mul (Const 2, Var "x"), Var "y");;

// pg. 16
// ------------------------------------------------------------------------- //
// Example.                                                                  //
// ------------------------------------------------------------------------- //

let e = Add (Mul (Add (Mul (Const 0, Var "x"), Const 1), Const 3), Const 12);;

// intro.p002
simplify e;;

// intro.p003
lex (explode @"2*((var_1 + x') + 11)");;

// intro.p004
lex (explode @"if //p1-- == *p2++) then f() else g()");;

// pg. 20
// ------------------------------------------------------------------------- //
// Our parser.                                                               //
// ------------------------------------------------------------------------- //

// intro.p005
parse_exp @"x + 1";;

fsi.AddPrinter sprint_exp;;

// pg. 21
// ------------------------------------------------------------------------- //
// Demonstrate automatic installation.                                       //
// ------------------------------------------------------------------------- //

// intro.p006
parse_exp @"(x1 + x2 + x3) * (1 + 2 + 3 * x + y)";;

// pg. 21
// ------------------------------------------------------------------------- //
// Examples.                                                                 //
// ------------------------------------------------------------------------- //

// intro.p007
string_of_exp (parse_exp @"x + 3 * y");;

// intro.p008
parse_exp @"x + 3 * y";;

// intro.p009
parse_exp @"(x + 3) * y";;

// intro.p010
parse_exp @"1 + 2 + 3";;

// intro.p011
parse_exp @"((1 + 2) + 3) + 4";;

// pg. 22
// ------------------------------------------------------------------------- //
// Example shows the problem.                                                //
// ------------------------------------------------------------------------- //

// intro.p012
parse_exp @"(x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10) * (y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10)";;