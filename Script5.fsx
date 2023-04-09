﻿open System.IO
open FSharp.Charting
open System.Windows.Forms.DataVisualization.Charting

open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.Distributions

let filePath = @"smile_data.csv"

/// Split row on commas
let splitCommas (l:string) =
    l.Split(',')

/// Read a file into a string array
let openFile (name : string) =
    try
        let content = File.ReadAllLines(name)
        content |> Array.toList
    with
        | :? System.IO.FileNotFoundException as e -> printfn "Exception! %s " e.Message; ["empty"]

/// Read the data from a CSV file and returns
/// a tuple of strike price and implied volatility%
let readVolatilityData =
    openFile filePath
    |> List.map splitCommas
    |> List.map (fun cols -> (cols.[2], cols.[3]))

/// 83.2
/// Calculates moneyness and parses strings into numbers
let calcMoneyness spot list =
    list
    |> List.map (fun (strike, imp) -> (spot / (float strike), (float imp)))

let list = readVolatilityData
let mlist = calcMoneyness 83.2 list

/// Plot values using FSharpChart
fsi.AddPrinter(fun (ch:FSharp.Charting.ChartTypes.GenericChart) -> ch.ShowChart(); "FSharpChartingSmile")    
Chart.Point(mlist)

/// Final step - Plot data

/// Sample points
//let xdata = [ 0.0; 1.0; 2.0; 3.0; 4.0 ]
//let ydata = [ 1.0; 1.4; 1.6; 1.3; 0.9 ]

let xdata = mlist |> Seq.map (fun (x, _) -> x) |> Seq.toList
let ydata = mlist |> Seq.map (fun (_, y) -> y) |> Seq.toList

let N = xdata.Length
let order = 2

/// Generating a Vandermonde row given input v
let vandermondeRow v = [for x in [0..order] do yield v ** (float x)]

/// Creating Vandermonde rows for each element in the list
let vandermonde = xdata |> Seq.map vandermondeRow |> Seq.toList

/// Create the A Matrix
let A = vandermonde |> DenseMatrix.ofRowsList N (order + 1)
A.Transpose()

/// Create the Y Matrix
let createYVector order l = [for x in [0..order] do yield l]
let Y = (createYVector order ydata |> DenseMatrix.ofRowsList (order + 1) N).Transpose()

/// Calculate coefficients using least squares
let coeffs = (A.Transpose() * A).LU().Solve(A.Transpose() * Y).Column(0)

let calculate x = (vandermondeRow(x) |> DenseVector.ofList) * coeffs

let fitxs = [(Seq.min xdata).. 0.01 ..(Seq.max xdata)]
let fitys = fitxs |> List.map calculate
let fits = [for x in [(Seq.min xdata).. 0.01 ..(Seq.max xdata)] do yield (x, calculate x)]

let chart = Chart.Combine [Chart.Point(mlist); Chart.Line(fits).WithTitle("Volatility Smile")]