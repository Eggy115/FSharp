(*
Examples of tips and smells
*)

// ======================================
// Code smells around not fully embracing functional programming
//
// * use immutable data 
// * use higher order functions
// * treat everything an an expression
// ======================================


// For example, here is a function to count the elements of a list using a loop and a mutable counter:

let countElements list = 
    let mutable count = 0
    for element in list do
        count <- count + 1
    count

// In cases when you are iterating over a list and accumulating things, a `fold` can do the same thing in fewer lines:

let countElementsWithFold list = 
    let foldAction state element = state + 1
    let initialValue = 0
    list |> Seq.fold foldAction initialValue

(*
Let's say that you have a list of objects, and some way of extracting a key, and you want to find the one that is largest, or newest, or whatever. 

In a mutable approach, you would have a global candidate key and element which is updated
if you find a better one as you iterate through the list ([C# version](http://stackoverflow.com/a/914198/1136133)).

*)

/// Given a list of objects, and a function to extract a key, and an initial candidate
/// return the object with the highest key.
/// Return the initialCandidate if the list is empty.
let maxBy keySelector initialCandidate list = 
    let mutable candidate = initialCandidate 
    let mutable maxKeySoFar = keySelector initialCandidate 
    for element in list do
        let key = keySelector element 
        if key > maxKeySoFar then
            maxKeySoFar <- key
            candidate <- element 
    candidate 

   
// Here's the same version rewritten using `fold`.
    
/// Given a list of objects, and a function to extract a key, and an initial candidate
/// return the object with the highest key.
/// Return the initialCandidate if the list is empty.
let maxByWithFold keySelector initialCandidate list = 

    // given an existing candidate and the next element
    // compare their keys and return the one with the higher key
    let compareKeys state element = 
        let (maxKeySoFar,candidate) = state
        let key = keySelector element 
        if key > maxKeySoFar then
             key,element   // new state
        else
             state // old state

    // use the fold function to iterate over the list                    
    let initialKey = keySelector initialCandidate 
    let initialState = initialKey,initialCandidate 
    list |> Seq.fold compareKeys initialState |> snd

// Everything that you can do with `fold` can be done with recursion and vice versa. So here's a recursive version as well.


/// Given a list of objects, and a function to extract a key
/// return the object with the highest key.
/// Return the initialCandidate if the list is empty.
let maxByWithRecursion keySelector initialCandidate list = 

    // given an existing candidate and the next element
    // compare their keys and return the one with the higher key
    let compareKeys state element = 
        let (maxKeySoFar,candidate) = state
        let key = keySelector element 
        if key > maxKeySoFar then
             key,element   // new state
        else
             state // old state

    // inner recursive version        
    let rec loop state list =
        match list with
        | [] -> state
        | element::rest -> 
            let newState = compareKeys state element 
            loop newState rest

    // use the inner loop function to iterate over the list                    
    let initialKey = keySelector initialCandidate 
    let initialState = initialKey,initialCandidate 
    loop initialState list |> snd


// In general I prefer to use `fold` rather than recursion because it's cleaner and avoids any issues with not having tail recursion.
// On the other hand, recursion can be simpler sometimes, especially if you want to return or break early.
// Either approach is better than using `mutable`!

// One more example. Let's say that you need to read lines from the input until a blank line occurs, and then process them.

open System

/// Read lines from the input until a blank line occurs
/// then return the list of strings
let readLinesFromInput()  = 
    let mutable list = []
    let mutable input = Console.ReadLine()
    while input <> "" do
        list  <- list @ [input]
        input <- Console.ReadLine()
    list 

(*
To test this, paste this into the F# interactive window:

readLinesFromInput();;
*)


// Now here's what a recursive version might look like:


/// Read lines from the input until a blank line occurs
/// then return the list of strings
let readLinesFromInputRecursive()  = 
    // inner recursive function
    let rec loop listSoFar = 
        let input = Console.ReadLine()
        match input with 
        | "" -> 
            // terminate loop and return
            listSoFar 
        | _ -> 
            // append to make a new list and keep going
            let newList = listSoFar @ [input]
            loop newList 
    loop []

// And here's a version using `Seq.initInfinite` which is of course lazy:

/// Read lines from the input until a blank line occurs
/// then return the list of strings
let readLinesFromInputSeq()  = 
    Seq.initInfinite (fun i -> Console.ReadLine())
    |> Seq.takeWhile (fun s -> s <> "")
    |> Seq.toList



// ======================================
// taking full advantage of the built-in collection functions
// ======================================

(*
There are lot of really useful functions available to you in `List` and `Seq` modules.
If you understand what they all do, you can often save yourself quite a bit of time and make your code simpler.

And if you are looking for functions that aren't there, chances are someone else has already written it for you.
For example, the excellent "[FSharpx.Collections](https://github.com/fsprojects/FSharpx.Collections)" project
has [useful extensions for `List`, `Map`, etc.](https://fsprojects.github.io/FSharpx.Collections/reference/index.html) so try there first.

*)


// I could have just written:

let countElements list = 
    Seq.length list

let maxBy keySelector initialCandidate list = 
    // prepend the initialCandidate 
    let newSeq = seq { yield initialCandidate; yield! list}
    newSeq |> Seq.maxBy keySelector 

// ======================================
// Choose `choose`
// ======================================

(*
One collection function that deserves more attention is `choose`, which can replace a `filter` and `map` with one step.

For example, let's say that you have a collection of options, and you only want to return the valid ones, where they are `Some`.

if you didn't know about `choose` you might use `filter` to find all the valid ones, and then `map` to extract the values, like this:
*)

let selectSome (aListOfOptions:'a option list)  = 
    aListOfOptions
    // only include the valid ones
    |> List.filter (fun e -> e.IsSome )
    // extract the data
    |> List.map (fun e -> e.Value)

// test
[Some 1; Some 2; None] |> selectSome

(*
This is also really smelly because I am using the methods `e.IsSome` and `e.Value`,
which in turn means that I have specify the type of the `aListOfOptions` parameter.

I could replace `(fun e -> e.IsSome )` with just `Option.isSome`, which also has the benefit of letting the type inference do its thing,
so that we no longer need the type annotation on the parameter:
*)

let selectSomeV2 aListOfOptions= 
    aListOfOptions
    // only include the valid ones
    |> List.filter Option.isSome
    // extract the data
    |> List.map (fun e -> e.Value)

// But why even bother with this two step approach -- `choose` will do this in one step!

let selectSomeV3 aListOfOptions = 
    aListOfOptions
    |> List.choose id

// test    
[Some 1; Some 2; None] |> selectSomeV3     


// In this case, I'm using `id` as the choose function, because the elements in the list are options already.

// In this next example, the elements in the list are of type `Person`, and I want to extract the names of persons that fulfil a certain property.

// Using `filter` and `map` I might write it like this:

type Person = {name:string; age:int}

let selectNamesOfPeopleLegalToDrive listOfPeople = 
    listOfPeople
    // only include the valid ones
    |> List.filter (fun e -> e.age >= 18)
    // extract the data
    |> List.map (fun e -> e.name)

// test
let persons = [ 
    {name="Alice"; age=10}
    {name="Bob"; age=20}
    {name="Carol"; age=30} ]
    
persons |> selectNamesOfPeopleLegalToDrive    

// But with `choose`, I could write it like this instead:

let selectNamesOfPeopleLegalToDriveV2 listOfPeople = 
    let nameOfLegalPerson p = 
        if p.age >= 18 then Some p.name else None

    listOfPeople
    |> List.choose nameOfLegalPerson 

// test    
persons |> selectNamesOfPeopleLegalToDriveV2        

// ======================================
// Treating lists like indexed collections
// ======================================

(*
If you are coming from C#, you might have a tendency to think of F# lists as just like indexable collections such as arrays or `List<T>`.

F# lists are *not* indexable collections. You cannot do `myList[0]` in F# -- not when `myList` is a list, anyway.  

And yes, it is quite confusing that the C# `List<T>` is not the same as the F# `list`. Sorry about that!
In fact the C# `List<T>` *is* available in F#, but under the name `ResizeArray`, which reflects how it is used, as a resizable, mutable, indexable collection.

The clue that someone is thinking of F# lists as indexable collections is when you see `List.nth` used a lot. For example, in something like this:
*)

let printList list  = 
    let len = List.length list
    for i=0 to len-1 do
        let element = List.nth list i
        printfn "The %ith element is %A" i element
        
// test
[1..5] |> printList       

(*
There are two reasons not to do this:

First, `List.nth` involves traversing the list from the beginning *every time*, because lists are implemented somewhat like linked lists ([here is good explanation of this](http://diditwith.net/2008/03/03/WhyILoveFListsTheBasics.aspx)).

Second, because there is already a built-in function that will do what you want: `iteri` in this case (there is also `mapi`). Here's a version using `iteri`:
*)
let printListV2 list  = 
    list
    |> List.iteri (fun i element -> 
        printfn "The %ith element is %A" i element )
        
// test
[1..5] |> printListV2


// When using collection functions like this, I also like to make little helper functions internally, which help keep the main pipeline code clean and easy to understand:


let printListV3 list  = 
    // internal helper
    let printElement i element =
        printfn "The %ith element is %A" i element

    // main pipeline is easier to understand
    list
    |> List.iteri printElement 



// ======================================
// Treating lists as appendable collections
// ======================================

(*
Another smell is when you see items appended to the end of a list. 

In C#, a `List<T>` is commonly appended to with methods such as [`Add`](https://msdn.microsoft.com/en-us/library/bb310301(v=vs.110).aspx).
In F# appending to a list is expensive, while prepending is cheap.  

This leads to an idiom where new lists are built by *prepending only* and then reversed as a final step.

For example, in the `readLinesFromInputRecursive` example above, I used the code `listSoFar @ [input]` to append a one element list (`[input]`) to the `listSoFar`:
*)

/// Read lines from the input until a blank line occurs
/// then return the list of strings
let readLinesFromInputRecursive()  = 
    // inner recursive function
    let rec loop listSoFar = 
        let input = Console.ReadLine()
        match input with 
        | "" -> 
            // terminate loop and return
            listSoFar 
        | _ -> 
            // append to make a new list and keep going
            let newList = listSoFar @ [input]
            loop newList 
    loop []

(*
The use of `@` in that code is a bit funny. A much more idiomatic approach would be to do
`let newList = input :: listSoFar` for each iteration and then do `listSoFar |> List.rev` at the end, like this:
*)

let readLinesFromInputRecursiveV2()  = 
    // inner recursive function
    let rec loop listSoFar = 
        let input = Console.ReadLine()
        match input with 
        | "" -> 
            // terminate loop and return reversed list
            listSoFar |> List.rev
        | _ -> 
            // prepend to make a new list and keep going
            let newList = input :: listSoFar 
            loop newList 
    loop []

(*
Appending to lists is considered to be much slower than prepending, but for small lists (less than 100 elements) it might not be that big a deal.
([I did some performance tests here](http://fsharpforfunandprofit.com/posts/monoids-part3/#performance)).  

It is more of idiomatic thing -- you should be aware of how lists work!
*)


// ======================================
// Ignoring the output of an expression
// ======================================

(*
In F# everything is an expression and therefore there is always an "output" that should be consumed.
When the value of an expression is ignored, that can often be a sign of bad code.

Now it is true that you *do* sometimes have to ignore the value of an expression, especially when interacting with side-effecting methods in the .NET libraries.
For example, you might safely ignore the result of [`SqlCommand.ExecuteNonQuery`](https://msdn.microsoft.com/enus/library/system.data.sqlclient.sqlcommand.executenonquery(v=vs.110).aspx)
or even something like [`List<T>.RemoveAll`](https://msdn.microsoft.com/en-us/library/wdka673a(v=vs.110).aspx).

But if you are interacting with pure F# code, seeing `ignore` is generally a sign of a bad design.

Let's look at an example. Say that you have a `updateDatabase` function that returns success or failure, like this:
*)

type SuccessFailure<'a> =
    | Success of 'a
    | Failure of string

let updateDatabase record = 
    // do something
    Success "OK"


// And then you want to update two records in a row like this:


let highLevelFunction() = 
    let record1 = "test"
    updateDatabase record1  // This expression should have type 'unit', 

    let record2 = "test"
    updateDatabase record2 


// The problem is that the compiler complains about the `updateDatabase record1` line, because it should have type 'unit' rather than type `SuccessFailure`. 

// No problem -- just add `ignore` afterwards to make the error go away, right?


let highLevelFunctionWithIgnore() = 
    let record1 = "test"
    updateDatabase record1  |> ignore

    let record2 = "test"
    updateDatabase record2  |> ignore


// Nooooo! That's the code smell right there. Why are you ignoring the error? What should you do if `record1` was *not* updated? 

// By using `ignore` you've turned off a helpful diagnostic tool that the compiler is giving you.

// One approach which is better is to handle the error explicitly, like this:


let highLevelWithErrorHandling() = 
    let record1 = "test"
    match updateDatabase record1 with
    | Success _ ->
        let record2 = "test"
        updateDatabase record2 
    | Failure err ->
        Failure err 

(*
There are other approaches that you can use too. But please, make the errors visible rather than ignoring them.

If you think that handling all these error codes will get complicated, please see my talk on [error handling](/rop/).
*)

let bind f xResult = 
    match xResult with 
    | Success x -> f x
    | Failure err -> Failure err 

let highLevelWithErrorHandlingV2() = 
    let record1 = "test"
    let record2 = "test"

    updateDatabase record1 
    |> bind (fun _ -> updateDatabase record2)


// ======================================
// Throwing exceptions rather than returning error types.
// ======================================

(*
I don't recommend throwing exceptions except for certain cases such as truly unrecoverable errors or simple scripts.

The reason is that exceptions do not show up in the type signature, and so you can not be sure what a function does just by looking at its signature.

For example, if I have this code: 

*)


let updateDatebase record = 
    // do something
    let result = true
    if result then 
        () // no return value
    else 
        failwith "Duplicate Key"

(*

It has the signature `Customer -> unit`, say, which implies that it will always succeed. This is not true! The signature is lying to us!

On the other hand, if the code is written like this:
*)

let updateDatebaseWithError record = 
    // do something
    let result = true
    if result then 
        Success ()
    else 
        Failure "Duplicate Key"

(*
It has the signature `Customer -> SuccessFailure`, say, which implies that it might not always succeed and that we have to be prepared to handle errors.
This signature is not lying to us, and so it is preferable to the exception throwing version.
*)

