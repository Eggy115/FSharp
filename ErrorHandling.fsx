[<RequireQualifiedAccess>]
module Result =
    (**

    Error handling types and functions

    See fsharpforfunandprofit.com/rop

    **)

    type Result<'success, 'failure> = 
        | Success of 'success
        | Failure of 'failure

    /// Create a success case
    let success x = Success x

    /// Create a failure case
    let failure x = Failure x 

    /// Create a failure case where the failures are expected to be a list (e.g for validation)
    let failureList x = Failure [ x ]

    /// Map a function over the success case
    let map f result = 
        match result with
        | Success success -> Success(f success)
        | Failure msgs -> Failure msgs

    /// Map a function over the failure case
    let mapFailure f result = 
        match result with
        | Success success -> Success success
        | Failure msgs -> Failure(f msgs)

    /// Map a unit function over the success case
    let iter (f : _ -> unit) result = map f result |> ignore

    /// Apply Result function to a Result value 
    let apply fR xR = 
        match fR, xR with
        | Success f, Success x -> Success(f x)
        | Failure msgs1, Success _ -> Failure msgs1
        | Success _, Failure msgs2 -> Failure msgs2
        | Failure msgs1, Failure msgs2 -> Failure(msgs1 @ msgs2)

    /// Apply a monadic function to a Result value 
    let bind f result = 
        match result with
        | Success success -> f success
        | Failure msgs -> Failure msgs

    /// Lift a two parameter function to use Result parameters
    let lift2 f x1 x2 = 
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2

    /// Lift a three parameter function to use Result parameters
    let lift3 f x1 x2 x3 = 
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3

    /// Lift a four parameter function to use Result parameters
    let lift4 f x1 x2 x3 x4 = 
        let (<!>) = map
        let (<*>) = apply
        f <!> x1 <*> x2 <*> x3 <*> x4

    /// Apply a monadic two parameter function 
    let bind2 f x1 x2 = lift2 f x1 x2 |> bind id

    /// Apply a monadic three parameter function 
    let bind3 f x1 x2 x3 = lift3 f x1 x2 x3 |> bind id

    /// Convert an Option into a Result
    let fromOption msg opt = 
        match opt with
        | Some v -> success v
        | None -> failure msg

    /// Convert the success case into an Option (useful for List.choose)
    let toOption = 
        function 
        | Success s -> Some s
        | Failure _ -> None

    /// Convert the failure case into an Option (useful for List.choose)
    let toFailureOption = 
        function 
        | Success _ -> None
        | Failure e -> Some e

    /// Predicate that returns true on success
    let isSuccess = 
        function 
        | Success _ -> true
        | Failure _ -> false

    /// Predicate that returns true on failure
    let isFailure x = 
        x
        |> isSuccess
        |> not

    /// Lift a given predicate into a predicate that works on Results
    let filter pred = 
        function 
        | Success x -> pred x
        | Failure _ -> true

    /// Return a value for the failure case
    let ifFailure defaultVal = 
        function 
        | Success x -> x
        | Failure _ -> defaultVal

    /// Convert a list of Result into a Result<list> using applicative style. 
    /// All errors will be combined. The error type must be a list.
    let sequence resultList = 
        let cons head tail = head :: tail
        let consR = lift2 cons
        let initR = success []
        List.foldBack consR resultList initR

    /// Convert a list of Result into a Result<list> using monadic style. 
    /// Only the first error is returned. The error type need not be a list.
    let sequenceM resultList = 
        let folder result state = 
            state |> bind (fun list -> 
            result |> bind (fun element -> 
                success (element :: list)
                ))
        let initState = success []
        List.foldBack folder resultList initState 
    
    /// Get the Success value and throw an exception if failure
    /// Warning: use only in scripts or testing, not in production!
    let successValue = 
        function 
        | Success x -> x
        | Failure _ -> failwith "Expected Success state"

/// The `result` computation expression is available globally without qualification
[<AutoOpen>]
module ResultCE = 
    // ==================================
    // Computation expressions
    // ==================================
    type ResultBuilder() = 
        member __.Return(x) = Result.success x
        member __.Bind(x, f) = Result.bind f x
    
        member __.ReturnFrom(x) = x
        member this.Zero() = this.Return ()

        member __.Delay(f) = f
        member __.Run(f) = f()

        member this.While(guard, body) =
            if not (guard()) 
            then this.Zero() 
            else this.Bind( body(), fun () -> 
                this.While(guard, body))  

        member this.TryWith(body, handler) =
            try this.ReturnFrom(body())
            with e -> handler e

        member this.TryFinally(body, compensation) =
            try this.ReturnFrom(body())
            finally compensation() 

        member this.Using(disposable:#System.IDisposable, body) =
            let body' = fun () -> body disposable
            this.TryFinally(body', fun () -> 
                match disposable with 
                    | null -> () 
                    | disp -> disp.Dispose())

        member this.For(sequence:seq<_>, body) =
            this.Using(sequence.GetEnumerator(),fun enum -> 
                this.While(enum.MoveNext, 
                    this.Delay(fun () -> body enum.Current)))

        member this.Combine (a,b) = 
            this.Bind(a, fun () -> b())

    let result = ResultBuilder()

