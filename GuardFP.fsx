// From https://github.com/JimBobSquarePants/ImageProcessor/blob/Core/src/ImageProcessorCore/Common/Helpers/Guard.cs

open System
open System.Diagnostics

/// Provides methods to protect against invalid parameters.
module Guard =

    /// Verifies, that the method parameter with specified object value is not null
    /// and throws an exception if it is found to be so.
    let notNull(target, parameterName) = 
        if (target = null) then
            nullArg parameterName
        
    let notNullWithMessage(target, parameterName, message) = 
        if (target = null) then
            if (String.IsNullOrWhiteSpace(message)) then
                raise <| ArgumentNullException(parameterName,message)
            else
                nullArg parameterName

    let (|IsNullOrWhiteSpace|_|) str = 
        if String.IsNullOrWhiteSpace(str) then
            Some IsNullOrWhiteSpace
        else    
            None

    /// Verifies, that the string method parameter with specified object value and message
    /// is not null, not empty and does not contain only blanks and throws an exception
    /// if the object is null.
    let notNullOrEmpty(target, parameterName) =
        match target with
        | null ->
            nullArg parameterName
        | IsNullOrWhiteSpace -> 
            invalidArg parameterName "Value cannot be null or empty and cannot contain only blanks."
        | _ ->
            () // value is OK                
        
    let rangeCheck predicate message value parameterName= 
        if predicate value then
            raise <| ArgumentOutOfRangeException(paramName=parameterName, message=message)

    /// Verifies that the specified value is less than a maximum value
    /// and throws an exception if it is not.
    let mustBeLessThan max parameterName = 
        let predicate (value:#IComparable) = 
            value.CompareTo(max) >= 0    
        let message = sprintf "Value must be less than %O." max
        rangeCheck predicate message 
        
    /// Verifies that the specified value is less than or equal to a maximum value
    /// and throws an exception if it is not.
    let mustBeLessThanOrEqualTo max parameterName = 
        let predicate (value:#IComparable) = 
            value.CompareTo(max) > 0
        let message = sprintf "Value must be less than or equal to %O." max
        rangeCheck predicate message 

    /// Verifies that the specified value is greater than a minimum value
    /// and throws an exception if it is not.
    let mustBeGreaterThan min parameterName = 
        let predicate (value:#IComparable) = 
            value.CompareTo(min) <= 0
        let message = sprintf "Value must be greater than %O." min
        rangeCheck predicate message 
        
    /// Verifies that the specified value is greater than or equal to a minimum value
    /// and throws an exception if it is not.
    let mustBeGreaterThanOrEqualTo min parameterName = 
        let predicate (value:#IComparable) = 
            value.CompareTo(min) < 0
        let message = sprintf "Value must be greater than or equal to %O." min
        rangeCheck predicate message 
        
    /// Verifies that the specified value is greater than or equal to a minimum value and less than
    /// or equal to a maximum value and throws an exception if it is not.
    let mustBeBetweenOrEqualTo min max parameterName = 
        let predicate (value:#IComparable) = 
            value.CompareTo(min) < 0 || value.CompareTo(max) > 0
        let message = sprintf "Value must be greater than or equal to %O and less than or equal to %O." min max
        rangeCheck predicate message 
   
        
    

