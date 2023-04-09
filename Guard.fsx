// From https://github.com/JimBobSquarePants/ImageProcessor/blob/Core/src/ImageProcessorCore/Common/Helpers/Guard.cs

open System
open System.Diagnostics

/// Provides methods to protect against invalid parameters.
type Guard() =

    /// Verifies, that the method parameter with specified object value is not null
    /// and throws an exception if it is found to be so.
    static member NotNull(target, parameterName, ?message0) = 
        let message = defaultArg message0 ""
    
        if (target = null) then
            if (String.IsNullOrWhiteSpace(message)) then
                raise <| ArgumentNullException(parameterName, message)
            raise <| ArgumentNullException(parameterName)
        
    

    /// Verifies, that the string method parameter with specified object value and message
    /// is not null, not empty and does not contain only blanks and throws an exception
    /// if the object is null.
    static member NotNullOrEmpty(target, parameterName) =
    
        if (target = null) then
            raise <| ArgumentNullException(parameterName)

        if (String.IsNullOrWhiteSpace(target)) then
            raise <| ArgumentException("Value cannot be null or empty and cannot contain only blanks.", parameterName)
        
    

    /// Verifies that the specified value is less than a maximum value
    /// and throws an exception if it is not.
    static member MustBeLessThan<'TValue when 'TValue :> IComparable<'TValue> >(value:'TValue, max:'TValue, parameterName) = 
        
        if (value.CompareTo(max) >= 0) then
            raise <| ArgumentOutOfRangeException(
                parameterName,
                "Value must be less than max.")
        
    /// Verifies that the specified value is less than or equal to a maximum value
    /// and throws an exception if it is not.
    static member MustBeLessThanOrEqualTo<'TValue when 'TValue :> IComparable<'TValue> >(value:'TValue, max:'TValue, parameterName) =
                //where TValue : IComparable<TValue>
    
        if (value.CompareTo(max) > 0) then
            raise <| ArgumentOutOfRangeException(
                parameterName,
                "Value must be less than or equal to max.")
        
    

    /// Verifies that the specified value is greater than a minimum value
    /// and throws an exception if it is not.
    static member MustBeGreaterThan<'TValue when 'TValue :> IComparable<'TValue> >(value:'TValue, min:'TValue, parameterName) =
                
        if (value.CompareTo(min) <= 0) then
            raise <| ArgumentOutOfRangeException(
                parameterName,
                "Value must be greater than min.")
        
    

    /// Verifies that the specified value is greater than or equal to a minimum value
    /// and throws an exception if it is not.
    static member MustBeGreaterThanOrEqualTo<'TValue when 'TValue :> IComparable<'TValue> >(value:'TValue, min:'TValue, parameterName) =
    
        if (value.CompareTo(min) < 0) then
            raise <| ArgumentOutOfRangeException(
                parameterName,
                "Value must be greater than or equal to min.")
        
    /// Verifies that the specified value is greater than or equal to a minimum value and less than
    /// or equal to a maximum value and throws an exception if it is not.
    static member MustBeBetweenOrEqualTo<'TValue when 'TValue :> IComparable<'TValue> >(value:'TValue, min:'TValue, max:'TValue, parameterName) =
   
        if (value.CompareTo(min) < 0 || value.CompareTo(max) > 0) then
            raise <| ArgumentOutOfRangeException(
                parameterName,
                "Value must be greater than or equal to min and less than or equal to max.")
        
    

