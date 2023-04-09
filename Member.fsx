// from https://github.com/VaughnVernon/IDDD_Samples_NET/blob/master/iddd_agilepm/Domain.Model/Teams/Member.cs


open System

#load "Guard.fsx"
open Guard

#load "MemberChangeTracker.fsx"
open MemberChangeTracker

/// =======================================
/// Dummy classes to make code compile
/// =======================================

type TenantId = TenantId of int

type AssertionConcern() =
    static member AssertArgumentNotEmpty(value, message) = ()
    static member AssertArgumentLength(value,length,message) = ()
    

/// =======================================
/// Main code
/// =======================================

[<AbstractClass>]
type Member(tenantId : TenantId,
            userName : string ,
            firstName : string ,
            lastName : string ,
            emailAddress : string ,
            initializedOn : DateTime ) =
    
    let mutable tenantId = tenantId
    let mutable userName = userName 
    let mutable emailAddress = emailAddress
    let mutable enabled = true
    let mutable firstName = firstName
    let mutable lastName = lastName
    let mutable changeTracker = MemberChangeTracker(initializedOn, initializedOn, initializedOn)

    member this.TenantId :TenantId = tenantId

    member this.Username 
        with get () = userName
        and set (value) = 
            AssertionConcern.AssertArgumentNotEmpty(value, "The username must be provided.")
            AssertionConcern.AssertArgumentLength(value, 250, "The username must be 250 characters or less.")
            userName <- value
    
    member this.EmailAddress
        with get () = this.EmailAddress
        and set (value) = 
            if (value <> null) then
                AssertionConcern.AssertArgumentLength(emailAddress, 100, "Email address must be 100 characters or less.")
            emailAddress <- value

    member this.FirstName
        with get () = firstName
        and set (value) = 
            if (value <> null) then
                AssertionConcern.AssertArgumentLength(value, 50, "First name must be 50 characters or less.")
            firstName <- value

    member this.LastName
        with get () = lastName
        and set (value) = 
            if (value <> null) then
                AssertionConcern.AssertArgumentLength(value, 50, "Last name must be 50 characters or less.")
            this.LastName <- value

    member this.Enabled = enabled

    member this.ChangeEmailAddress(emailAddress:string, asOfDate:DateTime) =
        if changeTracker.CanChangeEmailAddress(asOfDate)
            && this.EmailAddress <> emailAddress then
            this.EmailAddress <- emailAddress
            changeTracker <- changeTracker.EmailAddressChangedOn(asOfDate)

    member this.ChangeName(firstName:string, lastName:string, asOfDate:DateTime) = 
        if changeTracker.CanChangeName(asOfDate) then
            this.FirstName <- firstName
            this.LastName <- lastName
            changeTracker <- changeTracker.NameChangedOn(asOfDate)

    member this.Disable(asOfDate:DateTime) = 
        if changeTracker.CanToggleEnabling(asOfDate) then
            enabled <- false
            changeTracker <- changeTracker.EnablingOn(asOfDate)

    member this.Enable(asOfDate:DateTime) = 
        if changeTracker.CanToggleEnabling(asOfDate) then
            enabled <- true
            changeTracker <- changeTracker.EnablingOn(asOfDate)
