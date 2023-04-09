// from https://github.com/VaughnVernon/IDDD_Samples_NET/blob/master/iddd_agilepm/Domain.Model/Teams/Member.cs


open System

#load "ErrorHandling.fsx"
open ErrorHandling

/// =======================================
/// Dummy classes to make code compile
/// =======================================

type TenantId = TenantId of int

type AssertionConcern() =
    static member AssertArgumentNotEmpty(value, message) = ()
    static member AssertArgumentLength(value,length,message) = ()
    

/// =======================================
/// Common domain
/// =======================================

// for how to make the constructors private, see https://gist.github.com/swlaschin/54cfff886669ccab895a
type String50 = String50 of string
type String100 = String100 of string
type String250 = String250 of string

let createString50 s = 
    if String.IsNullOrEmpty(s) then
        Result.failureList "NullOrEmpty"
    elif String.length s > 50 then
        Result.failureList "Longer than 50"
    else
        Result.success (String50 s)

let createString100 s = 
    if String.IsNullOrEmpty(s) then
        Result.failureList "NullOrEmpty"
    elif String.length s > 100 then
        Result.failureList "Longer than 100"
    else
        Result.success (String100 s)

let createString250 s = 
    if String.IsNullOrEmpty(s) then
        Result.failureList "NullOrEmpty"
    elif String.length s > 250 then
        Result.failureList "Longer than 250"
    else
        Result.success (String250 s)

/// =======================================
/// Main domain
/// =======================================

type UserName = UserName of String250
type FirstName  = FirstName  of String50
type LastName = LastName of String50
type EmailAddress = EmailAddress of String100

type PersonalName = {
    FirstName : FirstName 
    LastName : LastName 
    }

type MemberInfo = {
    TenantId : TenantId
    UserName : UserName
    PersonalName : PersonalName 
    EmailAddress : EmailAddress 
    PersonalNameChangedOn : DateTime 
    EmailAddressChangedOn : DateTime 
    }
    
type TeamMember = 
    | EnabledMember of MemberInfo * DateTime
    | DisabledMember of MemberInfo * DateTime
    

/// =======================================
/// Business logic
/// =======================================

let changeEmailAddress emailAddress asOfDate memberInfo =  // note that member info is last parameter
    if memberInfo.EmailAddressChangedOn < asOfDate && memberInfo.EmailAddress <> emailAddress then
        Some {memberInfo with EmailAddress=emailAddress; EmailAddressChangedOn=asOfDate}
    else
        None
        // could change design to return same object, but we probably want to know
        // that a change happened so we can update the database

let changeName personalName asOfDate memberInfo = 
    if memberInfo.PersonalNameChangedOn < asOfDate && memberInfo.PersonalName <> personalName then
        Some {memberInfo with PersonalName=personalName; PersonalNameChangedOn=asOfDate}
    else
        None

let disable asOfDate teamMember = 
    match teamMember with
    | EnabledMember (memberInfo,stateChanged) -> 
        if stateChanged < asOfDate then
            Some (DisabledMember (memberInfo,asOfDate))
        else
            None
    | DisabledMember _ -> 
        None

let enable asOfDate teamMember = 
    match teamMember with
    | DisabledMember (memberInfo,stateChanged) -> 
        if stateChanged < asOfDate then
            Some (EnabledMember (memberInfo,asOfDate))
        else
            None
    | EnabledMember _ -> 
        None

let mapTeamMember f teamMember =
    match teamMember with
    | EnabledMember (memberInfo,stateChanged) -> 
        EnabledMember (f memberInfo,stateChanged)
    | DisabledMember (memberInfo,stateChanged) -> 
        DisabledMember (f memberInfo,stateChanged)


/// =======================================
/// Constructors
/// =======================================

let createTenantId id = 
    if id <= 0 then
        Result.failureList "TenantId must be positive"
    else
        Result.success (TenantId id)

let createUserName s = 
    createString250 s
    |> Result.map (fun s250 -> UserName s250) 

let createFirstName s = 
    createString50 s
    |> Result.map FirstName 

let createLastName s = 
    createString50 s
    |> Result.map LastName 

let createEmailAddress s = 
    createString100 s
    |> Result.bind (fun s100 -> 
        if s.Contains("@") then
            Result.success (EmailAddress s100)
        else
            Result.failureList "Email must contain an @ sign"
        )

let createPersonalName firstName lastName = 
    let ctor firstName lastName =     
        {FirstName=firstName; LastName=lastName}
    let firstNameResult = createFirstName firstName
    let lastNameResult = createLastName lastName
    Result.lift2 ctor firstNameResult lastNameResult 
    
let createMemberInfo utcNow tenantId userName firstName lastName emailAddress = 
    let ctor tenantId userName firstName lastName emailAddress =     
        let personalName = {FirstName=firstName; LastName=lastName}
        {
        TenantId = tenantId
        UserName = userName
        PersonalName = personalName
        EmailAddress = emailAddress
        PersonalNameChangedOn = utcNow 
        EmailAddressChangedOn = utcNow 
        }
    
    let tenantIdResult = createTenantId tenantId
    let userNameResult = createUserName userName
    let firstNameResult = createFirstName firstName
    let lastNameResult = createLastName lastName
    let emailAddressResult = createEmailAddress emailAddress

    let ( <!> ) = Result.map
    let ( <*> ) = Result.apply
    
    ctor <!> tenantIdResult <*> userNameResult <*> firstNameResult <*> lastNameResult <*> emailAddressResult


/// =======================================
/// Constructors
/// =======================================

let time1 = DateTime(2016,1,1)
let time2 = DateTime(2016,2,2)

// example
let emailAddressResult = createEmailAddress "me@example.com"
let emailAddressResult2 = createEmailAddress "example.com"

let memberInfoResult = createMemberInfo time1 1 "aadams" "Alice" "Adams" "me@example.com"
let memberInfoResult2 = createMemberInfo time1 0 "aadams" "Alice" "Adams" "example.com"

let changeEmailAddressR email asOf mi = 
    changeEmailAddress email asOf mi 
    |> Result.fromOption ["Couldn't change email address"]

// try to change email address
let emailAddress1 = createEmailAddress "me@example.com" |> Result.successValue
let emailAddress2 = createEmailAddress "test@example.com" |> Result.successValue



memberInfoResult 
|> Result.bind (changeEmailAddressR emailAddress1 time2)

memberInfoResult 
|> Result.bind (changeEmailAddressR emailAddress2 time2)
