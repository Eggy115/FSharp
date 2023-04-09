// From https://github.com/VaughnVernon/IDDD_Samples_NET/blob/master/iddd_agilepm/Domain.Model/Teams/MemberChangeTracker.cs

open System

type MemberChangeTracker(enablingOn:DateTime, nameChangedOn:DateTime, emailAddressChangedOn:DateTime) =

    member this.CanChangeEmailAddress(asOfDateTime:DateTime) : bool =
        emailAddressChangedOn < asOfDateTime
    
    member this.CanChangeName(asOfDateTime:DateTime) : bool =
        nameChangedOn < asOfDateTime
    
    member this.CanToggleEnabling(asOfDateTime:DateTime) : bool =
        enablingOn < asOfDateTime
    
    member this.EmailAddressChangedOn(asOfDateTime:DateTime) : MemberChangeTracker =
        MemberChangeTracker(enablingOn, nameChangedOn, asOfDateTime)

    member this.EnablingOn(asOfDateTime:DateTime) : MemberChangeTracker =
        MemberChangeTracker(asOfDateTime, nameChangedOn, emailAddressChangedOn)

    member this.NameChangedOn(asOfDateTime:DateTime) : MemberChangeTracker =
        MemberChangeTracker(enablingOn, asOfDateTime, emailAddressChangedOn)
    
    member this.GetEqualityComponents() = 
        seq {
        yield this.EnablingOn
        yield this.NameChangedOn
        yield this.EmailAddressChangedOn
        }

