module CriteriaQueryBuilder

open System
open Microsoft.FSharp.Reflection
open Newtonsoft.Json
open Newtonsoft.Json.Serialization

type OptionConverter() =
  inherit JsonConverter()

  override x.CanConvert(t) =
    t.IsGenericType && t.GetGenericTypeDefinition() = typedefof<option<_>>

  override x.WriteJson(writer, value, serializer) =
    let value =
      if value = null then null
      else
        let _,fields = FSharpValue.GetUnionFields(value, value.GetType())
        fields.[0]
    serializer.Serialize(writer, value)

  override x.ReadJson(reader, t, existingValue, serializer) =
    let innerType = t.GetGenericArguments().[0]
    let innerType =
      if innerType.IsValueType then (typedefof<Nullable<_>>).MakeGenericType([|innerType|])
      else innerType
    let value = serializer.Deserialize(reader, innerType)
    let cases = FSharpType.GetUnionCases(t)
    if value = null then FSharpValue.MakeUnion(cases.[0], [||])
    else FSharpValue.MakeUnion(cases.[1], [|value|])

let fromJson<'a> json =
  let settings = new JsonSerializerSettings()
  settings.Converters.Add(new Converters.StringEnumConverter())
  settings.Converters.Add(new OptionConverter())
  settings.NullValueHandling <- NullValueHandling.Ignore

  let result = JsonConvert.DeserializeObject<'a>(json, settings)
  //rintfn "JSON:\n %A" result
  result

let toUnderscoreJson model =
  let settings = new JsonSerializerSettings()
  let contractResolver = new DefaultContractResolver()
  contractResolver.NamingStrategy <- new SnakeCaseNamingStrategy()
  settings.ContractResolver <- contractResolver
  settings.Converters.Add(new Converters.JavaScriptDateTimeConverter())
  settings.Converters.Add(new Converters.StringEnumConverter())
  settings.Converters.Add(new OptionConverter())
  settings.NullValueHandling <- NullValueHandling.Ignore

  let result = JsonConvert.SerializeObject(model, settings)
  //printfn "JSON:\n %s" result
  result

type LogicOperator =
  | And = 0
  | Or = 1

type ConditionType =
  | Any = 0
  | GreaterThan = 1
  | GreaterThanEqual = 2
  | LessThan = 3
  | LessThanEqual = 4
  | Equal = 5
  | NotEqual = 6
  | Contains = 7
  | NotContains = 8
  | StartsWith = 9
  | EndsWith = 10

type Criterion = {
  field: string
  condition: ConditionType
  value: string
}

type Criteria = {
  logic: LogicOperator option
  items: Criterion list
}

type CriteriaGroup = {
  index: int
  joinGroupIndex: int option
  joinGroupLogic: LogicOperator option
  criteria: Criteria
}

type QueryString = {
  defaultOperator: string
  query: string
}

type Query = {
  queryString: QueryString
}

type QueryModel = {
  query: Query
}

let getCriteriaGroupListFromJson json =
  try
    let result: CriteriaGroup list = fromJson json
    result
   with
   | ex ->
     printfn "Exception: %s" ex.Message
     []

let toQueryModel query = {
  query =
  {
    queryString =
    {
      defaultOperator = "AND"
      query = query
    }
  }
}

let buildWordItem field value =
  sprintf "%s:%s" field value

let buildSentenceItem field value =
  sprintf "%s:('%s')" field value

let buildItem field (value: string) =
  match value.Contains(" ") with
  | true -> buildSentenceItem field value
  | false -> buildWordItem field value

let tailItemsQuery (items: Criterion list) (logic: string) =
  let queryItems = [
    for item in items |> List.tail ->
    sprintf "%s %s" (logic.ToUpper()) (buildItem item.field item.value)
  ]
  queryItems |> String.concat " "

let buildItems (items: Criterion list) logic =
  if items.Length < 0 then "" // TODO: should be error
  elif items.Length = 1 then
    let head = items |> List.head
    buildItem head.field head.value
  else
    let head = items |> List.head
    let headItem = buildItem head.field head.value
    let tailItems = tailItemsQuery items logic
    sprintf "(%s %s)" headItem tailItems

let criteriaQuery (criteria: Criteria) =
  match criteria.logic with
  | None -> buildItems criteria.items ""
  | _ ->
    let logic = criteria.logic.Value.ToString()
    buildItems criteria.items logic

let buildQueryModel (criteriaList: CriteriaGroup list) =
  let items = criteriaList |> List.sortBy (fun x -> x.index)
  let queryItems = [
    for item in items ->
      match item.joinGroupLogic with
      | None ->
        criteriaQuery item.criteria
      | _ ->
        let logic = item.joinGroupLogic.Value.ToString().ToUpper()
        sprintf "%s %s" logic (criteriaQuery item.criteria)
  ]
  //printfn "%A" queryItems
  queryItems
  |> String.concat " "
  |> toQueryModel

let buildQueryText json =
  json
  |> fromJson
  |> buildQueryModel
  |> toUnderscoreJson
