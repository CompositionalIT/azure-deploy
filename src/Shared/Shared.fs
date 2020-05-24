namespace Shared

namespace Shared

type AvailableWebSku = Free | B1 | B2 | S1

type AzureResource =
    | WebApp of {| OperatingSystem : string; NumberOfWorkers : int; Sku : AvailableWebSku |}
    | Storage
    | Search of {| ReplicaCount:int; PartitionCount:int |}
    member this.Name =
        match this with
        | WebApp _ -> "Web Application"
        | Storage -> "Storage Account"
        | Search _ -> "Search Index"
    member this.Description =
        match this with
        | WebApp args -> Some (sprintf "%s with %d workers, %O" args.OperatingSystem args.NumberOfWorkers args.Sku)
        | Search args -> Some (sprintf "%d partitions and %d replicas" args.PartitionCount args.ReplicaCount)
        | Storage -> None

type Resource =
    { Name : string
      Kind : AzureResource }

type DeployRequest =
    { Location : string
      ResourceGroup : string
      Resources : Resource list }

module Route =
    /// Defines how routes are generated on server and mapped from client
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type IRefData =
    { GetLocations : unit -> string list Async }

type IDeployment =
    { Deploy : DeployRequest -> Result<unit, string> Async }

