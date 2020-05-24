open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Giraffe
open Saturn
open Shared
open System.IO
open Farmer
open Farmer.Builders

let deploy (request:DeployRequest) =
    let resources : CoreTypes.IBuilder list = [
        for resource in request.Resources do
            match resource.Kind with
            | WebApp args ->
                webApp {
                    name resource.Name
                    operating_system (match args.OperatingSystem with "Linux" -> Linux | _ -> Windows)
                    number_of_workers args.NumberOfWorkers
                    sku (match args.Sku with Free -> Web.Free | B1 -> Web.Sku.B1 | B2 -> Web.Sku.B2 | S1 -> Web.Sku.S1)
                }
            | Storage ->
                storageAccount {
                    name resource.Name
                }
            | Search args ->
                search {
                    name resource.Name
                    replicas args.ReplicaCount
                    partitions args.PartitionCount
                }
    ]
    let deploy = arm {
        location (Location request.Location)
        add_resources resources
    }

    deploy
    |> Deploy.tryExecute request.ResourceGroup []
    |> Result.map ignore

let buildApi api =
    Remoting.createApi()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue api
    |> Remoting.buildHttpHandler

let allApis = choose [
    buildApi { Deploy = deploy >> async.Return }
    buildApi
        { GetLocations =
            fun () ->
                [ Location.NorthEurope
                  Location.WestEurope ]
                |> List.map(fun l -> l.ArmValue)
                |> async.Return }
]

let app = application {
    url "http://0.0.0.0:8085/"
    use_router allApis
    memory_cache
    use_static (Path.GetFullPath "../Client/public")
    use_gzip
}

run app
