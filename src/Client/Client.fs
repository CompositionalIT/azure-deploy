module Client

open Elmish
open Elmish.React
open Feliz
open Feliz.Bulma
open Shared
open System
open Zanaptak.TypedCssClasses

type EditDetails =
    { SelectedLocation : string
      ResourceGroup : string
      Resources : Resource list
      ResourceName : string
      SelectedAzureResource : AzureResource }

type DeployState = Editing of EditDetails | Deploying of DeployRequest | DeployCompleted of Result<unit, string>

type Model =
    { Locations : string list
      State : DeployState }

[<AutoOpen>]
module Lookups =
    let resourceLibrary =
        [ WebApp {| OperatingSystem = "Linux"; NumberOfWorkers = 1; Sku = Free |}
          Storage
          Search {| ReplicaCount = 1; PartitionCount = 1; |} ]
        |> List.map (fun x -> x.Name, x)
        |> Map
    let resourceLibraryNames = resourceLibrary |> Map.toList |> List.map fst
    let allWebSkus = [ Free; B1; B2; S1; ] |> List.map(fun x -> string x, x) |> Map
    let allWebSkuNames = allWebSkus |> Map.toList |> List.map fst

type Msg =
    | GotLocations of string list
    | ModelChanged of EditDetails
    | AddResource
    | RemoveResource of Resource
    | Deploy
    | Deployed of Result<unit, string>

type Icon = CssClasses<"https://pro.fontawesome.com/releases/v5.10.0/css/all.css", Naming.PascalCase>

module Server =
    open Fable.Remoting.Client
    let inline makeProxy<'TApi> =
      Remoting.createApi()
      |> Remoting.withRouteBuilder Route.builder
      |> Remoting.buildProxy<'TApi>

    let refData = makeProxy<IRefData>
    let deployApi = makeProxy<IDeployment>

    let getLocations() = Cmd.OfAsync.perform refData.GetLocations () GotLocations
    let deploy request = Cmd.OfAsync.perform deployApi.Deploy request Deployed

let init () =
    let initialModel =
        { Locations = []
          State =
            Editing { ResourceGroup = "my-resource-group"
                      Resources = []
                      SelectedLocation = ""
                      ResourceName = ""
                      SelectedAzureResource = AzureResource.Storage }
        }
    initialModel, Server.getLocations()

let update msg model =
    match msg, model with
    | ModelChanged m, _ ->
        { model with State = Editing m }, Cmd.none
    | GotLocations locations, { State = Editing state } ->
        { model with
            Locations = locations
            State = Editing { state with SelectedLocation = locations.[0] }
        }, Cmd.none
    | AddResource, { State = Editing state } ->
        { model with
            State =
                Editing
                    { state with
                        Resources =
                            { Name = state.ResourceName
                              Kind = state.SelectedAzureResource } :: state.Resources
                        ResourceName = "" }
        }, Cmd.none
    | RemoveResource resource, { State = Editing state } ->
        { model with
            State = Editing { state with Resources = state.Resources |> List.except [ resource ] }
        }, Cmd.none
    | Deploy, { State = Editing state } ->
        let request =
            { Location = state.SelectedLocation
              ResourceGroup = state.ResourceGroup
              Resources = state.Resources }
        { model with State = Deploying request }, Server.deploy request
    | Deployed result, _ ->
        { model with State = DeployCompleted result }, Cmd.none
    | _ ->
        model, Cmd.none

module FormHelpers =
    let makeField content (label:string) =
        Bulma.field.div [
            Bulma.label [ Html.text label ]
            Bulma.control.div [ prop.children [ content ] ]
        ]
    let makeTextField dispatch label placeholder (value:string) onChange =
        let content =
            Bulma.input.text [
                prop.placeholder placeholder
                prop.value value
                prop.onTextChange(onChange >> ModelChanged >> dispatch)
            ]
        makeField content label
    let makeSelectField dispatch label options selected (onChange:string -> _) =
        let content =
            Bulma.select [
                prop.onChange(onChange >> ModelChanged >> dispatch)
                prop.children [
                    for (option:string) in options do
                        Html.option [
                            if option = selected then prop.value "" else ()
                            prop.children [ Html.text option ]
                        ]
                ]
            ]
        makeField content label

let makeIcon icon =
    Html.i [
        prop.className (Icon.Fa + " " + icon)
    ]

module View =
    let makeRemoveIcon dispatch resource =
        Bulma.panelIcon [
            Bulma.color.hasTextDanger
            prop.children [ makeIcon Icon.FaMinusCircle ]
            prop.onClick(fun _ -> dispatch (RemoveResource resource))
            prop.style [ style.cursor "pointer" ]
        ]
    let makeKindIcon resource =
        Bulma.panelIcon [
            Bulma.color.hasTextInfo
            prop.children [
                match resource.Kind with
                | AzureResource.Search _ -> Icon.FaSearch
                | AzureResource.WebApp _ -> Icon.FaGlobe
                | AzureResource.Storage -> Icon.FaBoxes
                |> makeIcon
            ]
        ]
    let makeResourceLibrary dispatch state = [
        Bulma.title.h3 "Resource Library"
        Bulma.box [
            prop.children [
                FormHelpers.makeSelectField dispatch "Resource Type" resourceLibraryNames state.SelectedAzureResource.Name (fun resource -> { state with SelectedAzureResource = resourceLibrary.[resource] })
                FormHelpers.makeTextField dispatch "Name" "Enter the name for the new resource" state.ResourceName (fun name -> { state with ResourceName = name })
                match state.SelectedAzureResource with
                | Search args ->
                    FormHelpers.makeSelectField dispatch "Replicas" (List.map string [ 1 .. 5 ]) (string args.ReplicaCount) (fun replicas -> { state with SelectedAzureResource = Search {| args with ReplicaCount = int replicas |}})
                    FormHelpers.makeSelectField dispatch "Partitions" (List.map string [ 1 .. 5 ]) (string args.PartitionCount) (fun partitions -> { state with SelectedAzureResource = Search {| args with PartitionCount = int partitions |}})
                | WebApp args ->
                    FormHelpers.makeSelectField dispatch "OS" [ "Linux"; "Windows" ] ( args.OperatingSystem) (fun os -> { state with SelectedAzureResource = WebApp {| args with OperatingSystem = os |}})
                    FormHelpers.makeSelectField dispatch "SKU" allWebSkuNames (string args.Sku) (fun sku -> { state with SelectedAzureResource = WebApp {| args with Sku = allWebSkus.[sku] |}})
                    FormHelpers.makeSelectField dispatch "Workers" (List.map string [ 1 .. 5 ]) (string args.NumberOfWorkers) (fun workers -> { state with SelectedAzureResource = WebApp {| args with NumberOfWorkers = int workers |}})
                | Storage ->
                    ()

                Bulma.button.span [
                    if String.IsNullOrWhiteSpace state.ResourceName then prop.disabled true
                    else prop.onClick(fun _ -> dispatch AddResource)
                    Bulma.color.isPrimary
                    prop.children [
                        Html.text "Add"
                    ]
                ]
            ]
        ]
    ]

    let drawEdit dispatch model state = [
        FormHelpers.makeTextField dispatch "Resource Group" "Enter your resource group name" state.ResourceGroup (fun x -> { state with ResourceGroup = x })
        FormHelpers.makeSelectField dispatch "Location" model.Locations state.SelectedLocation (fun location -> { state with SelectedLocation = location })

        Bulma.columns [
            Bulma.column [
                Bulma.column.is6
                prop.children (makeResourceLibrary dispatch state)
            ]
            Bulma.column [
                prop.children [
                    Bulma.title.h3 "Planned Deployment"
                    Bulma.panel [
                        prop.children [
                            for resource in state.Resources do
                                Bulma.panelBlock.div [
                                    prop.children [
                                        makeRemoveIcon dispatch resource
                                        makeKindIcon resource
                                        let details = resource.Kind.Description |> Option.map (sprintf " (%s)") |> Option.defaultValue ""
                                        Html.text (resource.Name + details)
                                    ]
                                ]
                        ]
                    ]
                ]
            ]
        ]
        Bulma.button.span [
            Bulma.color.isPrimary
            Bulma.button.isFullWidth
            if List.isEmpty state.Resources then prop.disabled true
            else prop.onClick(fun _ -> dispatch Deploy)
            prop.children [
                Html.text "Deploy!"
            ]
        ]
    ]

    let drawDeploying (state:DeployRequest) = [
        Bulma.title.h3 "Deployment In Progress..."
        Bulma.progress [
            Bulma.color.isInfo
        ]
        Bulma.panel [
            prop.children [
                for resource in state.Resources do
                    Bulma.panelBlock.div [
                        prop.children [
                            makeKindIcon resource
                            let details = resource.Kind.Description |> Option.map (sprintf " (%s)") |> Option.defaultValue ""
                            Html.text (resource.Name + details)
                        ]
                    ]
            ]
        ]
    ]

let view model dispatch =
    Html.div [
        Bulma.navbar [
            color.isPrimary
            prop.children [
                Bulma.navbarItem.div [
                    Bulma.title.h3 [ Html.text "Farmer Deploy" ]
                ]
            ]
        ]
        Bulma.section [
            Bulma.container [
                prop.children [
                    Bulma.title.h2 "ARM Deploy Tool"
                    Bulma.subtitle.h4 "Point and click devops!"
                    match model.State with
                    | Editing state -> yield! View.drawEdit dispatch model state
                    | Deploying state -> yield! View.drawDeploying state
                    | DeployCompleted (Ok ()) ->
                        Bulma.title.h3 "Deployment Succeeded!"
                        Bulma.progress [ Bulma.color.isSuccess; prop.value 100; prop.max 100 ]
                    | DeployCompleted (Error error) ->
                        Bulma.title.h3 "Deployment Failed!"
                        Bulma.subtitle.h4 error
                        Bulma.progress [ Bulma.color.isDanger; prop.value 100; prop.max 100 ]
                ]
            ]
        ]
    ]


#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactSynchronous "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
