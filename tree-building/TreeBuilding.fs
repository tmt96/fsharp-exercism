// This is the file you need to modify for your own solution.
// The unit tests will use this code, and it will be used by the benchmark tests
// for the "Mine" row of the summary table.

// Remember to not only run the unit tests for this exercise, but also the
// benchmark tests using `dotnet run -c Release`.
// Please refer to the instructions for more information about the benchmark tests.

module TreeBuilding

open TreeBuildingTypes

type Tree =
    | Branch of int * Tree list
    | Leaf of int

let recordId t =
    match t with
    | Branch(id, c) -> id
    | Leaf id -> id

let isBranch t =
    match t with
    | Branch(id, c) -> true
    | Leaf id -> false

let children t =
    match t with
    | Branch(id, c) -> c
    | Leaf id -> []

let private validate (acc: (int * int) list) (record: Record): (int * int) list =
    let prevId =
        acc
        |> List.tryHead
        |> Option.map fst
        |> Option.defaultValue -1

    match record with
    | { RecordId = rid; ParentId = _ } when rid <> prevId + 1 -> failwith "Non-continuous list"
    | { RecordId = rid; ParentId = pid } when pid >= rid -> failwith "Nodes with invalid parents"
    | _ -> (record.RecordId, record.ParentId) :: acc

let processRecords acc =
    List.fold validate acc
    >> List.groupBy snd
    >> List.map (fun (x, y) ->
        (x,
         y
         |> List.map fst
         |> List.rev))
    >> Map.ofList

let buildTree records =
    if List.isEmpty records then failwith "Empty input"

    let records = List.sortBy (fun x -> x.RecordId) records
    match records with
    | [] -> failwith "Empty input"
    | { RecordId = 0; ParentId = 0 } :: rest ->
        let map = processRecords [ 0, -1 ] rest

        let rec helper key =
            match Map.tryFind key map with
            | Some children -> Branch(key, List.map helper children)
            | _ -> Leaf key

        helper 0
    | _ -> failwith "Root node is invalid"
