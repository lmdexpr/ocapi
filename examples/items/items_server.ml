(* Newest first; [Atomic] because requests run concurrently on their connections' fibers. *)
type store = Items_api.item list Atomic.t

let store () = Atomic.make []
let take n items = List.filteri (fun i _ -> i < n) items

let rec add store name =
  let items = Atomic.get store in
  let item = Items_api.{ id = Printf.sprintf "item-%d" (List.length items + 1); name } in
  if Atomic.compare_and_set store items (item :: items) then item else add store name

let routes store =
  [
    Items_api.Index.endpoint
    |> Ocapi.Server.handle (fun { Items_api.Index.limit } ->
      let items = Atomic.get store |> List.rev in
      Option.fold limit ~none:items ~some:(fun n -> take n items));
    Items_api.Get.endpoint
    |> Ocapi.Server.handle (fun { Items_api.Get.id } ->
      Atomic.get store
      |> List.find_opt (fun (item : Items_api.item) -> item.id = id)
      |> Option.to_result ~none:"not_found");
    Items_api.Create.endpoint
    |> Ocapi.Server.handle (fun { Items_api.Create.name } -> add store name);
  ]
