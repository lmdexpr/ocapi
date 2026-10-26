(* Newest first; [Atomic] because requests run concurrently on their connections' fibers. *)
type store = Items_api.item list Atomic.t

let store () = Atomic.make []
let take n items = List.filteri (fun i _ -> i < n) items

let rec add store name =
  let items = Atomic.get store in
  let item = Items_api.{ id = Printf.sprintf "item-%d" (List.length items + 1); name } in
  if Atomic.compare_and_set store items (item :: items) then item else add store name

let routes store =
  Ocapi.Server.
    [
      route
        (module Items_api.Index)
        (fun { limit } ->
          let items = Atomic.get store |> List.rev in
          Option.fold limit ~none:items ~some:(fun n -> take n items));
      route
        (module Items_api.Get)
        (fun { id } ->
          Atomic.get store
          |> List.find_opt (fun (item : Items_api.item) -> item.id = id)
          |> Option.fold
               ~none:(Items_api.Get.Not_found { error = "not_found" })
               ~some:(fun item -> Items_api.Get.Found item));
      route (module Items_api.Create) (fun { item = { name } } -> Created (add store name));
    ]
