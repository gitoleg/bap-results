open Core_kernel
open Bap.Std
include Self ()

module Parse = struct

  open Sexp

  let point_of_sexp x = match x with
    | List _ -> None
    | Atom x ->
       match String.split ~on:':' x  with
       | [_; x] -> Some x
       | _ -> None

  let trace_of_sexps xs =
    List.filter_map ~f:point_of_sexp xs

  let locs_of_sexps xs =
    List.filter_map xs ~f:(function
        | Atom s -> Some s
        | _ -> None)

  let get_location = function
    | List (Atom "incident-location" :: List [Atom loc_id; List points] :: _)  ->
       (match trace_of_sexps points with
        | addr :: _ -> Some (loc_id, addr)
        | _ -> None)
    | _ -> None

  let get_incident = function
    | List (Atom "incident" :: List (Atom name :: locs) :: _) ->
       Some (name, locs_of_sexps locs)
    | _ -> None

end

let read ch =
  try
    Sexp.input_sexp ch |> Option.some
  with _ -> None

let main file traces_only addrs_only =
  let ch = In_channel.create file in
  let rec loop acc loc =
    match read ch with
    | None -> acc
    | Some s ->
       match loc with
       | None -> loop acc (Parse.get_location s)
       | Some (trace,addr) ->
          match Parse.get_incident s with
          | None -> loop acc None
          | Some (name,_) ->
             let acc = Map.update acc name ~f:(function
                           | None -> Map.singleton (module String) addr trace
                           | Some addrs -> Map.set addrs addr trace) in
             loop acc None in
  let acc = loop (Map.empty (module String)) None in

  if traces_only then
    Map.iteri acc ~f:(fun ~key:name ~data:locs  ->
        Map.iteri ~f:(fun ~key:addr ~data:trace -> printf "%s " trace) locs;
        printf "\n\n")
  else if addrs_only then
    Map.iteri acc ~f:(fun ~key:name ~data:locs  ->
        Map.iteri ~f:(fun ~key:addr ~data:trace -> printf "0x%s " addr) locs;
        printf "\n\n")
  else
    Map.iteri acc ~f:(fun ~key:name ~data:locs  ->
        printf "%s:  %d incidents\n" name (Map.length locs));
  In_channel.close ch


open Cmdliner

let filename : string Term.t =
  let doc = "Input filename." in
  Arg.(required & pos 0 (some non_dir_file) None &
       info [] ~doc ~docv:"FILE")

let traces_only : bool Term.t =
  let doc = "Print only trace numbers" in
  Arg.(value & flag & info ["traces-only"] ~doc)

let addrs_only : bool Term.t =
  let doc = "Print only addresses" in
  Arg.(value & flag & info ["addrs-only"] ~doc)



let () =
  let x = Term.(const main $(filename) $(traces_only) $(addrs_only)) in
  let info = Term.info "list_incidents" ~version:Config.version ~doc~man:[] in
  match Cmdliner.Term.eval ~argv ~catch:false (x,info) with
  | `Ok x -> ()
  | _ -> ()
