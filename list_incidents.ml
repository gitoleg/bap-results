open Core_kernel
open Bap.Std
include Self ()


let incident_of_string line =
  let open Sexp in
  try
    match of_string line with
    | Atom _ -> None
    | List xs ->
       match xs with
       | Atom "incident" :: List xs :: _  ->
          (match xs with
           | Atom name :: _ -> Some name
           | _ -> None)
       | _ -> None
  with _ -> None

let incident_location_of_string line =
  let open Sexp in
  try
    match of_string line with
    | Atom _ -> None
    | List xs ->
       match xs with
       | Atom "incident-location" :: List xs :: _  ->
          (match xs with
           | _ :: List xs :: _ ->
              (match xs with
               | Atom addr :: _ ->
                  (match String.split addr ~on:':' with
                  | trace :: addr :: _ -> Some (trace,addr)
                  | _ -> None)
               | _ -> None)
           | _ -> None)
       | _ -> None
  with _ -> None

let rec read ?(prev="") ch =
  let line = In_channel.input_line ch in
  match line with
  | None -> None
  | Some line ->
     let line = prev ^ line in
     let opened = String.count line ~f:(fun c -> c = '(') in
     let closed = String.count line ~f:(fun c -> c = ')') in
     if opened <> closed then read ~prev:line ch
     else Some line

let main file traces_only addrs_only =
  let ch = In_channel.create file in
  let rec loop acc loc =
    match read ch with
    | None -> acc
    | Some line ->
       match loc with
       | None -> loop acc (incident_location_of_string line)
       | Some (trace,addr) ->
          match incident_of_string line with
          | None -> loop acc None
          | Some name ->
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
