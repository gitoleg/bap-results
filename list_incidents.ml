open Core_kernel


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

let () =
  if Array.length Sys.argv < 2 then
    let () = eprintf "file expected!\n" in
    exit 1;
  else
  let file = Sys.argv.(1)  in
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

  Map.iteri acc ~f:(fun ~key:name ~data:locs  ->
      printf "%s: %d cases \n" name (Map.length locs);
      Map.iteri ~f:(fun ~key:addr ~data:trace -> printf "%s " trace) locs;
      printf "\n\n");

  (* Map.iteri acc ~f:(fun ~key:name ~data:locs  ->
   *     printf "%s:  %d incidents\n" name (Map.length locs)); *)
  In_channel.close ch
