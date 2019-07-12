open Core_kernel
open Bap.Std
open Bap_plugins.Std

module Cfg = Graphs.Cfg
module Dis = Disasm_expert.Basic

let () =
  match Plugins.load () |> Result.all with
  | Ok plugins -> ()
  | Error (path, er) -> ()

let get_file () =
  try
    Some Sys.argv.(1)
  with _ ->  None

let get_addrs () =
  Array.foldi Sys.argv ~init:[] ~f:(fun i acc a ->
      if i > 1 then a :: acc
      else acc) |>
    function
    | [] -> eprintf "addresses weren't provided\n"; exit 1
    | xs ->
       List.rev_map xs ~f:(fun s ->
           let s =
             if String.is_suffix s ~suffix:":64u" then s
             else s ^ ":64u" in
           Addr.of_string s)


let () =
  match get_file () with
  | None -> eprintf "file not set!\n"
  | Some filename ->
    let input = Project.Input.file filename in
    match Project.create input with
    | Error _ -> eprintf "oops! something went wrong!\n"
    | Ok p ->
       let addrs = get_addrs () in
       let mem = Project.memory p in
       List.iter addrs ~f:(fun a ->
           let vs = Memmap.lookup mem a in
           Seq.find_map vs ~f:(fun (_,v) ->
               Value.get Image.symbol v) |>
             function
             | None -> printf "%a not found\n%!" Addr.ppo a
             | Some s -> printf "%s %a\n%!" s Addr.ppo a)
