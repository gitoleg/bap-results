open Core_kernel



let chop_prefix ~p s =
  match String.chop_prefix s ~prefix:p with
  | None -> s
  | Some x -> x

exception Command_failed of Unix.process_status

let cmd fmt =
  let run c =
    try
      let inp = Unix.open_process_in c in
      let res = In_channel.input_all inp in
      match Unix.close_process_in inp with
      | Unix.WEXITED 0 -> Some res
      | s -> raise (Command_failed s)
    with e ->
      eprintf "command '%s' failed: %s\n" c (Exn.to_string e);
      None in
  ksprintf run fmt

let files' = ["httpd-2.4.18"; "libbfd-2.31.1"; "lighttpd-1.4.15"]

let files = match cmd "find . -name results" with
  | None -> []
  | Some rs -> String.split ~on:'\n' rs

let is_interesting f = not (List.mem files' f ~equal:String.equal)
let arti file = Filename.dirname file |> chop_prefix ~p:"./"

let files = List.filter files ~f:(fun f -> is_interesting (arti f))

let split_on_three file line =
  String.split ~on:' ' line |>
    List.map ~f:String.strip |>
    List.filter ~f:(fun x -> x <> "") |>
    function
    | [_func; addr; "Confirmed"] -> Some [file; "MUST"; "unused-return-value"; addr;]
    | [_func; addr; "False_pos"] -> Some [file; "MAY"; "unused-return-value"; addr;]
    | _ -> None


let parse file =
  let isp ~p s = String.is_prefix ~prefix:p s in
  let dir = arti file in
  let rec read start acc = function
    | [] -> acc
    | line :: lines when isp line ~p:"Unused:" -> read true acc lines
    | _ :: lines when not start -> read start acc lines
    | line :: lines when isp line ~p:"Time" -> read start acc lines
    | line :: lines when line = "" -> read false acc lines
    | line :: lines ->
       match split_on_three dir line with
       | Some x -> read start (x :: acc) lines
       | None -> read start acc lines in
  let lines = In_channel.with_file file ~f:In_channel.input_lines in
  let lines = List.map ~f:String.strip lines in
  read false [] lines

let () = List.iter files ~f:(printf "%s\n")

let results = List.fold ~init:[] files ~f:(fun acc file ->
                  let res = parse file in
                  let res = printf "\n" :: res in
                  acc @ res)
