open Core_kernel

type check =
  | Unused
  | Null
  | Forbidden
  | Complex
  | Non_structural
  | Recursive
[@@deriving bin_io, compare, sexp]

type info =
  | Time of string
  | Total of int
  | Confirmed of int
  | False_pos of int
  | Text of string list
[@@deriving sexp]

type stat = {
    total   : int;
    false_p : int;
    confirm : int;
    unknown : int;
    time    : string;
  }

let html_header =  {|
<!DOCTYPE html>
<html>
  <head>
  <style>
   table, th, td {
     border: 1px solid black;
     border-collapse: collapse;
   }

   table#top tr:nth-child(even) {
     background-color: #f2f2f2;
   }
  </style>
  </head>
  <body>
|}

let html_finish = {|
  </body>
</html>
|}

let create_stat total confirm false_p time=
  let unknown = total - confirm - false_p in
  {total; false_p; confirm; unknown; time}

let no_incidents = function
  | ["not found"] -> true
  | _ -> false

module Parse = struct

  let parse_check name =
    try
      Some (check_of_sexp (Sexp.of_string name))
    with _ ->
          if name = "Non-structured" then Some Non_structural
          else None

  let int_of_str x =
    try
      Some (int_of_string (String.strip x))
    with _ -> None

  let with_num x f =
    Option.value_map ~default:None (int_of_str x)
      ~f:(fun x -> Some (f x))

  let check_of_string s =
    match String.split ~on:':' s with
    | s' :: _ when String.(s = s') -> None
    | name :: _ -> parse_check name
    | _ -> None

  let info_of_string s =
    match String.split ~on:':' s with
    | [_; ""] -> None
    | ["Total"; x] -> with_num x (fun x -> Total x)
    | ["Confirmed"; x] -> with_num x (fun x -> Confirmed x)
    | ["False_pos"; x] -> with_num x (fun x -> False_pos x)
    | "Time" :: xs ->
       let tm = String.concat xs ~sep:":" |> String.strip in
       Some (Time tm)
    | _ -> None

  let normalize data =
    List.fold data ~init:[] ~f:(fun acc (check, infos) ->
        let infos = List.fold infos ~init:[]
          ~f:(fun acc -> function
              | Text text -> Text (List.rev text) :: acc
              | info -> info :: acc) in
        (check, infos) :: acc)

  let process file =
    let lines = In_channel.with_file file ~f:In_channel.input_lines in
    let add_check data infos = function
      | None -> data
      | Some name -> (name, infos) :: data in
    let rec loop data check acc = function
      | [] ->
         add_check data acc check
      | line :: lines ->
         let line = String.strip line in
         if line = "" then loop data check acc lines
         else if String.get line 0 = '#' then loop data check acc lines
         else
           match check_of_string line with
           | Some check' ->
              let data = add_check data acc check in
              loop data (Some check') [] lines
           | None ->
              match info_of_string line with
              | Some info ->
                 loop data check (info :: acc) lines
              | None ->
                 let acc = match acc with
                   | Text text :: acc -> Text (line :: text) :: acc
                   | _ -> Text [line] :: acc in
                 loop data check acc lines in
    loop [] None [] lines |> normalize

end

module Template = struct

  let map_checkname = function
    | Forbidden -> "Forbidden functions"
    | Unused ->  "Unused return value"
    | Complex -> "Functions with cyclomatic complexity > 50"
    | Recursive -> "Recursive functions"
    | Non_structural -> "Functions with non-structural cfg"
    | Null -> "Null pointer dereference"

  let ref_to_top = {|<p><a href="#top">Top</a></p>|}

  let check_id arti check =
    sprintf "%s+%s" arti @@ Sexp.to_string (sexp_of_check check)

  let render_check arti check =
    let id = check_id arti check in
    sprintf "<b id=\"%s\">%s</b>" id (map_checkname check)

  let render_text text =
    let text = if no_incidents text then "no incidents found"
               else String.concat text ~sep:"\n" in
    sprintf "<pre>\n%s\n</pre>" text

  let find_text infos =
    let rec loop = function
      | [] -> None
      | Text t :: _ -> Some t
      | _ :: infos -> loop infos in
    loop infos

  let render_stat infos stat =
    let render =
      sprintf
        "<pre>Total/Confirmed/False positive/Unclassified: %d/%d/%d/%d</pre>"
        stat.total stat.confirm stat.false_p stat.unknown in
    if stat.total <> 0 then render
    else
      match find_text infos with
         | Some text when no_incidents text -> ""
         | _ -> render

  let render_check arti check stat infos =
    let doc =
      List.fold infos ~init:[] ~f:(fun doc -> function
          | Text text -> render_text text :: doc
          | Time time -> sprintf "<pre>Time: %s</pre>" time :: doc
          | _ -> doc) in
    render_check arti check :: render_stat infos stat :: List.rev doc

  let get_stat check stat =
    List.find stat ~f:(fun (check',s) -> check = check') |> function
    | None -> create_stat 0 0 0 ""
    | Some (_,s) -> s

  let render_artifact name data stat =
    let init = [
        "<tr>";
        sprintf "<td><h3>%s</h3></td>" name;
        "<td>";
        sprintf "<p id=\"%s\"> </p>" name;
        ref_to_top;
      ] in
    let doc =
      List.fold data ~init ~f:(fun  doc (check, infos) ->
          let stat = get_stat check stat in
          let doc' = render_check name check stat infos in
          doc @ doc') in
    doc @ ["</tr>"; "</td>"]

  let render_summary sum =
    let cell x = sprintf "<td align=\"center\">%s</td>" x in
    let digit x = cell (string_of_int x) in
    let hdr = {|
               <table id="top" style="width:50%">
               <tr>
               <th>artifact</th>
               <th>check</th>
               <th>Total</th>
               <th>Confirmed</th>
               <th>False positive</th>
               <th>Unclassified</th>
               <th>Time</th>
               </tr>|} in
    let data = List.fold sum ~init:[] ~f:(fun acc (file, stat) ->
                   let stat = List.map ~f:(fun (x, y) -> file, x, y) stat in
                   acc @ stat) in
    let data =
      List.fold data ~init:[hdr] ~f:(fun acc (arti, check, res) ->
          let id = check_id arti check in
          let name  =
            sprintf "<td><a href=\"#%s\">%s</a></td>" arti arti in
          let check =
            sprintf "<td><a href=\"#%s\">%s</a></td>"
              id (map_checkname check) in
          let totl = digit res.total in
          let conf = digit res.confirm in
          let fals = digit res.false_p in
          let unkn = digit res.unknown in
          let time = cell  res.time in
          "</tr>" :: time :: unkn :: fals :: conf :: totl :: check :: name :: "<tr>" :: acc) in
    List.rev ("</br>" :: "</table>" :: data)
end

module Stat = struct
  let words_count text =
    List.fold text ~init:0 ~f:(fun num line ->
        num +
          (String.split_on_chars ~on:[' '; '\n'] line |>
             List.filter ~f:(fun s -> s <> "") |>
             List.length))

  let infer_total check infos =
    let rec loop = function
      | [] -> None
      | Text text :: _  -> Some (words_count text)
      | _ :: infos -> loop infos in
    match loop infos with
    | None -> None
    | Some num ->
       match check with
       | Null | Unused -> Some (num / 2)
       | _ -> Some num

  let find_time infos =
    let rec loop = function
      | [] -> "-"
      | Time t :: _ -> t
      | _ :: xs -> loop xs in
    loop infos

  let infer_stat time = function
    | None, Some cnf, Some fp -> create_stat (fp + cnf) cnf fp time
    | Some total, None, None -> create_stat total 0 0 time
    | Some total, Some cnf, None -> create_stat total cnf 0 time
    | Some total, None, Some fp -> create_stat total 0 fp time
    | _ -> create_stat 0 0 0 time

  let get_stat check infos =
    let rec loop ((total,confirmed,false_pos) as r) = function
      | Total x :: infos -> loop (Some x,confirmed,false_pos) infos
      | False_pos x :: infos -> loop (total,confirmed,Some x) infos
      | Confirmed x :: infos -> loop (total,Some x,false_pos) infos
      | Text text :: _ when no_incidents text -> Some 0, Some 0, Some 0
      | _ :: infos -> loop r infos
      | [] -> r in
    let time = find_time infos in
    let stat =
      match loop (None, None, None) infos with
      | (Some total, _, _) as r -> infer_stat time r
      | None, cnf, fp -> infer_stat time (infer_total check infos,cnf,fp) in
    match check with
    | Forbidden -> {stat with unknown = 0; confirm = stat.total}
    | _ -> stat

  let of_data data =
    List.fold data ~init:[]
      ~f:(fun acc (check,infos) -> (check, get_stat check infos) :: acc)
    |> List.rev
end


let (/) = Filename.concat

let file_exists file =
  try
    FileUtil.(test Exists file)
  with _ -> false


let start_table = {|
     <table id="Results" style="width:100%" frame=void rules=rows >
       <tr>
         <th></th>
         <th></th>
       </tr>
|}

let debug_print_data = function
  | [] -> printf "EMPTY!\n"
  | data ->
  List.iter data ~f:(fun (check, infos) ->
      printf "%s\n" (Sexp.to_string (sexp_of_check check));
      List.iter infos ~f:(function
          | Time s -> printf "  Time %s\n" s
          | Total x -> printf "  Total %d\n" x
          | Confirmed x -> printf "  Confirmed %d\n" x
          | False_pos x -> printf "  False_pos %d\n" x
          | Text text ->
             printf "  Text: \n";
             List.iter text ~f:(printf "   %s\n")))

let () =
  let files = FileUtil.ls "." in
  let docs,sum =
    List.fold files ~init:([],[]) ~f:(fun (docs,sum) dir ->
        let r = dir / "results" in
        if file_exists r then
          let arti = Filename.basename dir in
          let data = Parse.process r in
          let stat = Stat.of_data data in
          let doc  = Template.render_artifact arti data stat in
          doc::docs, (arti, stat) :: sum
        else docs, sum) in
  let sum = Template.render_summary (List.rev sum) in
  Out_channel.with_file "results.html" ~f:(fun ch ->
      Out_channel.output_string ch html_header;
      Out_channel.output_lines ch sum;
      Out_channel.output_string ch start_table;
      List.iter (List.rev docs) ~f:(Out_channel.output_lines ch);
      Out_channel.output_string ch "</table>";
      Out_channel.output_string ch html_finish)


(*  Timing null-ptr-deref:
    httpd  01:36:00
    samba 00:00:40
    lighttpd  00:15:12
    openssl  00:46:44
    ntpd   01:35:31
    nginx  04:47:01
    smtpd  01:08:20
    ntpdc 00:25:22
    sshd 01:53:16
    swfcombine 00:18:13


defective-symbol
httpd-2.4.18 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:30.16
libbfd-2.31.1 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:47.97
lighttpd-1.4.15 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:08.53
nginx-1.7 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:29.26
ntpd-4.2.8p5 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:26.63
ntpdc-4.2.8p5 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:09.86
openssl-1.1.0 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:18.33
samba-4.7.6 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:03.73
smtpd-5.7.3p2 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:18.93
sqlite3-2.27.2 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:47.28
sshd-7.3.p1 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:29.46
swfc-0.9.2 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:38.53
swfcombine-0.9.2 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:06.31
swfextract-0.9.2 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:23.55
tshark-2.6.0 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:11.46
wav2swf-0.9.2 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:07.71
wpa_cli-2.2 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:05.37
wpa_supplicant-2.2 Elapsed (wall clock) time (h:mm:ss or m:ss): 0:36.91



sizes:
 873K  httpd-2.4.18
 6.0M  libbfd-2.31.1
 770K  lighttpd-1.4.15
 859K  nginx-1.7
 3.1M  ntpd-4.2.8p5
 1.1M  ntpdc-4.2.8p5
 745K  openssl-1.1.0
 46K   samba-4.7.6
 2.2M  smtpd-5.7.3p2
 1.1M  sqlite3-2.27.2
 3.0M  sshd-7.3.p1
 1.4M  swfc-0.9.2
 113K  swfcombine-0.9.2
 1.1M  swfextract-0.9.2
 1.6M  tshark-2.6.0
 241K  wav2swf-0.9.2
 114K  wpa_cli-2.2
 934K  wpa_supplicant-2.2

*)
