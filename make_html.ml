open Core_kernel

type check =
  | Forbidden
  | Unused
  | Recursive
  | Non_structured
  | Null
  | Complex [@@deriving sexp]



type result = {
    total   : int;
    false_p : int;
    confirm : int;
    unknown : int;
  }

type stat = check * result

let chop_suffix str suffix =
  match String.chop_suffix str ~suffix with
  | Some s -> s
  | None -> str

let check_of_string s =
  match chop_suffix s ":" with
  | "Forbidden" -> Some Forbidden
  | "Unused" -> Some Unused
  | "Complex" -> Some Complex
  | "Recursive" -> Some Recursive
  | "Non-structured" -> Some Non_structured
  | "Null" -> Some Null
  | _ -> None


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


let check_id arti check =
  sprintf "%s+%s" arti @@ Sexp.to_string (sexp_of_check check)

let map_checkname = function
  | Forbidden -> "Forbidden functions"
  | Unused ->  "Unused return value"
  | Complex -> "Functions with cyclomatic complexity > 50"
  | Recursive -> "Recursive functions"
  | Non_structured -> "Functions with non-structural cfg"
  | Null -> "Null pointer dereference"

let name_of_check arti check =
  let id = check_id arti check in
  sprintf "<b id=\"%s\">%s</b>" id (map_checkname check)

let make_overview sum =
  let digit x = sprintf "<td align=\"center\">%d</td>" x in
  let hdr = {|
  <table id="top" style="width:50%">
      <tr>
        <th>artifact</th>
        <th>check</th>
        <th>Total</th>
        <th>Confirmed</th>
        <th>False positive</th>
        <th>Unclassified</th>
     </tr>|} in
  let data = List.fold sum ~init:[] ~f:(fun acc (file, stat) ->
                 let stat = List.map ~f:(fun (x, y) -> file, x, y) stat in
                 acc @ stat) in
  let data = List.rev data in
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
        "</tr>" :: unkn :: fals :: conf :: totl :: check :: name :: "<tr>" :: acc) in
  List.rev ("</br>" :: "</table>" :: data)

let create_result total confirm false_p unknown =
  {total; confirm; false_p; unknown}

let add_summary doc r =
  sprintf
    "<pre>Total/Confirmed/False positive/Unclassified: %d/%d/%d/%d</pre>"
    r.total r.confirm r.false_p r.unknown :: doc

let make_summary check text =
  if String.is_prefix text ~prefix:"not found" then
    create_result 0 0 0 0
  else
    let words =
      String.split_on_chars ~on:[' '; '\n'] text |>
        List.filter ~f:(fun x -> x <> "") in
    let wc = List.length words in
    match check with
    | Null | Unused ->
       let wc = wc / 2 in
       create_result wc 0 0 wc
    | Forbidden -> create_result wc wc 0 0
    | _ -> create_result wc 0 0 wc

let parse_stat s =
  let words = String.split_on_chars s ~on:[':'; '/'] in
  let words =
    List.filter_map words
      ~f:(fun s ->
        let s = String.strip s in
        if s = "" then None
        else
          try Some (int_of_string s)
          with _ -> None) in
  match words with
  | [a;b;c;d] -> Some (create_result a b c d)
  | _ -> None

let close_previous_artifact = function
  | [] -> []
  | doc -> "</tr>" :: "</td>" :: doc

let start_artifact doc name =
  let doc = close_previous_artifact doc in
  "<td>" :: sprintf "<td><h3>%s</h3></td>" name :: "<tr>" :: doc

let filter_out_stat text =
  let is_stat = String.is_prefix ~prefix:"Stat:" in
  List.find text ~f:is_stat |> function
  | None -> text, None
  | Some line ->
     List.filter text ~f:(Fn.non is_stat), parse_stat line

let process_text doc check text =
  let text, sum = filter_out_stat text in
  let text = String.concat (List.rev text) ~sep:"\n" in
  let sum = match sum with
    | Some _ -> sum
    | None ->
       match check with
       | None -> None
       | Some check -> Some (make_summary check text) in
  let doc = match sum with
    | None -> doc
    | Some sum -> add_summary doc sum in
  sprintf "<pre>\n%s\n</pre>" text :: doc, sum

let is_artifact line = line = "Name:"

let ref_to_top = {|<p><a href="#top">Top</a></p>|}

let parse_file doc arti file =
  let lines = In_channel.with_file file ~f:In_channel.input_lines in
  let doc,check,text,stat =
  List.fold lines ~init:(ref_to_top :: doc, None, [], [])
    ~f:(fun ((doc,check,text,stat) as acc) line ->
      let line = String.strip line in
      if line = "" then acc
      else
      match check_of_string line with
      | None -> doc, check, line :: text, stat
      | Some name ->
         let doc,sum = process_text doc check text in
         let stat = match check,sum with
           | Some check, Some sum -> (check,sum) :: stat
           | _ -> stat in
         name_of_check arti name :: doc, Some name, [], stat) in
  let doc, sum = process_text doc check text in
  let doc = close_previous_artifact doc in
  let stat =
    match check, sum with
    | None, _ | _, None -> stat
    | Some check, Some sum -> (check, sum) :: stat in
  doc, stat

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

let () =
  let files = FileUtil.ls "." in
  let doc,sum =
    List.fold files ~init:([],[]) ~f:(fun (doc,sum) file ->
        let r = file / "results" in
        if file_exists r then
          let arti = Filename.basename file in
          let doc = start_artifact doc arti in
          let anchor = sprintf "<p id=\"%s\"> </p>" arti in
          let doc, stat = parse_file (anchor :: doc) arti r in
          doc, (arti, stat) :: sum
        else doc, sum) in
  let doc = List.rev doc in
  let sum = make_overview sum in
  Out_channel.with_file "results.html" ~f:(fun ch ->
      Out_channel.output_string ch html_header;
      Out_channel.output_lines ch sum;
      Out_channel.output_string ch start_table;
      Out_channel.output_lines ch doc;
      Out_channel.output_string ch "</table>";
      Out_channel.output_string ch html_finish)



(* TODO:
refactoring needed
add a keywords type
add a _silent keyword

*)


(*  Timing:
   httpd Null 1.36

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
