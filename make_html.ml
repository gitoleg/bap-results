open Core_kernel

type check =
  | Test of check
  | Unused
  | Null
  | Forbidden
  | Complex
  | Non_structural
  | Recursive
  | Hardcoded_socket_address
  | Memcheck_double_release
  | Memcheck_out_of_bound
  | Memcheck_use_after_release
  | Value_was_used_before_check
  | Untrusted_argument
[@@deriving bin_io, compare, sexp]

type name = string [@@deriving sexp]
type addr = string [@@deriving sexp]

type status =
  | Confirmed
  | False_pos
  | False_neg
  | Undecided
[@@deriving sexp]

type info =
  | Size of string
  | Time of string
  | Data of (string list * status) list
  | Stat of int * int * int * int (* total/confirned/false_pos/false_neg *)
[@@deriving sexp]

type stat = {
    false_pos : int;
    false_neg : int;
    confirmed : int;
    undecided : int;
    total     : int;
    time      : string;
  }

type result = check * stat * info list

module Check = struct
  type t = check [@@deriving bin_io,compare, sexp]
  let hash = Hashtbl.hash
end

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

   table#data tr {
     background-color: #F2F2F2;
   }

   table#data td {
     border: 0;
   }

   start-line {
     width: 100%;
     display: table;
     border: 1px solid #444444;
   }

   .line-elt {
     display: table-cell;
   }

  </style>
  </head>
  <body>
|}

let html_finish = {|
  </body>
</html>
|}

let descr_of_check = function
  | Test Null -> `Link "https://cwe.mitre.org/data/definitions/476.html"
  | Test Unused -> `Link "https://cwe.mitre.org/data/definitions/252.html"
  | Unused -> `Link "https://raw.githubusercontent.com/BinaryAnalysisPlatform/bap-toolkit/master/jpl-rule-14/descr"
  | Null ->  `Link "https://raw.githubusercontent.com/BinaryAnalysisPlatform/bap-toolkit/master/av-rule-174/descr"
  | Complex -> `Link "https://raw.githubusercontent.com/BinaryAnalysisPlatform/bap-toolkit/master/av-rule-3/descr"
  | Non_structural -> `Link "https://raw.githubusercontent.com/BinaryAnalysisPlatform/bap-toolkit/master/av-rule-189/descr"
  | Recursive  -> `Link "https://raw.githubusercontent.com/BinaryAnalysisPlatform/bap-toolkit/master/jpl-rule-4/descr"
  | Memcheck_use_after_release -> `Link "https://cwe.mitre.org/data/definitions/416.html"
  | Memcheck_double_release -> `Link "https://cwe.mitre.org/data/definitions/415.html"
  | Forbidden -> `Link "https://raw.githubusercontent.com/BinaryAnalysisPlatform/bap-toolkit/master/forbidden-symbol/descr"
  | _ -> `Absent

let get_descr check =
  match descr_of_check check with
  | `Absent -> ""
  | `Link s -> s


let words_of_line x =
  String.split ~on:' ' x |>
  List.filter ~f:(fun y -> y <> "")


module Parse = struct

  let parse_check name data =
    try
      let name = String.map ~f:(fun c -> if c = '-' then '_' else c) name in
      let name = String.capitalize name in
      match data with
      | "" -> Some (check_of_sexp (Sexp.of_string name))
      | s -> Some (check_of_sexp (Sexp.(List [Atom name; Atom data])))
    with _ -> None

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
    | name :: data -> parse_check name (String.(strip (concat data)))
    | _ -> None


  let int_of_str x =
    try
      Some (int_of_string (String.strip x))
    with _ -> None

  let parse_stat str =
    match String.split str ~on:'/' with
    | [total;confirmed;false_pos;false_neg] ->
       Option.(
        int_of_str total >>= fun total ->
        int_of_str confirmed >>= fun confirmed ->
        int_of_str false_pos >>= fun false_pos ->
        int_of_str false_neg >>= fun false_neg ->
        Some (Stat (total,confirmed,false_pos,false_neg)))
    | _ -> None

  let info_of_string s =
    match String.split ~on:':' s with
    | "Time" :: xs ->
       let tm = String.concat xs ~sep:":" |> String.strip in
       Some (Time tm)
    | "Size" :: s :: _ -> Some (Size s)
    | "Stat" :: data -> parse_stat (String.concat data)
    | _ -> None

  let parse_plain_line line =
    let words = words_of_line line in
    match List.rev words with
    | [] -> [], Undecided
    | hd :: words' ->
       try
         let s = status_of_sexp (Sexp.of_string hd) in
         List.rev words', s
       with _ -> words, Undecided

  let find_time infos =
    let rec loop = function
      | [] -> "-"
      | Time t :: _ -> t
      | _ :: xs -> loop xs in
    loop infos

  let check_status s (_, s') = s = s'

  let is_false_pos = check_status False_pos
  let is_false_neg = check_status False_neg
  let is_confirmed = check_status Confirmed
  let is_undecided = check_status Undecided

  let stub_stat time = {
      total = 0; false_pos = 0; false_neg = 0;
      confirmed = 0; undecided = 0; time
    }

  let create_stat time data =
    match data with
    | [ ["not found"],_; ] -> stub_stat time
    | _ -> {
      total = List.length data;
      false_pos = List.count data ~f:is_false_pos;
      false_neg = List.count data ~f:is_false_neg;
      confirmed = List.count data ~f:is_confirmed;
      undecided = List.count data ~f:is_undecided;
      time;
    }

  let find_stat data =
    List.find_map data ~f:(function
        | Stat (a,b,c,d) -> Some (a,b,c,d)
        | _ -> None)

  let normalize_info info =
    List.rev_map info ~f:(function
        | Data d -> Data (List.rev d)
        | x -> x)

  let normalize xs =
    List.rev_map xs ~f:(fun (check, info) ->
        let info = normalize_info info in
        let time = find_time info in
        match
        List.find_map info ~f:(function
           | Data d -> Some (create_stat time d)
           | _ -> None) with
        | Some s -> check, s, info
        | None  ->
           match find_stat info with
           | None -> check, stub_stat time, info
           | Some (total, confirmed, false_pos, false_neg) ->
              let stat = {
                total; false_pos; false_neg;
                confirmed;
                undecided = total - confirmed - false_pos - false_neg;
                time;} in
              check,stat,info)

  let process file : string option * result list =
    let lines = In_channel.with_file file ~f:In_channel.input_lines in
    let add_check data infos = function
      | None -> data
      | Some name -> (name, infos) :: data in
    let rec loop ((size,data) as data') check acc = function
      | [] -> size, add_check data acc check
      | line :: lines ->
         let line = String.strip line in
         if line = "" then loop data' check acc lines
         else if String.get line 0 = '#' then loop data' check acc lines
         else
           match check_of_string line with
           | Some check' ->
              let data = add_check data acc check in
              loop (size,data) (Some check') [] lines
           | None ->
              match info_of_string line with
              | Some (Size s) -> loop (Some s,data) check acc lines
              | Some info -> loop (size,data) check (info :: acc) lines
              | None ->
                 let res = parse_plain_line line in
                 let acc = match acc with
                   | Data data :: acc -> Data (res :: data) :: acc
                   | _ -> Data [res] :: acc in
                 loop (size,data) check acc lines in
    let size, data = loop (None,[]) None [] lines in
    size, normalize data

end

module Template = struct

  let color_of_status = function
    | Confirmed -> "#D4EFE2"
    | False_neg -> "#F7D3CE"
    | False_pos -> "#add8e6"
    | Undecided -> "#F2F2F2"

  let stub_color = "#FFFFFF"

  let legend = sprintf {|
    <table>
      <tr>
      <td bgcolor=%s>&nbsp&nbsp&nbsp&nbsp&nbsp</td>
      <td bgcolor="#f2f2f2"> confirmed </td>
      </tr>
      <tr>
      <td bgcolor=%s></td>
      <td bgcolor="#f2f2f2"> false negative </td>
      </tr>
      <tr>
      <td bgcolor=%s></td>
      <td bgcolor="#f2f2f2"> false positive </td>
      </tr>
      <tr>
      <td bgcolor=%s></td>
      <td bgcolor="#f2f2f2"> undecided </td>
      </tr>
    </table></br>|}
      (color_of_status Confirmed)
      (color_of_status False_neg)
      (color_of_status False_pos)
      (color_of_status Undecided)


  let rec string_of_check = function
    | Test c -> sprintf  "Testcase: %s" (string_of_check c)
    | Forbidden -> "Forbidden functions"
    | Unused ->  "Unused return value"
    | Complex -> "Functions with cyclomatic complexity > 50"
    | Recursive -> "Recursive functions"
    | Non_structural -> "Functions with non-structural cfg"
    | Null -> "Null pointer dereference"
    | Hardcoded_socket_address -> "Hardcoded socket address"
    | Memcheck_double_release -> "Memory check: double free"
    | Memcheck_out_of_bound -> "Memory check: out of bound"
    | Memcheck_use_after_release -> "Use after free"
    | Value_was_used_before_check -> "Value was used before check"
    | Untrusted_argument -> "Untrusted argument"

  let ref_to_top = {|<p><a href="#top">Top</a></p>|}

  let check_id arti check =
    sprintf "%s+%s" arti @@ Sexp.to_string (sexp_of_check check)

  let render_checkname arti check =
    let id = check_id arti check in
    sprintf "<b id=\"%s\">%s</b>" id (string_of_check check)

  let is_no_incidents stat = stat.total = 0

  let find_check_text infos =
    let rec loop = function
      | [] -> None
      | Data d :: _ -> Some d
      | _ :: infos -> loop infos in
    loop infos

  let render_text text = match text with
    | [] -> ""
    | text ->
       let text = List.map text ~f:fst in
       let text = List.concat text in
       let text = String.concat text ~sep:"\n" in
       sprintf "<pre>\n%s\n</pre>" text

  let compare_strings (data,_) (data',_) =
    let rec compare xs ys =
      match xs, ys with
      | [],[] -> 0
      | x :: xs, y :: ys ->
         let r = String.compare x y in
         if r = 0 then compare xs ys
         else r
      | _ -> assert false in
    compare data data'

  let render_table ?(max_cols = 6) data =
    let make_cell ?align color word =
      let align = match align with
        | None -> ""
        | Some align -> sprintf " align=\"%s\"" align  in
      sprintf "<td%s bgcolor=\"%s\">&nbsp%s&nbsp</td>" align color word in
    let render_row (words,status) =
      match words with
      | [] -> ""
      | fst :: others ->
         let color = color_of_status status in
         let row =
           List.fold others ~init:([make_cell color fst; "<tr>"])
             ~f:(fun acc w -> make_cell ~align:"right" color w :: acc) in
         let row = List.rev ("</tr>" :: row) in
         String.concat row in
    match data with
    | [] -> ""
    | (fst,s) :: _  ->
       let cols = List.length fst in
       if not @@ List.for_all data  ~f:(fun (x,_) -> List.length x = cols) then
         render_text data
       else
         let data = List.sort data ~compare:compare_strings in
         let data = List.map data ~f:render_row in
         let empty_tab = ["<table id=\"data\">"; "<div class=\"line-elt\">"] in
         let space_elt = "<div class=\"line-elt\">&nbsp&nbsp</div>" in
         let close_tab tab =
           List.rev (space_elt :: "</div>" :: "</table>" :: tab) |> String.concat in
         let add_row tab row = row :: tab in
         let len = List.length data in
         let cols = match len with
           | n when n < 10 -> 1
           | n when n < 30 -> 2
           | n when n < 60 -> 3
           | _  -> max_cols in
         let rows = len / cols  in
         let rows = if len - rows * cols = 1 then len / (cols + 1)
                    else rows in
         let acc, tab, _ =
           List.fold data ~init:(["<div class=\"start-line\">"],empty_tab,0) ~f:(fun (acc,tab,i) ws ->
               let tab = add_row tab ws in
               if i + 1 < rows then acc, tab, i + 1
               else close_tab tab :: acc, empty_tab, 0) in
         let acc = "</div>" :: close_tab tab :: acc in
         List.rev acc |> String.concat

  let render_data ?max_cols stat data =
    if is_no_incidents stat then
      "<table><tr><td>no incidents found</td></tr></table>"
    else render_table ?max_cols data

  let render_stat infos stat =
    let render =
      sprintf
        "<pre>Total/Confirmed/False positive/False negative/Unclassified: %d/%d/%d/%d/%d</pre>"
        stat.total stat.confirmed stat.false_pos stat.false_neg stat.undecided in
    if stat.total <> 0 then render
    else
      match find_check_text infos with
      | Some d when stat.total = 0 -> ""
      | _ -> render

  let render_check arti check stat infos =
    let doc =
      List.fold infos ~init:["<div>"] ~f:(fun doc info ->
          match info, check with
          | Time time, _ -> sprintf "<pre>Time: %s</pre>" time :: doc
          | Data d, Test _ -> render_data ~max_cols:4 stat d :: doc
          | Data d, _  -> render_data stat d :: doc
          | _ -> doc) in
    "</div>"  ::
    render_checkname arti check :: render_stat infos stat :: List.rev doc

  let render_artifact name size (data : result list) =
    let size = match size with
      | None -> ""
      | Some s -> sprintf "size: %s" s in
    let init = [
        "<tr>";
        sprintf "<td><pre><h3>%s</h3>%s</pre></td>" name size;
        "<td>";
        sprintf "<p id=\"%s\"> </p>" name;
        ref_to_top;
        legend;
      ] in
    let doc =
      List.fold data ~init ~f:(fun  doc (check, stat, infos) ->
          let doc' = render_check name check stat infos in
          doc @ "</br>" :: doc') in
    doc @ ["</tr>"; "</td>"]

  let hover_check arti check =
    let descr = get_descr check in
    let name = string_of_check check in
    sprintf "<td><a href=\"%s\">%s</a></td>" descr name

  let render_summary sum =
    let cell x = sprintf "<td align=\"center\">%s</td>" x in
    let digit x = cell (string_of_int x) in
    let time = sprintf "<pre>Time\nhh:mm:ss</pre>" in
    let hdr = {|
               <table id="top" style="width:50%">
               <tr>
               <th>artifact</th>
               <th>check</th>
               <th>Total</th>
               <th>Confirmed</th>
               <th>False positive</th>
               <th>False negative</th>
               <th>Unclassified</th>|} in
    let hdr = sprintf "%s<th>%s</th></tr>" hdr time in
    let data =
      List.fold sum ~init:[hdr] ~f:(fun acc (arti, size, data) ->
          let size = match size with
            | None -> ""
            | Some s -> sprintf "\nsize: %s" s in
          let name  =
            sprintf "<td bgcolor=\"white\" rowspan=\"%d\" >
                         <pre><a href=\"#%s\">%s</a>%s</pre>
                     </td>"
              (List.length data) arti arti size in
          fst @@
            List.fold data ~init:(name :: "<tr>" :: acc, true) ~f:
              (fun (acc,has_tr) (check, res,_) ->
                let check = hover_check arti check in
                let totl = digit res.total in
                let conf = digit res.confirmed in
                let falp = digit res.false_pos in
                let faln = digit res.false_neg in
                let unkn = digit res.undecided in
                let time = cell  res.time in
                let tr = if has_tr then "" else "<tr>" in
                "</tr>" :: time :: unkn :: faln :: falp :: conf :: totl :: check
                :: tr :: acc, false)) in
    List.rev ("</br>" :: "</table>" :: data)

end


let (/) = Filename.concat

let file_exists file =
  try
    FileUtil.(test Exists file)
  with _ -> false


let start_table = {|
     <table id="Results" style="width:100%" frame=void rules=rows >
|}


let () =
  let files = FileUtil.ls "." in
  let tests, artis =
    List.fold files ~init:([],[]) ~f:(fun (tests,artis) dir ->
        let r = dir / "results" in
        if file_exists r then
          let arti = Filename.basename dir in
          let size, data = Parse.process r in
          let doc = Template.render_artifact arti size data in
          let sum = arti, size, data in
          if String.is_substring arti ~substring:"juliet" then
            (doc,sum) :: tests, artis
          else tests, (doc,sum)::artis
        else tests, artis) in
  let get_doc = List.rev_map ~f:fst in
  let get_sum = List.rev_map ~f:snd in
  let docs = get_doc tests @ get_doc artis in
  let sum = get_sum tests @ get_sum artis in
  let sum = Template.render_summary sum in
  Out_channel.with_file "results.html" ~f:(fun ch ->
      Out_channel.output_string ch html_header;
      Out_channel.output_lines ch sum;
      Out_channel.output_string ch start_table;
      List.iter docs ~f:(Out_channel.output_lines ch);
      Out_channel.output_string ch "</table>";
      Out_channel.output_string ch html_finish)
