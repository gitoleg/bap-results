open Core_kernel

module Types = struct

  type status =
    | Confirmed
    | False_pos
    | False_neg
    | Undecided
[@@deriving sexp]

  type stat = {
      false_pos : int;
      false_neg : int;
      confirmed : int;
      undecided : int;
      took_time : string;
    }

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

end

open Types


let color_of_status = function
  | Confirmed -> "#D4EFE2"
  | False_neg -> "#F7D3CE"
  | False_pos -> "#ADD8E6"
  | Undecided -> "#F2F2F2"

let legend =
  sprintf
    {|
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
     </table></br>
     |}
    (color_of_status Confirmed)
    (color_of_status False_neg)
    (color_of_status False_pos)
    (color_of_status Undecided)


type entry = string list * status
type data = entry list

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

let html_bottom = {|
  </body>
</html>
|}

let description_of_check = function
  | Test Null -> "https://cwe.mitre.org/data/definitions/476.html"
  | Test Unused ->  "https://cwe.mitre.org/data/definitions/252.html"
  | Unused ->  "https://raw.githubusercontent.com/BinaryAnalysisPlatform/bap-toolkit/master/jpl-rule-14/descr"
  | Null ->   "https://raw.githubusercontent.com/BinaryAnalysisPlatform/bap-toolkit/master/av-rule-174/descr"
  | Complex -> "https://raw.githubusercontent.com/BinaryAnalysisPlatform/bap-toolkit/master/av-rule-3/descr"
  | Non_structural -> "https://raw.githubusercontent.com/BinaryAnalysisPlatform/bap-toolkit/master/av-rule-189/descr"
  | Recursive  -> "https://raw.githubusercontent.com/BinaryAnalysisPlatform/bap-toolkit/master/jpl-rule-4/descr"
  | Memcheck_use_after_release -> "https://cwe.mitre.org/data/definitions/416.html"
  | Memcheck_double_release -> "https://cwe.mitre.org/data/definitions/415.html"
  | Forbidden -> "https://raw.githubusercontent.com/BinaryAnalysisPlatform/bap-toolkit/master/forbidden-symbol/descr"
  | _ -> ""

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

let render_as_text text = match text with
  | [] -> ""
  | text ->
    let text = List.map text ~f:(fun (x,_) -> String.concat x) in
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

let is_tableable = function
  | [] -> false
  | (x,_) :: xs ->
    let len = List.length x in
    List.for_all xs ~f:(fun (y,_) -> List.length y = len)

let make_cell ?align color word =
  let align = match align with
    | None -> ""
    | Some align -> sprintf " align=\"%s\"" align  in
  sprintf "<td%s bgcolor=\"%s\">&nbsp%s&nbsp</td>" align color word

let make_table_row words color = match words with
  | [] -> ""
  | fst :: others ->
    let row =
      List.fold others ~init:([make_cell color fst; "<tr>"])
        ~f:(fun acc w -> make_cell ~align:"right" color w :: acc) in
    List.rev ("</tr>" :: row) |> String.concat

let lines_number max data =
  let len = List.length data in
  let cols = match len with
    | n when n < 10 -> 1
    | n when n < 30 -> 2
    | n when n < 60 -> 3
    | n when n > 200 -> 4
    | _  -> max in
  let rows = len / cols  in
  if len - rows * cols = 1 then len / (cols + 1)
  else rows

let render_data ?(max_cols=6) data =
  let render_row (words,status) =
    let color = color_of_status status in
    make_table_row words color in
  match data with
  | [] -> ""
  | data  ->
    if not (is_tableable data) then render_as_text data
    else
      let data = List.sort data ~compare:compare_strings in
      let data = List.map data ~f:render_row in
      let start_all =   {|<div class="start-line">|} in
      let close_all =   {|</div>|} in
      let empty_tab = [ {|<div class="line-elt"><table id="data">|} ] in
      let space_elt =   {|<div class="line-elt">&nbsp&nbsp</div>|} in
      let close_tab =   {|</table></div>|} ^ space_elt in
      let close_tab tab =
        close_tab :: tab |> List.rev |> String.concat in
      let rows = lines_number max_cols data in
      let doc, last, _ =
        List.fold data ~init:([start_all],empty_tab,0) ~f:(fun (acc,tab,i) ws ->
            let tab = ws :: tab in
            if i + 1 < rows then acc, tab, i + 1
            else close_tab tab :: acc, empty_tab, 0) in
      List.rev (close_all :: close_tab last :: doc) |> String.concat

let render_checkname arti check =
  let check = Sexp.to_string (sexp_of_check check) in
  let id = sprintf "%s+%s" arti check in
  sprintf "<b id=\"%s\">%s</b>" id check

let total_of_stat s = s.confirmed + s.false_pos + s.false_neg + s.undecided


let render_stat = function
  | None -> ""
  | Some s ->
    let total = total_of_stat s in
    if total = 0 then ""
    else
      sprintf
        "<pre>Total/Confirmed/False positive/False negative/Unclassified: %d/%d/%d/%d/%d</pre>"
        total s.confirmed s.false_pos s.false_neg s.undecided

let render_time = function
  | None -> ""
  | Some s ->
    sprintf "<pre>Time: %s</pre>" s.took_time

let render_check ~artifact check ?stat data =
  String.concat [
    render_checkname artifact check;
    render_time stat;
    render_stat stat;
    render_data data;
    "</br>";
  ]

let ref_to_top = {|<p><a href="#top">Top</a></p>|}

let render_artifact ~name ~size (checks : (check * stat * data) list) =
  let start = [
    "<tr>";
    sprintf "<td><pre><h3>%s</h3>size: %s</pre></td>" name size;
    "<td>";
    sprintf "<p id=\"%s\"> </p>" name;
    ref_to_top;
    legend;
  ] in
  let doc =
    List.fold checks ~init:[] ~f:(fun  doc (check, stat, data) ->
        render_check ~artifact:name check ~stat data :: doc) in
  start @ List.rev doc @ ["</td></tr>"] |> String.concat

let render_artifacts artis =
  let init =
    [ {|<table id="Results" style="width:100%" frame=void rules=rows>|} ] in
  let doc = List.fold artis ~init
              ~f:(fun doc (name,size,checks) ->
                render_artifact ~name ~size checks :: doc) in
  List.rev ("</table>" :: doc)


let describe_check arti check =
  let descr = description_of_check check in
  let name = string_of_check check in
  sprintf "<td><a href=\"%s\">%s</a></td>" descr name



let summary_table_header =
{|
<table id="top" style="width:50%">
 <tr>
   <th>artifact</th>
   <th>check</th>
   <th>Total</th>
   <th>Confirmed</th>
   <th>False positive</th>
   <th>False negative</th>
   <th>Unclassified</th>
   <th><pre>Time\nhh:mm:ss</pre></th>
</tr>
|}

let render_summary artifacts =
  let cell x = sprintf "<td align=\"center\">%s</td>" x in
  let digit x = cell (string_of_int x) in
  let hdr = summary_table_header in
  let data =
    List.fold artifacts ~init:[hdr] ~f:(fun acc (arti, size, checks) ->
        let name  =
          sprintf "<td bgcolor=\"white\" rowspan=\"%d\" >
                   <pre><a href=\"#%s\">%s</a>\nsize: %s</pre>
                   </td>"
            (List.length checks) arti arti size in
        fst @@
          List.fold checks ~init:(name :: "<tr>" :: acc, true) ~f:
            (fun (acc,has_tr) (check, res,_) ->
              let check = describe_check arti check in
              let totl = digit (total_of_stat res) in
              let conf = digit res.confirmed in
              let falp = digit res.false_pos in
              let faln = digit res.false_neg in
              let unkn = digit res.undecided in
              let time = cell  res.took_time in
              let tr = if has_tr then "" else "<tr>" in
              "</tr>" :: time :: unkn :: faln :: falp :: conf :: totl :: check
              :: tr :: acc, false)) in
  List.rev ("</br>" :: "</table>" :: data)


type arti = string * string * ((check * stat * data) list)

let render artifacts =
  let summary   = render_summary artifacts in
  let artifacts = render_artifacts artifacts in
  String.concat (html_header :: summary  @ artifacts @ [ html_bottom; ])
