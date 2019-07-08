open Core_kernel

type check =
  | Test
  | Unused
  | Null
  | Forbidden
  | Complex
  | Non_structural
  | Recursive
  | Hardcoded_socket_address
  | Memcheck_double_release
  | Memcheck_out_of_bound
  | Memcheck_use_after_free
  | Value_was_used_before_check
[@@deriving bin_io, compare, sexp]

type info =
  | Size of string
  | Time of string
  | Total of int
  | Confirmed of int
  | False_pos of int
  | False_neg of int
  | Text of string list
[@@deriving sexp]

type stat = {
    total   : int;
    false_p : int;
    false_n : int;
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

let create_stat total confirm false_p false_n time =
  let unknown = total - confirm - false_p - false_n in
  {total; false_p; confirm; unknown; time; false_n; }

let is_no_incidents = function
  | [x] -> x = "not found"
  | _ -> false

let no_incidents_stmt = "no incidents found"

module Parse = struct

  let parse_check name =
    try
      let name = String.map ~f:(fun c -> if c = '-' then '_' else c) name in
      let name = String.capitalize name in
      Some (check_of_sexp (Sexp.of_string name))
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
    | name :: _ -> parse_check name
    | _ -> None

  let info_of_string s =
    match String.split ~on:':' s with
    | [_; ""] -> None
    | ["Total"; x] -> with_num x (fun x -> Total x)
    | ["Confirmed"; x] -> with_num x (fun x -> Confirmed x)
    | ["False_pos"; x] -> with_num x (fun x -> False_pos x)
    | ["False_neg"; x] -> with_num x (fun x -> False_neg x)
    | "Time" :: xs ->
       let tm = String.concat xs ~sep:":" |> String.strip in
       Some (Time tm)
    | "Size" :: s :: _ -> Some (Size s)
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
                 let acc = match acc with
                   | Text text :: acc -> Text (line :: text) :: acc
                   | _ -> Text [line] :: acc in
                 loop (size,data) check acc lines in
    let size, data = loop (None,[]) None [] lines in
    size, normalize data

end

module Template = struct

  let map_checkname = function
    | Test -> "Testcase"
    | Forbidden -> "Forbidden functions"
    | Unused ->  "Unused return value"
    | Complex -> "Functions with cyclomatic complexity > 50"
    | Recursive -> "Recursive functions"
    | Non_structural -> "Functions with non-structural cfg"
    | Null -> "Null pointer dereference"
    | Hardcoded_socket_address -> "Hardcoded socket address"
    | Memcheck_double_release -> "Memory check: double free"
    | Memcheck_out_of_bound -> "Memory check: out of bound"
    | Memcheck_use_after_free -> "Memory check: use after free"
    | Value_was_used_before_check -> "Value was used before check"

  let ref_to_top = {|<p><a href="#top">Top</a></p>|}

  let check_id arti check =
    sprintf "%s+%s" arti @@ Sexp.to_string (sexp_of_check check)

  let render_checkname arti check =
    let id = check_id arti check in
    sprintf "<b id=\"%s\">%s</b>" id (map_checkname check)

  let render_text text =
    let text = if is_no_incidents text then no_incidents_stmt
               else String.concat text ~sep:"\n" in
    sprintf "<pre>\n%s\n</pre>" text

  let find_text infos =
    let rec loop = function
      | [] -> None
      | Text t :: _ -> Some t
      | _ :: infos -> loop infos in
    loop infos

  let rec list_compare xs ys =
    match xs,ys with
    | [], [] -> 0
    | x :: [], y :: [] -> String.compare x y
    | x :: xs, y :: ys ->
       let r = String.compare x y  in
       if r <> 0 then r
       else compare xs ys
    | _ ->
       assert false

  let render_data words_per_record lines =
    let words_of_line line =
      String.split ~on:' ' line |>
        List.map ~f:String.strip |>
        List.filter ~f:(fun x -> x <> "") in
    let empty_tab = ["<table id=\"data\">"; "<div class=\"line-elt\">"] in
    let space_elt = "<div class=\"line-elt\">&nbsp&nbsp</div>" in
    let close_tab tab =
      List.rev (space_elt :: "</div>" :: "</table>" :: tab) |> String.concat in
    let add_row tab words =
      let tab = List.fold words ~init:("<tr>" :: tab)
                  ~f:(fun tab x -> "</td>" :: "&nbsp" :: x
                                :: "&nbsp" :: "<td>" :: tab ) in
      "</tr>" :: tab in
    let rec reformat acc row = function
      | [] -> List.filter ~f:(fun x -> x <> []) (List.rev row :: acc)
      | x :: words when List.length row < words_per_record ->
         reformat acc (x :: row) words
      | x :: words ->
         reformat (List.rev row :: acc) [x]  words in
    let data =
      List.fold lines ~init:[] ~f:(fun acc line ->
          match words_of_line line with
          | [] -> acc
          | ws ->
             let acc' = reformat [] [] ws in
             acc @ acc') in
    let data = List.sort data ~compare:list_compare in

    let data = if is_no_incidents lines then [ [no_incidents_stmt] ]
               else data in
    let len = List.length data in
    let tabs = match len with
      | n when n < 10 -> 1
      | n when n < 30 -> 2
      | n when n < 60 -> 3
      | n when n < 400 -> 5
      | _ -> 7 in
    let rows = len / tabs  in
    (* for a case of a hangling single column  *)
    let rows = if len - rows * tabs = 1 then len / (tabs + 1)
               else rows in
    let acc, tab, _ =
      List.fold data ~init:(["<div class=\"start-line\">"],empty_tab,0) ~f:(fun (acc,tab,i) ws ->
          let tab = add_row tab ws in
          if i + 1 < rows then acc, tab, i + 1
          else close_tab tab :: acc, empty_tab, 0) in
    let acc = "</div>" :: close_tab tab :: acc in
    List.rev acc |> String.concat


  let render_stat infos stat =
    let render =
      sprintf
        "<pre>Total/Confirmed/False positive/False negative/Unclassified: %d/%d/%d/%d/%d</pre>"
        stat.total stat.confirm stat.false_p stat.false_n stat.unknown in
    if stat.total <> 0 then render
    else
      match find_text infos with
      | Some text when is_no_incidents text -> ""
      | _ -> render

  let render_testdata data =
    let pass_color = "#D4EFE2" in
    let fail_color = "#F7D3CE" in
    let stub_color = "#FFFFFF" in
    let render_cell name status =
      let color = match status with
        | "pass" -> pass_color
        | "fail" -> fail_color
        | _ -> stub_color in
      sprintf "<td bgcolor=\"%s\">%s</td>" color name in
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
    </table></br>|} pass_color fail_color in
    let cols = 3 in
    let doc,_ =
      List.fold data ~init:(["<table>"; legend],0) ~f:(fun (doc,col_i) line ->
          match String.split ~on:' ' line with
          | name :: status :: _ ->
             let doc = render_cell name status :: doc  in
             let col_i = col_i + 1 in
             if col_i = cols then "<tr></tr>" :: doc, 0
             else doc, col_i
          | _ -> doc, col_i) in
    List.rev ("</br>"::"</table>" :: doc) |> String.concat

  let render_check arti check stat infos =
    let doc =
      List.fold infos ~init:["<div>"] ~f:(fun doc -> function
          | Text text when check = Test -> render_testdata text :: doc
          | Text text when check = Forbidden -> render_data 1 text :: doc
          | Text text when check = Complex -> render_data 1 text :: doc
          | Text text when check = Recursive -> render_data 1 text :: doc
          | Text text when check = Non_structural -> render_data 1 text :: doc
          | Text text when check = Unused -> render_data 2 text :: doc
          | Text text when check = Null ->  render_data 2 text :: doc
          | Text text -> render_text text :: doc
          | Time time -> sprintf "<pre>Time: %s</pre>" time :: doc
          | _ -> doc) in
    "</div>"  ::
    render_checkname arti check :: render_stat infos stat :: List.rev doc

  let get_stat check stat =
    List.find stat ~f:(fun (check',s) -> check = check') |> function
    | None -> create_stat 0 0 0 0 ""
    | Some (_,s) -> s

  let render_artifact name size data stat =
    let size = match size with
      | None -> ""
      | Some s -> sprintf "size: %s" s in
    let init = [
        "<tr>";
        sprintf "<td><pre><h3>%s</h3>%s</pre></td>" name size;
        "<td>";
        sprintf "<p id=\"%s\"> </p>" name;
        ref_to_top;
      ] in
    let doc =
      List.fold data ~init ~f:(fun  doc (check, infos) ->
          let stat = get_stat check stat in
          let doc' = render_check name check stat infos in
          doc @ "</br>" :: doc') in
    doc @ ["</tr>"; "</td>"]

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
              (fun (acc,has_tr) (check, res) ->
              let check =
                sprintf "<td><a href=\"#%s\">%s</a></td>"
                  (check_id arti check) (map_checkname check) in
              let totl = digit res.total in
              let conf = digit res.confirm in
              let falp = digit res.false_p in
              let faln = digit res.false_n in
              let unkn = digit res.unknown in
              let time = cell  res.time in
              let tr = if has_tr then "" else "<tr>" in
              "</tr>" :: time :: unkn :: faln :: falp :: conf :: totl :: check
              :: tr :: acc, false)) in
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
    | None, Some cnf, Some fp, Some fn ->
       create_stat (fp + fn + cnf) cnf fp fn time
    | Some total, None, None, None -> create_stat total 0 0 0 time
    | Some total, Some cnf, None, None -> create_stat total cnf 0 0 time
    | Some total, None, Some fp, None -> create_stat total 0 fp 0 time
    | _ -> create_stat 0 0 0 0 time

  let infer_stat time (total, cnf, fp, fn) =
    let with_default x = Option.value ~default:0 x in
    let cnf = with_default cnf in
    let fp  = with_default fp in
    let fn  = with_default fn in
    let total = match total with
      | None -> cnf + fp + fn
      | Some total -> total in
    create_stat total cnf fp fn time

  let get_stat check infos =
    let rec loop ((total,confirmed,false_pos,false_neg) as r) = function
      | Total x :: infos -> loop (Some x,confirmed,false_pos,false_neg) infos
      | False_pos x :: infos -> loop (total,confirmed,Some x,false_neg) infos
      | False_neg x :: infos -> loop (total,confirmed,false_pos,Some x) infos
      | Confirmed x :: infos -> loop (total,Some x,false_pos,false_neg) infos
      | Text text :: _ when is_no_incidents text -> Some 0, Some 0, Some 0, Some 0
      | Text text :: _ when check = Test ->
         let total, pass, fail =
           List.fold text ~init:(0,0,0) ~f:(fun (total,pass,fail) line ->
               if String.is_substring line ~substring:"pass" then
                 total + 1, pass + 1, fail
               else if String.is_substring line ~substring:"fail" then
                 total + 1, pass, fail + 1
               else
                 total + 1, pass, fail) in
         Some total, Some pass, Some 0, Some fail
      | _ :: infos -> loop r infos
      | [] -> r in
    let time = find_time infos in
    let stat =
      match loop (None, None, None, None) infos with
      | (Some total, _, _, _) as r -> infer_stat time r
      | None, cnf, fp, fn -> infer_stat time (infer_total check infos,cnf,fp,fn) in
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

|}

let debug_print_data = function
  | [] -> printf "EMPTY!\n"
  | data ->
  List.iter data ~f:(fun (check, infos) ->
      printf "%s\n" (Sexp.to_string (sexp_of_check check));
      List.iter infos ~f:(function
          | Size s -> printf "  Size %s\n" s
          | Time s -> printf "  Time %s\n" s
          | Total x -> printf "  Total %d\n" x
          | Confirmed x -> printf "  Confirmed %d\n" x
          | False_pos x -> printf "  False_pos %d\n" x
          | False_neg x -> printf "  False_neg %d\n" x
          | Text text ->
             printf "  Text: \n";
             List.iter text ~f:(printf "   %s\n")))

let () =
  let files = FileUtil.ls "." in
  let tests, artis =
    List.fold files ~init:([],[]) ~f:(fun (tests,artis) dir ->
        let r = dir / "results" in
        if file_exists r then
          let arti = Filename.basename dir in
          let size, data = Parse.process r in
          let stat = Stat.of_data data in
          let doc = Template.render_artifact arti size data stat in
          let sum = arti, size, stat in
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
