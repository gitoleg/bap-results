open Core_kernel

open Template.Types


module Parse = struct

  type info =
    | Time of string
    | Size of string
    | Stat of int * int * int * int
    | Data of (string list * status) list

  let words_of_line x =
  String.split ~on:' ' x |>
  List.filter ~f:(fun y -> y <> "")

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
        let undecided = total - confirmed - false_pos - false_neg in
        Some (Stat (confirmed,false_pos,false_neg,undecided)))
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

  let create_stat cnf fp fn und time = {
      false_pos = fp; false_neg = fn;
      confirmed = cnf; undecided = und; took_time=time
    }

  let stub_stat = create_stat 0 0 0 0

  let infer_stat took_time data =
    match data with
    | [ ["not found"],_; ] -> stub_stat took_time
    | _ -> {
      false_pos = List.count data ~f:is_false_pos;
      false_neg = List.count data ~f:is_false_neg;
      confirmed = List.count data ~f:is_confirmed;
      undecided = List.count data ~f:is_undecided;
      took_time;
    }

  let get_stat info data time =
    List.find_map info ~f:(function
        | Stat (cnf,fp,fn,un) ->
           Some (create_stat cnf fp fn un time)
        | _ -> None) |>
      function
      | None -> infer_stat time data
      | Some s -> s

  let find_data info =
    List.find_map info ~f:(function
        | Data d -> Some (List.rev d)
        | _ -> None) |> Option.value ~default:[]

  let normalize xs =
    List.fold xs ~init:([])
      ~f:(fun acc (check, info) ->
        let data = find_data info in
        let time = find_time info in
        let stat = get_stat info data time in
        (check,stat,data) :: acc)

  let process file  =
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
    let size = Option.value ~default:"" size in
    let data = normalize data in
    size, data

end

let (/) = Filename.concat

let file_exists file =
  try
    FileUtil.(test Exists file)
  with _ -> false

let () =
  let files = FileUtil.ls "." in
  let tests, artis =
    List.fold files ~init:([],[]) ~f:(fun (tests,artis) dir ->
        let r = dir / "results" in
        if file_exists r then
          let arti = Filename.basename dir in
          let size,data = Parse.process r in
          let sum = arti, size, data in
          if String.is_substring arti ~substring:"juliet" then
            sum :: tests, artis
          else tests, sum::artis
        else tests, artis) in
  let all = tests @ artis in
  let doc = Template.render all in
  Out_channel.with_file "results.html"
    ~f:(fun ch -> Out_channel.output_string ch doc)
