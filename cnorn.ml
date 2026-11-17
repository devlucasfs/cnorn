module SS = Set.Make (String)

type mode = Once | Always

let die fmt =
  Printf.ksprintf (fun s -> prerr_endline ("cnorn: " ^ s); exit 1) fmt

let seen = ref SS.empty

let read_file path =
  let ic = open_in_bin path in
  let n = in_channel_length ic in
  let s = really_input_string ic n in
  close_in ic;
  s

let normalize path =
  let abs =
    if Filename.is_relative path then Filename.concat (Sys.getcwd ()) path
    else path
  in
  let rec go acc = function
    | [] -> List.rev acc
    | ("" | ".") :: rest -> go acc rest
    | ".." :: rest -> go (match acc with _ :: t -> t | [] -> []) rest
    | p :: rest -> go (p :: acc) rest
  in
  "/" ^ String.concat "/" (go [] (String.split_on_char '/' abs))

let find_sub s sub from =
  let n = String.length s and m = String.length sub in
  let rec go i =
    if i + m > n then None
    else if String.sub s i m = sub then Some i
    else go (i + 1)
  in
  go from

let strip_final_newline s =
  let n = String.length s in
  if n > 0 && s.[n - 1] = '\n' then String.sub s 0 (n - 1) else s

let detect_mode lines =
  let rec go skipped = function
    | l :: rest when String.trim l = "" -> go (l :: skipped) rest
    | l :: rest ->
      (match String.trim l with
       | "#pragma once" -> (Once, List.rev_append skipped rest)
       | "#pragma always" -> (Always, List.rev_append skipped rest)
       | _ -> (Once, List.rev_append skipped (l :: rest)))
    | [] -> (Once, List.rev skipped)
  in
  go [] lines

let tag = "@import("

let parse_args line from =
  let n = String.length line in
  let skip_ws i =
    let i = ref i in
    while !i < n && (line.[!i] = ' ' || line.[!i] = '\t') do incr i done;
    !i
  in
  let rec parse_str i acc =
    let i = skip_ws i in
    if i >= n || line.[i] <> '"' then None
    else
      match String.index_from_opt line (i + 1) '"' with
      | None -> None
      | Some j ->
        let acc = String.sub line (i + 1) (j - i - 1) :: acc in
        let i = skip_ws (j + 1) in
        if i >= n then None
        else if line.[i] = ',' then parse_str (i + 1) acc
        else if line.[i] = '"' then parse_str i acc
        else if line.[i] = ')' then Some (List.rev acc, i + 1)
        else None
  in
  parse_str from []

let rec expand_file stack path =
  let full = normalize path in
  if not (Sys.file_exists full) then die "file not found: %s" path;
  let mode, lines = detect_mode (String.split_on_char '\n' (read_file full)) in
  match mode with
  | Once when SS.mem full !seen -> ""
  | _ ->
    if mode = Once then seen := SS.add full !seen;
    if List.mem full stack then
      die "circular import in a pragma always file: %s" full;
    let dir = Filename.dirname full in
    let stack = full :: stack in
    String.concat "\n" (List.map (expand_line stack dir) lines)

and expand_line stack dir line =
  let n = String.length line in
  let buf = Buffer.create n in
  let rec loop pos =
    match find_sub line tag pos with
    | None -> Buffer.add_substring buf line pos (n - pos)
    | Some start ->
      let args_start = start + String.length tag in
      (match parse_args line args_start with
       | None ->
         Buffer.add_substring buf line pos (args_start - pos);
         loop args_start
       | Some (parts, after) ->
         Buffer.add_substring buf line pos (start - pos);
         let rel = String.concat "/" parts in
         let e = ref after in
         let k = ref !e in
         while !k < n && (line.[!k] = ' ' || line.[!k] = '\t') do incr k done;
         if !k < n && line.[!k] = ';' then e := !k + 1;
         let target =
           if Filename.is_relative rel then Filename.concat dir rel else rel
         in
         let body = expand_file stack target in
         Buffer.add_string buf (strip_final_newline body);
         loop !e)
  in
  loop 0;
  Buffer.contents buf

let () =
  match Array.to_list Sys.argv with
  | [ _; input ] -> print_string (expand_file [] input)
  | [ _; input; output ] ->
    let out = expand_file [] input in
    let oc = open_out_bin output in
    output_string oc out;
    close_out oc
  | _ ->
    prerr_endline "use: cnorn input.c [output.c]";
    exit 1
