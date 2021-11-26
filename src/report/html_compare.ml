open Bisect_common

module Status_line = struct
  type t = Both | Added | Removed | Ignore

  let create x y =
    if x <= 0 && y <= 0 then Ignore
    else if x > 0 && y > 0 then Both
    else if x > 0 && y <= 0 then Removed
    else if x <= 0 && y >= 0 then Added
    else assert false

  let to_int = function Both -> 0 | Added -> 1 | Removed -> 2 | Ignore -> 3

  let of_int x =
    assert (x >= 0 && x <= 3);
    match x with
    | 0 -> Both
    | 1 -> Added
    | 2 -> Removed
    | 3 -> Ignore
    | _ -> assert false

  let to_string = function
    | Both -> "Both"
    | Added -> "Added"
    | Removed -> "Removed"
    | Ignore -> "Ignore"

  type merge = { removed : int; both : int; added : int; ignored : int }

  let print_merge m =
    Printf.printf
      {|{
     removed : %d;
     both : %d;
     added : %d;
     ignored : %d;
}|}
      m.removed m.both m.added m.ignored

  let print_list l =
    print_string "[";
    List.iter (fun x -> Printf.printf " %s;" (to_string x)) l;
    print_string "]\n"

  let merge l =
    let acc = { removed = 0; both = 0; added = 0; ignored = 0 } in

    List.fold_left
      (fun acc x ->
        let acc =
          match x with
          | Removed -> { acc with removed = acc.removed + 1 }
          | Both -> { acc with both = acc.both + 1 }
          | Added -> { acc with added = acc.added + 1 }
          | Ignore -> { acc with ignored = acc.ignored + 1 }
        in
        acc)
      acc l

  type line = Only_removed | Removed_and_add | Both | Only_added | Ignore

  let to_class : merge -> string =
   fun merge ->
    let removed = merge.removed in
    let both = merge.both in
    let added = merge.added in
    let s =
      match (removed, both, added) with
      | r, 0, 0 when r > 0 -> "only-removed"
      | r, _, a when r > 0 && a > 0 -> "removed-and-add"
      | 0, b, 0 when b > 0 -> "both"
      | 0, 0, a when a > 0 -> "only-added"
      | _ -> ""
    in
    Printf.sprintf {|class="%s""|} s

  let to_line : merge -> line =
   fun merge ->
    let removed = merge.removed in
    let both = merge.both in
    let added = merge.added in
    match (removed, both, added) with
    | r, 0, 0 when r > 0 -> Only_removed
    | r, _, a when r > 0 && a > 0 -> Removed_and_add
    | 0, b, 0 when b > 0 -> Both
    | 0, 0, a when a > 0 -> Only_added
    | _ -> Ignore

  type stat = {
    only_removed : float;
    removed_and_add : float;
    both : float;
    only_added : float;
  }

  let stat_from_lines lines =
    let only_removed = ref 0 in
    let removed_and_add = ref 0 in
    let both = ref 0 in
    let only_added = ref 0 in
    let total = List.length lines |> Float.of_int in

    let () =
      List.iter
        (function
          | Only_removed -> incr only_removed
          | Removed_and_add -> incr removed_and_add
          | Both -> incr both
          | Only_added -> incr only_added
          | Ignore -> ())
        lines
    in
    let open Float in
    {
      only_removed = of_int !only_removed /. total;
      removed_and_add = of_int !removed_and_add /. total;
      both = of_int !both /. total;
      only_added = of_int !only_added /. total;
    }
end

let colors_chart ?(only_removed = "Lost coverage")
    ?(removed_and_add = "Mix of loss and win") ?(both = "Covered in both")
    ?(only_added = "Win coverage") ?(grey = "Not covered in both")
    ?(with_grey = false) () =
  Printf.sprintf
    {|<div class="label" id="colors">
       <div id="colors-only-removed">%s</div><div id="colors-removed-and-add">%s</div><div id="colors-both">%s</div><div id="colors-only-added">%s</div>%s
   </div>|}
    only_removed removed_and_add both only_added
    (if with_grey then {|<div id="colors-grey">|} ^ grey ^ "</div>" else "")

let coverage cov =
  if cov = "" then raise (Invalid_argument "--x and --y must be present");

  Input.load_coverage ~coverage_files:[ cov ] ~coverage_paths:[] ~expect:[]
    ~do_not_expect:[]

let merge_counts k x y =
  assert (x.filename = y.filename);
  assert (x.points = y.points);

  let counts =
    Array.map2 (fun x y -> Status_line.(create x y |> to_int)) x.counts y.counts
  in
  { filename = x.filename; points = x.points; counts }

let merge cov_x cov_y : coverage =
  let coverage = Hashtbl.create 17 in
  let () =
    Hashtbl.iter
      (fun k v ->
        let x = v in
        let y = Hashtbl.find cov_y k in
        Hashtbl.add coverage k (merge_counts k x y))
      cov_x
  in
  coverage

let theme_class = function
  | `Light -> {| class="light"|}
  | `Dark -> {| class="dark"|}
  | `Auto -> ""

let split_filename name =
  let dirname =
    match Filename.dirname name with
    | "" -> ""
    | dir when dir = Filename.current_dir_name -> ""
    | dir -> dir ^ Filename.dir_sep
  in
  let basename = Filename.basename name in
  (dirname, basename)

let percentage (visited, total) =
  if total = 0 then 100. else 100. *. float_of_int visited /. float_of_int total

let output_html_index title theme filename
    (files : (string * string * Status_line.stat) list) =
  Util.info "Writing index file...";

  let channel =
    try open_out filename
    with Sys_error message ->
      Util.fatal "cannot open output file '%s': %s" filename message
  in
  try
    let write format = Printf.fprintf channel format in

    write
      {|<!DOCTYPE html>
<html lang="en"%s>
  <head>
    <meta charset="utf-8"/>
    <title>%s</title>
    <meta name="description" content="%s coverage overall"/>
    <link rel="stylesheet" type="text/css" href="coverage.css"/>
  </head>
  <body>
    <div id="header">
      <h1>%s</h1>
      %s
    </div>
    <div id="files">
|}
      (theme_class theme) title "foo" title
      (colors_chart ~with_grey:true ());

    files
    |> List.iter (fun (name, html_file, stats) ->
           let percentage_only_removed =
             Float.floor (Status_line.(stats.only_removed) *. 100.0)
             |> Printf.sprintf "%.00f"
           in

           let percentage_removed_and_add =
             Float.floor (Status_line.(stats.removed_and_add) *. 100.0)
             |> Printf.sprintf "%.00f"
           in
           let percentage_both =
             Float.floor (Status_line.(stats.both) *. 100.0)
             |> Printf.sprintf "%.00f"
           in

           let percentage_only_added =
             Float.floor (Status_line.(stats.only_added) *. 100.0)
             |> Printf.sprintf "%.00f"
           in

           let percentage_ignore =
             Printf.sprintf "%.00f"
             @@ Float.floor
                  (100.0
                  -. ((Status_line.(stats.only_removed) *. 100.0)
                     +. (Status_line.(stats.both) *. 100.0)
                     +. ((Status_line.(stats.only_added) *. 100.0)
                        +. (Status_line.(stats.removed_and_add) *. 100.0))))
           in

           let dirname, basename = split_filename name in
           let relative_html_file =
             if Filename.is_relative html_file then html_file
             else
               let prefix_length = String.length Filename.dir_sep in
               String.sub html_file prefix_length
                 (String.length html_file - prefix_length)
           in
           write
             {|      <div>
        <span class="meter">
          <span class="only-removed" style="width: %s%%"></span><span class="removed_and_add" style="width: %s%%"></span><span class="both" style="width: %s%%"></span><span class="only-added" style="width: %s%%"></span>
        </span>
        <span class="percentage">%s</span>
        <a href="%s">
          <span class="dirname">%s</span>%s
        </a>
      </div>
              |}
             percentage_only_removed percentage_removed_and_add percentage_both
             percentage_only_added
             (colors_chart
                ~only_removed:(percentage_only_removed ^ "%")
                ~removed_and_add:(percentage_removed_and_add ^ "%")
                ~both:(percentage_both ^ "%")
                ~only_added:(percentage_only_added ^ "%")
                ~grey:(percentage_ignore ^ "%") ~with_grey:true ())
             relative_html_file dirname basename);

    write {|    </div>
  </body>
</html>
|};

    close_out channel
  with
  | Sys_error message ->
      Util.fatal "cannot write output file '%s': %s" filename message
  | exn ->
      close_out_noerr channel;
      raise exn

let escape_line tab_size line offset points =
  let buff = Buffer.create (String.length line) in
  let ofs = ref offset in
  let pts = ref points in

  let marker_if_any content =
    match !pts with
    | (o, n) :: tl when o = !ofs ->
        Printf.bprintf buff {|<span data-count="%i">%s</span>|} n content;
        pts := tl
    | _ -> Buffer.add_string buff content
  in
  line
  |> String.iter (fun ch ->
         let s =
           match ch with
           | '<' -> "&lt;"
           | '>' -> "&gt;"
           | '&' -> "&amp;"
           | '\t' -> String.make tab_size ' '
           | c -> Printf.sprintf "%c" c
         in
         marker_if_any s;
         incr ofs);
  Buffer.contents buff

(* Individual HTML files corresponding to each source file. *)

let output_for_source_file tab_size title theme source_file_on_disk
    html_file_on_disk { Bisect_common.filename; points; counts } =
  let len = Array.length counts in
  let stats = ref (0, 0) in
  let points =
    points
    |> List.mapi (fun index offset -> (offset, index))
    |> List.sort compare
  in
  let pts =
    ref
      (points
      |> List.map (fun (offset, index) ->
             let nb = if index < len then counts.(index) else 0 in
             let visited, total = !stats in
             let visited = if nb > 0 then visited + 1 else visited in
             stats := (visited, total + 1);
             (offset, nb)))
  in
  let dirname, basename = split_filename filename in
  Util.mkdirs (Filename.dirname html_file_on_disk);
  let in_channel =
    try open_in source_file_on_disk
    with Sys_error message ->
      Util.fatal "cannot open source file '%s': %s" source_file_on_disk message
  in
  let out_channel =
    try open_out html_file_on_disk
    with Sys_error message ->
      Util.fatal "cannot open output file '%s': %s" html_file_on_disk message
  in
  let rec make_path_to_report_root acc in_file_path_remaining =
    if
      in_file_path_remaining = ""
      || in_file_path_remaining = Filename.current_dir_name
      || in_file_path_remaining = Filename.dir_sep
    then acc
    else
      let path_component = Filename.basename in_file_path_remaining in
      let parent = Filename.dirname in_file_path_remaining in
      if path_component = Filename.current_dir_name then
        make_path_to_report_root acc parent
      else
        make_path_to_report_root
          (Filename.concat acc Filename.parent_dir_name)
          parent
  in
  let path_to_report_root =
    make_path_to_report_root "" (Filename.dirname filename)
  in
  let style_css = Filename.concat path_to_report_root "coverage.css" in
  let coverage_js = Filename.concat path_to_report_root "coverage.js" in
  let highlight_js = Filename.concat path_to_report_root "highlight.pack.js" in
  let index_html = Filename.concat path_to_report_root "index.html" in
  let lines_status = ref [] in
  (try
     let lines, line_count =
       let rec read number acc =
         let start_ofs = pos_in in_channel in
         try
           let line = input_line in_channel in
           let end_ofs = pos_in in_channel in
           let before, after = Util.split (fun (o, _) -> o < end_ofs) !pts in
           pts := after;
           let line' = escape_line tab_size line start_ofs before in
           let visited, unvisited =
             List.fold_left
               (fun (v, u) (_, nb) -> (v || nb > 0, u || nb = 0))
               (false, false) before
           in
           let status = List.map snd before |> List.map Status_line.of_int in
           lines_status := status :: !lines_status;
           read (number + 1) ((number, line', visited, unvisited, status) :: acc)
         with End_of_file -> (List.rev acc, number - 1)
       in
       read 1 []
     in

     let class_of_visited = function _ -> "" in

     let write format = Printf.fprintf out_channel format in

     (* Head and header. *)
     let file_coverage = Printf.sprintf "%.02f%%" (percentage !stats) in
     write
       {|<!DOCTYPE html>
<html lang="en"%s>
  <head>
    <meta charset="utf-8"/>
    <title>%s &mdash; %s</title>
    <meta name="description" content="%s coverage in %s">
    <link rel="stylesheet" href="%s"/>
    <script src="%s"></script>
    <script>hljs.initHighlightingOnLoad();</script>
  </head>
  <body>
    <div id="header">
      <h1>
        <a href="%s">
          <span class="dirname">%s</span>%s
        </a>
      </h1>
      %s
    </div>
    <div id="navbar">
|}
       (theme_class theme) basename title file_coverage filename style_css
       highlight_js index_html dirname basename (colors_chart ());

     (* Navigation bar items. *)
     lines
     |> List.iter (fun (number, _, visited, unvisited, _) ->
            if unvisited then
              let offset =
                float_of_int number /. float_of_int line_count *. 100.
              in
              let origin, offset =
                if offset <= 50. then ("top", offset)
                else ("bottom", 100. -. offset)
              in
              write "      <span %s style=\"%s:%.02f%%\"></span>\n"
                (class_of_visited (visited, unvisited))
                origin offset);

     write
       {|    </div>
    <div id="report">
      <div id="lines-layer">
        <pre>
|};

     (* Line highlights. *)
     lines
     |> List.iter (fun (number, _, _, _, status) ->
            write "<a id=\"L%i\"></a><span %s> </span>\n" number
              Status_line.(merge status |> to_class));

     write
       {|</pre>
      </div>
      <div id="text-layer">
        <pre id="line-numbers">
|};

     let width = string_of_int line_count |> String.length in

     (* Line numbers. *)
     lines
     |> List.iter (fun (number, _, _, _, _) ->
            let formatted = string_of_int number in
            let padded =
              String.make (width - String.length formatted) ' ' ^ formatted
            in
            write "<a href=\"#L%s\">%s</a>\n" formatted padded);

     let syntax =
       if Filename.check_suffix basename ".re" then "reasonml" else "ocaml"
     in

     write "</pre>\n";
     write "<pre><code class=\"%s\">" syntax;

     (* Code lines. *)
     lines |> List.iter (fun (_, markup, _, _, _) -> write "%s\n" markup);

     write
       {|</code></pre>
      </div>
    </div>
    <script src="%s"></script>
  </body>
</html>
|}
       coverage_js
   with e ->
     close_in_noerr in_channel;
     close_out_noerr out_channel;
     raise e);

  close_in_noerr in_channel;
  close_out_noerr out_channel;

  let merges = List.map Status_line.merge !lines_status in
  let lines = List.map Status_line.to_line merges in
  Status_line.stat_from_lines lines

(* Assets, such as CSS and JavaScript files. *)

let output_string_to_separate_file content filename =
  let channel =
    try open_out filename
    with Sys_error message ->
      Util.fatal "cannot open output file '%s': %s" filename message
  in
  try
    Printf.fprintf channel "%s" content;
    close_out channel
  with
  | Sys_error message ->
      Util.fatal "cannot write output file '%s': %s" filename message
  | exn ->
      close_out_noerr channel;
      raise exn

let output ~to_directory ~title ~tab_size ~source_paths ~ignore_missing_files
    ~cov_x ~cov_y =
  let theme = `Dark in

  let cov_x = coverage cov_x in
  let cov_y = coverage cov_y in

  let coverage = merge cov_x cov_y in

  (* Write each of the HTML files corresponding to each source file. *)
  Util.mkdirs to_directory;
  let files =
    Hashtbl.fold
      (fun _ file acc ->
        let filename = Bisect_common.(file.filename) in
        let source_file_on_disk =
          Util.find_source_file ~source_roots:source_paths ~ignore_missing_files
            ~filename
        in
        match source_file_on_disk with
        | None -> acc
        | Some source_file_on_disk ->
            let html_file_on_disk =
              Filename.concat to_directory filename ^ ".html"
            in
            let html_file_relative = filename ^ ".html" in
            let stats =
              output_for_source_file tab_size title theme source_file_on_disk
                html_file_on_disk file
            in
            (filename, html_file_relative, stats) :: acc)
      coverage []
  in

  (* Write the coverage report landing page. *)
  output_html_index title theme
    (Filename.concat to_directory "index.html")
    (List.sort
       (fun (_, _, x) (_, _, y) ->
         let open Status_line in
         match compare x.only_removed y.only_removed with
         | 0 -> compare x.removed_and_add y.removed_and_add
         | x -> x)
       files
    |> List.rev);

  (* Write the asset files. *)
  output_string_to_separate_file Assets.js
    (Filename.concat to_directory "coverage.js");
  output_string_to_separate_file Assets.highlight_js
    (Filename.concat to_directory "highlight.pack.js");
  output_string_to_separate_file Assets.css
    (Filename.concat to_directory "coverage.css")
