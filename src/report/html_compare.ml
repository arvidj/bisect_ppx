open Bisect_common

module Status_line = struct
  (* comparison of the visits of the same point by two traces *)
  type t =
    Both
  (* the point was visited in both traces *)
  | Added
  (* the point was visited in [to]-trace but not [from]-trace *)
  | Removed
  (* the point was visited in [from]-trace but not in [to]-trace *)
  | Ignore
  (* the point was not visited in either trace *)

  let create visits_from visits_to =
    if visits_from <= 0 && visits_to <= 0 then Ignore
    else if visits_from > 0 && visits_to > 0 then Both
    else if visits_from > 0 && visits_to <= 0 then Removed
    else if visits_from <= 0 && visits_to >= 0 then Added
    else assert false

  let to_int = function Both -> 0 | Added -> 1 | Removed -> 2 | Ignore -> 3

  let of_int x =
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

  (* represent the aggregated comparison of a set of points *)
  type merge = { removed : int; both : int; added : int; ignored : int }

  let merge_total { removed; both; added; ignored }= removed + both + added + ignored

  let _merge_is_empty m = (merge_total m = 0)

  let _print_merge m =
    Printf.printf
      {|{
     removed : %d;
     both : %d;
     added : %d;
     ignored : %d;
}|}
      m.removed m.both m.added m.ignored

  let _print_list l =
    print_string "[";
    List.iter (fun x -> Printf.printf " %s;" (to_string x)) l;
    print_string "]\n"

  let merge_empty = { removed = 0; both = 0; added = 0; ignored = 0 }
  
  let merge_add x y =
    { removed = x.removed + y.removed;
      both = x.both + y.both;
      added = x.added + y.added;
      ignored =x.ignored + y.ignored
    }

  let merge_sum ms = List.fold_left merge_add merge_empty ms

  let merge l =
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
      merge_empty l

  (* a summary of the aggregated comparison of a set of points
     corresponding to one source-code line *)
  type line =
    Only_removed
  (* we only lost coverage: [cov(to)] is a strict subset of [cov(from)]  *)
  | Removed_and_add
  (* some coverage were removed and some where added *)
  | Both
  (* the same set of points were covered in both traces: [cov(to) = cov(from)] *)
  | Only_added
  (* we only gained coverage: [cov(to)] is a strict superset of [cov(from)]  *)
  | Ignore
  (* no points on this line was visited in either trace *)
  | Empty
  (* the set of points was empty *)

  
  let merge_to_line : merge -> line =
    fun { removed; both; added; ignored } ->
    match (removed, both, added, ignored) with
    | 0, 0, 0, 0 -> Empty
    | r, 0, 0, _ when r > 0 -> Only_removed
    | r, _, a, _ when r > 0 && a > 0 -> Removed_and_add
    | 0, b, 0, _ when b > 0 -> Both
    | 0, 0, a, _ when a > 0 -> Only_added
    | _ -> Ignore

  let line_to_class : line -> string =
    fun line ->
    let cls = match line with
      | Only_removed -> "only-removed"
      | Removed_and_add -> "removed-and-add"
      | Both -> "both"
      | Only_added -> "only-added"
      | Ignore -> "ignore"
      | Empty -> "empty"
    in
    Printf.sprintf {|class="%s"|} cls

  let percentage_from_merge ({ removed; both; added; ignored=_ } as merge) =
    let total = merge_total merge in
    if total = 0 then
      None
    else
      let open Float in
      let total = of_int total in
      let removed = of_int removed /. total in
      (* let removed_and_add = of_int removed_and_add /. total in *)
      let both = of_int both /. total in
      let added = of_int added /. total in

      let pct x = floor (x *. 100.0) |> Printf.sprintf "%.00f" in

      let pct_removed = pct removed in
      (* let pct_removed_and_add = pct removed_and_add in *)
      let pct_both = pct both in
      let pct_added = pct added in
      let pct_ignore =
        pct
          (1.0 -.
             (removed +. both +. added))
      in
      Some (pct_removed, pct_both, pct_added, pct_ignore)

    module Stat = struct

  (* a stat represents the aggregation of a set of lines *)
  type stat = {
    only_removed : int;
    removed_and_add : int;
    both : int;
    only_added : int;
    total : int;
  }

  let _pp_stat {
          only_removed ;
          removed_and_add ;
          both;
          only_added;
          total
        } =
    Printf.sprintf "{only_removed = %d; removed_and_add = %d; both = %d; only_added = %d; total = %d}"
      only_removed
      removed_and_add 
      both
      only_added
      total

  let stat_empty = {
      only_removed = 0 ;
      removed_and_add = 0 ;
      both = 0;
      only_added = 0;
      total = 0
    }

  let _stat_add { only_removed; removed_and_add; both; only_added; total } b =
    {
      only_removed = b.only_removed + only_removed;
      removed_and_add = b.removed_and_add + removed_and_add;
      both = b.both + both;
      only_added = b.only_added + only_added;
      total = b.total + total;
    }

  let _stat_from_lines lines =
    let only_removed = ref 0 in
    let removed_and_add = ref 0 in
    let both = ref 0 in
    let only_added = ref 0 in
    let total = ref 0 in

    let () =
      List.iter
        (function
          | Only_removed -> incr only_removed; incr total
          | Removed_and_add -> incr removed_and_add; incr total
          | Both -> incr both; incr total
          | Only_added -> incr only_added; incr total
          | Ignore -> incr total;
          | Empty -> ()
        )
        lines
    in
    (* todo: hack to have 100% in files with no visitable lines *)
    if !total = 0 then
      {stat_empty with both = 1; total = 1}
    else
      {
        only_removed = !only_removed;
        removed_and_add = !removed_and_add;
        both = !both;
        only_added = !only_added;
        total = !total;
      }

  let _percentage_from_stat { only_removed; removed_and_add; both; only_added; total } =
    let open Float in
    let total = of_int total in
    let only_removed = of_int only_removed /. total in
    let removed_and_add = of_int removed_and_add /. total in
    let both = of_int both /. total in
    let only_added = of_int only_added /. total in

    let pct x = floor (x *. 100.0) |> Printf.sprintf "%.00f" in

    let pct_only_removed = pct only_removed in
    let pct_removed_and_add = pct removed_and_add in
    let pct_both = pct both in
    let pct_only_added = pct only_added in
    let pct_ignore =
      pct
        (1.0 -.
           (only_removed +. both +. only_added +. removed_and_add))
    in
    (pct_only_removed, pct_removed_and_add, pct_both, pct_only_added, pct_ignore)

    end
end

let tt_wrap ~html s = if html then Printf.sprintf "<tt>%s</tt>" s else s

let colors_chart
      ?(with_titles = false)
      ?(with_tooltips = false)
      ?(with_ignored = false)
      ~cov_from_label
      ~cov_to_label
      merge_stats_pcts =
  let box t cls s  =
    Printf.sprintf
      {|<div %sclass="%s">%s</div>|}
      (if with_tooltips then Printf.sprintf {|title="%s" |} (t ~html:false) else "")
      cls
      (if with_titles then (t ~html:true) ^ ": " ^ s else s)
  in
  let (removed, both, added, ignored) = match merge_stats_pcts with
    | None -> "-", "-", "-", "-"
    | Some (removed, both, added, ignored) ->
       (removed ^ "%", both ^ "%", added ^ "%", ignored ^ "%")
  in
  Printf.sprintf
    {|<div class="colors label">
       %s%s%s%s
   </div>|}
    (box (fun ~html -> "Coverage lost from " ^ (tt_wrap ~html cov_from_label)) "colors-only-removed" removed)
    (box (fun ~html:_ -> "Covered in both") "colors-both" both)
    (box (fun ~html -> "Coverage won in " ^ (tt_wrap ~html cov_to_label)) "colors-only-added" added)
    (if with_ignored then (box (fun ~html:_ -> "Covered in neither") "colors-ignored" ignored) else "")

let coverage cov =
  if cov = "" then raise (Invalid_argument "--x and --y must be present");

  Input.load_coverage ~coverage_files:[ cov ] ~coverage_paths:[] ~expect:[]
    ~do_not_expect:[]

let merge_counts _k x y =
  assert (x.filename = y.filename);
  assert (x.points = y.points);

  let counts =
    Array.map2 (fun x y -> Status_line.(create x y |> to_int)) x.counts y.counts
  in
  { filename = x.filename; points = x.points; counts }

let merge cov_from cov_to : coverage =
  let coverage = Hashtbl.create 17 in
  let () =
    Hashtbl.iter
      (fun k v ->
        let x = v in
        let y = Hashtbl.find cov_to k in
        Hashtbl.add coverage k (merge_counts k x y))
      cov_from
  in
  coverage

let theme_class = function
  | `Light -> {| class="light"|}
  | `Dark -> {| class="dark"|}
  | `Auto -> ""

let make_title ?(html=false) ~cov_from_label ~cov_to_label title =
  Printf.sprintf "%s [%s vs. %s]" title
    (tt_wrap ~html cov_from_label)
    (tt_wrap ~html cov_to_label)

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

let output_html_index ~title theme filename ~cov_from_label ~cov_to_label
    (files : (string * string * Status_line.merge) list) =
  Util.info "Writing index file...";

  let channel =
    try open_out filename
    with Sys_error message ->
      Util.fatal "cannot open output file '%s': %s" filename message
  in
  try
    let write format = Printf.fprintf channel format in
    
    let merge_total =
      files
      |> List.map (fun (_, _, stats) -> stats)
      |> Status_line.merge_sum
    in
    
    let merge_total_pcts =
      Status_line.percentage_from_merge merge_total in

    (* let title =  in *)
    write
      {|<!DOCTYPE html>
<html lang="en"%s>
  <head>
    <meta charset="utf-8"/>
    <title>%s</title>
    <meta name="description" content="Coverage comparison of %s and %s"/>
    <link rel="stylesheet" type="text/css" href="coverage.css"/>
  </head>
  <body>
    <div id="header">
      <h1>%s</h1>
      %s
    </div>
    <div id="files">
|}
      (theme_class theme)
      (make_title ~html:false ~cov_from_label ~cov_to_label title)
      cov_from_label cov_to_label
      (make_title ~html:true ~cov_from_label ~cov_to_label title)
      (colors_chart
         ~with_titles:true
         ~with_ignored:true
         ~cov_from_label
         ~cov_to_label
         merge_total_pcts) ;

    let null = open_out "/dev/null" in
    files
    |> List.iter (fun (
                    name,
                    html_file,
                    merge_total
                  ) ->
           let _dbg =
             let ch =
               if
                 (* name = "src/lib_shell/validator_event.ml" *)
                 name = "src/lib_base/tzPervasives.ml"
               then
                 Unix.out_channel_of_descr Unix.stderr
               else
                 null
             in Printf.fprintf ch
           in
           (* dbg "Name: %s\n" name ; *)

           (* dbg "Stat: %s\n" (Status_line.pp_stat stats) ; *)
           
           let merge_total_pcts =
             Status_line.percentage_from_merge merge_total in

           let meter_from_merge_pcts =
             let wrap s = {| <span class="meter"> |} ^ s ^ {| </span> |} in
             function
             | None -> wrap ""
             | Some (pct_removed, pct_both, pct_added, _pct_ignore) ->
                wrap @@
                  Printf.sprintf {|
                                  <span class="only-removed" style="width: %s%%"></span><span class="both" style="width: %s%%"></span><span class="only-added" style="width: %s%%"></span>
                                  |} pct_removed pct_both pct_added
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
        %s
        <span class="percentage">%s</span>
        <a href="%s">
          <span class="dirname">%s</span>%s
        </a>
      </div>
              |}
             (meter_from_merge_pcts merge_total_pcts)
             (colors_chart
                ~with_ignored:true
                ~with_tooltips:true
                ~cov_to_label
                ~cov_from_label
                merge_total_pcts)
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

let output_for_source_file
      tab_size
      ~title
      ~cov_from_label
      ~cov_to_label
      theme source_file_on_disk
    html_file_on_disk { Bisect_common.filename; points; counts } =
  let stats = ref (0, 0) in
  let points =
    points
    |> Array.to_list
    |> List.mapi (fun index offset -> (offset, index))
    |> List.sort compare
  in
  let pts =
    let len = Array.length counts in
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
  let merge_total = (try
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

     let merge_total =
       !lines_status |>
         List.map Status_line.merge |>
         Status_line.merge_sum
       (* let merges = List.map Status_line.merge !lines_status in
        * let lines = List.map Status_line.merge_to_line merges in
        * Status_line.stat_from_lines lines *)
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
       (theme_class theme)
       basename
       (make_title ~html:false ~cov_from_label ~cov_to_label title)
       file_coverage filename style_css
       highlight_js index_html dirname basename
       (colors_chart
          ~with_titles:true
          ~with_ignored:true
          ~cov_from_label
          ~cov_to_label
          (Status_line.percentage_from_merge merge_total));

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
              Status_line.(merge status |> merge_to_line |> line_to_class));

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
       coverage_js;
     merge_total
   with e ->
     close_in_noerr in_channel;
     close_out_noerr out_channel;
     raise e) in

  close_in_noerr in_channel;
  close_out_noerr out_channel;

  merge_total

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
    ~cov_from ~cov_to ?(cov_from_label="from") ?(cov_to_label="to") ~theme () =
  let cov_from = coverage cov_from in
  let cov_to = coverage cov_to in

  let coverage = merge cov_from cov_to in

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
              output_for_source_file
                tab_size
                 ~title
                 ~cov_from_label
                 ~cov_to_label
                theme
                source_file_on_disk
                html_file_on_disk file
            in
            (filename, html_file_relative, stats) :: acc)
      coverage []
  in

  (* Write the coverage report landing page. *)
  output_html_index ~title ~cov_from_label ~cov_to_label theme
    (Filename.concat to_directory "index.html")
    (List.sort
       (fun (name_x, _, _x) (name_y, _, _y) ->
         String.compare name_x name_y

         (* let open Status_line in
          * match compare x.only_removed y.only_removed with
          * | 0 -> compare x.removed_and_add y.removed_and_add
          * | x -> x *)
       )
       files);

  (* Write the asset files. *)
  output_string_to_separate_file Assets.js
    (Filename.concat to_directory "coverage.js");
  output_string_to_separate_file Assets.highlight_js
    (Filename.concat to_directory "highlight.pack.js");
  output_string_to_separate_file Assets.css
    (Filename.concat to_directory "coverage.css")
