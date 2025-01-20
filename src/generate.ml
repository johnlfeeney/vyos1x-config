(* Load interface definitions from a directory into a reference tree *)
exception Load_error of string
exception Write_error of string

module I = Internal.Make(Reference_tree)

let load_interface_definitions dir =
    let open Reference_tree in
    let dir_paths = FileUtil.ls dir in
    let relative_paths =
      List.filter (fun x -> Filename.extension x = ".xml") dir_paths
    in
    let absolute_paths =
        try Ok (List.map Util.absolute_path relative_paths)
        with Sys_error no_dir_msg -> Error no_dir_msg
    in
    let load_aux tree file =
        load_from_xml tree file
    in
    try begin match absolute_paths with
        | Ok paths  -> Ok (List.fold_left load_aux default paths)
        | Error msg -> Error msg end
    with Bad_interface_definition msg -> Error msg

let interface_definitions_to_cache from_dir cache_path =
    let ref_tree_result =
        load_interface_definitions from_dir
    in
    let ref_tree =
    match ref_tree_result with
        | Ok ref -> ref
        | Error msg -> raise (Load_error msg)
    in
    I.write_internal ref_tree cache_path

let reference_tree_cache_to_json cache_path render_file =
    let ref_tree =
        I.read_internal cache_path
    in
    let out = Reference_tree.render_json ref_tree in
    let oc =
        try
            open_out render_file
        with Sys_error msg -> raise (Write_error msg)
    in
    Printf.fprintf oc "%s" out;
    close_out oc

let merge_reference_tree_cache cache_dir primary_name result_name =
    let file_arr = Sys.readdir cache_dir in
    let file_list' = Array.to_list file_arr in
    let file_list =
        List.filter (fun x -> x <> primary_name && x <> result_name) file_list' in
    let file_path_list =
        List.map (FilePath.concat cache_dir) file_list in
    let primary_tree = I.read_internal (FilePath.concat cache_dir primary_name) in
    let ref_trees = List.map I.read_internal file_path_list in
    match ref_trees with
    | [] ->
        I.write_internal primary_tree (FilePath.concat cache_dir result_name)
    | _ ->
        let f _ v = v in
        let res = List.fold_left (fun p r -> Tree_alg.RefAlg.tree_union r p f) primary_tree ref_trees in
        I.write_internal res (FilePath.concat cache_dir result_name)

let reference_tree_to_json ?(internal_cache="") from_dir to_file =
    let ref_tree_result =
        load_interface_definitions from_dir
    in
    let ref_tree =
    match ref_tree_result with
        | Ok ref -> ref
        | Error msg -> raise (Load_error msg)
    in
    let out = Reference_tree.render_json ref_tree in
    let oc =
        try
            open_out to_file
        with Sys_error msg -> raise (Write_error msg)
    in
    Printf.fprintf oc "%s" out;
    close_out oc;
    match internal_cache with
    | "" -> ()
    | _ -> I.write_internal ref_tree internal_cache
