type node_type = Leaf | Tag | Other

type value_constraint =
    | Regex of string [@name "regex"]
    | External of string * string option [@name "exec"]
    [@@deriving yojson]

type ref_node_data = {
    node_type: node_type;
    constraints: value_constraint list;
    help: string;
    value_help: (string * string) list;
    constraint_error_message: string;
    multi: bool;
    valueless: bool;
    owner: string option;
    priority: string option;
    keep_order: bool;
    hidden: bool;
    secret: bool;
}

type t = ref_node_data Vytree.t

exception Bad_interface_definition of string

exception Validation_error of string

let default_data = {
    node_type = Other;
    constraints = [];
    help = "No help available";
    value_help = [];
    constraint_error_message = "Invalid value";
    multi = false;
    valueless = false;
    owner = None;
    priority = None;
    keep_order = false;
    hidden = false;
    secret = false;
}

let default = Vytree.make default_data ""

(* Loading from XML *)

let node_type_of_string s =
    match s with
    | "node" -> Other
    | "tagNode" -> Tag
    | "leafNode" -> Leaf
    | _ -> raise (Bad_interface_definition
                  (Printf.sprintf "node, tagNode, or leafNode expected, %s found" s))

let load_constraint_from_xml d c =
    let aux d c =
        match c with
        | Xml.Element ("regex", _, [Xml.PCData s]) ->
            let cs = (Regex s) :: d.constraints in
            {d with constraints=cs}
        | Xml.Element ("validator", [("name", n); ("argument", a)], _) ->
            let cs = (External (n, Some a)) :: d.constraints in
            {d with constraints=cs}
        | Xml.Element ("validator", [("name", n)], _) ->
            let cs = (External (n, None)) :: d.constraints in
            {d with constraints=cs}
        | _ -> raise (Bad_interface_definition "Malformed constraint")
    in Xml.fold aux d c

(** Find a child node in xml-lite *)
let find_xml_child name xml =
    let find_aux e =
        match e with
        | Xml.Element (name', _, _) when name' = name -> true
        | _ -> false
    in
    match xml with
    | Xml.Element (_, _, children) -> Vylist.find find_aux children
    | Xml.PCData _ -> None

let data_from_xml d x =
    let aux d x =
        match x with
        | Xml.Element ("help", _, [Xml.PCData s]) -> {d with help=s}
        | Xml.Element ("valueHelp", _,
                       [Xml.Element ("format", _, [Xml.PCData fmt]);
                        Xml.Element ("description", _, [Xml.PCData descr])]) ->
            let vhs = d.value_help in
            let vhs' = (fmt, descr) :: vhs in
            {d with value_help=vhs'}
        | Xml.Element ("multi", _, _) -> {d with multi=true}
        | Xml.Element ("valueless", _, _) -> {d with valueless=true}
        | Xml.Element ("constraintErrorMessage", _, [Xml.PCData s]) ->
            {d with constraint_error_message=s}
        | Xml.Element ("constraint", _, _) -> load_constraint_from_xml d x
        | Xml.Element ("priority", _, [Xml.PCData i]) ->
            {d with priority=Some i}
        | Xml.Element ("hidden", _, _) -> {d with hidden=true}
        | Xml.Element ("secret", _, _) -> {d with secret=true}
        | Xml.Element ("keepChildOrder", _, _) -> {d with keep_order=true}
        | _ -> raise (Bad_interface_definition "Malformed property tag")
    in Xml.fold aux d x

let rec insert_from_xml basepath reftree xml =
    match xml with
    | Xml.Element (_, _,  _) ->
        let props = find_xml_child "properties" xml in
        let data =
            (match props with
            | None -> default_data
            | Some p -> data_from_xml default_data p)
        in
        let node_type = node_type_of_string (Xml.tag xml) in
        let node_owner = try let o = Xml.attrib xml "owner" in Some o
                         with _ -> None
        in
        let data = {data with node_type=node_type; owner=node_owner} in
        let name = Xml.attrib xml "name" in
        let path = basepath @ [name] in
        let new_tree = Vytree.insert reftree path data in
        (match node_type with
        | Leaf -> new_tree
        | _ ->
            let children = find_xml_child "children" xml in
            (match children with
             | None -> raise (Bad_interface_definition (Printf.sprintf "Node %s has no children" name))
             | Some c ->  List.fold_left (insert_from_xml path) new_tree (Xml.children c)))
    | _ -> raise (Bad_interface_definition "PCData not allowed here")

let load_from_xml reftree file =
    let xml_to_reftree xml reftree =
        match xml with
        | Xml.Element ("interfaceDefinition", _, children) ->
            List.fold_left (insert_from_xml []) reftree children
        | _ -> raise (Bad_interface_definition "Should start with <interfaceDefinition>")
    in
    try
        let xml = Xml.parse_file file in
        xml_to_reftree xml reftree
    with
    | Xml.File_not_found msg -> raise (Bad_interface_definition msg)
    | Xml.Error e -> raise (Bad_interface_definition (Xml.error e))

let is_multi reftree path =
    let data = Vytree.get_data reftree path in
    data.multi

let is_hidden reftree path =
    let data = Vytree.get_data reftree path in
    data.hidden

let is_secret reftree path =
    let data = Vytree.get_data reftree path in
    data.secret

let is_tag reftree path =
    let data = Vytree.get_data reftree path in
    match data.node_type with
    | Tag -> true
    | _ -> false

let is_leaf reftree path =
    let data = Vytree.get_data reftree path in
    match data.node_type with
    | Leaf -> true
    | _ -> false

let is_valueless reftree path =
    let data = Vytree.get_data reftree path in
    data.valueless

let get_keep_order reftree path =
    let data = Vytree.get_data reftree path in
    data.keep_order

let get_owner reftree path =
    let data = Vytree.get_data reftree path in
    data.owner

let get_help_string reftree path =
    let data = Vytree.get_data reftree path in
    data.help

let get_value_help reftree path =
    let data = Vytree.get_data reftree path in
    data.value_help

let get_completion_data reftree path =
    let aux node =
        let data = Vytree.data_of_node node in
        (data.node_type, data.multi, data.help)
    in List.map aux (Vytree.children_of_node @@ Vytree.get reftree path)
