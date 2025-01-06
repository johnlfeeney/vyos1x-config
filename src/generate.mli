exception Load_error of string
exception Write_error of string

val load_interface_definitions : string -> (Reference_tree.t, string) result
val reference_tree_to_json : ?internal_cache:string -> string -> string -> unit
val interface_definitions_to_cache : string -> string -> unit
val reference_tree_cache_to_json : string -> string -> unit
val merge_reference_tree_cache : string -> string -> string -> unit
