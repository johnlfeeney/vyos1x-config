type 'a t [@@deriving yojson]

exception Empty_path
exception Duplicate_child
exception Nonexistent_path
exception Insert_error of string

type position = Before of string | After of string | Lexical | End | Default

val make : 'a -> string -> 'a t
val make_full : 'a -> string -> ('a t) list -> 'a t

val name_of_node : 'a t -> string
val data_of_node : 'a t -> 'a
val children_of_node : 'a t -> 'a t list

val find : 'a t -> string -> 'a t option
val find_or_fail : 'a t -> string -> 'a t

val adopt : 'a t -> 'a t -> 'a t

val replace : 'a t -> 'a t -> 'a t

val insert : ?position:position -> ?children:('a t list) -> 'a t -> string list -> 'a -> 'a t

val insert_maybe : ?position:position -> 'a t -> string list -> 'a -> 'a t

val insert_or_update : ?position:position -> 'a t -> string list -> 'a -> 'a t

val insert_multi_level : 'a -> 'a t -> string list -> string list -> 'a -> 'a t

val merge_children : ('a -> 'a -> 'a) -> (string -> string -> int) -> 'a t -> 'a t

val delete : 'a t -> string list -> 'a t

val update : 'a t -> string list -> 'a -> 'a t

val rename : 'a t -> string list -> string -> 'a t

val list_children : 'a t -> string list

val get : 'a t -> string list -> 'a t

val get_existent_path : 'a t -> string list -> string list

val get_data : 'a t -> string list -> 'a

val exists : 'a t -> string list -> bool

val children_of_path : 'a t -> string list -> string list

val sorted_children_of_node : (string -> string -> int) -> 'a t -> ('a t) list

val sort_children : (string -> string -> int) -> 'a t -> 'a t

val copy : 'a t -> string list -> string list -> 'a t

val move : 'a t -> string list -> position -> 'a t

val is_terminal_path : 'a t -> string list -> bool

val fold_tree_with_path: (string list * 'acc -> 'b t -> string list * 'acc) -> string list * 'acc -> 'b t -> string list * 'acc
