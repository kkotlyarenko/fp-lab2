(** Interface file for Red-Black Tree Dictionary with Lazy Evaluation *)

(** Color of Red-Black Tree nodes *)
type color = Red | Black

(** Red-Black Tree structure with lazy evaluation *)
type ('k, 'v) tree

(** Dictionary type *)
type ('k, 'v) t = ('k, 'v) tree

(** {1 Basic Operations} *)

(** Empty dictionary *)
val empty : ('k, 'v) t

(** Check if dictionary is empty *)
val is_empty : ('k, 'v) t -> bool

(** Insert a key-value pair into the dictionary *)
val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t

(** Find a value by key *)
val find : 'k -> ('k, 'v) t -> 'v option

(** Check if a key exists in the dictionary *)
val mem : 'k -> ('k, 'v) t -> bool

(** Remove a key from the dictionary *)
val remove : 'k -> ('k, 'v) t -> ('k, 'v) t

(** {1 Higher-Order Functions} *)

(** Map function over dictionary values *)
val map : ('v -> 'w) -> ('k, 'v) t -> ('k, 'w) t

(** Filter dictionary by predicate *)
val filter : ('k -> 'v -> bool) -> ('k, 'v) t -> ('k, 'v) t

(** Fold left over dictionary *)
val fold_left : ('acc -> 'k -> 'v -> 'acc) -> 'acc -> ('k, 'v) t -> 'acc

(** Fold right over dictionary *)
val fold_right : ('k -> 'v -> 'acc -> 'acc) -> ('k, 'v) t -> 'acc -> 'acc

(** {1 Monoid Operations} *)

(** Monoid: empty element *)
val mempty : ('k, 'v) t

(** Monoid: combine two dictionaries (right-biased for duplicate keys) *)
val mappend : ('k, 'v) t -> ('k, 'v) t -> ('k, 'v) t

(** {1 Conversion Functions} *)

(** Create dictionary from list *)
val of_list : ('k * 'v) list -> ('k, 'v) t

(** Convert dictionary to list *)
val to_list : ('k, 'v) t -> ('k * 'v) list

(** {1 Utility Functions} *)

(** Get size of dictionary *)
val size : ('k, 'v) t -> int

(** Check equality of two dictionaries *)
val equal : ('k, 'v) t -> ('k, 'v) t -> bool

(** String representation of dictionary *)
val to_string : ('k -> string) -> ('v -> string) -> ('k, 'v) t -> string
