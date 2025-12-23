type color = Red | Black

type ('k, 'v) tree =
  | Empty
  | Node of color * ('k, 'v) tree Lazy.t * 'k * 'v * ('k, 'v) tree Lazy.t

type ('k, 'v) t = ('k, 'v) tree

let empty = Empty

let is_empty = function Empty -> true | _ -> false

let balance = function
  | ( Black,
      (lazy (Node (Red, a, xk, xv, (lazy (Node (Red, b, yk, yv, c)))))),
      zk,
      zv,
      d )
  | ( Black,
      (lazy (Node (Red, (lazy (Node (Red, a, xk, xv, b))), yk, yv, c))),
      zk,
      zv,
      d )
  | ( Black,
      a,
      xk,
      xv,
      (lazy (Node (Red, (lazy (Node (Red, b, yk, yv, c))), zk, zv, d))) )
  | ( Black,
      a,
      xk,
      xv,
      (lazy (Node (Red, b, yk, yv, (lazy (Node (Red, c, zk, zv, d)))))) ) ->
      let left_tree = lazy (Node (Black, a, xk, xv, b)) in
      let right_tree = lazy (Node (Black, c, zk, zv, d)) in
      Node (Red, left_tree, yk, yv, right_tree)
  | color, left, key, value, right -> Node (color, left, key, value, right)

let insert key value tree =
  let rec ins = function
    | Empty -> Node (Red, lazy Empty, key, value, lazy Empty)
    | Node (color, left, k, v, right) ->
        let cmp = compare key k in
        if cmp < 0 then
          balance (color, lazy (ins (Lazy.force left)), k, v, right)
        else if cmp > 0 then
          balance (color, left, k, v, lazy (ins (Lazy.force right)))
        else Node (color, left, key, value, right)
  in
  match ins tree with
  | Node (_, left, k, v, right) -> Node (Black, left, k, v, right)
  | Empty -> Empty

let rec find key = function
  | Empty -> None
  | Node (_, left, k, v, right) ->
      let cmp = compare key k in
      if cmp < 0 then find key (Lazy.force left)
      else if cmp > 0 then find key (Lazy.force right)
      else Some v

let mem key tree = match find key tree with Some _ -> true | None -> false

let fold_left f acc tree =
  let rec go acc = function
    | Empty -> acc
    | Node (_, left, k, v, right) ->
        let acc = go acc (Lazy.force left) in
        let acc = f acc k v in
        go acc (Lazy.force right)
  in
  go acc tree

let fold_right f tree acc =
  let rec go acc = function
    | Empty -> acc
    | Node (_, left, k, v, right) ->
        let acc = go acc (Lazy.force right) in
        let acc = f k v acc in
        go acc (Lazy.force left)
  in
  go acc tree

let to_list tree =
  fold_right (fun k v acc -> (k, v) :: acc) tree []

let size tree = fold_left (fun acc _ _ -> acc + 1) 0 tree

let map f =
  let rec go = function
    | Empty -> Empty
    | Node (color, left, k, v, right) ->
        Node
          ( color,
            lazy (go (Lazy.force left)),
            k,
            f v,
            lazy (go (Lazy.force right)) )
  in
  go

let filter pred tree =
  fold_left
    (fun acc k v -> if pred k v then insert k v acc else acc)
    empty tree

let remove key tree =
  fold_left
    (fun acc k v -> if compare k key = 0 then acc else insert k v acc)
    empty tree

let mempty = Empty

let mappend t1 t2 =
  fold_left (fun acc k v -> insert k v acc) t1 t2

let of_list lst = List.fold_left (fun acc (k, v) -> insert k v acc) empty lst

let equal t1 t2 =
  let rec push_left stack = function
    | Empty -> stack
    | Node (_, left, k, v, right) ->
        push_left ((k, v, right) :: stack) (Lazy.force left)
  in
  let next = function
    | [] -> None
    | (k, v, right) :: stack ->
        let stack = push_left stack (Lazy.force right) in
        Some ((k, v), stack)
  in
  let rec go s1 s2 =
    match (next s1, next s2) with
    | None, None -> true
    | Some ((k1, v1), s1'), Some ((k2, v2), s2') ->
        compare k1 k2 = 0 && v1 = v2 && go s1' s2'
    | _ -> false
  in
  go (push_left [] t1) (push_left [] t2)

let to_string key_to_string value_to_string tree =
  let pairs = to_list tree in
  let pair_strings =
    List.map (fun (k, v) -> key_to_string k ^ " -> " ^ value_to_string v) pairs
  in
  "{" ^ String.concat ", " pair_strings ^ "}"
