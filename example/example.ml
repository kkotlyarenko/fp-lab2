(** Example usage of Red-Black Tree Dictionary with Lazy Evaluation *)

open Rb_dict_lazy

let () =
  Printf.printf "=== Red-Black Tree Dictionary Example ===\n\n";

  (* Create empty dictionary *)
  let dict = empty in
  Printf.printf "1. Created empty dictionary\n";
  Printf.printf "   Size: %d\n\n" (size dict);

  (* Insert elements *)
  let dict =
    dict
    |> insert 1 "one"
    |> insert 2 "two"
    |> insert 3 "three"
    |> insert 4 "four"
    |> insert 5 "five"
  in
  Printf.printf "2. Inserted 5 elements\n";
  Printf.printf "   Size: %d\n" (size dict);
  Printf.printf "   Contents: %s\n\n"
    (to_string string_of_int (fun s -> s) dict);

  (* Find elements *)
  Printf.printf "3. Finding elements:\n";
  Printf.printf "   find 3: %s\n"
    (match find 3 dict with Some v -> v | None -> "Not found");
  Printf.printf "   find 10: %s\n\n"
    (match find 10 dict with Some v -> v | None -> "Not found");

  (* Map operation *)
  let dict_upper = map String.uppercase_ascii dict in
  Printf.printf "4. Mapped to uppercase:\n";
  Printf.printf "   Contents: %s\n\n"
    (to_string string_of_int (fun s -> s) dict_upper);

  (* Filter operation *)
  let dict_filtered = filter (fun k _ -> k > 2) dict in
  Printf.printf "5. Filtered (key > 2):\n";
  Printf.printf "   Size: %d\n" (size dict_filtered);
  Printf.printf "   Contents: %s\n\n"
    (to_string string_of_int (fun s -> s) dict_filtered);

  (* Fold operation *)
  let keys_sum = fold_left (fun acc k _ -> acc + k) 0 dict in
  Printf.printf "6. Sum of keys (fold_left): %d\n\n" keys_sum;

  (* Monoid operations *)
  let dict1 = empty |> insert 1 "a" |> insert 2 "b" in
  let dict2 = empty |> insert 2 "B" |> insert 3 "c" in
  let merged = mappend dict1 dict2 in
  Printf.printf "7. Monoid mappend (right-biased):\n";
  Printf.printf "   dict1: %s\n"
    (to_string string_of_int (fun s -> s) dict1);
  Printf.printf "   dict2: %s\n"
    (to_string string_of_int (fun s -> s) dict2);
  Printf.printf "   merged: %s\n" (to_string string_of_int (fun s -> s) merged);
  Printf.printf "   (Notice key 2 has value 'B' from dict2)\n\n";

  (* Remove operation *)
  let dict_removed = remove 3 dict in
  Printf.printf "8. Removed key 3:\n";
  Printf.printf "   Size: %d\n" (size dict_removed);
  Printf.printf "   Contents: %s\n\n"
    (to_string string_of_int (fun s -> s) dict_removed);

  (* Convert to/from list *)
  let pairs = to_list dict in
  Printf.printf "9. Convert to list:\n";
  Printf.printf "   %s\n"
    (String.concat ", "
       (List.map (fun (k, v) -> Printf.sprintf "(%d, %s)" k v) pairs));
  let dict_from_list = of_list [ (10, "ten"); (20, "twenty"); (30, "thirty") ] in
  Printf.printf "   From list: %s\n\n"
    (to_string string_of_int (fun s -> s) dict_from_list);

  Printf.printf "=== Example completed successfully! ===\n"
