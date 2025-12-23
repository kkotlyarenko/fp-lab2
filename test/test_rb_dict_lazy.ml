open Rb_dict_lazy

let test_empty () =
  let dict = empty in
  Alcotest.(check bool) "empty dict is empty" true (is_empty dict);
  Alcotest.(check int) "empty dict has size 0" 0 (size dict)

let test_insert_find () =
  let dict = empty |> insert 1 "one" |> insert 2 "two" |> insert 3 "three" in
  Alcotest.(check bool) "dict is not empty" false (is_empty dict);
  Alcotest.(check int) "dict has size 3" 3 (size dict);
  Alcotest.(check (option string)) "find 1" (Some "one") (find 1 dict);
  Alcotest.(check (option string)) "find 2" (Some "two") (find 2 dict);
  Alcotest.(check (option string)) "find 3" (Some "three") (find 3 dict);
  Alcotest.(check (option string)) "find 4 (not found)" None (find 4 dict)

let test_update () =
  let dict = empty |> insert 1 "one" |> insert 1 "ONE" in
  Alcotest.(check (option string)) "updated value" (Some "ONE") (find 1 dict);
  Alcotest.(check int) "size after update" 1 (size dict)

let test_remove () =
  let dict =
    empty |> insert 1 "one" |> insert 2 "two" |> insert 3 "three" |> remove 2
  in
  Alcotest.(check int) "size after remove" 2 (size dict);
  Alcotest.(check (option string)) "removed key not found" None (find 2 dict);
  Alcotest.(check (option string)) "other keys still exist" (Some "one")
    (find 1 dict)

let test_mem () =
  let dict = empty |> insert 1 "one" |> insert 2 "two" in
  Alcotest.(check bool) "mem 1" true (mem 1 dict);
  Alcotest.(check bool) "mem 2" true (mem 2 dict);
  Alcotest.(check bool) "mem 3" false (mem 3 dict)

let test_map () =
  let dict = empty |> insert 1 10 |> insert 2 20 |> insert 3 30 in
  let mapped = map (fun v -> v * 2) dict in
  Alcotest.(check (option int)) "mapped 1" (Some 20) (find 1 mapped);
  Alcotest.(check (option int)) "mapped 2" (Some 40) (find 2 mapped);
  Alcotest.(check (option int)) "mapped 3" (Some 60) (find 3 mapped)

let test_filter () =
  let dict =
    empty |> insert 1 10 |> insert 2 20 |> insert 3 30 |> insert 4 40
  in
  let filtered = filter (fun _ v -> v > 20) dict in
  Alcotest.(check int) "filtered size" 2 (size filtered);
  Alcotest.(check bool) "1 filtered out" false (mem 1 filtered);
  Alcotest.(check bool) "2 filtered out" false (mem 2 filtered);
  Alcotest.(check bool) "3 remains" true (mem 3 filtered);
  Alcotest.(check bool) "4 remains" true (mem 4 filtered)

let test_fold_left () =
  let dict = empty |> insert 1 10 |> insert 2 20 |> insert 3 30 in
  let sum = fold_left (fun acc _ v -> acc + v) 0 dict in
  Alcotest.(check int) "fold_left sum" 60 sum

let test_fold_right () =
  let dict = empty |> insert 1 10 |> insert 2 20 |> insert 3 30 in
  let sum = fold_right (fun _ v acc -> acc + v) dict 0 in
  Alcotest.(check int) "fold_right sum" 60 sum

let test_mempty () =
  let dict = mempty in
  Alcotest.(check bool) "mempty is empty" true (is_empty dict)

let test_mappend () =
  let dict1 = empty |> insert 1 "one" |> insert 2 "two" in
  let dict2 = empty |> insert 2 "TWO" |> insert 3 "three" in
  let merged = mappend dict1 dict2 in
  Alcotest.(check int) "merged size" 3 (size merged);
  Alcotest.(check (option string)) "merged 1" (Some "one") (find 1 merged);
  Alcotest.(check (option string)) "merged 2 (right-biased)" (Some "TWO")
    (find 2 merged);
  Alcotest.(check (option string)) "merged 3" (Some "three") (find 3 merged)

let test_list_conversion () =
  let pairs = [ (1, "one"); (2, "two"); (3, "three") ] in
  let dict = of_list pairs in
  let result = to_list dict |> List.sort compare in
  let expected = List.sort compare pairs in
  Alcotest.(check (list (pair int string))) "list conversion" expected result

let test_equal () =
  let dict1 = empty |> insert 1 "one" |> insert 2 "two" in
  let dict2 = empty |> insert 2 "two" |> insert 1 "one" in
  let dict3 = empty |> insert 1 "one" in
  Alcotest.(check bool) "equal dicts" true (equal dict1 dict2);
  Alcotest.(check bool) "not equal dicts" false (equal dict1 dict3)

let test_large_dict () =
  let rec make_large n dict =
    if n <= 0 then dict else make_large (n - 1) (insert n (string_of_int n) dict)
  in
  let dict = make_large 100 empty in
  Alcotest.(check int) "large dict size" 100 (size dict);
  Alcotest.(check (option string)) "large dict find 50" (Some "50")
    (find 50 dict);
  Alcotest.(check (option string)) "large dict find 100" (Some "100")
    (find 100 dict)

let unit_tests =
  [
    ("empty", `Quick, test_empty);
    ("insert and find", `Quick, test_insert_find);
    ("update", `Quick, test_update);
    ("remove", `Quick, test_remove);
    ("mem", `Quick, test_mem);
    ("map", `Quick, test_map);
    ("filter", `Quick, test_filter);
    ("fold_left", `Quick, test_fold_left);
    ("fold_right", `Quick, test_fold_right);
    ("monoid mempty", `Quick, test_mempty);
    ("monoid mappend", `Quick, test_mappend);
    ("list conversion", `Quick, test_list_conversion);
    ("equal", `Quick, test_equal);
    ("large dictionary", `Quick, test_large_dict);
  ]

let gen_small_nat = QCheck.Gen.(0 -- 1000)

let arb_small_nat = QCheck.make gen_small_nat

let dict_gen =
  QCheck.Gen.(
    list (pair gen_small_nat gen_small_nat) >|= fun pairs -> of_list pairs)

let dict_arb = QCheck.make dict_gen

let prop_insert_find =
  QCheck.Test.make ~count:1000 ~name:"insert then find"
    QCheck.(pair arb_small_nat arb_small_nat)
    (fun (k, v) ->
      let dict = insert k v empty in
      find k dict = Some v)

let prop_monoid_left_identity =
  QCheck.Test.make ~count:1000 ~name:"monoid left identity" dict_arb (fun dict ->
      equal (mappend mempty dict) dict)

let prop_monoid_right_identity =
  QCheck.Test.make ~count:1000 ~name:"monoid right identity" dict_arb (fun dict ->
      equal (mappend dict mempty) dict)

let prop_monoid_associativity =
  QCheck.Test.make ~count:1000 ~name:"monoid associativity"
    QCheck.(triple dict_arb dict_arb dict_arb)
    (fun (d1, d2, d3) ->
      let left = mappend (mappend d1 d2) d3 in
      let right = mappend d1 (mappend d2 d3) in
      equal left right)

let prop_map_preserves_size =
  QCheck.Test.make ~count:1000 ~name:"map preserves size" dict_arb (fun dict ->
      let mapped = map (fun v -> v + 1) dict in
      size dict = size mapped)

let prop_filter_size =
  QCheck.Test.make ~count:1000 ~name:"filter size" dict_arb (fun dict ->
      let filtered = filter (fun k _ -> k mod 2 = 0) dict in
      size filtered <= size dict)

let prop_list_roundtrip =
  QCheck.Test.make ~count:1000 ~name:"list roundtrip" dict_arb (fun dict ->
      let pairs = to_list dict in
      let dict' = of_list pairs in
      equal dict dict')

let prop_remove_find =
  QCheck.Test.make ~count:1000 ~name:"remove then find"
    QCheck.(pair dict_arb arb_small_nat)
    (fun (dict, k) ->
      let dict' = remove k dict in
      find k dict' = None)

let prop_fold_equivalence =
  QCheck.Test.make ~count:1000 ~name:"fold equivalence" dict_arb (fun dict ->
      let sum_left = fold_left (fun acc _ v -> acc + v) 0 dict in
      let sum_right = fold_right (fun _ v acc -> acc + v) dict 0 in
      sum_left = sum_right)

let property_tests =
  [
    ( "insert then find",
      `Quick,
      fun () -> QCheck.Test.check_exn prop_insert_find );
    ( "monoid left identity",
      `Quick,
      fun () -> QCheck.Test.check_exn prop_monoid_left_identity );
    ( "monoid right identity",
      `Quick,
      fun () -> QCheck.Test.check_exn prop_monoid_right_identity );
    ( "monoid associativity",
      `Quick,
      fun () -> QCheck.Test.check_exn prop_monoid_associativity );
    ( "map preserves size",
      `Quick,
      fun () -> QCheck.Test.check_exn prop_map_preserves_size );
    ( "filter size",
      `Quick,
      fun () -> QCheck.Test.check_exn prop_filter_size );
    ( "list roundtrip",
      `Quick,
      fun () -> QCheck.Test.check_exn prop_list_roundtrip );
    ( "remove then find",
      `Quick,
      fun () -> QCheck.Test.check_exn prop_remove_find );
    ( "fold equivalence",
      `Quick,
      fun () -> QCheck.Test.check_exn prop_fold_equivalence );
  ]

let () =
  Alcotest.run "Red-Black Tree Dictionary with Lazy Evaluation"
    [
      ("Unit tests", unit_tests);
      ("Property-based tests", property_tests);
    ]
