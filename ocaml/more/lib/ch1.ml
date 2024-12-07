open Base

let remove_expense_from_budget exs b =
  let f budget ex = budget - ex in
  List.fold_left ~f ~init:b exs

let length xs = List.fold_left ~f:(fun l _ -> l + 1) ~init:0 xs
let last xs = List.fold_left ~f:(fun _ x -> Option.Some x) ~init:Option.None xs
let rev xs = List.fold_left ~f:(fun l x -> x :: l) ~init:[] xs


let%test_unit "remove_expense_from_budget 1" =
  [%test_eq: int] (remove_expense_from_budget [ 1; 2; 3 ] 10) 4

let%test_unit "remove_expense_from_budget 2" =
  [%test_eq: int] (remove_expense_from_budget [ 3; 2; 1; 2; 3 ] 20) 9

let%test_unit "length 1" = [%test_eq: int] (length [ 1; 2; 3 ]) 3
let%test_unit "length 2" = [%test_eq: int] (length []) 0

let%test_unit "last 1" =
  [%test_eq: int option] (last [ 1; 2; 3 ]) (Option.Some 3)

let%test_unit "last 2" = [%test_eq: int option] (last []) Option.None
let%test_unit "rev 1" = [%test_eq: int list] (rev [ 1; 2; 3 ]) [ 3; 2; 1 ]
let%test_unit "rev 2" = [%test_eq: int list] (rev []) []
