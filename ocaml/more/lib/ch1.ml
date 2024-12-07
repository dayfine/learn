open Base


let remove_expense_from_budget exs b = 
  let f budget ex = budget - ex
  in List.fold_left ~f:f ~init:b exs

let%test_unit "remove_expense_from_budget 1" =
  [%test_eq: int] (remove_expense_from_budget [1;2;3] 10) 4
let%test_unit "remove_expense_from_budget 2" =
  [%test_eq: int] (remove_expense_from_budget [3;2;1;2;3] 20) 9
