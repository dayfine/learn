open Base

let remove_expense_from_budget exs b =
  let f budget ex = budget - ex in
  List.fold_left ~f ~init:b exs

let length xs = List.fold_left ~f:(fun l _ -> l + 1) ~init:0 xs
let last xs = List.fold_left ~f:(fun _ x -> Option.Some x) ~init:Option.None xs
let rev xs = List.fold_left ~f:(fun l x -> x :: l) ~init:[] xs

let mem xs target =
  List.fold_left ~f:(fun found x -> found || equal target x) ~init:false xs

let concat xs =
  let f ws w =
    match (ws, w) with
    | "", "" -> ""
    | "", w -> w
    | ws, "" -> ws
    | ws, w -> ws ^ " " ^ w
  in
  List.fold_left ~f ~init:"" xs

type 'a tree = Lf | Br of 'a * 'a tree * 'a tree

let rec fold_tree f e t =
  match t with
  | Lf -> e
  | Br (x, l, r) -> f x (fold_tree f e l) (fold_tree f e r)

let max_tree_depth t = fold_tree (fun _ l r -> 1 + max l r) 1 t

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
let%test_unit "mem 1" = [%test_eq: bool] (mem [] 1) false
let%test_unit "mem 2" = [%test_eq: bool] (mem [ 3; 2; 1 ] 1) true
let%test_unit "concat 1" = [%test_eq: string] (concat []) ""
let%test_unit "concat 2" = [%test_eq: string] (concat [ "" ]) ""

let%test_unit "concat 3" =
  [%test_eq: string] (concat [ "foo"; "bar" ]) "foo bar"

let%test_unit "max_tree_depth empty tree" =
  [%test_eq: int]
    (let t = Lf in
     max_tree_depth t)
    1

let%test_unit "max_tree_depth tree" =
  [%test_eq: int]
    (let t = Br ((), Br ((), Br ((), Lf, Lf), Lf), Lf) in
     max_tree_depth t)
    4
