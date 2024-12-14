let get_readable_time {Unix.tm_hour; Unix.tm_min; _} 
  = (string_of_int @@ tm_hour + 1) ^ ":" ^ (string_of_int tm_min)

let s =
  let tm = Unix.gmtime (Unix.time()) in
  "It's " ^ (get_readable_time tm) 
  ^ " on "
;; 

print_endline s
