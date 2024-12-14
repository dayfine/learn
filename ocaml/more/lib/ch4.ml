open Base

type input = {
  pos_in : unit -> int;
  seek_in : int -> unit;
  input_char : unit -> char option;
  in_channel_length : int;
}

let input_of_channel (ch : Stdio.In_channel.t) : input =
  let open Int64 in
  {
    pos_in = (fun () -> Stdio.In_channel.pos ch |> to_int_trunc);
    seek_in = (fun i -> Stdio.In_channel.seek ch @@ of_int i);
    input_char = (fun () -> Stdio.In_channel.input_char ch);
    in_channel_length = Stdio.In_channel.length ch |> to_int_trunc;
  }

let input_of_string (s : string) : input =
  let pos = ref 0 in
  {
    pos_in = (fun () -> !pos);
    seek_in =
      (fun p ->
        if p < 0 then raise (Invalid_argument "seek before beginning");
        pos := p);
    input_char =
      (fun () ->
        if !pos > String.length s - 1 then None
        else
          let c = s.[!pos] in
          pos := !pos + 1;
          Some c);
    in_channel_length = String.length s;
  }

let input_of_char_array (a : char array) : input =
  let pos = ref 0 in
  {
    pos_in = (fun () -> !pos);
    seek_in =
      (fun p ->
        if p < 0 then raise (Invalid_argument "seek before beginning");
        pos := p);
    input_char =
      (fun () ->
        if !pos > Array.length a - 1 then None
        else
          let c = Array.get a !pos in
          pos := !pos + 1;
          Some c);
    in_channel_length = Array.length a;
  }

let rewind (i : input) : unit = i.seek_in (i.pos_in () - 1)

let is_non_letter x =
  match x with
  | ' ' | '!' | '(' | ')' | '.' | ',' | ';' | ':' -> true
  | _ -> false

let rec skip_characters (i : input) : unit =
  match i.input_char () with
  | Some c -> if is_non_letter c then skip_characters i else rewind i
  | None -> raise End_of_file

let rec collect_characters (filter_fn : char -> bool) (b : Buffer.t) (i : input)
    : string =
  match i.input_char () with
  | None -> Buffer.contents b
  | Some c ->
      if filter_fn c then (
        Buffer.add_char b c;
        collect_characters filter_fn b i)
      else Buffer.contents b

let input_string (i : input) : string =
  collect_characters (Fn.const true) (Buffer.create 100) i

let read_word (i : input) : string option =
  try
    skip_characters i;
    Some
      (collect_characters
         (fun x -> not @@ is_non_letter x)
         (Buffer.create 20) i)
  with End_of_file -> None

let rec read_words_inner (i : input) (a : string list) : string list =
  match read_word i with
  | None -> List.rev (List.map ~f:String.lowercase a)
  | Some w -> read_words_inner i (w :: a)

let read_words (i : input) = read_words_inner i []

type output = { output_char : char -> unit; out_channel_length : unit -> int }

let output_of_channel (ch : Stdio.Out_channel.t) : output =
  {
    output_char = (fun c -> Stdio.Out_channel.output_byte ch (Char.to_int c));
    out_channel_length =
      (fun () -> Stdio.Out_channel.length ch |> Int64.to_int_trunc);
  }

let output_of_bytes (b : bytes) : output =
  let pos = ref 0 in
  {
    output_char =
      (fun c ->
        if !pos < Bytes.length b then (
          Bytes.set b !pos c;
          pos := !pos + 1)
        else raise End_of_file);
    out_channel_length = (fun () -> Bytes.length b);
  }

let output_of_buffer (b : Buffer.t) : output =
  {
    output_char = (fun c -> Buffer.add_char b c);
    out_channel_length = (fun () -> Buffer.length b);
  }

let output_int_list (o : output) (ls : int list) : unit =
  o.output_char '[';
  List.iteri
    ~f:(fun i n ->
      String.iter ~f:o.output_char (Int.to_string n);
      o.output_char ';';
      if Int.equal i (List.length ls - 1) then () else o.output_char ' ')
    ls;
  o.output_char ']'

let%test_unit "read words from string" =
  [%test_eq: string list]
    (read_words @@ input_of_string "There were four of them; more than before")
    [ "there"; "were"; "four"; "of"; "them"; "more"; "than"; "before" ]

let%test_unit "input from and to string" =
  [%test_eq: string]
    (input_string @@ input_of_string "There were four of them; more than before")
    "There were four of them; more than before"

let%test_unit "read words from char array" =
  [%test_eq: string list]
    (read_words
    @@ input_of_char_array
         (let s = "There were four of them; more than before" in
          Array.init (String.length s) ~f:(String.get s)))
    [ "there"; "were"; "four"; "of"; "them"; "more"; "than"; "before" ]

let%test_unit "output_int_list with bytes" =
  [%test_eq: string]
    (let b = Bytes.create 16 in
     output_int_list (output_of_bytes b) [ 1; 2; 3; 4; 5 ];
     Bytes.to_string b)
    "[1; 2; 3; 4; 5;]"

let%test_unit "output_int_list with buffer" =
  [%test_eq: string]
    (let b = Buffer.create 16 in
     output_int_list (output_of_buffer b) [ 1; 2; 3; 4; 5 ];
     Buffer.contents b)
    "[1; 2; 3; 4; 5;]"
