(* This is Hamming(15,11) code implementation *)

(* Always 11 bits *)
let data = [ 1; 0; 1; 1; 0; 1; 0; 0; 0; 0; 1 ]
(* let flip x = match x with 0 -> 1 | 1 -> 0 | _ -> 0 *)

let update_list_at (list : 'a list) i (v : 'a) =
  let rec aux list i acc =
    match (list, i) with
    | [], _ -> List.rev acc
    | _ :: tl, 0 -> List.rev_append acc (v :: tl)
    | hd :: tl, _ -> aux tl (i - 1) (hd :: acc)
  in
  aux list i []

let next_parity_bit_index parity_bit_index =
  if parity_bit_index <= 0 then 1 else parity_bit_index * 2

let fill_unparity_bits list data =
  let rec aux list data i i_skip =
    if i = i_skip then aux list data (i + 1) i_skip
    else if i > i_skip then aux list data i (next_parity_bit_index i_skip)
    else
      match data with
      | [] -> list
      | hd :: tl ->
          let updated_list = update_list_at list i hd in
          aux updated_list tl (i + 1) i_skip
  in
  aux list data 0 0

let get_nth_col data n =
  let index = n - 1 in
  let next_index i = i + 4 in
  let rec aux n acc =
    try
      if List.length acc = 4 then acc
      else aux (next_index n) (List.nth data n :: acc)
    with ex ->
      List.length data |> Printf.sprintf "Length %d" |> print_endline;
      prerr_endline (Printexc.to_string ex);
      acc
  in
  aux index []

let get_nth_row data n =
  let index = (n - 1) * 4 in
  let next_index i = i + 1 in
  let rec aux n acc =
    if List.length acc = 4 then acc
    else aux (next_index n) (List.nth data n :: acc)
  in
  aux index []

let is_even int = if int mod 2 > 0 then false else true

let pretty_print data =
  let rec aux data acc i =
    match data with
    | [] -> acc
    | hd :: tl ->
        let acc = Printf.sprintf "%s%d " acc hd in
        if (i + 1) mod 4 = 0 then aux tl (acc ^ "\n") (i + 1)
        else aux tl acc (i + 1)
  in
  let str_to_print = aux data "" 0 in
  print_string str_to_print

let encode data =
  let empty = List.init 16 (fun _ -> 0) in
  let filled = fill_unparity_bits empty data in
  let second_col = get_nth_col filled 2 in
  let third_col = get_nth_col filled 3 in
  let forth_col = get_nth_col filled 4 in

  let second_row = get_nth_row filled 2 in
  let third_row = get_nth_row filled 3 in
  let forth_row = get_nth_row filled 4 in

  let second_forth_col_sum =
    List.fold_left2 (fun acc a b -> acc + a + b) 0 second_col forth_col
  in
  let third_forth_col_sum =
    List.fold_left2 (fun acc a b -> acc + a + b) 0 third_col forth_col
  in

  let second_forth_row_sum =
    List.fold_left2 (fun acc a b -> acc + a + b) 0 second_row forth_row
  in
  let third_forth_row_sum =
    List.fold_left2 (fun acc a b -> acc + a + b) 0 third_row forth_row
  in

  let parity_index = next_parity_bit_index 0 in
  let data =
    if not (is_even second_forth_col_sum) then
      update_list_at filled parity_index 1
    else filled
  in

  let parity_index = next_parity_bit_index parity_index in
  let data =
    if not (is_even third_forth_col_sum) then update_list_at data parity_index 1
    else data
  in

  let parity_index = next_parity_bit_index parity_index in
  let data =
    if not (is_even second_forth_row_sum) then
      update_list_at data parity_index 1
    else data
  in

  let parity_index = next_parity_bit_index parity_index in
  let data =
    if not (is_even third_forth_row_sum) then update_list_at data parity_index 1
    else data
  in

  let all_sum = List.fold_left Int.add 0 data in
  if not (is_even all_sum) then update_list_at data 0 1 else data

let () = encode data |> pretty_print
