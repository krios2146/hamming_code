(* This is Hamming(15,11) code implementation *)

(* Always 11 bits *)
let data = [ 1; 0; 1; 1; 0; 1; 0; 0; 0; 0; 1 ]
let flip x = match x with 0 -> 1 | 1 -> 0 | _ -> 0

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

let calculate_error_bit_pos first_parity_bit_sum second_parity_bit_sum =
  match (is_even first_parity_bit_sum, is_even second_parity_bit_sum) with
  | false, false -> 4
  | false, true -> 2
  | true, false -> 3
  | true, true -> 1

let decode data =
  let second_col = get_nth_col data 2 in
  let third_col = get_nth_col data 3 in
  let forth_col = get_nth_col data 4 in

  let second_forth_col_sum =
    List.fold_left2 (fun acc a b -> acc + a + b) 0 second_col forth_col
  in
  let third_forth_col_sum =
    List.fold_left2 (fun acc a b -> acc + a + b) 0 third_col forth_col
  in

  let second_row = get_nth_row data 2 in
  let third_row = get_nth_row data 3 in
  let forth_row = get_nth_row data 4 in

  let second_forth_row_sum =
    List.fold_left2 (fun acc a b -> acc + a + b) 0 second_row forth_row
  in
  let third_forth_row_sum =
    List.fold_left2 (fun acc a b -> acc + a + b) 0 third_row forth_row
  in

  let col_err_pos =
    calculate_error_bit_pos second_forth_col_sum third_forth_col_sum
  in

  let row_err_pos =
    calculate_error_bit_pos second_forth_row_sum third_forth_row_sum
  in
  let all_sum = List.fold_left Int.add 0 data in

  match (col_err_pos, row_err_pos, is_even all_sum) with
  | 1, 1, true -> "No errors or undetectable error"
  | 1, 1, false -> "No errors in data bits, error in first parity bit"
  | _, _, true -> "At least two errors"
  | x, y, false -> Printf.sprintf "One error at %d %d" x y

let random_int_from_os_range max_value =
  let ic = open_in_bin "/dev/urandom" in
  let byte = input_byte ic in
  close_in ic;
  byte mod max_value

let corrupt_one_bit data =
  let random_index = random_int_from_os_range 16 in
  update_list_at data random_index (flip (List.nth data random_index))

let corrupt_two_bit data = corrupt_one_bit data |> corrupt_one_bit
let corrupt_three_bit data = corrupt_two_bit data |> corrupt_one_bit

let () =
  let encoded = encode data in
  print_endline "Encoded data:";
  pretty_print encoded;
  print_endline "\n-------------";

  print_endline "\nCorrupitng one bit\n";
  let corrupted = corrupt_one_bit encoded in
  let decoded = decode corrupted in

  print_endline "Corrupted data:";
  pretty_print corrupted;

  print_endline "\nDecoding result:";
  print_endline decoded;

  print_endline "\n-------------";

  print_endline "\nCorrupitng two bits\n";
  let corrupted = corrupt_two_bit encoded in
  let decoded = decode corrupted in

  print_endline "Corrupted data:";
  pretty_print corrupted;

  print_endline "\nDecoding result:";
  print_endline decoded;

  print_endline "\n-------------";

  print_endline "\nCorrupitng three bits\n";
  let corrupted = corrupt_three_bit encoded in
  let decoded = decode corrupted in

  print_endline "Corrupted data:";
  pretty_print corrupted;

  print_endline "\nDecoding result:";
  print_endline decoded
