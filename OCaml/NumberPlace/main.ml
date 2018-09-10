let rec natural_sequence start length =
    if length > 0 then
        start :: natural_sequence (start + 1) (length - 1)
    else
        []

let number_sort = List.sort (fun a b -> a - b)

exception ExitLoop

let size = 3
let size2 = size * size
let number_sequence = natural_sequence 1 size2
let indices = natural_sequence 0 size2
let all_points = List.flatten (List.map (fun i -> List.map (fun j -> (i, j)) indices) indices)
let corner_points = List.filter (fun (i, j) -> i mod size = 0 && j mod size == 0) all_points

let rec read_lines n channel =
    if n > 0 then
        let line = input_line channel in
        line :: read_lines (n - 1) channel
    else
        []

let rec print_lines = function
    | [] -> ()
    | x :: xs -> print_string x; print_newline (); print_lines xs

let array_of_line s =
    let length = String.length s in
    if length > 0 then
        let a = Array.make length 0 in
        for i = 0 to size * size - 1 do
            a.(i) <- if s.[i] = '.' then 0 else Char.code s.[i] - Char.code '0'
        done;
        a
    else
        [||]

let read_from_file path =
    let channel = open_in path in
    let lines = read_lines size2 channel in
    close_in channel;
    lines

let load_field path =
    let lines = read_from_file path in
    Array.of_list (List.map (fun line -> array_of_line line) lines)

let next_point (i, j) =
    if j < size2 - 1 then
        (i, j + 1)
    else
        (i + 1, 0)

let box_numbers field (i, j) =
    let i_ = natural_sequence i size in
    let j_ = natural_sequence j size in
    let ij_list = List.flatten (List.map (fun i -> List.map (fun j -> (i, j)) j_) i_) in
    let n_list = List.map (fun (i, j) -> field.(i).(j)) ij_list in
    n_list

let filled field = List.for_all (fun (i, j) -> field.(i).(j) > 0) all_points

let depricated_list n_list = 
    let rec depricated_block n = function
        | [] -> false
        | 0 :: xs -> depricated_block n xs
        | x :: xs -> if x = n then true else depricated_block x xs
    in
    depricated_block 0 (number_sort n_list)

let depricated field =
    if List.exists (fun i -> depricated_list (List.map (fun j -> field.(i).(j)) indices)) indices then
        true
    else if List.exists (fun j -> depricated_list (List.map (fun i -> field.(i).(j)) indices)) indices then
        true
    else
        List.exists (fun (i, j) -> depricated_list (box_numbers field (i, j))) corner_points

let print_field field =
    for i = 0 to size2 - 1 do
        print_string (List.fold_left (^) "" (List.map (fun j -> string_of_int field.(i).(j)) indices) ^ "\n")
    done

let rec search field =
    if filled field then
        if not (depricated field) then
            begin
                print_field field;
                print_newline ();
            end
        else
            ()
    else if depricated field then
        ()
    else
        try
            begin
                for i = 0 to size2 - 1 do
                    for j = 0 to size2 - 1 do
                        if field.(i).(j) = 0 then
                            begin
                                for n = 1 to size2 do
                                    field.(i).(j) <- n;
                                    search field;
                                    field.(i).(j) <- 0
                                done;
                                raise ExitLoop
                            end
                        else
                            ()
                    done
                done
            end
        with e -> ()

let _ =
    let field = load_field Sys.argv.(1) in
    search field
