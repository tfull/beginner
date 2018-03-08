exception UserError of string

let infty = 100000000

let rec input_edges l =
    try
        let x = Scanf.scanf "%d %d %d\n" (fun x y z -> ((x, y), z)) in
        input_edges (x :: l)
    with End_of_file -> l

let input () =
    let n = Scanf.scanf "%d\n" (fun x -> x) in
    let (s, g) = Scanf.scanf "%d %d\n" (fun x y -> (x, y)) in
    let edges = input_edges [] in
    (n, s, g, edges)

let rec make_0n_list n =
    if n < 0 then [] else n :: make_0n_list (n - 1)

let rec make_list x n =
    if n < 1 then [] else x :: make_list x (n - 1)

let rec remove z = function
    | [] -> []
    | x :: xs -> if x = z then xs else x :: remove z xs

let rec set z n = function
    | [] -> raise (UserError "index out of bounds")
    | x :: xs -> if n = 0 then z :: xs else x :: set z (n - 1) xs

let rec get_short vs ds =
    let rec sub (i, p) vs =
        match vs with
            | [] -> (i, p)
            | v :: vs -> if List.nth ds v < p then sub (v, List.nth ds v) vs else sub (i, p) vs
    in
    sub (-1, infty) vs

let rec sub_loop i distance previous = function
    | [] -> (distance, previous)
    | ((e0, e1), v) :: es ->
        if e0 = i || e1 = i then
            let j = e0 + e1 - i in
            if List.nth distance j > List.nth distance i + v then
                sub_loop i (set (List.nth distance i + v) j distance) (set i j previous) es
            else
                sub_loop i distance previous es
        else
            sub_loop i distance previous es

let rec main_loop edges previous distance vertex =
    let (i, _) = get_short vertex distance in
    if i = -1 then
        (previous, distance)
    else
        let vertex = remove i vertex in
        let (distance, previous) = sub_loop i distance previous edges in
        main_loop edges previous distance vertex

let rec get_route i ps =
    let j = List.nth ps i in
    if j = -1 then [i] else get_route j ps @ [i]

let rec join s = function
    | [] -> ""
    | [x] -> x
    | x :: xs -> x ^ s ^ join s xs

let _ =
    let (n, start, goal, edges) = input () in
    let previous = make_list (-1) n in
    let distance = set 0 start (make_list infty n) in
    let vertex = make_0n_list (n - 1) in
    let (previous, distance) = main_loop edges previous distance vertex in
    let route = get_route goal previous in
    print_string (join " -> " (List.map string_of_int route));
    print_string "\n";
    print_string (if List.hd route = start then "distance: " ^ string_of_int (List.nth distance goal) else "unreachable");
    print_string "\n"
