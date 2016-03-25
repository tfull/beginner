let fizz_buzz i =
    if i mod 15 = 0 then "FIzzBuzz"
    else if i mod 5 = 0 then "Buzz"
    else if i mod 3 = 0 then "Fizz"
    else string_of_int i

let rec loop i =
    if  i > 100 then
        ()
    else
        begin
            print_string (fizz_buzz i ^ "\n");
            loop (i + 1)
        end

let _ = loop 1
