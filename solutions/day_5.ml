(* Nekaj od teh funkcij je izposojenih iz repozitorija našega asistenta Filipa Koprivca, in sicer iz datoteke project_windows.ms *)
(* funkcija explode je vzeta iz https://stackoverflow.com/questions/10068713/string-to-list-of-char/10069969 *)

(*---------------------------------------- pomozne funkcije ----------------------------------------*)

let preberi_datoteko ime_datoteke =
  let chan = open_in ime_datoteke in
  let vsebina = really_input_string chan (in_channel_length chan) in
  close_in chan;
  vsebina

let izpisi_datoteko ime_datoteke vsebina =
  let chan = open_out ime_datoteke in
  output_string chan vsebina;
  close_out chan

module List = struct
  include List

  let int_list l = List.map int_of_string l
  
  let lines = String.split_on_char '\n'
end

(*-------------------------------------------- moja koda --------------------------------------------*)

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec maximum max sez = match sez with
  | [] -> max
  | x :: xs ->
    if x > max then maximum x xs else maximum max xs

let poglej_znake crka = match crka with
  | 'F' -> 0
  | 'B' -> 1
  | 'R' -> 1
  | 'L' -> 0
  | _ -> failwith "Neveljaven znak"

let rec zmnozi vrsta stol sez =
  let dolzina = List.length sez in
  if dolzina > 4 then match sez with
    | x :: xs ->  zmnozi (2 * (x + vrsta)) stol xs
    | _ -> failwith "Takega seznam nimam"
  else if dolzina > 3 then match sez with
    | x :: xs ->  zmnozi (x +  vrsta) stol xs
    | _ -> failwith "Takega seznam nimam"
  else if dolzina > 1 then match sez with
    | x :: xs ->  zmnozi vrsta (2 * (x + stol)) xs
    | _ -> failwith "Takega seznam nimam"
  else match sez with
    | [] -> (vrsta * 8) + stol
    | x :: xs -> zmnozi vrsta (x + stol) xs

let rec moj_stol sez = match sez with
  | x :: y :: rest -> if (y - x) = 2 then (y - 1) else moj_stol (y :: rest)
  |_ -> failwith "Letalo je polno"

let naloga1 data =
  let lines = List.lines data in
  lines |> List.map explode |> List.map (List.map poglej_znake) |> List.map (zmnozi 0 0) |> maximum 0 |> string_of_int

let naloga2 data =
  let lines = List.lines data in
  lines |> List.map explode |> List.map (List.map poglej_znake) |> List.map (zmnozi 0 0) |> List.sort compare |> moj_stol |> string_of_int
 
(*------------------------------------- koda se poganja tu spodaj -------------------------------------*)

let main () =
  let podatki = preberi_datoteko ("data/day_5.in") in
  let resitev1 = naloga1 podatki in
  let resitev2 = naloga2 podatki in
  print_endline resitev1;
  print_endline resitev2;
  izpisi_datoteko ("out/day_5_1.out") resitev1;
  izpisi_datoteko ("out/day_5_2.out") resitev2;
  ()

let _ = main ()