(* Nekaj od teh funkcij je izposojenih iz repozitorija našega asistenta Filipa Koprivca, in sicer iz datoteke project_windows.ms *)

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

(*---------------------------------------- dejanske funkcije ----------------------------------------*)

let int_to_int z = match z with 
    | Some x -> x
    | None   -> 0

let rec komplement_stevila a b data = match data with
  | [] -> None
  | y :: ys -> if y = (b - a) then (Some y) else komplement_stevila a b ys

let rec najdi_kandidata a = function
  | [] -> None
  | x :: xs -> (match komplement_stevila x a xs with
    | None -> najdi_kandidata a xs
    | Some y -> Some (x * y)
  )

let rec najdi_tri a = function
  | [] -> None
  | x :: xs -> (match najdi_kandidata (a-x) xs with 
    | None -> najdi_tri a xs
    | Some z -> Some (z * x)
  )

let naloga1 data = 
  let lines = List.lines data in
  lines |> List.int_list |> najdi_kandidata 2020 |> int_to_int |> string_of_int

let naloga2 data = 
  let lines = List.lines data in
  lines |> List.int_list |> najdi_tri 2020 |> int_to_int |> string_of_int

(*--------------------------------- funkcije se poganjajo tu spodaj ---------------------------------*)

let main () =
  let podatki = preberi_datoteko ("data/day_1.in") in
  let resitev1 = naloga1 podatki in
  let resitev2 = naloga2 podatki in
  print_endline resitev1;
  print_endline resitev2;
  izpisi_datoteko ("out/day_1_1.out") resitev1;
  izpisi_datoteko ("out/day_1_2.out") resitev2;
  ()

let _ = main ()