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

let razdeli_meje str = 
  match String.split_on_char '-' str with
  | st1 :: st2 :: rest -> (int_of_string st1, int_of_string st2)
  | _ -> failwith "Slabe meje"

let preberi_input vrstica = 
  let gesla = String.split_on_char ' ' vrstica in
  match gesla with
    | meje :: zahteva :: geslo :: rest -> let min, max = razdeli_meje meje in (min, max, zahteva.[0], geslo)
    | _ -> failwith "Vsa gesla so preverjena"

let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let rec prestej_ponovitve crka sez =
  List.fold_left (fun x y -> x + (if y = crka then 1 else 0)) 0 (explode sez)

let je_ustrezno (st1, st2, zah, geslo) =
  let p = prestej_ponovitve zah geslo in 
  st1 <= p && p <= st2 

let je_ustrezno2 (st1, st2, zah, geslo) = 
  if st1 > List.length (explode geslo) then false
  else (List.nth (explode geslo) (st1 - 1) = zah) <> (List.nth_opt (explode geslo) (st2 - 1) = Some zah)

let koliko_dobrih sez = 
  List.fold_left (fun x y -> x + (if y = true then 1 else 0)) 0 sez

let naloga1 data = 
  let lines = List.lines data in
  lines |> List.map preberi_input |> List.map je_ustrezno |> koliko_dobrih |> string_of_int

let naloga2 data =
  let lines = List.lines data in
  lines |> List.map preberi_input |> List.map je_ustrezno2 |> koliko_dobrih |> string_of_int
 
(*------------------------------------- koda se poganja tu spodaj -------------------------------------*)

let main () =
  let podatki = preberi_datoteko ("data/day_2.in") in
  let resitev1 = naloga1 podatki in
  let resitev2 = naloga2 podatki in
  print_endline resitev1;
  print_endline resitev2;
  izpisi_datoteko ("out/day_2_1.out") resitev1;
  izpisi_datoteko ("out/day_2_2.out") resitev2;
  ()

let _ = main ()