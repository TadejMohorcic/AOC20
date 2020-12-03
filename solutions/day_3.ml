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

let koliko_dreves desno sez =
  let dolzina = List.length (List.nth sez 0) in
  let rec spust_pomozna premik drevesa proga = match proga with 
    | [] -> drevesa
    | x :: xs -> 
      let polje = List.nth x premik in
      let zadetek = (if polje = '#' then 1 else 0) in
      spust_pomozna ((premik + desno) mod dolzina) (drevesa + zadetek) xs
    in spust_pomozna 0 0 sez

let hiter_spust desno sez =
  let dolzina = List.length (List.nth sez 0) in
  let rec hitrejse_pomozna premik drevesa proga = match proga with
    | x :: y :: xs ->
      let polje = List.nth x premik in
      let zadetek = (if polje = '#' then 1 else 0) in
      hitrejse_pomozna ((premik + desno) mod dolzina) (drevesa + zadetek) xs
    | _ -> drevesa
  in hitrejse_pomozna 0 0 sez

let naloga1 data = 
  let lines = List.lines data in
  lines |> List.map explode |> koliko_dreves 3 |> string_of_int

let naloga2 data =
  let lines = List.lines data in
  let sez_lines = List.map explode lines in
  let spust1 = koliko_dreves 1 sez_lines in
  let spust3 = koliko_dreves 3 sez_lines in
  let spust5 = koliko_dreves 5 sez_lines in
  let spust7 = koliko_dreves 7 sez_lines in
  let spust2 = hiter_spust 1 sez_lines in
  string_of_int (spust1 * spust3 * spust5 * spust7 * spust2)

(*------------------------------------- koda se poganja tu spodaj -------------------------------------*)

let main () =
  let podatki = preberi_datoteko ("data/day_3.in") in
  let resitev1 = naloga1 podatki in
  let resitev2 = naloga2 podatki in
  print_endline resitev1;
  print_endline resitev2;
  izpisi_datoteko ("out/day_3_1.out") resitev1;
  izpisi_datoteko ("out/day_3_2.out") resitev2;
  ()

let _ = main ()