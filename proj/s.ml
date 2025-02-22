(*c is the structure definitions, 
unix is for animation frame delay and 
graphics are for visualization in the pop-up window*)
(* 
#load "c.cmo";;
#load "unix.cma";;
#load "graphics.cma";; *)

open Graphics
open Unix
open C

(*game of life functionality*)
let neighbors { x; y } =
  [
    { x; y = y + 1 };
    { x = x + 1; y = y + 1 };
    { x = x + 1; y };
    { x = x + 1; y = y - 1 };
    { x; y = y - 1 };
    { x = x - 1; y = y - 1 };
    { x = x - 1; y };
    { x = x - 1; y = y + 1 };
  ]

let modst coord state board =
  match CoordMap.find_opt coord board with
  | None -> CoordMap.add coord { c = state; n = None } board
  | Some cell ->
      if cell.c then
        board
      else
        CoordMap.update coord (fun _ -> Some { c = state; n = Some false }) board
         
let draw_halo board coord =
  List.fold_left (fun acc neighbor -> modst neighbor false acc) board (neighbors coord)
   
let draw board coord =
  draw_halo (modst coord true board) coord

let drawl board coords =
  List.fold_left (fun acc coord -> draw acc (crd coord)) board coords

let createsim born live cells =
  ({ born; live }, drawl CoordMap.empty cells)

let nstep board =
  CoordMap.fold (fun coord cell acc -> match cell.n with
      | None -> CoordMap.remove coord acc
      | Some false -> CoordMap.update coord (fun _ -> Some { c = false; n = None }) acc
      | Some true -> draw_halo (CoordMap.update coord (fun _ -> Some { c = true; n = None }) acc) coord
    ) board board

let nupd life =
  let rules, board = life in

  let count_live_neighbors coord =
    neighbors coord
    |> List.filter_map (fun neighbor -> CoordMap.find_opt neighbor board)
    |> List.filter (fun cell -> cell.c)
    |> List.length
  in

  let process_cell coord cell acc =
    let live_neighbors = count_live_neighbors coord in
    let next_state =
      if (List.fold_left (fun acc c -> acc + count_live_neighbors c) 0 (neighbors coord)) = 0 then
        { cell with n = None }
      else if cell.c then
        { cell with n = Some (List.mem live_neighbors rules.live) }
      else
        { cell with n = Some (List.mem live_neighbors rules.born) }
    in
    CoordMap.add coord next_state acc
  in

  let new_board = CoordMap.fold process_cell board CoordMap.empty in
  (rules, new_board)

let lifestep life =
  let rules, board = life in
  (rules, nstep (snd (nupd life)))

(*useful stuff for display*)
let global_bounds = 
  let rec bounds_rec acc life steps =
    if steps > 0 then bounds_rec (
      CoordMap.fold
      (fun { x; y } _ ((min_x, max_x), (min_y, max_y)) ->
         (min min_x x, max max_x x), (min min_y y, max max_y y))
      (snd life)
      acc
    ) (lifestep life) (steps - 1) else acc
  in bounds_rec ((max_int, min_int), (max_int, min_int))
;;

let life_as_arr board gb =
  let w = snd (fst gb) - fst (fst gb) + 1 in
  let h = snd (snd gb) - fst (snd gb) + 1 in
  List.init h (fun r -> List.init w (fun c -> match CoordMap.find_opt {x = (fst (fst gb)) + c; y = (snd (snd gb)) - r} board with | Some s -> if s.c then '#' else ' ' | None -> ' '))
;;

let sim_as_arrs life steps =
  let gb = global_bounds life steps in
  let rec f life steps =
    if steps > 0 then (life_as_arr (snd life) gb) :: f (lifestep life) (steps - 1) else []
  in f life steps
;;

(*window pop-up whatever*)
let cell_size = 4;;
let char_to_color c =
  match c with
  | '#' -> white
  | _   -> black

let draw_cell x y color =
  set_color color;
  fill_rect (x * cell_size) (y * cell_size) cell_size cell_size

let draw_grid grid =
  let rows = List.length grid in
  let cols = List.length (List.hd grid) in
  for y = 0 to rows - 1 do
    for x = 0 to cols - 1 do
      let color = char_to_color (List.nth (List.nth grid y) x) in
      draw_cell x (rows - 1 - y) color
    done
  done

(*doing despicable things to the input >:P*)
let read_file_to_list filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  read_lines []

let charify s = List.init (String.length s) (String.get s) ;;
let lines = read_file_to_list "input.txt" ;;
let b = List.map (fun c -> Stdlib.int_of_string (String.make 1 c)) (charify (List.hd lines));;
let l = List.map (fun c -> Stdlib.int_of_string (String.make 1 c)) (charify (List.hd (List.tl lines)));;
let s = Stdlib.int_of_string (List.hd (List.tl (List.tl lines)));;
let brdarr = List.tl (List.tl (List.tl lines));;
let rows = List.length brdarr ;;

let alcels_some = List.mapi (fun j r -> List.mapi (fun i c -> if c = '#' then Some (i, rows - j) else None) (List.init (String.length r) (String.get r))) brdarr;;

let alcels = List.flatten (List.map (List.filter_map Fun.id) alcels_some);;

(*finally getting to the nitty-gritty of it all :D*)
let sim = createsim b l alcels;;

let simasarr = sim_as_arrs sim (s+1) ;;

let w = List.length (List.hd (List.hd simasarr));;
let h = List.length (List.hd simasarr);;

(try close_graph () with _ -> ());
open_graph (Printf.sprintf " %dx%d" (w * cell_size) (h * cell_size));

let rec gifify grids =
  draw_grid (List.hd grids);
  (* Unix.sleepf 0.05; *)
  if List.length grids > 1 then
    (gifify (List.tl grids);)
  else 
    ignore (read_key ());
    close_graph ()

in gifify simasarr
;;

(*type this in to run!*)

(*#cd "/home/pan-bodzanowski/Dokumenty/OCaml/proj";;*)
(*#use "s.ml";;*)