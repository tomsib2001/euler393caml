type dir = N | W | S | E;;

let opp = function
  | N -> S
  | W -> E
  | S -> N
  | E -> W

type from = From of dir option
type towards = To of dir option

let def_from = From (None);;
let def_towards = To (None);;
let def_pair = (def_from, def_towards)

let admissible n (i,j)  =
  (0 <= i && i < n && 0 <= j && j < n);;

let get_neighbour (i,j) d = match d with
  | N -> (i-1,j)
  | S -> (i+1,j)
  | W -> (i,j-1)
  | E -> (i,j+1);;

let all_dirs = [N;E;S;W];;

let get_neighbours n (i,j) = List.filter (admissible n) (List.map (get_neighbour (i,j)) all_dirs);;

let get_neighbours_by_dir n (i,j) =
  List.map fst (
  List.filter
    (fun(dir,x) -> admissible n x)
    (List.map (fun dir -> (dir,get_neighbour (i,j) dir)) all_dirs));;


let make_all_neighbours n =
  let res = Array.make_matrix n n [] in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      res.(i).(j) <- get_neighbours n (i,j)
    done;
  done;
res
;;

let make_all_neighbours_by_dir n =
  let res = Array.make_matrix n n [] in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      res.(i).(j) <- get_neighbours_by_dir n (i,j)
    done;
  done;
res
;;


get_neighbours 10 (0,0);;
get_neighbours 10 (2,3);;

let all_neighbours_4 = make_all_neighbours_by_dir 4;;
(* let all_neighbours_10 = make_all_neighbours 10;;  *)

let make_from_to_mat n =
  let res = Array.make_matrix n n (def_pair) in
  res;;

let to_from4 = make_from_to_mat 4;;

let check_for_lonesome_cells_rows mat n =
  let j = ref 0 in
  let res = ref false in
  let counter = ref 0 in
  while(!j < n && not(!res)) do
    for i = 0 to n-1 do
      if (mat.(i).(!j) = def_pair) then
	counter := !counter + 1
    done;
    if !counter = 1 then res := true;
    j := !j +1
  done;
  !res;;

let check_for_lonesome_cells_cols mat n =
  let i = ref 0 in
  let res = ref false in
  let counter = ref 0 in
  while(!i < n && not(!res)) do
    for j = 0 to n-1 do
      if (mat.(!i).(j) = def_pair) then
	counter := !counter + 1
    done;
    if !counter = 1 then res := true;
    i := !i +1
  done;
  !res;;

let efficiency_counter = ref 0;;

let check_for_lonesome_cells mat n =
  let res = check_for_lonesome_cells_cols mat n || check_for_lonesome_cells_rows mat n in
  if res then efficiency_counter := !efficiency_counter + 1;
  res
;;

let get_next_available mat n print_cell =
  let i = ref 0 in
  let j = ref 0 in
  let b = ref false in
  while((mat.(!i).(!j) <> def_pair) && not !b) do
    (* Printf.printf "i=%d,j=%d\n" !i !j; *)
    (* (print_cell mat.(!i).(!j)); *)
    (* if mat.(!i).(!j) = def_pair then Printf.printf " coucou"; *)
    (* print_newline(); *)
    (* flush stdout; *)
    if !j = n-1 then
      begin
	if !i = n-1 then
	  b := true
	else
	  begin
	    i := !i+1;
	    j := 0
	  end
      end
    else
      j := !j + 1
  done;
  if !b then None else Some (!i,!j);;


(* for i = 0 to 3 do *)
(*   for j = 0 to 3 do *)
(*     to_from4.(i).(j) <- (From(Some N),To(Some N)) *)
(*   done *)
(* done;; *)

(* get_next_available to_from4  4;; *)

(* some tools to manipulate graphs *)
type cell = int*int
type graph = cell list

let shift (g : graph) ((i,j) : cell) =
  List.map (fun (x,y) -> (x-i,y-j)) g;;

let sort_graph (g : graph) = 
  List.sort_uniq 
    (fun (a,b) (c,d) -> 
      match Pervasives.compare a c with
      | 0 -> Pervasives.compare b d
      | k -> k)
    g;;

let canonical (g : graph) =
  let g1 = sort_graph g in
  shift g1 (List.hd g1);;

let example_graph = [(6,7);(1,2);(1,2);(2,1)];;
sort_graph example_graph;;
let can_g = canonical example_graph;;

module Cell = struct 
  type t = cell
  let compare = (fun (a,b) (c,d) -> 
      match Pervasives.compare a c with
      | 0 -> Pervasives.compare b d
      | k -> k)
end;;
module CellSet = Set.Make(Cell);;

let build_graph (start : cell) (mat : (from*towards) array array) neighbour_mat =
  let empty = CellSet.empty in
  let rec aux cur_set (i,j) =
    let temp = List.fold_right (fun x (y,b) -> 
      let (i1,j1) = x in
      let b1 = 
	if not(CellSet.mem x y) then 
	  true 
	else 
	  b in 
      (if mat.(i1).(j1) = def_pair then 
	  CellSet.add x y 
       else 
	  y),b1) neighbour_mat.(i).(j) (cur_set,false)
    in aux empty start;;
  

let sumlist l = List.fold_right (+) l 0;;

let print_matrix print_cell mat n =
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      print_cell(mat.(i).(j))
    done;
    Printf.printf "\n"
  done;
Printf.printf "\n\n"
;;

let dir_to_string = function
  | N -> "N"
  | W -> "W"
  | S -> "S"
  | E -> "E"

let print_from = function
  | From (Some x) -> Printf.printf "from %s " (dir_to_string x)
  | From(None) -> Printf.printf "fr None"

let print_to = function
  | To (Some x) -> Printf.printf "to %s  " (dir_to_string x)
  | To(None) -> Printf.printf "None  "

let print_pair = function
  | (From None,To None) -> Printf.printf "xxxxxxxxxxx  "
  | (a,b) -> print_from a; print_to b;;

let print_dir_list dir_list =
  print_string (List.fold_right (fun x y -> (dir_to_string x) ^" "^ y) dir_list "\n");;

let print_dir_list_as_coords dir_list (i,j) =
  print_string (List.fold_right (fun (a,b) y -> (string_of_int a)^","^(string_of_int b) ^" "^ y) (List.map (get_neighbour (i,j)) dir_list) "\n");;

(* print_dir_list_as_coords all_dirs (0,0);; *)

let find_all_cycle_dec n =
  let all_neighboursn = make_all_neighbours_by_dir n in
  let from_to_n = make_from_to_mat n in
  let rec aux (i,j) =
    (* Printf.printf "i= %d, j= %d \n" i j; *)
    let neighbs = all_neighboursn.(i).(j) in
    (* Printf.printf "current neighbours of %d,%d:  " i j; *)
    (* print_dir_list_as_coords neighbs (i,j); *)
    sumlist
      (List.map
      (fun dir ->
	(* Printf.printf "considering direction %s for (i,j) = (%d,%d)\n" (dir_to_string dir) i j; *)
	let (i1,j1) = get_neighbour (i,j) dir in
	match from_to_n.(i1).(j1) with
	| (From(Some _),To(Some _)) -> 0
	| (From(None),To(Some cell)) as oldi1j1 -> (* closing a cycle *)
	  if cell = opp dir then 0 else
	    if check_for_lonesome_cells from_to_n n then 0 else
	      begin
		(* first update (i,j) to take into account visit to (i1,j1) *)
		let (from1,towards1) = from_to_n.(i).(j) in
		from_to_n.(i).(j) <- (from1,To(Some(dir)));
		(* now update "from" field in (i1,j1) to finish the cycle *)
		from_to_n.(i1).(j1) <- (From(Some(opp dir)),To(Some cell));
	        (* Printf.printf "finished a cycle at (%d,%d)! \n" i1 j1; *)
	        (* print_matrix print_pair from_to_n n; *)
	        (* find an ant from which to start the next cycle *)
		let new_guy = get_next_available from_to_n n print_pair in
		let temp = (match new_guy with
		  | None ->
		  (* Printf.printf "\n no new cycle to start\n"; (\* we have succeeded in filling the graph with cycles *\) *)
		  (* print_matrix (print_pair) from_to_n n; *)  1
		  | Some ng ->
		  (* let (a,b) = ng in *)
		  (* Printf.printf "starting a new cycle at  (%d, %d)\n" a b;  *)(* new cycle starting from lowest node in lexicographic order *)
		  (* now recurse on this new ant *)
		    aux ng
		) in
		(* and then reestablish past situation *)
			   from_to_n.(i).(j) <- (from1,towards1);
			   from_to_n.(i1).(j1) <- oldi1j1(* (From(None),To(Some cell)) *);
			   temp
	      end
	| (From(None),To(None)) ->
	  (* Printf.printf "continuing current cycle with (%d,%d)\n" i1 j1; *)
	  (* print_matrix (print_pair) from_to_n n; *)
	  let (from1,towards1) = from_to_n.(i).(j) in
	  assert(towards1 = To None (* && from1 <> From None *));
	  from_to_n.(i).(j) <- (from1,To(Some(dir)));
	  from_to_n.(i1).(j1) <- (From(Some(opp dir)),To(None));
	  let temp = aux (i1,j1) in
	  from_to_n.(i).(j) <- (from1,towards1);
	  from_to_n.(i1).(j1) <- (From(None),To(None));
	  temp
	| (From(Some _),To(None)) -> failwith "never happens"
       )
		  neighbs)
       in aux (0,0);;

let res = find_all_cycle_dec 4;;

Printf.printf "efficiency_counter: %d\n" !efficiency_counter;;

Printf.printf "result: %d\n" res;;
