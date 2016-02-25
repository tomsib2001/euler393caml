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

let sumlist l = List.fold_right (+) l 0;;

let print_matrix print_cell mat n =
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      print_cell(mat.(i).(j))
    done;
    Printf.printf "\n"
  done;
Printf.printf ""
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

let print_coord_list coord_list =
  print_string (List.fold_right (fun (a,b) y -> (string_of_int a)^","^(string_of_int b) ^" "^ y) coord_list "\n");;


(* print_dir_list_as_coords all_dirs (0,0);; *)

let get_vect = function
  | N -> (0,1)
  | E -> (1,0)
  | S -> (0,-1)
  | W -> (-1,0);;

let det (a,b) (c,d) = a*d-b*c;;

let get_angle (f : from) (d : dir) =
  match f with
  | From None -> 0
  | From (Some dir_old) -> det (get_vect dir_old) (get_vect d);;

(* get_angle (From(Some(S))) E;; *)

let fast_exp a n =
  let rec aux res n = 
    if n = 0 then 1 else if n=1 then res else
	match (n mod 2) with
	| 0 -> aux (res*res) (n/2)
	| _ -> aux (res*res*a) (n/2)
  in aux a n;;

let powers_of_2 n = 
  let res = Array.make ((n*n) / 4 + 1) 0 in
  res.(0) <- 1;
  for i = 1 to ((n*n)/4 + 1)-1 do
    res.(i) <- res.(i-1)*2;
  done;
  res;;

(* powers_of_2 10;; *)

let update_mat n mat count_col count_row (i,j) newval =
  let oldval = mat.(i).(j) in
  begin
    (* update counters in one or the other direction *)
    if oldval = def_pair && newval <> def_pair then
      begin
	count_col.(j) <- count_col.(j) - 1;
	count_row.(i) <- count_row.(i) - 1;
      end 
    else
      if oldval <> def_pair && newval = def_pair then
	begin
	  count_col.(j) <- count_col.(j) + 1;
	  count_row.(i) <- count_row.(i) + 1;
	end
  end;
  mat.(i).(j) <- newval;;

(* let check_for_lonesome_cells count_col count_row n = *)
(*       let i = ref 0 in *)
(*       while(!i < n && not(count_col.(!i) = 1 || count_row.(!i) = 1)) do *)
(* 	i := !i  + 1; *)
(*       done; *)
(*       (!i <> n);; *)
	
let delete l x = List.filter (fun y -> y <> x) l;;

let has_at_least_two_neighbours from_to n (i,j) all_neighboursn =
  (* Printf.printf "entering has_at_least_two_neighbours with %d,%d\n " i j; *)
  (* print_matrix print_pair from_to n; *)
  let neighbs = List.map (get_neighbour (i,j)) all_neighboursn.(i).(j) in
  (* let def_pair_neighbours = (List.filter (fun (a,b) -> from_to.(a).(b) = def_pair) neighbs) in *)
  (* print_coord_list def_pair_neighbours; *)
  let res =
  (from_to.(i).(j) <> def_pair) ||
    (from_to.(i).(j) = def_pair &&
    List.length (List.filter (fun (a,b) -> fst from_to.(a).(b) = From(None)) neighbs) >= 2) in 
  (* if res then print_string "true\n\n\n\n" else print_string "false\n\n\n\n";  *)
  (* Printf.printf "leaving has_at_least_two_neighbours with %d,%d\n " i j; *)
  res;;

let all_neighbours_have_two_neighbours from_to n (i,j) excluded_neighb all_neighboursn =
  let res = 
  List.for_all (fun neighb -> has_at_least_two_neighbours from_to n neighb all_neighboursn) (delete (List.map (get_neighbour (i,j)) all_neighboursn.(i).(j)) excluded_neighb) in
  (* if res then print_string "truuuuuuuuuuuuuuuuuue\n\n\n\n" else print_string "faaaaaaaaaaaaaaaalse\n\n\n\n"; *) res
;;
  

let find_all_cycle_dec n =
  let all_neighboursn = make_all_neighbours_by_dir n in
  let from_to_n = make_from_to_mat n in
  let rec aux (i,j) num_cycles cur_angle cur_surface cur_surface_excedent =
    (* Printf.printf "i= %d, j= %d \n" i j; *)
    let neighbs = all_neighboursn.(i).(j) in
    let pow2 = powers_of_2 n in
    let pow2 k = if k < ((n*n)/4 + 1) then pow2.(k) else failwith "too high power" in
    (* let count_col = Array.make n n in *)
    (* let count_row = Array.make n n in *)
    let update (i,j) newval = from_to_n.(i).(j) <- newval in(* update_mat n from_to_n count_col count_row (i,j) newval in *)
    (* Printf.printf "current neighbours of %d,%d:  " i j; *)
    (* print_dir_list_as_coords neighbs (i,j); *)
    sumlist
      (List.map
	 (fun dir ->
	(* Printf.printf "considering direction %s for (i,j) = (%d,%d)\n" (dir_to_string dir) i j; *)
	   let (i1,j1) = get_neighbour (i,j) dir in
	   (* print_matrix print_pair from_to_n n; *)
	  
	     begin
	   let local_angle = get_angle (fst (from_to_n.(i).(j))) dir in
	   let local_surface_variation = - i * (j1 - j) + j * (i1 - i)  in
	   let excedent_variation = match local_angle with
	     | 1 -> 1
	     | 0 -> 2
	     | -1 -> 3
	     | _ -> failwith "never happens: not 1, 0 or -1" in
	   match from_to_n.(i1).(j1) with
	   | (From(Some _),To(Some _)) -> 0
	   | (From(None),To(Some cell)) as oldi1j1 -> (* closing a cycle *)
	     let surf = ((cur_surface + local_surface_variation)/2 - (cur_surface_excedent + excedent_variation)/4) in
	     if surf >= 0 && surf mod 2 = 1 then
	      begin Printf.printf "current surface at end of cycle: %d\n" ((cur_surface + local_surface_variation)/2 - (cur_surface_excedent + excedent_variation)/4);
	     print_matrix print_pair from_to_n n end else () ;
	     if (cell = opp dir || (cur_angle + local_angle <= 0) || check_for_lonesome_cells from_to_n n || (surf >= 0 && surf mod 2 = 1)) then 0 else
	        if (not (all_neighbours_have_two_neighbours from_to_n n (i,j) (i1,j1) all_neighboursn)) then 0 else
	       begin
		(* first update (i,j) to take into account visit to (i1,j1) *)
	       let (from1,towards1) = from_to_n.(i).(j) in

	       update (i,j) (from1,To(Some(dir)));
	       (* from_to_n.(i).(j) <- (from1,To(Some(dir))); *)

		(* now update "from" field in (i1,j1) to finish the cycle *)
	       update (i1,j1) (From(Some(opp dir)),To(Some cell));
	       (* from_to_n.(i1).(j1) <- (From(Some(opp dir)),To(Some cell)); *)

	        (* Printf.printf "finished a cycle at (%d,%d)! \n" i1 j1; *)
	        (* print_matrix print_pair from_to_n n; *)
	        (* find an ant from which to start the next cycle *)
	       let new_guy = get_next_available from_to_n n print_pair in
	       let temp = (match new_guy with
		 | None ->  pow2 num_cycles
		 | Some ng ->
		   aux ng (num_cycles+1) 1 0 0
	       ) in
			   (* and then reestablish past situation *)
			  update (i,j) (from1,towards1);
			  update (i1,j1) oldi1j1;
			  temp
	       end
	   | (From(None),To(None)) ->
	  (* Printf.printf "continuing current cycle with (%d,%d)\n" i1 j1; *)
	  (* print_matrix (print_pair) from_to_n n; *)
	      if (not (all_neighbours_have_two_neighbours from_to_n n (i,j) (i1,j1) all_neighboursn)) then 0 else
		begin
		  let (from1,towards1) = from_to_n.(i).(j) in
		  assert(towards1 = To None (* && from1 <> From None *));
		  update (i,j) (from1,To(Some(dir)));
	     (* from_to_n.(i).(j) <- (from1,To(Some(dir))); *)
		  update (i1,j1) (From(Some(opp dir)),To(None));
	     (* from_to_n.(i1).(j1) <- (From(Some(opp dir)),To(None)); *)
		  let temp = aux (i1,j1) num_cycles (cur_angle+local_angle) (cur_surface + local_surface_variation) (cur_surface_excedent + excedent_variation) in
		  update (i,j) (from1,towards1);
	     (* from_to_n.(i).(j) <- (from1,towards1); *)
		  update (i1,j1) (From(None),To(None));
	     (* from_to_n.(i1).(j1) <- (From(None),To(None)); *)
		  temp
		end
	   | (From(Some _),To(None)) -> failwith "never happens"
	     end )
	 neighbs)
       in aux (0,0) 1 1 0 0;;

let res = find_all_cycle_dec 10;;

Printf.printf "efficiency_counter: %d\n" !efficiency_counter;;

Printf.printf "result: %d\n" res;;
