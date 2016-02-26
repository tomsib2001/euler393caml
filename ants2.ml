type dir = N | W | S | E;;

let all_dirs = [N;E;S;W];;

let opp_dir = function
  | N -> S
  | W -> E
  | S -> N
  | E -> W


let int_of_dir =  function
  | N -> 0
  | W -> 1
  | S -> 2
  | E -> 3

let dir_of_int = function
  | 0 -> N
  | 1 -> W
  | 2 -> S
  | 3 -> E
  | _ -> failwith "not a direction";;


type flow = In | Out | Zero;;

let all_flows = [In;Out;Zero];;

let opp_flow = function
  | In -> Out
  | Out -> In
  | Zero -> Zero;;

type cell = (flow option) array;;

let (def_cell : cell) = [|None;None;None;None|];;

let is_decided (f : flow option) = match f with
  | None -> false
  | Some _ -> true;;

let get_val (f : flow option) = match f with
  | None -> raise Not_found
  | Some x -> x;;

type line = cell array;;

type constrt = flow array;;

let size (c : 'a array) = Array.length c;;

let make_new_line n =
Array.make_matrix n 4 None;;

let make_template_line (c : constrt) =
  let n = size c in
  let l = make_new_line n in
  let i = int_of_dir N in
  l.(0).(int_of_dir W) <- Some Zero;
  l.(n-1).(int_of_dir E) <- Some Zero;
  for j = 0 to n-1 do
    l.(j).(i) <- Some (opp_flow c.(j))
  done;
  l
;;

let array_for_all (f : 'a -> bool) (t : 'a array) =
  let n = size t in
  let i = ref 0 in
  while (!i < n && f (t.(!i))) do
    i := !i + 1;
  done;
  !i = n;;

(* array_for_all (fun x -> x = 1) [|1;1;0;1|];; *)
(* array_for_all (fun x -> x = 1) [|1;1;1;1|];; *)

let lift_array_indices (t : 'a array) = (Array.mapi (fun i x -> (i,x)) t);;

let no_other_f (c : cell) (j : int) (f : flow) =
  not(List.exists (fun (k,x) -> x = Some f && k <> j) (Array.to_list (lift_array_indices c)));;

let not_all_others_are_zero (c : cell) (j : int) =
  not(array_for_all (fun (i,x) -> i=j || (c.(i)) = Some Zero) (lift_array_indices c));;
      
let correct_assignment (c : constrt) (l : flow option array array) (i : int) (j : int) (f : flow) =
  let n = size c in
  assert(size l = n);
  let dir = dir_of_int j in
  (match (i,dir) with
  | (0, W) -> (l.(i).(j) = Some Zero)
  | (i,E) when i = n-1 -> (l.(n-1).(j) = Some Zero)
  | (i,N) -> (l.(i).(j) = Some (opp_flow (c.(i))))
  | (i,d) -> true)
  &&
    (match f with
    | Zero -> not_all_others_are_zero l.(i) j
    | f -> no_other_f l.(i) j f
    );;

let print_flow = function
  | In -> print_string " in |"
  | Out -> print_string "out |"
  | Zero -> print_string "zero|";;

let print_option_flow = function
  | None -> print_string "none "
  | Some f -> print_flow f;;

let print_option_flow_array t =
  for i = 0 to (size t)-1 do
    for j = 0 to 3 do
      print_option_flow t.(i).(j)
    done;
    print_string " || ";
    done; print_newline();;

let find_all_sols (l : line) (c : constrt) =
  let n = size l in
  let next (k,i) = if i=3 then (k+1,0) else (k,i+1) in
  let rec aux = function
    | (k,0) when k = n -> (* print_option_flow_array l; *) let l1 = Array.copy l in [l1]
    | (k,i) ->
       begin
	 assert(i<4);
	 match l.(k).(i) with
	 | Some _ -> aux (next (k,i))
	 | None ->
	    List.concat
	      (
		(List.map
		  (fun f ->
		    if correct_assignment c l k i f
		    then
		      begin
			l.(k).(i) <- Some f;
			let res = aux (next(k,i)) in
			l.(k).(i) <- None;
			res
		      end
		    else
		      []
		  )
		  all_flows
		)
	      )
       end
  in aux (0,0);;


(* examples *)

let constr_1 = [|Out;Out;Zero;In;Zero|];;

let line = make_template_line constr_1;;

correct_assignment (constr_1) (make_template_line constr_1) 0 (int_of_dir S) Out;;
correct_assignment (constr_1) (make_template_line constr_1) 0 (int_of_dir S) In;;
correct_assignment (constr_1) (make_template_line constr_1) 0 (int_of_dir E) Out;;
correct_assignment (constr_1) (make_template_line constr_1) 0 (int_of_dir E) Zero;;

line.(0).(int_of_dir E) <- Some Out;;
correct_assignment (constr_1) line 0 (int_of_dir S) Out;;

let all_sols = find_all_sols (make_template_line constr_1) constr_1;;
List.length all_sols;;
(* end examples *)

(* let find_all_compatible_lines (c : constrt) = *)
(*   let n = size c in *)
(*   let rec aux res = function *)
(*     | k when k = n -> res *)
(*     | k -> *)
(*        List.map *)
