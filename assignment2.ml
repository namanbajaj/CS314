open List

let count123 l =
  let rec count123' l (c1, c2, c3) =
    match l with
    | [] -> (c1, c2, c3)
    | h::t -> match h with
              | 1 -> count123' t (c1+1, c2, c3)
              | 2 -> count123' t (c1, c2+1, c3)
              | 3 -> count123' t (c1, c2, c3+1)
              | _ -> count123' t (c1, c2, c3)
  in count123' l (0, 0, 0)

let rec n_times (f, n, v) =
  if n <= 0 then v
  else n_times (f, n-1, f v)

let rec delete p h t = 
  match t with 
  | [] -> []
  | head :: tail -> 
    if (p h head = false) 
      then [head] @ delete p h tail
    else delete p h tail

let rec list_exists func cur l = 
  match l with 
  | [] -> false
  | h :: t -> if (func cur h = false) 
    then (list_exists func cur t) 
  else true

let rec find_list value key l = 
  match l with 
  | [] -> []
  | h :: t -> if (value key h = false) then (find_list value key t)
  else [h] @ find_list value key t

let buckets p l = 
  let rec bucket' p l = 
    match l with 
    [] -> []
    | h :: t -> if(list_exists p h t = false) 
      then [[h]] @ bucket' p t
    else [[h] @ find_list p h t] @ bucket' p (match t with 
    | [] -> []
    | head :: tail -> if (p h head = false) then [head] @ delete p h tail
    else delete p h tail) 
  in bucket' p l

let fib_tailrec n =
  let rec fib_tailrec' n (a, b) =
    if n <= 0 then a
    else fib_tailrec' (n-1) (b, a+b)
  in fib_tailrec' n (0, 1)

let assoc_list lst =
  if lst = [] then []
  else
  let total lst x = List.fold_left (fun (a,acc) y -> 
    if x = y 
      then a , acc + 1
    else (a,acc)) (x,0) lst in
    let fullList = List.map (total lst) lst in List.fold_left (
      fun acc x -> (
        List.fold_left (
          fun acc1 x1 -> if x1 = x then acc1 else x1::acc1) [x] acc)) []
	fullList;;

let ap fs args =
  List.fold_left(fun x y -> x @ (List.map y args))[]fs;;  

let length lst = List.fold_left(fun x y -> x + 1) 0 lst;;

let maxl2 lst = 
  if length lst < 2 then 0
  else
    let n1 = List.fold_left max min_int lst in
    let n2 = List.fold_left max min_int (List.map (fun x -> if x = n1 then min_int else x) lst) in
    n1 + n2


type 'a tree = Leaf | Node of 'a tree * 'a * 'a tree

let rec insert tree x =
  match tree with
  | Leaf -> Node(Leaf, x, Leaf)
  | Node(l, y, r) ->
     if x = y then tree
     else if x < y then Node(insert l x, y, r)
     else Node(l, y, insert r x)

let construct l =
  List.fold_left (fun acc x -> insert acc x) Leaf l


let rec fold_inorder f acc t =
  match t with
  | Leaf -> acc
  | Node(l, x, r) -> fold_inorder f (f (fold_inorder f acc l) x) r

let rec search total acc v lst = 
  match total with
  Node (left, value, right) -> 
    if v != lst
      then search left (search right acc (v + 1) lst) (v + 1) lst
    else value :: acc
  | Leaf -> acc

let levelOrder t =
  let rec levelOrder' a count = 
    if (search a [] 1 count != []) 
      then (search a [] 1 count) :: (levelOrder' a (count + 1))
    else []
    in levelOrder' t 1


(********)
(* Done *)
(********)

let _ = print_string ("Testing your code ...\n")

let main () =
  let error_count = ref 0 in

  (* Testcases for count123 *)
  let _ =
    try
      assert (count123 [3;4;2;1;3] = (1,1,2));
      assert (count123 [4;4;1;2;1] = (2,1,0))
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for n_times *)
  let _ =
    try
      assert (n_times((fun x-> x+1), 50, 0) = 50);
      assert (n_times((fun x-> (x +. 2.0)), 50, 0.0) = 100.0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in
  
  (* Testcases for buckets *)
  let _ =
    try
      assert (buckets (=) [1;2;3;4] = [[1];[2];[3];[4]]);
      assert (buckets (=) [1;2;3;4;2;3;4;3;4] = [[1];[2;2];[3;3;3];[4;4;4]]);
      assert (buckets (fun x y -> (=) (x mod 3) (y mod 3)) [1;2;3;4;5;6] = [[1;4];[2;5];[3;6]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fib_tailrec *)
  let _ =
    try
      assert (fib_tailrec 50 = 12586269025);
      assert (fib_tailrec 90 = 2880067194370816120);
      assert (fib_tailrec 0 = 0)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for assoc_list *)
  let _ =
    let y = ["a";"a";"b";"a"] in
    let z = [1;7;7;1;5;2;7;7] in
    let a = [true;false;false;true;false;false;false] in
    let b = [] in
    let cmp x y = if x < y then (-1) else if x = y then 0 else 1 in
    try
      assert ([("a",3);("b",1)] = List.sort cmp (assoc_list y));
      assert ([(1,2);(2,1);(5,1);(7,4)] = List.sort cmp (assoc_list z));
      assert ([(false,5);(true,2)] = List.sort cmp (assoc_list a));
      assert ([] = assoc_list b)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for ap *)
  let _ =
    let x = [5;6;7;3] in
    let b = [3] in
    let c = [] in
    let fs1 = [((+) 2) ; (( * ) 7)] in
    try
      assert  ([7;8;9;5;35;42;49;21] = ap fs1 x);
      assert  ([5;21] = ap fs1 b);
      assert  ([] = ap fs1 c);
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in
  
  (* Testcases for maxl2 *)  
  let _ =
    try
      assert (maxl2 [1;10;2;100;3;400] = 500)
      ; assert (maxl2 [] = 0)
      ; assert (maxl2 [1000;29;10;5;10000;100000] = 110000)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for fold_inorder *)
  let _ =
    try
      assert (fold_inorder (fun acc x -> acc @ [x]) [] (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = [1;2;3]);
      assert (fold_inorder (fun acc x -> acc + x) 0 (Node (Node (Leaf,1,Leaf), 2, Node (Leaf,3,Leaf))) = 6)
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  (* Testcases for levelOrder *)
  let _ =
    try
      assert (levelOrder (construct [3;20;15;23;7;9]) = [[3];[20];[15;23];[7];[9]]);
      assert (levelOrder (construct [41;65;20;11;50;91;29;99;32;72]) = [[41];[20;65];[11;29;50;91];[32;72;99]])
    with e -> (error_count := !error_count + 1; print_string ((Printexc.to_string e)^"\n")) in

  if !error_count = 0 then  Printf.printf ("Passed all testcases.\n")
  else Printf.printf ("%d out of 9 programming questions are incorrect.\n") (!error_count)

let _ = main()
