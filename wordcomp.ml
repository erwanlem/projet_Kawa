module Env = Map.Make(String)

let levenshtein a b =
  let mat = Array.make_matrix (String.length a+1) (String.length b+1) 0 in
  let cost = ref 0 in
  for i = 0 to String.length a do
    mat.(i).(0) <- i;
  done;
  for j = 0 to String.length b do
    mat.(0).(j) <- j;
  done;
  for i = 1 to String.length a do
    for j = 1 to String.length b do
      if a.[i-1] = b.[j-1] then
        cost := 0
      else
        cost := 1;
        mat.(i).(j) <- Int.min (Int.min (mat.(i-1).(j)+1) (mat.(i).(j-1)+1)) (mat.(i-1).(j-1)+(!cost))
    done;
  done;
  mat.(String.length a).(String.length b)


let get_closest_word_map w m =
  let dist = ref (-1) in
  Env.fold (fun k v acc ->
    let l1 = levenshtein k w in
    if l1 < !dist || !dist = -1 then (dist := l1; k)
    else acc) m ""

let get_closest_word_attributes w l =
  let dist = ref (-1) in
  List.fold_left (fun acc (a, _, _, _) ->
    let l1 = levenshtein a w in
    if l1 < !dist || !dist = -1 then (dist := l1; a)
    else acc) "" l