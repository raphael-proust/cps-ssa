

module L = struct

  let concat_map f l = List.concat (List.map f l)

  (* [exists_one p l] tests wheter the predicate [p] is true for exactly one
   * element of the list [l]. *)

  let exists_one predicate l =
    let rec aux flag = function
      | [] -> flag
      | h::t ->
        let f = predicate h in
        (not (flag && f)) || (aux (flag || f) t)
    in
    aux false l

  let unique extract l =
    let hshtbl = Hashtbl.create 32 in
    let rec aux = function
      | [] -> true
      | h::t ->
        let x = extract h in
        let h = Hashtbl.hash x in
        try
          if Hashtbl.find hshtbl h = x then
            false
          else
            raise Not_found (*hackish way to DRY*)
        with
          | Not_found ->
            Hashtbl.add hshtbl h x;
            aux t
    in
    aux l


end
