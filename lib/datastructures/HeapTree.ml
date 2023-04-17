module M : Heap.M with type vt = Expression.t = struct

  type  vt = Expression.t

  type range = (vt * vt)

  type tree_t =
    | Leaf of range * vt
    | Node of range * tree_t list

  type t = (int, tree_t) Hashtbl.t

  let init () : t = Hashtbl.create Parameters.size

  let malloc (h : t) (sz : vt) (pc : vt PathCondition.t) : (t * vt * vt PathCondition.t) list =
    let tree = Leaf ((Expression.Val (Integer 0), sz), Expression.Val (Integer 0)) in
    let l = Hashtbl.length h in
    Hashtbl.replace h l tree;
    [ (h, Expression.Val (Loc l), pc) ]

  let update h (arr : vt) (index : vt) (v : vt) (pc : vt PathCondition.t)  : (t * vt PathCondition.t) list =
    let rec update_tree (tree : tree_t) (index : vt) (v : vt) (pc : vt PathCondition.t) : ((tree_t * vt PathCondition.t) list) option =
      match tree with
      | Leaf ((left, right), old_v) ->
          let ge_left = Expression.BinOp (Gte, index, left) in
          let l_right = Expression.BinOp (Lt, index, right) in
          let cond = Expression.BinOp (And, ge_left, l_right) in
          let pc' = cond :: pc in
          if Encoding.is_sat pc' then
              let index_plus_1 = Expression.BinOp (Plus, index, Val (Integer 1)) in
              let leaves =
                [
                Leaf ((left, index), old_v);
                Leaf ((index, index_plus_1), v);
                Leaf ((index_plus_1, right), old_v)
              ]
                  in
              Some [Node ((left, right), leaves), pc']
          else
            None
      | Node ((left, right), trees) -> begin
          let ge_left = Expression.BinOp(Gte, index, left) in
          let l_right = Expression.BinOp(Lt, index, right) in
          let cond = Expression.BinOp (And, ge_left, l_right) in
          let pc' = cond :: pc in
          if Encoding.is_sat pc' then
            let l = List.map (fun t -> update_tree t index v pc') trees in
            let t1, t2, t3 = match trees with
            | t1 :: t2 :: [t3] -> t1, t2, t3
            | _ -> failwith "unreachable"
            in
            match l with
            | nt1 :: nt2 :: [nt3] ->
              let l1 = match nt1 with
              | Some l1 ->
                List.map (fun (nt1, pc1) -> Node((left, right), [nt1; t2; t3]), pc1) l1
              | None -> []
              in
              let l2 = match nt2 with
              | Some l2 ->
                List.map (fun (nt2, pc2) -> Node((left, right), [t1; nt2; t3]), pc2) l2
              | None -> []
              in
              let l3 = match nt3 with
              | Some l3 ->
                List.map (fun (nt3, pc3) -> Node((left, right), [t1; t2; nt3]), pc3) l3
              | None -> []
              in
              Some (l1 @ l2 @ l3)
            | _ -> failwith "unreachable"
          else
            None
      end
    in
    let i = begin
      match arr with
      | Val (Loc i) -> i
      | _ -> failwith "Invalid allocation index"
      end
    in
    let tree = Hashtbl.find h i in
    let new_trees =
      update_tree tree index v pc
    in
    match new_trees with
    | Some new_trees  -> List.map
      (fun (new_tree, pc') ->
        let new_h = Hashtbl.copy h in
        Hashtbl.replace new_h i new_tree;
        new_h, pc') new_trees
    | None -> failwith "Out of bounds access"

  let lookup h (arr : vt) (index : vt) (pc : vt PathCondition.t) : (t * vt * vt PathCondition.t) list =
    ignore index;
    ignore arr;
    ignore pc;
    ignore h;
    []

  let free h (arr : vt) (pc : vt PathCondition.t) : (t * vt PathCondition.t) list =
    begin
    match arr with
    | Val (Loc i) ->
        Hashtbl.remove h i
    | _ -> failwith "Invalid allocation index"
    end;
    [h, pc]

end
