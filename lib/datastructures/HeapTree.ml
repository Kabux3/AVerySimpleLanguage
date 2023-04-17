module M : Heap.M with type vt = Value.t * Expression.t = struct
  
  type  vt = Expression.t

  type range = (vt * vt)

  type tree_t = 
    | Leaf -> range * vt
    | Node -> range * tree_t
  
  type t = (int, tree_t) Hashtbl.t

  let init () : t = Hashtbl.create Parameters.size

  let malloc h (sz : vt) (pc : vt PathCondition.t) : (t * vt * vt PathCondition.t) list =
    ignore sz;
    ignore pc;
    ignore h;
    []
  
  let update h (arr : vt) (index : vt) (v : vt) (pc : vt PathCondition.t)  : (t * vt PathCondition.t) list =
    ignore arr;  
    ignore v;
    ignore index;
    ignore h;
    ignore pc;
    []

  let lookup h (arr : vt) (index : vt) (pc : vt PathCondition.t) : (t * vt * vt PathCondition.t) list =
    ignore index;
    ignore arr;
    ignore pc;
    ignore h;
    []

  let free h (arr : vt) (pc : vt PathCondition.t) : (t * vt PathCondition.t) list =
    ignore arr;
    ignore pc;
    ignore h;
    []

end