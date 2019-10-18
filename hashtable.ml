module type MyHashTableSig = sig
  type ('k, 'v) table
  val cLOAD_MIN : float
  val cLOAD_MAX : float
  (*val empty : ('k, 'v) table 
    val is_empty : ('k, 'v) table -> bool *)
  val insert : 'k -> 'v -> ('k, 'v) table -> unit
  val mem : 'k -> ('k, 'v) table -> bool
  val find : 'k -> ('k, 'v) table -> 'v
  val remove : 'k -> ('k, 'v) table -> unit
  val to_list : ('k, 'v) table -> ('k * 'v) list
  val to_string : ('k, 'v) table -> ('k -> string) -> ('v -> string) -> string
end

module MyHashTable : MyHashTableSig = struct

  let cLOAD_MIN = 0.5
  let cLOAD_MAX = 2.0

  type ('k, 'v) table = {
    mutable buckets : (('k * 'v) list) array;
    mutable size : int;
    mutable bindings : int;
    hash_function : 'k -> int;
  }

  let empty ={
    buckets = Array.make 1 [];
    size = 1;
    bindings = 0;
    hash_function = Hashtbl.hash;
  } 

  let is_empty d =
    d = empty

  let load_factor d = 
    float_of_int d.bindings /. float_of_int d.size

  let insert_helper k v d = 
    let hash = d.hash_function k in 
    let bucket = hash mod d.size in 
    let linked_list = d.buckets.(bucket) in 
    let new_linked_list = 
      List.sort_uniq 
        (fun (key1, value1) (key2, value2) -> compare key1 key2) 
        ((k, v) :: linked_list) 
    in 
    d.buckets.(bucket) <- new_linked_list

  let rehash_elements old_dict new_dict = 
    let list_of_bindings = 
      Array.fold_left 
        (fun init bucket_lst -> bucket_lst :: init) [] old_dict.buckets in 
    let bindings = List.flatten list_of_bindings in 
    List.map 
      (fun (key, value) -> insert_helper key value new_dict) 
      bindings 
    |> ignore

  let rehash_array d sizing = 
    let old_dict = d in
    if sizing then
      (d.size <- max 1 d.size / 2;
       d.buckets <- Array.make d.size [];
       rehash_elements old_dict d;)
    else 
      (d.size <- d.size * 2;
       d.buckets <- Array.make d.size [];
       rehash_elements old_dict d;)

  let resize_array d = 
    let l_factor = load_factor d in 
    if l_factor >= cLOAD_MIN || l_factor <= cLOAD_MAX then ()
    else rehash_array d (if l_factor < cLOAD_MIN then true else false)

  let insert k v d = 
    insert_helper k v d;
    d.bindings <- d.bindings + 1;
    resize_array d

  let mem k d = 
    let hash = d.hash_function k in 
    let bucket = hash mod d.size in 
    let linked_list = d.buckets.(bucket) in 
    let key_list = List.map fst linked_list in 
    List.mem k key_list

  let find k d = 
    let hash = d.hash_function k in 
    let bucket = hash mod d.size in 
    let linked_list = d.buckets.(bucket) in 
    List.assoc k linked_list

  let remove k d = 
    let hash = d.hash_function k in 
    let bucket = hash mod d.size in 
    let linked_list = d.buckets.(bucket) in 
    d.buckets.(bucket) <- List.filter (fun (key, value) -> key <> k) linked_list;
    d.bindings <- d.bindings - 1;
    resize_array d

  let to_list d = 
    let list_of_bindings = 
      Array.fold_left 
        (fun init bucket_lst -> bucket_lst :: init) [] d.buckets in 
    List.flatten list_of_bindings

  let to_string d key_to_string value_to_string = 
    let list_repr = to_list d in 
    List.fold_left 
      (fun init (key, value) -> 
         init ^ key_to_string key ^ " , " ^ value_to_string value) 
      ""
      list_repr

end