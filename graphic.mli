(**
   insert description
*)
val update_graphics_2 : 'a option list list -> 'a option list list -> float -> 
  int -> bool -> float -> int -> bool -> unit

val update_graphics_1 : 'a option list list -> float -> int -> bool -> unit

val quit : 'a -> unit

val pause : 'a -> unit

val init_graphics : 'a -> int -> unit 

val restart : string -> float list -> (int * int) * (int * int)

val help : 'a -> int * int

val start_window : string -> (int * int) * (int * int)

val player_selection : 'a -> (int * int) * (int * int) * (int * int)

val level_selection : 'a -> (int * int) * (int * int) * (int * int) * 
                            (int * int) * (int * int)
