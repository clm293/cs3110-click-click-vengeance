(**
   This module controls the interface between our game state and the OCaml
   Graphics module.
*)

(** [update_graphics_2 matrix1 score1 lives1 hs1 matrix2 score2 lives2 hs2] 
    is the updated graphics for double player. *)
val update_graphics_2 : 'a option list list -> 'a option list list -> float -> 
  int -> bool -> float -> int -> bool -> unit

(** [update_graphics_1 matrix score lives hs] is the updated graphics 
    for single player. *)
val update_graphics_1 : 'a option list list -> float -> int -> bool -> unit

(** [quit s] is the graphics screen when the game is quit. *)
val quit : 'a -> unit

(** [pause s] is the graphics screen when the game is paused. *)
val pause : 'a -> unit

(** [init_graphics s num_players] is where the first screen 
    when the game officially begins. *)
val init_graphics : 'a -> int -> unit 

(** [restart s lst] is the graphics screen when the player quits/loses/wins 
    where [s] gives the quit/lose/win message to display. *)
val restart : string -> float list -> (int * int) * (int * int)

(** [help s] draws the help screen with the instructions. *)
val help : 'a -> int * int

(** [start_window s] is the first window shown when the game starts. *)
val start_window : string -> string

(** [player_selection st] is the where the player(s) chooses 
    the number of players in the game. *)
val player_selection : 'a -> (int * int) * (int * int) * (int * int)

val player_selection_window : 'a -> int

(** [level_selection st] is where the player(s) chooses the difficulty. *)
val level_selection : 'a -> (int * int) * (int * int) * (int * int) * 
                            (int * int) * (int * int)

val level_selection_window : 'a -> string


val restart_window : string -> float list -> string
