module type Tap = sig

  type tap = Hit | Miss | Pause

  (** The abstract data type of values representing a player tap. *)
  type t

  (** [check_hit st t] is true if the player's tap is a hit, and false otherwise*)
  val check_hit : Game.t -> tap


end 