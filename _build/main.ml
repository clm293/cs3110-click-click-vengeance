
type key = Up | Down | Left | Right | Space

type inpt = key option

let get_key_pressed = 
  let e = Graphics.wait_next_event [Key_pressed] in
  match e.key with
  | 'i' -> Some Up
  | 'j' -> Some Left
  | 'k' -> Some Down
  | 'l' -> Some Right
  | _ -> None

let rec loop st = 
  match get_key_pressed with
  | Some Up -> Game.update st "up"
  | Some Left -> Game.update st "left"
  | Some Right -> Game.update st "right"
  | Some Down -> Game.update st "down"
  | Some Space -> failwith "pause"
  | None -> failwith "bad key"

(* let init_graphics = *)

let play_game song_file num_players =
  let song = Song.from_json (Yojson.Basic.from_file song_file) in
  let game = Game.init_state num_players (Song.bpm song) in
  Graphics.open_graph ""


let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to Tap Tap Revenge.\n");
  print_endline "Please enter the song you wish to play\n";
  print_string  "> ";
  (* 
  let song = match read_line () with
    | exception End_of_file -> ()
    | song_file -> play_game song_file *)

  (* print_endline "Please enter the song you wish to play\n";
     print_string  "> ";
     let num_players = match read_line () with
     | exception End_of_file -> ()
     | num -> Game.set_number_players  *)

