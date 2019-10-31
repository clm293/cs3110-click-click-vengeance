open Graphics

type key = Up | Down | Left | Right | Space

type inpt = key option

let get_key_pressed = 
  let e = wait_next_event [Key_pressed] in
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