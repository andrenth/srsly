let read_srs_secrets () = 
  let secret = ref None in
  let secrets = ref [] in
  let file = Config.srs_secret_file () in
  let ch = open_in file in
  (try
    while true do
      let line = input_line ch in
      if line <> "" then
        if !secret = None then
          secret := Some line
        else
          secrets := line :: !secrets
    done
  with End_of_file ->
    close_in ch);
  match !secret with
  | None -> failwith ("read_srs_secrets: no available secrets in " ^ file)
  | Some s -> s, List.rev !secrets
