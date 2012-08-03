let read_srs_secrets () = 
  let secret = ref "" in
  let secrets = ref [] in
  let ch = open_in (Config.srs_secret_file ()) in
  let first = ref true in
  (try
    while true do
      if !first then begin
        secret := input_line ch;
        first := false
      end else
        secrets := input_line ch :: !secrets
    done
  with End_of_file ->
    close_in ch);
  !secret, List.rev !secrets
