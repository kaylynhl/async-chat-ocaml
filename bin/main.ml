let print_usage ~to_stderr =
  if to_stderr then prerr_endline (Chat_app.usage ~prog_name:Sys.argv.(0))
  else print_endline (Chat_app.usage ~prog_name:Sys.argv.(0))

let print_error msg = prerr_endline msg

let exit_with_usage ?error () =
  (match error with
  | Some msg -> print_error msg
  | None -> ());
  print_usage ~to_stderr:true;
  exit 1

let () =
  if
    Array.length Sys.argv = 2 && (Sys.argv.(1) = "--help" || Sys.argv.(1) = "-h")
  then print_usage ~to_stderr:false
  else
    match Chat_app.parse_cli Sys.argv with
    | Ok (Chat_app.Server, addr) -> Chat_app.run_server addr
    | Ok (Chat_app.Client username, addr) -> Chat_app.run_client addr username
    | Error "missing_required_arguments" -> exit_with_usage ()
    | Error msg -> exit_with_usage ~error:msg ()
