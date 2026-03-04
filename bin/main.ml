let _ =
  let print_usage () =
    Printf.printf "%s\n" (Chat_app.usage ~prog_name:Sys.argv.(0))
  in
  if Array.length Sys.argv < 4 then (
    print_usage ();
    exit 1)
  else
    try
      match Chat_app.parse_cli Sys.argv with
      | Ok (Chat_app.Server, addr) -> Chat_app.run_server addr
      | Ok (Chat_app.Client username, addr) -> Chat_app.run_client addr username
      | Error "missing_required_arguments" ->
          print_usage ();
          exit 1
      | Error msg ->
          Printf.printf "%s\n" msg;
          print_usage ();
          exit 1
    with
    | Unix.Unix_error _ ->
        Printf.printf "Error: Issue with connecting to network.\n";
        print_usage ();
        exit 1
    | _ ->
        Printf.printf "Please refer to the following usage.\n";
        print_usage ();
        exit 1
