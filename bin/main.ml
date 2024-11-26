let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

let client_handler client_socket_address (client_in, client_out) =
  let%lwt () =
    Lwt_io.printlf "I got a connection from %s."
      (string_of_sockaddr client_socket_address)
  in
  let%lwt msg = Lwt_io.read_line client_in in
  let%lwt () =
    Lwt_io.printlf "Message received from client %s. %s"
      (string_of_sockaddr client_socket_address)
      msg
  in
  Lwt.return ()

let run_server addr =
  let server () =
    let%lwt () =
      Lwt_io.printlf "I am the server on %s." (string_of_sockaddr addr)
    in
    let%lwt running_server =
      Lwt_io.establish_server_with_client_address addr client_handler
    in
    fst (Lwt.wait ())
  in
  Lwt_main.run (server ())

let run_client addr username =
  let client () =
    let%lwt () =
      Lwt_io.printlf "I am a client connecting to %s with username %s."
        (string_of_sockaddr addr) username
    in
    let%lwt server_in, server_out = Lwt_io.open_connection addr in
    let%lwt () = Lwt_io.printlf "I connected to the server." in
    let%lwt msg = Lwt_io.read_line Lwt_io.stdin in
    let%lwt () = Lwt_io.write_line server_out msg in
    Lwt.return ()
  in
  Lwt_main.run (client ())

let _ =
  let print_usage () =
    Printf.printf "Usage: %s <server | client> <ip> <port> [username]\n"
      Sys.argv.(0)
  in
  if Array.length Sys.argv < 4 then (
    print_usage ();
    exit 1)
  else
    try
      let ip = Sys.argv.(2) in
      let port = int_of_string Sys.argv.(3) in
      let addr = Unix.ADDR_INET (Unix.inet_addr_of_string ip, port) in
      match Sys.argv.(1) with
      | "server" -> run_server addr
      | "client" ->
          if Array.length Sys.argv = 5 then
            let username = Sys.argv.(4) in
            run_client addr username
          else (
            Printf.printf
              "Error: Must provide a fifth argument for client username. To \
               have a space in your username, simply add quotation marks \
               around the argument.\n";
            print_usage ();
            exit 1)
      | _ ->
          Printf.printf
            "Error: First argument must be 'server' or 'client'. Case-sensitive.\n";
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
