let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

let clients_lst = ref []

let broadcast sender msg =
  Lwt_list.iter_p
    (fun (_, client_out) ->
      if client_out != sender then Lwt_io.write_line client_out msg
      else Lwt.return_unit)
    !clients_lst

let client_handler client_socket_address (client_in, client_out) =
  let%lwt () =
    Lwt_io.printlf "I got a connection from %s."
      (string_of_sockaddr client_socket_address)
  in

  let%lwt username = Lwt_io.read_line client_in in
  clients_lst := (client_in, client_out) :: !clients_lst;

  let%lwt () =
    broadcast client_out (Printf.sprintf "%s has entered the chat." username)
  in

  let rec client_msg_loop () =
    try%lwt
      let%lwt msg = Lwt_io.read_line client_in in
      let message = Printf.sprintf "%s: %s" username msg in

      (* print msg in server terminal *)
      let%lwt () =
        Lwt_io.printlf "Message from %s (%s): %s" username
          (string_of_sockaddr client_socket_address)
          msg
      in
      (* then broadcast msg *)
      let%lwt () = broadcast client_out message in
      client_msg_loop ()
    with
    | End_of_file ->
        let%lwt () =
          Lwt_io.printlf "Client %s has disconnected."
            (string_of_sockaddr client_socket_address)
        in
        clients_lst :=
          List.filter (fun (_, co) -> co != client_out) !clients_lst;
        Lwt.return ()
    | _ ->
        let%lwt () = Lwt_io.printlf "Error occurred with handling client." in
        Lwt.return ()
  in
  client_msg_loop ()

let run_server addr =
  let server () =
    try%lwt
      let%lwt () =
        Lwt_io.printlf "I am the server on %s." (string_of_sockaddr addr)
      in
      let%lwt running_server =
        Lwt_io.establish_server_with_client_address addr client_handler
      in
      fst (Lwt.wait ())
    with _ -> Lwt.return ()
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

    let%lwt () = Lwt_io.write_line server_out username in

    (* loop to send messages *)
    let rec client_msg_loop () =
      let%lwt msg = Lwt_io.read_line Lwt_io.stdin in
      let%lwt () = Lwt_io.write_line server_out msg in
      client_msg_loop ()
    in

    (* loop to get messages *)
    let rec client_get_loop () =
      try%lwt
        let%lwt msg = Lwt_io.read_line server_in in
        let%lwt () = Lwt_io.printlf "%s" msg in
        client_get_loop ()
      with
      | End_of_file ->
          let%lwt () = Lwt_io.printlf "Disconnected from the server." in
          Lwt.return_unit
      | _ ->
          let%lwt () = Lwt_io.printlf "Error with receiving messages." in
          Lwt.return_unit
    in
    let%lwt () = Lwt.pick [ client_msg_loop (); client_get_loop () ] in
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
