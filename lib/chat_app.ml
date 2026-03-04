type command =
  | Server
  | Client of string

let usage ~prog_name =
  Printf.sprintf "Usage: %s <server | client> <ip> <port> [username]" prog_name

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

let parse_cli argv =
  if Array.length argv < 4 then Error "missing_required_arguments"
  else
    let ip = argv.(2) in
    let port = int_of_string argv.(3) in
    let addr = Unix.ADDR_INET (Unix.inet_addr_of_string ip, port) in
    match argv.(1) with
    | "server" -> Ok (Server, addr)
    | "client" ->
        if Array.length argv = 5 then Ok (Client argv.(4), addr)
        else
          Error
            "Error: Must provide a fifth argument for client username. To have \
             a space in your username, simply add quotation marks around the \
             argument."
    | _ ->
        Error
          "Error: First argument must be 'server' or 'client'. Case-sensitive."

type client =
  { id : int
  ; out_chan : Lwt_io.output_channel
  }

let clients : client list ref = ref []
let next_client_id = ref 0

let register_client out_chan =
  let id = !next_client_id in
  incr next_client_id;
  clients := { id; out_chan } :: !clients;
  id

let unregister_client id =
  clients := List.filter (fun client -> client.id <> id) !clients

(* Broadcast walks the mutable client set once and prunes writers that failed,
   which avoids repeatedly attempting dead connections on future sends. *)
let broadcast ~sender_id msg =
  let rec loop alive = function
    | [] ->
        clients := List.rev alive;
        Lwt.return_unit
    | client :: rest ->
        if client.id = sender_id then loop (client :: alive) rest
        else
          Lwt.catch
            (fun () ->
              let%lwt () = Lwt_io.write_line client.out_chan msg in
              loop (client :: alive) rest)
            (fun _exn -> loop alive rest)
  in
  loop [] !clients

let client_handler client_socket_address (client_in, client_out) =
  let%lwt () =
    Lwt_io.printlf "I got a connection from %s."
      (string_of_sockaddr client_socket_address)
  in
  Lwt.catch
    (fun () ->
      let%lwt username = Lwt_io.read_line client_in in
      let client_id = register_client client_out in
      let%lwt () =
        broadcast ~sender_id:client_id
          (Printf.sprintf "%s has entered the chat." username)
      in
      let rec client_msg_loop () =
        try%lwt
          let%lwt msg = Lwt_io.read_line client_in in
          let message = Printf.sprintf "%s: %s" username msg in
          let%lwt () =
            Lwt_io.printlf "Message from %s (%s): %s" username
              (string_of_sockaddr client_socket_address)
              msg
          in
          let%lwt () = broadcast ~sender_id:client_id message in
          client_msg_loop ()
        with
        | End_of_file ->
            let%lwt () =
              Lwt_io.printlf "Client %s has disconnected."
                (string_of_sockaddr client_socket_address)
            in
            unregister_client client_id;
            Lwt.return_unit
        | _ ->
            let%lwt () = Lwt_io.printlf "Error occurred with handling client." in
            unregister_client client_id;
            Lwt.return_unit
      in
      client_msg_loop ())
    (fun _ ->
      Lwt_io.printlf "Client %s has disconnected."
        (string_of_sockaddr client_socket_address))

let run_server addr =
  let server () =
    let%lwt () = Lwt_io.printlf "I am the server on %s." (string_of_sockaddr addr) in
    let%lwt _running_server =
      Lwt_io.establish_server_with_client_address addr client_handler
    in
    fst (Lwt.wait ())
  in
  Lwt_main.run (Lwt.catch server (fun _ -> Lwt.return_unit))

let run_client addr username =
  let client () =
    let%lwt () =
      Lwt_io.printlf "I am a client connecting to %s with username %s."
        (string_of_sockaddr addr) username
    in
    let%lwt server_in, server_out = Lwt_io.open_connection addr in
    let%lwt () = Lwt_io.printlf "I connected to the server." in
    let%lwt () = Lwt_io.write_line server_out username in
    let rec client_msg_loop () =
      let%lwt msg = Lwt_io.read_line Lwt_io.stdin in
      let%lwt () = Lwt_io.write_line server_out msg in
      client_msg_loop ()
    in
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
    Lwt.return_unit
  in
  Lwt_main.run (client ())
