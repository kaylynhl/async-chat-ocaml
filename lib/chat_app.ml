type command =
  | Server
  | Client of string

let usage ~prog_name =
  Printf.sprintf "Usage: %s <server | client> <ip> <port> [username]" prog_name

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (ip, port) ->
      Printf.sprintf "%s:%d" (Unix.string_of_inet_addr ip) port

let parse_port raw_port =
  match int_of_string_opt raw_port with
  | None -> Error "Error: Port must be an integer between 1 and 65535."
  | Some port when port < 1 || port > 65535 ->
      Error "Error: Port must be an integer between 1 and 65535."
  | Some port -> Ok port

let parse_addr ip raw_port =
  match Unix.inet_addr_of_string ip with
  | exception Failure _ -> Error "Error: IP must be a valid IPv4/IPv6 address."
  | inet_addr ->
      let open Result in
      parse_port raw_port |> map (fun port -> Unix.ADDR_INET (inet_addr, port))

let parse_cli argv =
  if Array.length argv < 4 then Error "missing_required_arguments"
  else
    let ip = argv.(2) in
    let raw_port = argv.(3) in
    match parse_addr ip raw_port with
    | Error msg -> Error msg
    | Ok addr -> (
        match argv.(1) with
        | "server" ->
            if Array.length argv = 4 then Ok (Server, addr)
            else Error "Error: Server mode does not accept extra arguments."
        | "client" ->
            if Array.length argv = 5 then Ok (Client argv.(4), addr)
            else
              Error
                (if Array.length argv < 5 then
                   "Error: Must provide a fifth argument for client username. \
                    To have a space in your username, simply add quotation \
                    marks around the argument."
                 else "Error: Client mode accepts only one username argument.")
        | _ ->
            Error
              "Error: First argument must be 'server' or 'client'. \
               Case-sensitive.")

type client = { out_chan : Lwt_io.output_channel }

let clients : (int, client) Hashtbl.t = Hashtbl.create 16
let clients_lock = Lwt_mutex.create ()
let next_client_id = ref 0
let with_clients_lock f = Lwt_mutex.with_lock clients_lock f

let register_client out_chan =
  with_clients_lock (fun () ->
      let id = !next_client_id in
      incr next_client_id;
      Hashtbl.replace clients id { out_chan };
      Lwt.return id)

let unregister_client id =
  with_clients_lock (fun () ->
      Hashtbl.remove clients id;
      Lwt.return_unit)

let snapshot_clients () =
  with_clients_lock (fun () ->
      let snapshot = Hashtbl.to_seq clients |> List.of_seq in
      Lwt.return snapshot)

let remove_disconnected client_ids =
  if client_ids = [] then Lwt.return_unit
  else
    with_clients_lock (fun () ->
        List.iter (fun id -> Hashtbl.remove clients id) client_ids;
        Lwt.return_unit)

(* Broadcast to all clients except sender, then prune dead connections in one
   pass. *)
let broadcast ~sender_id msg =
  let%lwt snapshot = snapshot_clients () in
  let rec loop disconnected = function
    | [] -> remove_disconnected disconnected
    | (id, client) :: rest ->
        if id = sender_id then loop disconnected rest
        else
          Lwt.catch
            (fun () ->
              let%lwt () = Lwt_io.write_line client.out_chan msg in
              loop disconnected rest)
            (fun _exn -> loop (id :: disconnected) rest)
  in
  loop [] snapshot

let client_handler client_socket_address (client_in, client_out) =
  let%lwt () =
    Lwt_io.printlf "I got a connection from %s."
      (string_of_sockaddr client_socket_address)
  in
  Lwt.catch
    (fun () ->
      let%lwt username = Lwt_io.read_line client_in in
      let%lwt client_id = register_client client_out in
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
            let%lwt () = unregister_client client_id in
            Lwt.return_unit
        | _ ->
            let%lwt () =
              Lwt_io.printlf "Error occurred while handling client."
            in
            let%lwt () = unregister_client client_id in
            Lwt.return_unit
      in
      client_msg_loop ())
    (fun _ ->
      Lwt_io.printlf "Client %s has disconnected."
        (string_of_sockaddr client_socket_address))

let run_server addr =
  let server () =
    let%lwt () =
      Lwt_io.printlf "I am the server on %s." (string_of_sockaddr addr)
    in
    let%lwt _running_server =
      Lwt_io.establish_server_with_client_address addr client_handler
    in
    fst (Lwt.wait ())
  in
  Lwt_main.run
    (Lwt.catch server (fun exn ->
         let%lwt () =
           Lwt_io.eprintlf "Server error: %s" (Printexc.to_string exn)
         in
         Lwt.return_unit))

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
  Lwt_main.run
    (Lwt.catch client (fun exn ->
         let%lwt () =
           Lwt_io.eprintlf "Client error: %s" (Printexc.to_string exn)
         in
         Lwt.return_unit))
