open OUnit2

let tests =
  "chat app"
  >::: [
         ( "usage includes command shape" >:: fun _ ->
           let usage = Chat_app.usage ~prog_name:"chat" in
           assert_equal "Usage: chat <server | client> <ip> <port> [username]"
             usage );
         ( "string_of_sockaddr formats inet endpoints" >:: fun _ ->
           let addr = Unix.ADDR_INET (Unix.inet_addr_loopback, 9000) in
           assert_equal "127.0.0.1:9000" (Chat_app.string_of_sockaddr addr) );
         ( "parse_cli accepts server mode" >:: fun _ ->
           let argv = [| "chat"; "server"; "127.0.0.1"; "9000" |] in
           match Chat_app.parse_cli argv with
           | Ok (Chat_app.Server, Unix.ADDR_INET (_, 9000)) -> ()
           | _ -> assert_failure "expected server parse success" );
         ( "parse_cli rejects extra args in server mode" >:: fun _ ->
           let argv = [| "chat"; "server"; "127.0.0.1"; "9000"; "extra" |] in
           match Chat_app.parse_cli argv with
           | Error "Error: Server mode does not accept extra arguments." -> ()
           | Error _ -> assert_failure "expected server extra-argument error"
           | Ok _ -> assert_failure "expected parse error for extra args" );
         ( "parse_cli accepts client mode with username" >:: fun _ ->
           let argv = [| "chat"; "client"; "127.0.0.1"; "9000"; "alice" |] in
           match Chat_app.parse_cli argv with
           | Ok (Chat_app.Client "alice", Unix.ADDR_INET (_, 9000)) -> ()
           | _ -> assert_failure "expected client parse success" );
         ( "parse_cli rejects extra args in client mode" >:: fun _ ->
           let argv =
             [| "chat"; "client"; "127.0.0.1"; "9000"; "alice"; "x" |]
           in
           match Chat_app.parse_cli argv with
           | Error "Error: Client mode accepts only one username argument." ->
               ()
           | Error _ -> assert_failure "expected client extra-argument error"
           | Ok _ -> assert_failure "expected parse error for extra args" );
         ( "parse_cli rejects unknown mode" >:: fun _ ->
           let argv = [| "chat"; "host"; "127.0.0.1"; "9000" |] in
           match Chat_app.parse_cli argv with
           | Error msg ->
               assert_equal
                 "Error: First argument must be 'server' or 'client'. \
                  Case-sensitive."
                 msg
           | Ok _ -> assert_failure "expected unknown mode parse error" );
         ( "parse_cli requires username in client mode" >:: fun _ ->
           let argv = [| "chat"; "client"; "127.0.0.1"; "9000" |] in
           match Chat_app.parse_cli argv with
           | Error msg ->
               assert_bool "expected username error"
                 (String.starts_with ~prefix:"Error:" msg)
           | Ok _ -> assert_failure "expected username parse error" );
         ( "parse_cli rejects non-numeric port" >:: fun _ ->
           let argv = [| "chat"; "server"; "127.0.0.1"; "abc" |] in
           match Chat_app.parse_cli argv with
           | Error "Error: Port must be an integer between 1 and 65535." -> ()
           | Error _ -> assert_failure "expected specific port validation error"
           | Ok _ -> assert_failure "expected parse error for non-numeric port"
         );
         ( "parse_cli rejects out-of-range port" >:: fun _ ->
           let argv = [| "chat"; "server"; "127.0.0.1"; "70000" |] in
           match Chat_app.parse_cli argv with
           | Error "Error: Port must be an integer between 1 and 65535." -> ()
           | Error _ -> assert_failure "expected specific port range error"
           | Ok _ -> assert_failure "expected parse error for out-of-range port"
         );
         ( "parse_cli rejects invalid ip" >:: fun _ ->
           let argv = [| "chat"; "server"; "999.999.1.1"; "9000" |] in
           match Chat_app.parse_cli argv with
           | Error "Error: IP must be a valid IPv4/IPv6 address." -> ()
           | Error _ -> assert_failure "expected specific IP validation error"
           | Ok _ -> assert_failure "expected parse error for invalid ip" );
       ]

let _ = run_test_tt_main tests
