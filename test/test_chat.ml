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
         ( "parse_cli requires username in client mode" >:: fun _ ->
           let argv = [| "chat"; "client"; "127.0.0.1"; "9000" |] in
           match Chat_app.parse_cli argv with
           | Error msg ->
               assert_bool "expected username error"
                 (String.starts_with ~prefix:"Error:" msg)
           | Ok _ -> assert_failure "expected username parse error" );
       ]

let _ = run_test_tt_main tests
