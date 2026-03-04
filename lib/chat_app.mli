type command =
  | Server
  | Client of string

val usage : prog_name:string -> string
val string_of_sockaddr : Unix.sockaddr -> string
val parse_cli : string array -> (command * Unix.sockaddr, string) result
val run_server : Unix.sockaddr -> unit
val run_client : Unix.sockaddr -> string -> unit
