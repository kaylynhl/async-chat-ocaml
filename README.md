# OCaml Lwt Chat

A small terminal-based TCP chat server and client written in OCaml with Lwt.
It supports multiple concurrent clients and message broadcast over a shared
server connection.

## Why this is interesting

- Uses cooperative concurrency (`Lwt`) for non-blocking networking.
- Maintains live client membership and prunes failed connections safely.
- Separates CLI parsing from runtime behavior for testability.
- Includes deterministic OUnit tests for argument parsing and formatting paths.
- Uses Dune for reproducible build and test workflows.

## Build, run, test

```sh
dune build
dune exec ocaml-lwt-chat -- server 127.0.0.1 9000
dune exec ocaml-lwt-chat -- client 127.0.0.1 9000 "alice"
dune test
```

## Example usage

Start a server in one terminal:

```sh
dune exec ocaml-lwt-chat -- server 127.0.0.1 9000
```

Join from another terminal:

```sh
dune exec ocaml-lwt-chat -- client 127.0.0.1 9000 "alice"
```

Join from a third terminal:

```sh
dune exec ocaml-lwt-chat -- client 127.0.0.1 9000 "bob"
```

Messages typed by each client are broadcast to other connected clients.

Originally written for coursework and later revisited to improve structure,
tests, and documentation.
