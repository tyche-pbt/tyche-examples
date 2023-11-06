open Bst_demo.Bst
open! Core

type 'a visualizer = {
  to_string : 'a -> string;
  features : ('a -> int) Map.M(String).t;
  bucketings : ('a -> string) Map.M(String).t;
}

let bst_visualizer =
  {
    to_string = (fun t -> [%sexp_of: tree] t |> Sexp.to_string_hum);
    features = Map.of_alist_exn (module String) [ ("size", size) ];
    bucketings =
      Map.of_alist_exn
        (module String)
        [ ("is_bst", fun t -> if is_bst t then "valid" else "invalid") ];
  }

let test g v name ~f =
  let samples = ref [] in
  Quickcheck.test g ~trials:100 ~f:(fun t ->
      let item = `String (v.to_string t) in
      let features =
        Map.to_alist v.features
        |> List.map ~f:(fun (name, f) -> (name, `Int (f t)))
        |> fun x -> `Assoc x
      in
      let bucketings =
        Map.to_alist v.bucketings
        |> List.map ~f:(fun (name, f) -> (name, `String (f t)))
        |> fun x -> `Assoc x
      in
      samples :=
        `Assoc
          [ ("item", item); ("features", features); ("bucketings", bucketings) ]
        :: !samples;
      f t);
  let report =
    `Assoc
      [
        ("type", `String "success");
        ( "report",
          `Assoc
            [
              ( "properties",
                `Assoc
                  [
                    ( name,
                      `Assoc
                        [
                          ("samples", `List !samples);
                          ("outcome", `String "propertyPassed");
                          ("coverage", `Assoc []);
                        ] );
                  ] );
            ] );
      ]
  in
  let outc = Out_channel.create "/tmp/thing.json" in
  Out_channel.output_string outc (Yojson.Safe.to_string report);
  Out_channel.close outc;
  let websocket_script =
    "python -c \"import websocket; import json; report = \
     open('/tmp/thing.json', 'r').read(); ws = \
     websocket.create_connection('ws://localhost:8181'); ws.send(report)\""
  in
  Sys_unix.command_exn websocket_script

let () =
  test [%quickcheck.generator: tree] bst_visualizer "insert_valid" ~f:(fun t ->
      if is_bst t then assert (is_bst (insert (Random.int 100) t)) else ())
