let my_name = Unix.gethostname ()

let my_entry_byname = Unix.gethostbyname my_name

let my_addr = my_entry_byname.Unix.h_addr_list.(0)

let s_descr = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0

let addr_in, p_num =
  match Unix.getsockname s_descr with
  | Unix.ADDR_INET (a, n) -> (a, n)
  | _ -> failwith "not INET"
;;

print_endline "About to bind";;

Unix.bind s_descr (Unix.ADDR_INET (my_addr, 1235));;

print_endline "Done binding";;

Unix.listen s_descr 2

let f, a = Unix.accept s_descr

let read () =
  let b = Bytes.make 100 ' ' in
  let _ = Unix.read f b 0 100 in
  ();
  let s = Bytes.sub_string b 2 98 in
  String.trim s

let send message =
  let message = message ^ "\n" in
  let _ = Unix.write_substring f message 0 (String.length message) in
  ()

let close () =
  Unix.close s_descr;
  Unix.close f
