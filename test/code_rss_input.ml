(* SPDX-License-Identifier: BSD-3-Clause *)
(* Copyright (c) 2023-2026 Octra Labs <dev@octra.org> *)

let byte value = String.make 1 (Char.chr value)

let rec number limit value =
  if value < limit then byte value
  else byte ((value land 127) lor 128) ^ number limit (value lsr 7)

let size = number 128
let text value = size (String.length value) ^ value
let vector values = size (List.length values) ^ String.concat "" values
let section tag value = byte tag ^ text value
let const value = "\065" ^ number 64 value
let export name kind index = text name ^ byte kind ^ size index
let body instructions = text ("\000" ^ instructions ^ "\011")

let make tag =
  let manifest = {|{"methods":[{"name":"query","view":true},{"name":"update","view":false}]}|} in
  let response = "OCWS1\003\000\000\000\0017" in
  let data = manifest ^ response in
  let write offset length = const offset ^ const length ^ "\016\000\026" ^ const 0 in
  let count = 6000 in
  let work = "\032\000" ^ String.concat "" (List.init 32 (fun _ -> "\032\000\106")) in
  String.concat "" [
    "\000asm\001\000\000\000";
    section 0 (text "rss-case" ^ size tag);
    section 1 (vector ["\096\002\127\127\001\127"; "\096\001\127\001\127"]);
    section 2 (vector [text "octra" ^ text "host_response_write" ^ "\000\000"]);
    section 3 (vector ([size 1; size 0; size 0] @ List.init count (fun _ -> size 1)));
    section 5 (vector ["\000\001"]);
    section 7 (vector [export "memory" 2 0; export "octra_alloc" 0 1;
      export "octra_query" 0 2; export "octra_update" 0 2; export "octra_manifest" 0 3]);
    section 10 (vector ([body (const (String.length data));
      body (write (String.length manifest) (String.length response));
      body (write 0 (String.length manifest))]
      @ List.init count (fun _ -> body work)));
    section 11 (vector ["\000" ^ const 0 ^ "\011" ^ text data]);
  ]