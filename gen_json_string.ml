let default_length = 10

type json_schema = {
  typ : string [@key "type"];
  items : (json_schema option [@default None]);
  num_items : (int [@key "numItems"] [@default default_length]);
  required : (string array [@default [||]]);
  properties : (Yojson.Safe.t [@default `Assoc []]);
  max_length : (int [@key "maxLength"] [@default default_length]);
} [@@deriving of_yojson { exn = true }]

let get_or_else default = function
  | Some value -> value
  | None -> default

let sequence_gen =
  let open QCheck.Gen in
  []
  |> return
  |> List.fold_left (fun list_gen gen -> map2 List.cons gen list_gen)

let assert_obj_fields required names =
  let names = Array.of_list names in
  if Array.exists (fun name -> not (Array.mem name names)) required
  then invalid_arg "Object must contain all required properties"

let null_schema = {
  typ = "null";
  items = None;
  num_items = default_length;
  required = [||];
  properties = `Assoc [];
  max_length = default_length;
}

let rec gen_json_char () =
  let open QCheck.Gen in
  int_range 33 127 >>= function
    | 34 | 92 -> gen_json_char ()
    | int -> int |> char_of_int |> return

let rec gen_json_string {
  typ;
  items;
  num_items;
  required;
  properties;
  max_length;
  _
} =
  let open QCheck.Gen in
  match typ with
  | "null" ->
    return "null"
  | "string" ->
    map
      (fun string -> {|"|} ^ string ^ {|"|})
      (string_size ~gen:(gen_json_char ()) (int_bound max_length))
  | "number" ->
    map string_of_float float
  | "integer" ->
    map string_of_int nat
  | "boolean" ->
    map string_of_bool bool
  | "array" ->
    items
    |> get_or_else null_schema
    |> gen_json_string
    |> list_size (return num_items)
    |> map (fun commalist -> "[" ^ String.concat ", " commalist ^ "]")
  | "object" ->
    begin match properties with
      | `Assoc list ->
        list |> List.map fst |> assert_obj_fields required;
        list
        |> List.filter_map (gen_field_pair required)
        |> sequence_gen
        |> map (fun commalist ->
          "{" ^ String.concat ", " (List.rev commalist) ^ "}")
      | _ -> invalid_arg "Invalid properties"
    end
  | _ ->
    invalid_arg "Invalid JSON schema type"

and gen_field_pair required (name, value) =
  if Array.mem name required || Random.bool () then
    Some (value
      |> json_schema_of_yojson_exn
      |> gen_json_string
      |> QCheck.Gen.map (fun value -> {|"|} ^ name ^ {|": |} ^ value))
  else
    None

let () =
  Random.self_init ();
  stdin
  |> Yojson.Safe.from_channel
  |> json_schema_of_yojson_exn
  |> gen_json_string
  |> QCheck.Gen.generate1 ~rand:(Random.State.make_self_init ())
  |> print_endline

