type json_schema = {
  typ : string [@key "type"];
  items : (json_schema option [@default None]);
  num_items : (int option [@key "numItems"] [@default None]);
  required : (string array [@default [||]]);
  properties : (Yojson.Safe.t [@default `Assoc []]);
} [@@deriving of_yojson]

let get_or_else default = function
  | Some value -> value
  | None -> default

let filter_map f list = list
  |> List.map f
  |> List.filter (function Some _ -> true | None -> false)
  |> List.map (function
    | Some value -> value
    | None ->
      failwith "This branch is not reachable because all Nones have already been filtered out")

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
  num_items = None;
  required = [||];
  properties = `Assoc [];
}

let rec gen_json_char () =
  let open QCheck.Gen in
  int_range 33 127 >>= function
    | 34 | 92 -> gen_json_char ()
    | int -> int |> char_of_int |> return

let int10 = QCheck.Gen.int_bound 10

let rec gen_json_string { typ; items; num_items; required; properties; _ } =
  let open QCheck.Gen in
  match typ with
  | "null" -> return "null"
  | "string" ->
    map
      (fun string -> {|"|} ^ string ^ {|"|})
      (string_size ~gen:(gen_json_char ()) int10)
  | "number" -> map string_of_float float
  | "integer" -> map string_of_int nat
  | "boolean" -> map string_of_bool bool
  | "array" ->
    let num_items = Option.fold ~none:int10 ~some:return num_items in
    items
    |> get_or_else null_schema
    |> gen_json_string
    |> list_size num_items
    |> map (fun commalist -> "[" ^ String.concat ", " commalist ^ "]")
  | "object" ->
    begin match properties with
      | `Assoc list ->
        list |> List.map fst |> assert_obj_fields required;
        list
        |> filter_map (gen_field_pair required)
        |> sequence_gen
        |> map (fun commalist ->
          "{" ^ String.concat ", " (List.rev commalist) ^ "}")
      | _ -> invalid_arg "Invalid properties"
    end
  | _ -> invalid_arg "Invalid JSON schema type"

and gen_field_pair required (name, value) =
  match json_schema_of_yojson value with
  | Ok schema when Array.mem name required || Random.bool () ->
    let field_pair = schema
      |> gen_json_string
      |> QCheck.Gen.map (fun value -> {|"|} ^ name ^ {|": |} ^ value)
    in
    Some field_pair
  | _ -> None

let () =
  Random.self_init ();
  let output =
    match stdin |> Yojson.Safe.from_channel |> json_schema_of_yojson with
    | Ok schema -> QCheck.Gen.generate1 (gen_json_string schema)
    | Error msg -> invalid_arg msg
  in
  print_endline output

