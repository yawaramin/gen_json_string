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

let random_length () = Random.int 31

let random_char () = char_of_int (Random.int 94 + 33)

let random_string () =
  String.init (random_length ()) (fun _ -> random_char ())

let assert_obj_fields required names =
  let names = Array.of_list names in
  if Array.exists (fun name -> not (Array.mem name names)) required
  then failwith "Object must contain all required properties"

let rec gen_json_string {typ; items; num_items; required; properties} =
  match typ with
  | "null" -> "null"
  | "string" -> {|"|} ^ random_string () ^ {|"|}
  | "number" -> 10. |> Random.float |> string_of_float
  | "int" -> () |> random_length |> string_of_int
  | "boolean" -> () |> Random.bool |> string_of_bool
  | "array" ->
    let schema = get_or_else
      {
        typ = "null";
        items = None;
        num_items = None;
        required = [||];
        properties = `Assoc [];
      }
      items
    in
    let num_items = get_or_else (random_length ()) num_items in
    let commalist = (fun _ -> gen_json_string schema)
      |> List.init num_items
      |> String.concat ", "
    in
    "[" ^ commalist ^ "]"
  | "object" ->
    let commalist = match properties with
      | `Assoc list ->
        list |> List.map fst |> assert_obj_fields required;
        list |> filter_map (field_pair required) |> String.concat ", "
      | _ -> failwith "Invalid properties"
    in
    "{" ^ commalist ^ "}"
  | _ -> failwith "Invalid JSON schema type"

and field_pair required (name, value) =
  match json_schema_of_yojson value, Array.mem name required with
  | Ok schema, is_required when is_required || Random.bool () ->
    Some (name ^ ": " ^ gen_json_string schema)
  | _ -> None

let () =
  Random.self_init ();
  let output =
    match stdin |> Yojson.Safe.from_channel |> json_schema_of_yojson with
    | Ok schema -> gen_json_string schema
    | Error msg -> failwith msg
  in
  print_endline output
