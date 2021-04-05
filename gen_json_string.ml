type json_schema = {
  enum : (string list option [@default None]);
  items : (json_schema option [@default None]);
  maximum : (float option [@default None]);
  max_items : (int option [@default None]) [@key "maxItems"];
  max_length : (int option [@default None]) [@key "maxLength"];
  min_length : (int option [@default None]) [@key "minLength"];
  min_items : (int option [@default None]) [@key "minItems"];
  minimum : (float option [@default None]);
  num_items : (int option [@default None]) [@key "numItems"];
  properties : (Yojson.Safe.t [@default `Assoc []]);
  required : (string array [@default [||]]);
  typ : string [@key "type"];
} [@@deriving of_yojson { exn = true }]

(* This is the same as [Option.value] except we can use it with [|>]. *)
let get_or_else default = function Some value -> value | None -> default

(* This tool is not a JSON Schema validation tool, but we do need to do
   some minimal validation in order to actually generate a valid object. *)
let assert_obj_fields required names =
  let names = Array.of_list names in
  if Array.exists (fun name -> not (Array.mem name names)) required
  then invalid_arg "assert_obj: object must contain all required properties"

let null_schema = {
  enum = None;
  items = None;
  maximum = None;
  max_items = None;
  max_length = None;
  min_length = None;
  min_items = None;
  minimum = None;
  num_items = None;
  properties = `Assoc [];
  required = [||];
  typ = "null";
}

open QCheck.Gen

let sequence_gen = []
  |> return
  |> List.fold_left (fun list_gen gen -> map2 List.cons gen list_gen)

let rec gen_json_char () =
  let* int = int_range 33 127 in
  match int with
  | 34
  | 92 -> gen_json_char ()
  | _ -> int |> char_of_int |> return

let gen_string min_length max_length = function
  | Some enum ->
    oneofl enum
  | None ->
    let length = match min_length, max_length with
      | Some min_length, Some max_length -> min_length -- max_length
      | Some min_length, None -> min_length -- Int.max_int
      | None, Some max_length -> 0 -- max_length
      | None, None -> small_nat
    in
    let+ string = string_size ~gen:(gen_json_char ()) length in
    {|"|} ^ string ^ {|"|}

let gen_number minimum maximum = match minimum, maximum with
  | Some minimum, Some maximum -> minimum --. maximum
  | Some minimum, None -> minimum --. Float.max_float
  | None, Some maximum -> 0. --. maximum
  | None, None -> float

let render_int float = float |> int_of_float |> string_of_int

let rec gen_json_string = function
  | { typ = "null"; _ } ->
    return "null"
  | { typ = "string"; min_length; max_length; enum; _ } ->
    gen_string min_length max_length enum
  | { typ = "number"; minimum; maximum; _ } ->
    map string_of_float (gen_number minimum maximum)
  | { typ = "integer"; minimum; maximum; _ } ->
    map render_int (gen_number minimum maximum)
  | { typ = "boolean"; _ } ->
    map string_of_bool bool
  | { typ = "array"; min_items; num_items; max_items; items; _ } ->
    gen_array min_items num_items max_items items
  | { typ = "object"; required; properties; _ } ->
    gen_object required properties
  | { typ; _ } ->
    invalid_arg ("gen_json_string: invalid JSON Schema type: " ^ typ)

and gen_array_items num_items items =
  items
  |> gen_json_string
  |> list_size (return num_items)
  |> map (fun commalist -> "[" ^ String.concat ", " commalist ^ "]")

and gen_array min_items num_items max_items items =
  let items = get_or_else null_schema items in
  let* num_items = match min_items, num_items, max_items with
    | _, Some num_items, _ -> return num_items
    | Some min_items, None, Some max_items -> min_items -- max_items
    | Some min_items, None, None -> min_items -- Int.max_int
    | None, None, Some max_items -> 0 -- max_items
    | None, None, None -> small_nat
  in
  gen_array_items num_items items

and gen_object required properties = match properties with
  | `Assoc list ->
    list |> List.map fst |> assert_obj_fields required;
    list
    |> List.filter_map (gen_field_pair required)
    |> sequence_gen
    |> map (fun commalist ->
      "{" ^ String.concat ", " (List.rev commalist) ^ "}")
  | _ ->
    invalid_arg ("gen_object: invalid properties: " ^ Yojson.Safe.to_string properties)

and gen_field_pair required (name, value) =
  if Array.mem name required || Random.bool () then
    Some (value
      |> json_schema_of_yojson_exn
      |> gen_json_string
      |> map (fun value -> {|"|} ^ name ^ {|": |} ^ value))
  else
    None

let () =
  Random.self_init ();
  stdin
  |> Yojson.Safe.from_channel
  |> json_schema_of_yojson_exn
  |> gen_json_string
  |> generate1 ~rand:(Random.State.make_self_init ())
  |> print_endline
