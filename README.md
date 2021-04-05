## gen_json_string

OCaml CLI tool to generate random JSON value given a JSON Schema as
input. Currently supports a very small subset of JSON Schema.

Ref. https://json-schema.org/understanding-json-schema/reference/index.html

### License

GNU GPL v3 or later.

### Setup

Ensure that you have the dependencies listed in `dune-project`.

### Build

Run:

    dune build gen_json_string.exe

### Test

Run:

    dune exec ./gen_json_string.exe <<<'{"type": "object", "required": ["name", "age"], "properties": {"name": {"type": "string"}, "age": {"type": "integer"}, "likes": {"type": "array", "items": {"type": "string"}}}}'

