## gen-json-string

OCaml CLI tool to generate random JSON value given a JSON Schema as input.
Currently supports a very small subset of JSON Schema.

The following instructions are for development use.

### Setup

You will need opam (version 2) and dune (a recent version). Run:

    opam switch create --locked
    opam install . --deps-only --locked

### Build

Run:

    dune build gen_json_string.exe

### Test

Run:

    echo '{"type": "object", "required": ["name", "age"], "properties": {"name": {"type": "string"}, "age": {"type": "int"}, "likes": {"type": "array", "items": {"type": "string"}}}}' | ./_build/default/gen_json_string.exe
