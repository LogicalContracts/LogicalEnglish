# Logical English Web API (Preliminary DRAFT)

The LE API is a JSON-over-HTTP REST endpoint served at `/leapi` (default port 3050).

## Authentication

Every request must include `"token": "myToken123"` in the JSON body.

## Request / Response format

All requests are HTTP POST with `Content-Type: application/json`.  
All responses are JSON objects. On failure the response contains an `error` key.

---

## Operations

### `examples` — Retrieve a built-in example document

Returns the source text of a named LE example file.

**Request**

```json
{
  "token": "myToken123",
  "operation": "examples",
  "file": "<example_name>"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `file` | string | Name of the example (without extension), e.g. `"1_net_asset_value_test_3"` |

**Response**

```json
{ "document": "<LE source text>" }
```

On error: `{ "answer": "...", "details": "...", "document": "" }`

**curl example**

```bash
curl -s -X POST http://localhost:3050/leapi \
  -H 'Content-Type: application/json' \
  -d '{"token":"myToken123","operation":"examples","file":"1_net_asset_value_test_3"}'
```

---

### `answer` — Parse a document and answer one query/scenario pair

Loads the LE document, applies the named scenario, and returns an explanation for a single query.

**Request**

```json
{
  "token": "myToken123",
  "operation": "answer",
  "file": "<program_name>",
  "document": "<LE source text>",
  "theQuery": "<query sentence>",
  "scenario": "<scenario name>"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `file` | string | Logical name / identifier for the program |
| `document` | string | Full LE source text |
| `theQuery` | string | Name of the query to run, e.g. `"one"` |
| `scenario` | string | Name of the scenario to use, e.g. `"alice"` |

**Response**

```json
{ "answer": <explanation term> }
```

**curl example**

```bash
curl -s -X POST http://localhost:3050/leapi \
  -H 'Content-Type: application/json' \
  -d '{"token":"myToken123","operation":"answer","file":"testingle",
       "document":"...","theQuery":"one","scenario":"alice"}'
```

---

### `explain` — Parse a document and return all answers for a query/scenario

Like `answer` but collects every answer, not just the first.

**Request**

```json
{
  "token": "myToken123",
  "operation": "explain",
  "file": "<program_name>",
  "document": "<LE source text>",
  "theQuery": "<query sentence>",
  "scenario": "<scenario name>"
}
```

Same fields as `answer`.

**Response**

```json
{ "results": [ <explanation>, ... ] }
```

---

### `answer_via_llm` — Translate free-form user input via an LLM, then answer

Sends `userinput` to a configured LLM (Gemini) to generate a new LE scenario/query pair, appends it to `document`, and then runs the combined program.

**Environment variables required on the server:**

| Variable | Description |
|----------|-------------|
| `LE_LLM_K` | API key for the LLM service |
| `USED_LLM` | Model identifier, e.g. `gemini-2.5-flash` |

**Request**

```json
{
  "token": "myToken123",
  "operation": "answer_via_llm",
  "file": "<program_name>",
  "document": "<LE source text>",
  "userinput": "<free-form description / question>"
}
```

| Field | Type | Description |
|-------|------|-------------|
| `file` | string | Logical name for the program |
| `document` | string | Existing LE source text used as context |
| `userinput` | string | Natural-language description of the situation and question |

**Response**

```json
{
  "results": [ <explanation>, ... ],
  "translation": "<LLM-generated LE text>"
}
```

On failure: `"results"` contains an error dict and `"translation"` is `"I did not understand"`.

**curl example**

```bash
curl -s -X POST http://localhost:3050/leapi \
  -H 'Content-Type: application/json' \
  -d '{"token":"myToken123","operation":"answer_via_llm",
       "file":"testllm","document":"...","userinput":"Is Alice eligible?"}'
```

---

### `load` — Load a LE or Prolog program into a fresh session module

Parses and asserts a LE (or plain Prolog) program, returning its metadata.

**Request — inline LE source**

```json
{
  "token": "myToken123",
  "operation": "load",
  "le": "<LE source text>"
}
```

**Request — load from file**

```json
{
  "token": "myToken123",
  "operation": "load",
  "file": "<path under /moreExamples/>"
}
```

The `file` path must reside under `/moreExamples/`. Files ending in `.le` are parsed as Logical English; all others are loaded as plain Prolog.

**Response**

```json
{
  "sessionModule": "<generated module name>",
  "kb": "<kb name or null>",
  "predicates": [ "<predicate/arity>", ... ],
  "examples": [ { "name": "...", "scenarios": [ { "assertion": "...", "clauses": "..." } ] } ],
  "queries": [ "<query term>", ... ],
  "language": "le | prolog",
  "target": "taxlog | prolog"
}
```

The `sessionModule` value must be passed to subsequent `answeringQuery` and `loadFactsAndQuery` calls.

---

### `le2prolog` — Translate a LE program to Prolog source

Returns the Prolog text equivalent of a LE program without asserting anything.

**Request**

```json
{
  "token": "myToken123",
  "operation": "le2prolog",
  "le": "<LE source text>"
}
```

**Response**

```json
{
  "prolog": "<Prolog source text>",
  "kb": "<kb name>",
  "predicates": [ "<predicate string>", ... ],
  "examples": [ ... ],
  "queries": "<Prolog query clauses>",
  "target": "taxlog | prolog"
}
```

---

### `answeringQuery` — Run an English query against a loaded session module

Requires a prior `load` call to obtain `sessionModule`.

**Request**

```json
{
  "token": "myToken123",
  "operation": "answeringQuery",
  "query": "<English query string>",
  "scenario": "<Prolog scenario term>",
  "sessionModule": "<module name from load>"
}
```

**Response**

```json
{ "answer": "<answer term as string>", "result": "ok" }
```

---

### `loadFactsAndQuery` — Assert facts into a session module and run a goal

Asserts a list of ground facts into an existing session module, then optionally evaluates a goal against them.

**Request**

```json
{
  "token": "myToken123",
  "operation": "loadFactsAndQuery",
  "sessionModule": "<module name>",
  "facts": [ "<fact term string>", ... ],
  "goal": "<Prolog goal string>",
  "vars": [ "<var name>", ... ]
}
```

`goal` and `vars` are optional. Only ground facts (no `:-` heads) are accepted.

**Response**

```json
{
  "facts": [ ... ],
  "goal": "<goal>-(vars)",
  "answers": [ { "bindings": { "<var>": <value> }, "explanation": <tree> } ],
  "result": "true | false | unknown"
}
```

---

### `query` — Low-level Prolog/taxlog query (legacy taxkbapi)

Evaluates a Prolog term against a named module's knowledge base, optionally with hypothetical facts.

**Request**

```json
{
  "token": "myToken123",
  "operation": "query",
  "theQuery": "<Prolog term string>",
  "module": "<module URL or name>",
  "facts": [ "<fact term string>", ... ]
}
```

`facts` is optional.

**Response**

```json
{
  "results": [
    {
      "result": "true | false | unknown",
      "bindings": { "<VarName>": <value>, ... },
      "unknowns": [ { "goal": <term>, "module": "<module>" }, ... ],
      "why": <explanation tree>
    }
  ]
}
```

**curl examples**

```bash
# Simple query
curl -X POST http://localhost:3050/leapi \
  -H 'Content-Type: application/json' \
  -d '{"token":"myToken123","operation":"query","theQuery":"a(1,Y)","module":"http://tests.com"}'

# With hypothetical facts
curl -X POST http://localhost:3050/leapi \
  -H 'Content-Type: application/json' \
  -d '{"token":"myToken123","operation":"query","theQuery":"a(13,Y)","facts":["d(13)"],"module":"http://tests.com"}'
```

---

### `draft` — Draft a Prolog file from web-page content (legacy)

Accepts structured page content and returns a drafted Prolog text.

**Request**

```json
{
  "token": "myToken123",
  "operation": "draft",
  "pageURL": "http://mysite/page1#section2",
  "content": [
    { "url": "http://mysite/page1#section2!chunk1", "text": "john flies by instruments" }
  ]
}
```

**Response**

```json
{ "pageURL": "http://mysite/page1#section2", "draft": "<Prolog text>" }
```

---

## Explanation tree nodes

The `why` / explanation fields returned by `query` and `loadFactsAndQuery` are arrays of node objects:

```json
{
  "type": "<node type>",
  "literal": "<goal as string>",
  "module": "<module>",
  "source": "<source reference>",
  "textOrigin": "<origin>",
  "children": [ <node>, ... ]
}
```

---

## Starting the server

```prolog
?- use_module(api), start_api_server.        % listens on port 3050
?- use_module(api), start_api_server(8080).  % custom port
```

When loaded inside SWISH the server start is a no-op (SWISH already handles HTTP).
