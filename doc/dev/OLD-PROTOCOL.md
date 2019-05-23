This document describes Merlin protocol version 2.

During a Merlin session, the editor launches an ocamlmerlin process and communicates with it by writing queries on stdin and reading responses on stdout.

Merlin processes queries synchronously, reading them one by one and writing a response for each query, in the same order.  It will wait for more queries until stdin reaches end-of-file.

The complete set of commands is defined in `src/frontend/protocol.ml`.

Queries and responses can be serialized in two different formats:
- JSON, defined in `src/frontend/IO.ml`;
- SEXP, defined in `src/frontend/IO_sexp.ml`.

JSON is the default, SEXP can be selected by passing `-protocol sexp` to Merlin process.

The rest of the document will describe sample sessions and commands using JSON format.  The SEXP format is mechanically derived from JSON, flow is the same.

# Merlin commands

Commands can be classified in three categories:
- _synchronization_, to share and update the content of the editor
  buffer;
- _query_, to ask Merlin for information (type, completion, documentation);
- _context_, to describe the file being worked on and how it is
  related to the environment (dependencies, include paths, ...).

The basic workflow for an editor is to synchronize then run a query each time Merlin is invoked.

When working on a project with multiple files, context becomes useful to switch between buffers.

A simple session (user-commands prefixed by >, Merlin responses by <):

```javascript
> ["tell","start","end","let f x = x let () = ()"]
< ["return",true]
> ["type","expression","f","at","end"]
< ["return","'a -> 'a"]
```


## Responses

Responses are always of the form `[kind,payload]` where `payload` depends on the value of `kind`, which can be:

`"return"` when the command succeeded, `payload` depends on the command being responded to.

`"failure"` when Merlin was used in an incorrect way (e.g command was malformed or unknown), `payload` is a string describing the failure.  Editor mode should be fixed.

`"error"` when something wrong prevented Merlin from answering: invalid files (for instance wrong OCaml version), missing packages, etc.  `payload` is a string describing the error, user should be notified to fix the environment.

`"exception"` when something unexpected happened.  Merlin should be fixed, please report the error.  `payload` is an arbitrary json value.

    
## Synchronization

Merlin maintains a copy of the buffer from your editor.
Synchronization is done by replacing chunks of text from this copy by fresh content.

### Position

Most commands need to refer to a position in the buffer.  All positions are interpreted on the copy of the buffer, make sure Merlin is synchronized with the editor when you need to share a position.  
A position is a JSON value that can be one of :

```javascript
"start" // first position of the buffer
"end"   // last position of the buffer
1234    // An integer is an offset, in bytes, from the beginning of the buffer
{"line":12, "col":34} // Alternatively, you can specify a position as a line (first line is 1) and a column (indexed from 0).
```

### Tell

All telling commands return a cursor state.

```javascript
["tell",start_pos,end_pos,"source"]
```

Replace the content between the two positions by `"source"`.

The simplest way to synchronize your editor is to use `["tell","start","end","... full content of the buffer"]`.  It will update the whole buffer every time.

### Configuration

#### Flags

```javascript
["flags","set",["-rectypes", "-safe-string", ...]]
```

Set the flags you would pass to the OCaml compiler. Run `ocamlmerlin -help` to get a list of known flags.

Returns `{"result":true}` if everything went well or `{"failures":string list, "result":true}` in case of error.


```javascript
["flags","get"]
```

Returns the `string list` (eg `["-rectypes","-safe-string"]`) that was set by previous invocation of `["flags","set",[...]]`.

#### Findlib packages

```javascript
["find","use",["lwt","yojson",...]]
```

Load findlib packages in current buffer.
Returns `{"result":true}` if everything went well or `{"failures":string list, "result":true}` in case of error.


```javascript
["find","list"]
```

Returns a `string list` of all known packages.

#### Syntax extensions

```javascript
["extension","enable",["lwt","js",...]]
["extension","disable",["lwt","js",...]]
```

Enable or disable syntax extensions in current buffer.


```javascript
["extension","list"]
["extension","list","enabled"]
["extension","list","disabled"]
```

List all known / currently enabled / currently disabled extensions as a `string list`.

#### Paths

```javascript
["path","add","source",[path1, path2, ...]]
["path","add","build",[path1, path2, ...]]
["path","remove","source",[path1, path2, ...]]
["path","remove","build",[path1, path2, ...]]
```

Merlin maintains different list of paths to process buffer and queries.
`"source"` is where `.ml` and `.mli` files are searched for, `"build"` is for `.cmi` and `.cmt`.


```javascript
["path","list","source"]
["path","list","build"]
```

Get current value of path variables as a `string list`.


```javascript
["path","reset"]
```

Reset path variables to default value (by default just the standard library and the buffer directory).

### Queries

#### Type-checking

```javascript
["type","expression",string,"at",position]
```

Returns the type of the expression when typechecked in the environment around the specified position.


```javascript
["type","enclosing","at",position]
["type","enclosing",{"expr":string,"offset":int},position]
```

Returns a list of type information for all expressions at given position, sorted by increasing size.
That is asking for type enlosing around `2` in `string_of_int 2` will return the types of `2 : int` and `string_of_int 2 : string`. 

The `{"expr":string,"offset":int}` variant expects the string under cursor and the offset of the cursor in this string, to return more specific information. 

The result is returned as a list of:
```javascript
{
  "start": position,
  "end": position,
  "type": string,
  // is this expression not in tail position, in tail position, or even a tail call?
  "tail": ("no" | "position" | "call")
}
```


```javascript
["case","analysis","from",position,"to",position]
```

Try to destruct patterns in the specified range.
It returns a value with the shape `[{"start": position, "end": position}, content]`. The editor is expected to replace content between `start` and `end` by `content`.

#### Completion

```javascript
["complete","prefix",string,"at",position]
["complete","prefix",string,"at",position,"with","doc"]
["expand","prefix",string,"at",position]
```

These functions complete an identifier that the user started to type.
They all return a list of possible completion. The "with doc" variant also try to lookup OCamldoc, which is slightly more time consuming.

The expand function also try to complete partial or incorrect prefixes. For instance, `L.ma` can get expanded to `List.map`. This function is a useful fallback if normal completion gave no results.
Be careful that it always return fully qualified paths, whereas normal completion only completes an identifier (last part of a module path). 

The result has the form:
```javascript
{
  context: (null | ["application",{"argument_type": string, "labels": [{"name":string,"type":string}]}]),
  entries: [{"name":string,"kind":string,"desc":string,"info":string}]
}
```

Context describe where completion is occurring. Only application is distinguished now: that's when one is completing the arguments to a function call. In this case, one gets the type expected at the cursor as well as the other labels.

Entries is the list of possible completion. Each entry is made of:
- a name, the text that should be put in the buffer if selected
- a kind, one of `"value"`, `"variant"`, `"constructor"`, `"label"`, `"module"`, `"signature"`, `"type"`, `"method"`, `"#"` (for method calls), `"exn"`, `"class"`
- a description, most of the time a type or a definition line, to be put next to the name in completion box
- optional information which might not fit in the completion box, like signatures for modules or documentation string.


```javascript
["document",string,"at",position]
["document",null,"at",position]
```

Returns OCamldoc documentation as a string, either for the given qualified identifier or the one under cursor.

#### Navigation

```javascript
["occurrences","ident","at",position]
```

Returns a list of locations `{"start": position, "end": position}` of all occurrences in current buffer of the entity at the specified position.


```javascript
["locate",string,"ml","at",position]
["locate",null,"ml","at",position]
["locate",string,"mli","at",position]
["locate",null,"mli","at",position]
```

Finds the declaration of entity at the specified position, or referred to by specified string.
Returns either:
- if location failed, a `string` describing the reason to the user,
- `{"pos": position}` if the location is in the current buffer,
- `{"file": string, "pos": position}` if definition is located in a different file.


```javascript
["which","path",string list]
```

Returns the full path of the first file with a name listed in the argument.
E.g. `["which","path",["list.ml","list.mli"]]` should return the path of the standard _List_ implementation, unless another _List_ is defined in a user directory.


```javascript
["which","with_ext",string list]
```

Returns a list of module names for which a file exists in the path with an extension listed in the argument.

`["which","with_ext",[".ml",".mli"]]` lists all top modules with either a signature or an implementation in current project.
You can then use `["which","path",[module + ".ml", module + ".mli"]]` to open of them (in this case favoring implementations over interfaces).


```javascript
["outline"]
```

Returns a tree of objects `{"start": position, "end": position, "name": string, "kind": string, "children": subnodes}` describing the content of the buffer.


```javascript
["enclosing",position]
```

Returns a list of locations `{"start": position, "end": position}` in increasing size of all entities surrounding the position. Like s-exps around position but following OCaml syntax.

#### Error management

```javascript
["errors"]
```

Returns a list of errors in current buffer.
The value is a list where each item as the shape:

```javascript
{
  "start" : position,
  "end"   : position,
  "valid" : bool,
  "message" : string,
  "type"  : ("type"|"parser"|"env"|"warning"|"unknown")
}
```

`start` and `end` are omitted if error has no location (e.g. wrong file format), otherwise the editor should probably highlight / mark this range.
`type` is an attempt to classify the error.
`valid` is here mostly for informative purpose. It reflects whether Merlin was expecting such an error to be possible or not, and is useful for debugging purposes.
`message` is the error description to be shown to the user.


```javascript
["project","get"]
```

Returns an object `{"result":string list,"failures":string list}` listing all _.merlin_ files loaded for current buffer and a list of failures that might have happened during loading (missing package for instance, ill-formed .merlin, etc).
The `"failures"` field can be omitted if there has been no error.

### Context

Merlin keep tracks of multiple buffer. All commands apply to the active buffer.
`"checkout"` command allows to change the active buffer.
It returns a `cursor state` object describind the state of the checked out buffer (see `"tell"` command).


```javascript
["checkout", "ml"]
["checkout", "mli"]
```

Switch to "default" buffer for "ml", "mli".
It will be in the state you left it last time it was used, unless Merlin decided to garbage collect because of memory pressure (any buffer left in background is either untouched or reset because of collection).


```javascript
["checkout", "auto", string]
["checkout", "ml"  , string]
["checkout", "mli" , string]
```

Checkout buffer at a given path, interpreting it as an ml, an mli, or infer that from path extension (defaulting to ml).
File at path is not loaded, path is only used as a key to refer to the buffer and look for _.merlin_ files.


```javascript
["checkout", "dot_merlin", string list, "auto", string]
["checkout", "dot_merlin", string list, "ml"  , string]
["checkout", "dot_merlin", string list, "mli" , string]
```

Same as `["checkout", _, string]`, but rather than inferring the _.merlin_ from the path, use the explicit list of files.

#### Contextual commands

An important variant of this scheme are the _contextual commands_.
All Merlin commands except `"checkout"` can be wrapped in a dictionary looking like:

```javascript
{
  "context": context,
  "query": command
}
```

Where `command` is a Merlin command and context would be the list of arguments passed to `"checkout"`.

This has the same effect as executing:

```javascript
["checkout", context...]
[command...]
```

This is useful to prevent race conditions resulting from concurrent manipulations of different buffers in the editor, by making sure each command is interpreted in the appropriate context.

### Versioning

```javascript
["protocol","version",n]
```

This command notifies Merlin that the editor wants to communicate with protocol version `n`, where `n` is an integer.

Merlin will answer with a triple `{"selected": n0, "latest": n1, "merlin": "Version string"}`, where: 
- `n0` is the version that will be used for the rest of this session, 
- `n1` is the most recent version the local distribution of Merlin is able to use, 
- "Version string" is a human readable string describing the local installation of Merlin.

```javascript
["protocol","version"]
```

This command will return the same answer as the previous one, but won't try to select a protocol version.

```javascript
["version"]
```

Returns a string describing Merlin version.

### Debugging Merlin

Dump command allow to observe internal structures of Merlin.
Result is an arbitratry json object, targeted toward human readers.

```javascript
["dump","env"]
["dump","env","at",position]
["dump","full_env"]
["dump","full_env","at",position]
```

Dump content of environment.
`"env"` is limited to local definition, `"full_env"` also includes `Pervasives` and default environment.


```javascript
["dump","sig"]
```

Dump definitions in environment as an ML signature.


```javascript
["dump","tokens"]
["dump","parser"]
["dump","recover"]
```

Dump output of the lexer, state of the parser and possible recoveries.


```javascript
["dump","browse"]
["dump","typer","input"]
["dump","typer","output"]
```

Dump state of typechecker.
`"input"` is the AST has seen by the typer.
`"output"` is the annotated AST produced by the typer.
`"browse"` is a json-based tree built out of the `"output"`.


```javascript
["dump","flags"]
["dump","warnings"]
```

List of the flags and warnings set for current buffers.

# TODO

Logging infrastructure.
Explain responses verbosity.
