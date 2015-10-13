During a Merlin session, the editor launches an ocamlmerlin process
and communicates with it by writing queries on stdin and reading
responses on stdout.

Merlin processes queries synchronously, reading them one by one and
writing a response for each query, in the same order.  It will wait
for more queries until stdin reaches end-of-file.

The complete set of commands is defined in `src/frontend/protocol.ml`.

Queries and responses can be serialized in two different formats:
- JSON, defined in `src/frontend/IO.ml`;
- SEXP, defined in `src/frontend/IO_sexp.ml`.

JSON is the default, SEXP can be selected by passing `-protocol sexp`
to merlin process.

The rest of the document will describe sample sessions and commands
using JSON format.  The SEXP format is mechanically derived from JSON,
flow is the same.

# Merlin commands

Commands can be classified in three categories:
- _synchronization_, to share and update the content of the editor
  buffer;
- _query_, to ask merlin for information (type, completion, documentation);
- _context_, to describe the file being worked on and how it is
  related to the environment (dependencies, include paths, ...).

The basic workflow for an editor is to synchronize then run a query
each time merlin is invoked.

When working on a project with multiple files, context becomes useful
to switch between buffers.

A simple session (user-commands prefixed by >, merlin responses by <):

```javascript
> ["tell","source","let f x = x;;"]
< ["return",{"cursor":{"line":1,"col":13},"marker":false}]
> ["type","expression","f"]
< ["return","'a -> 'a"]
```


## Responses

Responses are always of the form `[kind,payload]` where `payload`
depends on the value of `kind`, which can be:

`"return"` when the command succeeded, `payload` depends on the
command being responded to.

`"failure"` when merlin was used in an incorrect way (e.g command was
malformed or unknown), `payload` is a string describing the failure.
Editor mode should be fixed.

`"error"` when something wrong prevented merlin from answering:
invalid files (for instance wrong OCaml version), missing packages,
etc.  `payload` is a string describing the error, user should be
notified to fix the environment.

`"exception"` when something unexpected happened.  Merlin should be
fixed, please report the error.  `payload` is an arbitraty json value.

    
## Synchronization

Merlin maintains a "virtual cursor", similar to the cursor that allows
to enter text in a text editor.

Sharing the content of the buffer to merlin is done by moving the
cursor to the desired position then sending text.

Two differences with the text cursor you are familiar with:
- text always replace what comes after the cursor, sending something clears previous contents,
- the cursor cannot be freely moved, merlin may align it with entities which it finds meaningful (usually a token).

Thus, after a move, merlin will always return the actual position of
the cursor.

Positions are represented as a pair of a line and a
column, `{"line":1,"col":0}` in JSON.
The first line has number 1.


### Tell

All telling commands return a cursor state.

```javascript
["tell","start"]
["tell","start","at",{"line":int, "col":int}]
```

Prepare merlin for receiving text. If a position is specified, the cursor will be moved there.
Merlin will return the actual position where text will be inserted as a cursor state object `{"cursor":position, "marker":bool}`, so the editor should be prepared to send more text.
Don't bother about the `"marker"` field yet.


```javascript
["tell","source",string]
["tell","file",string]
```

`"source"` appends the string argument to buffer contents (at the cursor position). Returns the updated cursor state.

`"file"` appends the contents of the file specified as argument.
Like calling `["tell","source",...]` with the contents of the file as argument.

Be careful that EOF is not set, see the next commands.

```javascript
["tell","eof"]
["tell","source-eof",string]
["tell","file-eof",string]
```

Signal EOF at the current cursor position, or after appending some text.
Merlin behaves slightly differently when EOF is not set, for instance by not reporting errors about unterminated statements.
You shouln't usually bother about that: unless you know you are working with unfinished contents (e.g REPL input), always set EOF at the end of the buffer.


```javascript
["tell","marker"]
```

Set the marker at the current position. Useful only in advanced use-cases, see the marker section for more information.


```javascript
["drop"]
```

This command makes merlin forget everything after the cursor.

### Seek

These commands move the cursor without affecting the content of the buffer. They also return a cursor state.

Moving the cursor is useful to control the prefix of the buffer that must be considered by merlin when processing queries.
Practical for debugging but doesn't matter for basic usecases.


```javascript
["seek","position"]
```

Returns the current position of the cursor without doing anything else.


```javascript
["seek","before",position]
["seek","exact",position]
```

Move the cursor to the requested position. If this position happens to be in the middle of a token, `"exact"` will set the cursor at this token while `"before"` will move to the preceding one.


```javascript
["seek","end"]
```

Move the cursor to the last position known to merlin.


```javascript
["seek","marker"]
```

Move the cursor to the last state where the marker was on stack. For advanced uses, see marker section.

### Configuration

#### Flags

```javascript
["flags","add",["-rectypes", "-safe-string", ...]]
["flags","clear"]
```

Pass the same flags as you would pass to the OCaml compiler. Run `ocamlmerlin -help` to get a list of known flags.
`"add"` appends to the list of flags, `"clear"` resets to the empty flag list.

Returns `{"result":true}` if everything went well or `{"failures":string list, "result":true}` in case of error.


```javascript
["flags","get"]
```

Returns a `string list list` (eg `[["-rectypes"],["-safe-string"]]`) resulting from the previous invocations of `["flags",("clear"/"add")]`.

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
["type","expression",string]
["type","expression",string,"at",{"line":int,"col":int}]
```

Returns the type of the expression when typechecked in the environment at cursor or around the specified position.


```javascript
["type","enclosing","at",{"line":int,"col":int}]
["type","enclosing",{"expr":string,"offset":int},{"line":int,"col":int}]
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
- optional informations which might not fit in the completion box, like signatures for modules or documentation string.


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
  "type"  : ("type"|"parser"|"env"|"warning"|"unkown")
}
```

`start` and `end` are omitted if error has no location (e.g. wrong file format), otherwise the editor should probably hightlight / mark this range.
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
It will be in the state you left it last time it was used, unless merlin decided to garbage collect because of memory pressure (any buffer left in background is either untouched or resetted because of collection).


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
All merlin commands except `"checkout"` can be wrapped in a dictionary looking like:

```javascript
{
  "context": context,
  "query": command
}
```

Where `command` is a merlin command and context would be the list of arguments passed to `"checkout"`.

This has the same effect as executing:

```javascript
["checkout", context...]
[command...]
```

This is useful to prevent race conditions resulting from concurrent manipulations of different buffers in the editor, by making sure each command is interpreted in the appropriate context.

### Misc

```javascript
["version"]
```

Returns a string describing merlin version.


```javascript
["boundary","next","at",position]
["boundary","prev","at",position]
["boundary","current","at",position]
["boundary","at",position]
```

DEPRECATED. The boundary command returns the location of the construction under or near the cursor.
Originally intended to implement features such as moving by phrase, alternative are being explored.

#### Debugging Merlin

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

Marker management.
Logging infrastructure.
Explain responses verbosity.
Cleanup list of type below.

# OLD

# Basic types

## Position

A json-object of the form: 

    position = {"line":int, "col":int}

## Cusor state

A json-object of the form: 

    cursor = {"cursor":position, "marker":bool}

## Location

A json-object having at least the fields: 

    location <= {"start":position,"end":position}

## Filenames

Strings addressing file system objects:

    path = string, points any file or directory
    directory = string, expected to name a directory
    filename = string, expected to name a regular file

# Wizardry

## type

Returns the type of an expression as a string.
['type','expression','... ml expression'] tries to type expression in global env.
['type','at',{'line':l,'col':c}] returns the type of the expression at given position(BUGGY)

### ["type","expression",string]
### ["type","expression",string,"at",position]
### ["type","at",position]
### ["type","enclosing",position]

## complete

['complete','prefix','...identifier path...'] or
['complete','prefix','...identifier path...', 'at', {'line':l,'col':c}]
returns possible completions in global environement or in environment
surrounding given position for given prefix/path (path of the form: Module.ident)

### ["complete","prefix",string]
### ["complete","prefix",string,"at",position]

## locate

['locate', ident] returns the position where this identifier is introduced.

Answers are of this shape :

- "Not found" (string)
- ["file": string , "pos": { "line" : int , "col": int }]

## boundary

### ["boundary", "prev|current|next"]
Return the boundary of the phrase before, at, or after the cursor.

### ["boundary", "prev|current|next", "at", pos]

Return the boundary of the phrase before, at, or after the given position.

## errors

### ["errors"]

# Path management

## find

### ["find","use",string list]
### ["find","list"]

## which

### ["which","path",string]
### ["which","with\_ext",string]

## cd

### ["cd",directory]

## path

    var = string

### ["path","list"]

List known path variables. As of today (07/2013), there is "build" and
"source".

### ["path","list",var]

List content of variable.

### ["path","add",var,directory]

Add a path to a variable.

### ["path","remove",var,directory]

Remove a path from a variable.

### ["path","reset"]

Reset all variables to default content (usually, ocaml std lib path).

### ["path","reset",var]

Reset specified variable to its default value.

## Project file

Loading of ".merlin" files.

### ["project","load",filename]

Parse and load given filename as a ".merlin".
Returns the list of files that have been loaded (as a ".merlin" can depend on
another one that will be loaded recursively).

### ["project","find",path]

Try to find a file named ".merlin" in any directory from given path to "/",
then behave as ["project","load"].with this file.

# Debug

## dump

### ["dump","env"]
### ["dump","env","at",position]
### ["dump","sig"]
### ["dump","chunks"]
### ["dump","tree"]

## help

## ["help"]

List known commands
