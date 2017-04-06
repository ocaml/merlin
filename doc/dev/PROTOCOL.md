# Merlin 3 protocol documentation

## Changes from merlin 2

The communication protocol was redesigned between merlin 2 and 3. Answers have the same format, but merlin is no longer invoked as an asynchronous process: a new merlin process is started for each query.
Under the hood, merlin will make its best to manage resources in an efficient way (via a resident process called *ocamlmerlin-server*).

In other word, editor modes no longer have to do process management.

Finally, commands no longer maintain state on merlin side. In previous versions, buffer specific settings (compiler flags, findlib packages, syntax extensions, ...) were set by calling the appropriate commands.  State was split between merlin and the editor, which was hard to track and could cause desynchronization.

In this version, all this settings are passed on the command line. Arguments look a lot like the ocaml compiler ones.

Try calling:
- `ocamlmerlin single -help` for general information
- `ocamlmerlin single -flags-help` for a detailed list of accepted flags
- `ocamlmerlin single -commands-help` for a list of supported commands

### Backward compatibility

This change is made in a backward compatible way: sessions that worked with merlin 2 should give the same answer with merlin 3.
This new protocol is only enabled if a command is passed on the commandline.

## Merlin 3 commands


### `case-analysis -start <position> -end <position>`

       -start <position>  Where analysis starts
         -end <position>  Where analysis ends

When the range determined by (-start, -end) positions is an expression,
this command replaces it with [match expr with _] expression where a branch is introduced for each immediate value constructor of the type that was determined for expr.
When it is a variable pattern, it is further expanded and new branches are introduced for each possible immediate constructor of this variable.
The return value has the shape `[{'start': position, 'end': position}, content]`, where content is string.


### `complete-prefix -position <position> [ -doc <bool> ] -prefix <string> [ -types <bool> ]`

    -position <position>  Position to complete
             -doc <bool>  Add docstring to entries (default is false)
        -prefix <string>  Prefix to complete
           -types <bool>  Report type information (default is true)

This functions completes an identifier that the user started to type.
It returns a list of possible completions.
With '-types y' (default), each completion comes with type information.
With '-doc y' it tries to lookup OCamldoc, which is slightly more time consuming.

The result has the form:
```javascript
{
  'context': (null | ['application',{'argument_type': string, 'labels': [{'name':string,'type':string}]}]),
  'entries': [{'name':string,'kind':string,'desc':string,'info':string}]
}
```

Context describe where completion is occurring. Only application is distinguished now: that's when one is completing the arguments to a function call. In this case, one gets the type expected at the cursor as well as the other labels.

Entries is the list of possible completion. Each entry is made of:
- a name, the text that should be put in the buffer if selected
- a kind, one of `'value'`, `'variant'`, `'constructor'`, `'label'`, `'module'`, `'signature'`, `'type'`, `'method'`, `'#'` (for method calls), `'exn'`, `'class'`
- a description, most of the time a type or a definition line, to be put next to the name in completion box
- optional informations which might not fit in the completion box, like signatures for modules or documentation string.

### `document -position <position> [ -identifier <string> ]`

    -position <position>  Position to complete
    -identifier <string>  Identifier

Returns OCamldoc documentation as a string.
If `-identifier ident` is specified, documentation for this ident is looked up from environment at `-position`.
Otherwise, Merlin looks for the documentation for the entity under the cursor (at `-position`).

### `enclosing -position <position>`

    -position <position>  Position to complete

Returns a list of locations `{'start': position, 'end': position}` in increasing size of all entities surrounding the position.
(In a lisp, this would be the locations of all s-exps that contain the cursor.)

### `errors`


Returns a list of errors in current buffer.
The value is a list where each item as the shape:

```javascript
{
'start' : position,
'end'   : position,
'valid' : bool,
'message' : string,
'type'  : ('type'|'parser'|'env'|'warning'|'unkown')
}
```

`start` and `end` are omitted if error has no location (e.g. wrong file format), otherwise the editor should probably highlight / mark this range.
`type` is an attempt to classify the error.
`valid` is here mostly for informative purpose. It reflects whether Merlin was expecting such an error to be possible or not, and is useful for debugging purposes.
`message` is the error description to be shown to the user.

### `expand-prefix -position <position> -prefix <string> [ -types <bool> ]`

    -position <position>  Position to complete
        -prefix <string>  Prefix to complete
           -types <bool>  Report type information (default is false)


The function behaves like `complete-prefix`, but it also handles partial, incorrect, or wrongly spelled prefixes (as determined by some heuristic).
For instance, `L.ma` can get expanded to `List.map`. This function is a useful fallback if normal completion gave no results.
Be careful that it always return fully qualified paths, whereas normal completion only completes an identifier (last part of a module path).

### `extension-list [ -status <all|enabled|disabled> ]`

-status <all|enabled|disabled>  Filter extensions

List all known / currently enabled / currently disabled extensions as a list of strings.

### `findlib-list`


Returns all known findlib packages as a list of string.

### `flags-list`


Returns supported compiler flags.The purpose of this command is to implement interactive completion of compiler settings in an IDE.

### `jump -target <string> -position <position>`

        -target <string>  Entity to jump to
    -position <position>  Position to complete

This command can be used to assist navigation in a source code buffer.
Target is a string that can contain one or more of the 'fun', 'let', 'module' and 'match' words.
It returns the starting position of the function, let definition, module or match expression that contains the cursor


### `phrase -target <next|prev> -position <position>`

     -target <next|prev>  Entity to jump to
    -position <position>  Position to complete

Returns the position of the next or previous phrase (top-level definition or module definition).

### `list-modules [ -ext <extension> -ext ... ]`

        -ext <extension>  file extensions to look for

Looks into project source paths for files with an extension matching and prints the corresponding module name.

### `locate [ -prefix <string> ] -position <position> [ -look-for <interface|implementation> ]`

        -prefix <string>  Prefix to complete
    -position <position>  Position to complete
-look-for <interface|implementation>  Prefer opening interface or implementation

Finds the declaration of entity at the specified position, Or referred to by specified string.
Returns either:
- if location failed, a `string` describing the reason to the user,
- `{'pos': position}` if the location is in the current buffer,
- `{'file': string, 'pos': position}` if definition is located in a different file.

### `occurrences -identifier-at <position>`

-identifier-at <position>  Position to complete

Returns a list of locations `{'start': position, 'end': position}` of all occurrences in current buffer of the entity at the specified position.

### `outline`


Returns a tree of objects `{'start': position, 'end': position, 'name': string, 'kind': string, 'children': subnodes}` describing the content of the buffer.

### `path-of-source -file <filename>`

        -file <filename>  filename to look for in project paths

Looks for first file with a matching name in the project source and build paths

### `shape -position <position>`

    -position <position>  Position 

This command can be used to assist navigation in a source code buffer.
It returns a tree of all relevant locations around the cursor.
It is similar to outline without telling any information about the entity at a given location.
```javascript
shape =
{
  'start' : position,
  'end'   : position,
  'children' : [shape]
}
```


### `type-enclosing -position <position> [ -expression <string> ] [ -cursor <int> ] [ -index <int> ]`

    -position <position>  Position to complete
    -expression <string>  Expression to type
           -cursor <int>  Position of the cursor inside expression
            -index <int>  Only print type of <index>'th result

Returns a list of type information for all expressions at given position, sorted by increasing size.
That is asking for type enlosing around `2` in `string_of_int 2` will return the types of `2 : int` and `string_of_int 2 : string`.

If `-expression` and `-cursor` are specified, the first result will be the type
relevant to the prefix ending at the `cursor` offset.

`-index` can be used to print only one type information. This is useful to
query the types lazily: normally, Merlin would return the signature of all
enclosing modules, which can be very expensive.

The result is returned as a list of:
```javascript
{
  'start': position,
  'end': position,
  'type': string,
  // is this expression not in tail position, in tail position, or even a tail call?
  'tail': ('no' | 'position' | 'call')
}
```

### `type-expression -position <position> -expression <string>`

    -position <position>  Position to complete
    -expression <string>  Expression to type

Returns the type of the expression when typechecked in the environment around the specified position.

### `check-configuration`


This command checks that merlin project and options are correct.
The return value has the shape:
```javascript
{
  'dot_merlins': [path], // a list of string
  'failures': [message]  // a list of string
}
```

## Details about the client/server protocol

single mode

the wrapper
socket rendez vous
stopping server
passing command line arguments
passing environment variable

## Miscellaneous

`__MERLIN_MASTER_PID` environment variable is set in processes invoked by merlin.