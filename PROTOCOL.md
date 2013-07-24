
# Basic types

## Position

A json-object of the form: 

    position = {"line":int, "col":int}

## Location

A json-object having at least the fields: 

    location <= {"start":position,"end":position}

## Filenames

Strings addressing file system objects:

    path = string, points any file or directory
    directory = string, expected to name a directory
    filename = string, expected to name a regular file

# Buffer management

## tell

Use ['tell','struct','...ocaml impl...'] to send source code from editor to
merlin.  The command answers the boolean 'true' if parsing ended, 'false' if it
needs more input.  In this case, the only allowed command is
['tell','struct','next source content'].  To interrupt the parser, run
['tell','struct',''] (meaning eof) or ['tell','struct',null].

### ["tell","struct",string]

Send a chunk of ml source code. Merlin will wait for the rest of the input.

### ["tell","end",string]

Same as above, but tell merlin that you want to stop as soon as possible.
Merlin will stop waiting for input once the current definition is complete.

### ["tell","struct",null]

Notify merlin that EOF has been reached.

### ["tell","end",null]

Same as ["tell","struct",null]

## seek

All seek functions return the position of Merlin virtual cursor after
their execution, as a position object.

### ["seek","position"]

Return the current position of the cursor.

### ["seek","before",position]

Move cursor just before the definition around specified position.

### ["seek","exact",position]

Move cursor to the position that has been given, or just after if this is in
the middle of an ml definition.

### ["seek","end"]

Put cursor at the end of file.

### ["seek","maximize\_scope"]

Move cursor down until the current scope is escaped (usually when
encountering an "end" of structure).

## Buffers management, dependency reloading

### ["reset"]

Clear content of virtual buffer.

### ["reset",path]

Clear content of virtual buffer and set its name.
This is used to report more informative error messages (the same the compiler
would report for a file with the given path).

### ["refresh"]

Reload all dependencies and try to retype the current file.

### ["refresh","quick"]

Try to detect dependencies that have changed and reload them. If needed,
retype current file.

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
