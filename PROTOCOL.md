
# Basic types

## Position

A json-object of the form: 

    position = {"line":int, "col":int}

## Location

A json-object having at least the fields: 

    location <= {"start":position,"end":position}

## Directory

A json-string representing a valid directory:

    directory = string

# Buffer management

## tell

Use ['tell','struct','...ocaml impl...'] to send source code from editor to merlin.
The command answers the boolean 'true' if parsing ended, 'false' if it needs more input.
In this case, the only allowed command is ['tell','struct','next source content'].
To interrupt the parser, run ['tell','struct',''] (meaning eof) or ['tell','struct',null].

### ["tell","struct",string]

### ["tell","struct",null]

### ["tell","end",string]

### ["tell","end",null]

## seek

### ["seek","position"]

### ["seek","before",position]

### ["seek","exact",position]

### ["seek","end\_of\_definition"]

### ["seek","end"]

### ["seek","maximize\_scope"]

## reset

### ["reset"]

## refresh

### ["refresh"]

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

## boundary

### ["boundary"]

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
### ["path","list",var]

### ["path","add",var,directory]
### ["path","remove",var,directory]

### ["path","reset"]
### ["path","reset",var]

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
