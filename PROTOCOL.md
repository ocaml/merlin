
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

### ["type","expression",string]
### ["type","expression",string,"at",position]
### ["type","at",position]
### ["type","enclosing",position]

## complete

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
