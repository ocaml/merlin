The server might already be running, we kill it to make sure we start from a
clean slate:

  $ $MERLIN server stop-server

[complete-prefix:1-1] Application context

  $ $MERLIN server complete-prefix -position 4:17 \
  > -filename ctxt.ml < ctxt.ml \
  > | tr '\n' ' ' | jq ".value.context"
  [
    "application",
    {
      "argument_type": "int",
      "labels": [
        {
          "name": "~j",
          "type": "int"
        }
      ]
    }
  ]

[complete-prefix:2-1] Disambiguation 1

  $ $MERLIN server complete-prefix -position 12:22 -prefix Foo -doc n \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "Foobar",
          "kind": "Constructor",
          "desc": "T.t",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }

[complete-prefix:3-1] Disambiguation 2

  $ $MERLIN server complete-prefix -position 20:21 -prefix T.f -doc n \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "foobar",
          "kind": "Label",
          "desc": "T.t -> int",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }

[complete-prefix:4-1] Disambiguation 3

  $ $MERLIN server complete-prefix -position 22:22 -prefix foo -doc n \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "foobar",
          "kind": "Label",
          "desc": "T.t -> int",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }

[complete-prefix:5-1] Disambiguation 4

  $ $MERLIN server complete-prefix -position 26:35 -prefix tes -doc n \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "test_other",
          "kind": "Label",
          "desc": "T.t -> float",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }


[expand-prefix:1-1] Expansion 1

  $ $MERLIN server expand-prefix -position 32:15 -prefix L.m -doc n \
  > -filename ctxt.ml < ctxt.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "Lazy.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "Lazy.map_val",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    }
  ]

[expand-prefix:1-1] Expansion 1

  $ $MERLIN server expand-prefix -position 32:15 -prefix L.m -doc n \
  > -filename ctxt.ml < ctxt.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "Lazy.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "Lazy.map_val",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    }
  ]



[expand-prefix:2-1] Expansion 2

  $ $MERLIN server expand-prefix -position 35:17 -prefix Lsi.m -doc n \
  > -filename ctxt.ml < ctxt.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "List.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    }
  ]

[complete-prefix:6-1] Infix 1

  $ $MERLIN server complete-prefix -position 49:12 -prefix "Z." -doc n \
  > -filename ctxt.ml < ctxt.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "(>>)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(>>=)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(>>|)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(|+)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(|-)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    }
  ]


[complete-prefix:7-1] Kind 1

  $ $MERLIN server complete-prefix -position 54:12 -prefix fu \
  > -filename ctxt.ml < ctxt.ml | jq ".value.entries[].name"
  "funnyny"

[complete-prefix:8-1] Kind 2

  $ $MERLIN server complete-prefix -position 54:12 -prefix fu \
  > -kind k -filename ctxt.ml < ctxt.ml | jq ".value.entries[].name"
  "function"
  "fun"
  "functor"

[complete-prefix:9-1] Kind 3

  $ $MERLIN server complete-prefix -position 54:12 -prefix fu \
  > -kind keyword -kind value -filename ctxt.ml < ctxt.ml | jq ".value.entries[].name"
  "funnyny"
  "function"
  "fun"
  "functor"

[complete-prefix:10-1] Kind 4

  $ $MERLIN server complete-prefix -position 52:4 -prefix f \
  > -kind k -extension lwt -filename ctxt.ml < ctxt.ml | jq ".value.entries[].name"
  "function"
  "false"
  "fun"
  "for"
  "functor"

[complete-prefix:11-1] Kind 5 [FIXME from single to server]

  $ $MERLIN single complete-prefix -position 56:16 \
  > -filename ctxt.ml -prefix List.f < ctxt.ml | jq ".value.entries[].name"
  "fast_sort"
  "filter"
  "filter_map"
  "filteri"
  "find"
  "find_all"
  "find_index"
  "find_map"
  "find_mapi"
  "find_opt"
  "flatten"
  "fold_left"
  "fold_left2"
  "fold_left_map"
  "fold_right"
  "fold_right2"
  "for_all"
  "for_all2"


[complete-prefix:12-1] Kind 6

  $ $MERLIN server complete-prefix -position 56:16 \
  > -filename ctxt.ml -kind k -prefix List.f < ctxt.ml | jq ".value.entries"
  []


[complete-prefix:12-1] Parenthesize 1

  $ $MERLIN server complete-prefix -position 70:17 -prefix MyList. \
  > -filename ctxt.ml < ctxt.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "(())",
      "kind": "Constructor",
      "desc": "MyList.u",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(::)",
      "kind": "Constructor",
      "desc": "'a * 'a MyList.t -> 'a MyList.t",
      "info": "",
      "deprecated": false
    },
    {
      "name": "([])",
      "kind": "Constructor",
      "desc": "'a MyList.t",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(mod)",
      "kind": "Value",
      "desc": "MyList.u",
      "info": "",
      "deprecated": false
    },
    {
      "name": "random",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "t",
      "kind": "Type",
      "desc": "type 'a t = (::) of 'a * 'a MyList.t | []",
      "info": "",
      "deprecated": false
    },
    {
      "name": "u",
      "kind": "Type",
      "desc": "type u = ()",
      "info": "",
      "deprecated": false
    }
  ]


[complete-prefix:13-1] issue1575 1

  $ $MERLIN server complete-prefix -position 80:20 -prefix go \
  > -filename ctxt.ml < ctxt.ml | jq '.value.entries'
  [
    {
      "name": "goo",
      "kind": "Value",
      "desc": "< bar : int -> int; bazs : 'a -> string >",
      "info": "",
      "deprecated": false
    }
  ]

[complete-prefix:14-1] issue1575 2

  $ $MERLIN server complete-prefix -position 89:22 -prefix "" \
  > -filename ctxt.ml < ctxt.ml | jq '.value.entries'
  [
    {
      "name": "bar",
      "kind": "#",
      "desc": "int -> int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "bazs",
      "kind": "#",
      "desc": "'a -> string",
      "info": "",
      "deprecated": false
    }
  ]

[complete-prefix:15-1] issue1575 3

  $ $MERLIN server complete-prefix -position 98:25 -prefix baz \
  > -filename ctxt.ml < ctxt.ml | jq '.value.entries'
  [
    {
      "name": "bazs",
      "kind": "#",
      "desc": "'a -> string",
      "info": "",
      "deprecated": false
    }
  ]


[complete-prefix:16-1] issue1575 4

  $ $MERLIN server complete-prefix -position 109:26 -prefix "ba" \
  > -filename ctxt.ml < ctxt.ml | jq '.value.entries'
  [
    {
      "name": "bar",
      "kind": "#",
      "desc": "int -> int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "bazs",
      "kind": "#",
      "desc": "'a -> string",
      "info": "",
      "deprecated": false
    }
  ]


[complete-prefix:17-1] issue-lsp-503 [FIXME from single to server]

  $ $MERLIN single complete-prefix -position 113:16 -prefix "List.ma" \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "map",
          "kind": "Value",
          "desc": "('a -> 'b) -> 'a list -> 'b list",
          "info": "",
          "deprecated": false
        },
        {
          "name": "mapi",
          "kind": "Value",
          "desc": "(int -> 'a -> 'b) -> 'a list -> 'b list",
          "info": "",
          "deprecated": false
        },
        {
          "name": "map2",
          "kind": "Value",
          "desc": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }


[construct:18-1] Module 1

  $ $MERLIN server construct -position 167:20 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 167,
          "col": 19
        },
        "end": {
          "line": 167,
          "col": 20
        }
      },
      [
        "struct
    type t = private b
    and b =
      | A 
      | B of t 
    type (-!'a, +!'b) t' =
      | T of ('a -> 'b) 
    type t2 =
      | A 
      | B of string 
      | C of t 
    type nonrec r = {
      lbl1: t ;
      lbl2: float list }
    type nonrec n = r
    and m = float
    type t_ext = ..
    type t_ext +=  
      | Str of string 
    type t_ext +=  
      | A 
    type v = [ `A of t_ext ]
    let i = _
    let f = _
    module Sub = struct let y = _ end
    [@@@ocaml.text
      \"Construct does not handle class types yet. Please replace this comment by [room]'s definition.\"]
    [@@@ocaml.text
      \"Construct does not handle classes yet. Please replace this comment by [croom]'s definition.\"]
    module type Another  = sig val i : int end
    module type Sig  =
      sig
        type t
        and b
        val f : int -> float
        module type STyp  = sig  end
        module D : Another
      end
    module Submod =
      struct
        type t
        and b
        let f = _
        module type STyp  = sig  end
        module D = struct let i = _ end
      end
    module SubFunc(M:Sig) = struct let g = _ end
  end"
      ]
    ],
    "notifications": []
  }


[construct:19-1] Module 2

  $ $MERLIN server construct -position 169:30 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 169,
          "col": 29
        },
        "end": {
          "line": 169,
          "col": 30
        }
      },
      [
        "((module struct type t = int end) : (module Small))"
      ]
    ],
    "notifications": []
  }

[construct:20-1] Module 3

  $ $MERLIN server construct -position 171:21 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 171,
          "col": 20
        },
        "end": {
          "line": 171,
          "col": 21
        }
      },
      [
        "struct type t = int end"
      ]
    ],
    "notifications": []
  }


[construct:21-1] Functor Application 1

  $ $MERLIN server construct -position 124:26 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 124,
          "col": 25
        },
        "end": {
          "line": 124,
          "col": 26
        }
      },
      [
        "struct let x = _ end"
      ]
    ],
    "notifications": []
  }


[construct:22-1] Depth 1.1

  $ $MERLIN server construct -depth 1 -position 177:32 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "None",
    "(Some _)"
  ]


[construct:23-1] Depth 1.2

  $ $MERLIN server construct -depth 2 -position 177:32 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "None",
    "(Some None)",
    "(Some (Some _))"
  ]

[construct:24-1] Depth 1.3

  $ $MERLIN server construct -depth 3 -position 177:32 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "None",
    "(Some None)",
    "(Some (Some 0))"
  ]


[construct:25-1] Depth 1.4

  $ $MERLIN server construct -depth 4 -position 177:32 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "None",
    "(Some None)",
    "(Some (Some 0))"
  ]

[construct:26-1] Depth 2.1

  $ $MERLIN server construct -depth 1 -position 181:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "{ a = _; b = _ }"
  ]

[construct:27-1] Depth 2.2

  $ $MERLIN server construct -depth 2 -position 181:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "{ a = (Some _); b = None }",
    "{ a = None; b = None }",
    "{ a = None; b = (Some _) }",
    "{ a = (Some _); b = (Some _) }"
  ]


[construct:28-1] Depth 2.3

  $ $MERLIN server construct -depth 3 -position 181:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "{ a = (Some (Some _)); b = None }",
    "{ a = None; b = None }",
    "{ a = (Some None); b = None }",
    "{ a = (Some None); b = (Some 0.0) }",
    "{ a = None; b = (Some 0.0) }",
    "{ a = (Some (Some _)); b = (Some 0.0) }"
  ]


[construct:29-1] Depth 2.4

  $ $MERLIN server construct -depth 4 -position 181:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "{ a = (Some (Some 0)); b = None }",
    "{ a = None; b = None }",
    "{ a = (Some None); b = None }",
    "{ a = (Some None); b = (Some 0.0) }",
    "{ a = None; b = (Some 0.0) }",
    "{ a = (Some (Some 0)); b = (Some 0.0) }"
  ]


[construct:30-1] Depth 3.1

  $ $MERLIN server construct -depth 2 -position 185:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "((Some _), None)",
    "(None, None)",
    "(None, (Some _))",
    "((Some _), (Some _))"
  ]

[construct:31-1] Error 1

  $ $MERLIN server construct -position 190:20 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "error",
    "value": "Construct only works on holes.",
    "notifications": []
  }

[construct:32-1] Error 2

  $ $MERLIN server construct -position 192:18 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "error",
    "value": "Construct only works on holes.",
    "notifications": []
  }

[construct:33-1] Error 3

  $ $MERLIN server construct -position 198:8 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "error",
    "value": "Construct only works on holes.",
    "notifications": []
  }

[construct:34-1] Error 4

  $ $MERLIN server construct -position 202:20 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "error",
    "value": "Construct only works on holes.",
    "notifications": []
  }

[construct:35-1] Error 5

  $ $MERLIN server construct -position 205:19 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "error",
    "value": "Could not find a module type to construct from. Check that you used a correct constraint.",
    "notifications": []
  }

[construct:36-1] Error 6

  $ $MERLIN server construct -position 208:15 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "error",
    "value": "Could not find a module type to construct from. Check that you used a correct constraint.",
    "notifications": []
  }

[construct:37-1] Fun 1

  $ $MERLIN server construct -position 217:6 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 217,
        "col": 6
      },
      "end": {
        "line": 217,
        "col": 7
      }
    },
    [
      "(fun the_type the_type_1 -> _)"
    ]
  ]

[construct:38-1] Fun 2

  $ $MERLIN server construct -position 223:6 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 223,
        "col": 6
      },
      "end": {
        "line": 223,
        "col": 7
      }
    },
    [
      "(fun int int_1 -> _)"
    ]
  ]


[construct:39-1] Fun 3

  $ $MERLIN server construct -position 233:6 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  []

[construct:40-1] Fun 4 (the result diverge since we have a richer context)

  $ $MERLIN server construct -position 233:6 -with-values local \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "x",
    "(Compl_kind.funnyny _)",
    "Compl_1575_1.something",
    "Compl_1575_2.something",
    "Compl_1575_3.something",
    "Compl_1575_4.something",
    "(Mymod.f _)",
    "Mymod.x"
  ]

[construct:41-1] Fun 5

  $ $MERLIN server construct -position 235:6 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "0.0"
  ]

[construct:42-1] Fun 6

  $ $MERLIN server construct -position 235:6 -with-values local \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "0.0",
    "(g _)",
    "(Compl_kind.funnyny _)",
    "Compl_1575_1.something",
    "Compl_1575_2.something",
    "Compl_1575_3.something",
    "Compl_1575_4.something"
  ]


[construct:43-1] Inline record 1

  $ $MERLIN server construct -position 245:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(Foo _)"
  ]

[construct:44-1] Inline record 2

  $ $MERLIN server construct -position 246:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(Bar _)"
  ]


[construct:45-1] Object 1

  $ $MERLIN server construct -position 252:8 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 252,
        "col": 8
      },
      "end": {
        "line": 252,
        "col": 9
      }
    },
    [
      "object method get = _ method a = _ end"
    ]
  ]

[construct:46-1] Object 2

  $ $MERLIN server construct -depth 2 -position 252:8 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "object method get int = _ method a = \"\" end"
  ]

[construct:47-1] Object 3

  $ $MERLIN server construct -depth 3 -position 252:8 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "object method get int = None method a = \"\" end",
    "object method get int = Some _ method a = \"\" end"
  ]


[construct:48-1] Object 4

  $ $MERLIN server construct -position 256:8 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "object method x = _ end"
  ]

[construct:49-1] Object 5

  $ $MERLIN server construct -position 258:8 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "object method x = _ end"
  ]

[construct:50-1] Object 6

  $ $MERLIN server construct -position 261:8 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "object method y = _ method x = _ end"
  ]

[construct:51-1] Parenthesis on sum

  $ $MERLIN server construct -position 266:36 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 266,
        "col": 35
      },
      "end": {
        "line": 266,
        "col": 38
      }
    },
    [
      "None",
      "(Some _)"
    ]
  ]

[construct:52-1] Prefix 1

  $ $MERLIN server construct -position 274:23 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 274,
        "col": 23
      },
      "end": {
        "line": 274,
        "col": 24
      }
    },
    [
      "(A _)",
      "B"
    ]
  ]


[construct:53-1] Prefix 2

  $ $MERLIN server construct -position 281:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 281,
        "col": 16
      },
      "end": {
        "line": 281,
        "col": 17
      }
    },
    [
      "(A _)",
      "B"
    ]
  ]

[construct:54-1] Prefix 3

  $ $MERLIN server construct -position 288:23 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(A _)",
    "B"
  ]

[construct:55-1] Prefix 4

  $ $MERLIN server construct -position 289:23 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "{ a = _ }"
  ]

[construct:56-1] Prefix 5

  $ $MERLIN server construct -position 291:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(A _)",
    "B"
  ]

[construct:57-1] Prefix 6

  $ $MERLIN server construct -position 292:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "{ a = _ }"
  ]

[construct:58-1] Prefix 7

  $ $MERLIN server construct -position 288:23 \
  > -filename ctxt.ml < ctxt.ml -w +disambiguated-name | jq ".value[1]"
  [
    "(Prefix.A _)",
    "Prefix.B"
  ]

[construct:59-1] Prefix 8

  $ $MERLIN server construct -position 289:23 \
  > -filename ctxt.ml < ctxt.ml -w +disambiguated-name | jq ".value[1]"
  [
    "{ Prefix.a = _ }"
  ]


[construct:60-1] Prefix 9

  $ $MERLIN server construct -position 291:16 \
  > -filename ctxt.ml < ctxt.ml -w +disambiguated-name | jq ".value[1]"
  [
    "(A _)",
    "B"
  ]

[construct:61-1] Prefix 10

  $ $MERLIN server construct -position 292:16 \
  > -filename ctxt.ml < ctxt.ml -w +disambiguated-name | jq ".value[1]"
  [
    "{ a = _ }"
  ]

[construct:62-1] Simple 1

  $ $MERLIN server construct -position 302:25 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 302,
        "col": 25
      },
      "end": {
        "line": 302,
        "col": 26
      }
    },
    [
      "None",
      "(Some _)"
    ]
  ]

[construct:63-1] Simple 2

  $ $MERLIN server construct -depth 2 -position 302:25 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 302,
        "col": 25
      },
      "end": {
        "line": 302,
        "col": 26
      }
    },
    [
      "None",
      "(Some 0)"
    ]
  ]

[construct:64-1] Simple 3

  $ $MERLIN server construct -with-values local -position 302:25 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 302,
        "col": 25
      },
      "end": {
        "line": 302,
        "col": 26
      }
    },
    [
      "None",
      "(Some _)",
      "nice_candidate",
      "(nice_candidate_with_arg _)",
      "(nice_candidate_with_labeled_arg ~x:_)",
      "(Compl_kind.funnyny _)",
      "Compl_1575_1.something",
      "Compl_1575_2.something",
      "Compl_1575_3.something",
      "Compl_1575_4.something"
    ]
  ]

[construct:65-1] Simple 4
FIXME: Results looks suspicious

  $ $MERLIN server construct -with-values local -depth 2 -position 302:25 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 302,
        "col": 25
      },
      "end": {
        "line": 302,
        "col": 26
      }
    },
    [
      "None",
      "(Some Compl_1575_4.something)",
      "(Some Compl_1575_2.something)",
      "(Some (Compl_kind.funnyny _))",
      "(Some (Compl_app_ctxt.foo ~j:_ ~i:_))",
      "(Some y)",
      "(Some 0)",
      "(Some Compl_app_ctxt.bar)",
      "(Some (Compl_app_ctxt.y ~j:_))",
      "(Some Compl_1575_1.something)",
      "(Some Compl_1575_3.something)",
      "(Some Construct_c_errors_a.x)",
      "nice_candidate",
      "(nice_candidate_with_arg _)",
      "(nice_candidate_with_labeled_arg ~x:_)",
      "(Compl_kind.funnyny _)",
      "Compl_1575_1.something",
      "Compl_1575_2.something",
      "Compl_1575_3.something",
      "Compl_1575_4.something"
    ]
  ]


[construct:66-1] Simple 5

  $ $MERLIN server construct -position 305:23 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 305,
        "col": 23
      },
      "end": {
        "line": 305,
        "col": 24
      }
    },
    [
      "[]",
      "(_ :: _)"
    ]
  ]

[construct:67-1] Simple 6

  $ $MERLIN server construct -position 308:22 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 308,
        "col": 22
      },
      "end": {
        "line": 308,
        "col": 23
      }
    },
    [
      "[]",
      "(_ :: _)"
    ]
  ]

[construct:68-1] Simple 7

  $ $MERLIN server construct -position 311:23 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 311,
        "col": 23
      },
      "end": {
        "line": 311,
        "col": 24
      }
    },
    [
      "(lazy _)"
    ]
  ]

[construct:69-1] Simple 8

  $ $MERLIN server construct -position 315:22 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 315,
        "col": 22
      },
      "end": {
        "line": 315,
        "col": 23
      }
    },
    [
      "One",
      "Another"
    ]
  ]


[construct:70-1] Simple 9

  $ $MERLIN server construct -position 320:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 320,
        "col": 16
      },
      "end": {
        "line": 320,
        "col": 17
      }
    },
    [
      "{ a = _; b = _ }"
    ]
  ]

[construct:71-1] Simple 10

  $ $MERLIN server construct -position 324:28 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 324,
        "col": 28
      },
      "end": {
        "line": 324,
        "col": 29
      }
    },
    [
      "(fun string -> _)"
    ]
  ]

[construct:72-1] Simple 11

  $ $MERLIN server construct -position 328:59 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 328,
        "col": 59
      },
      "end": {
        "line": 328,
        "col": 60
      }
    },
    [
      "(fun ~v float mytype mytype_1 -> _)"
    ]
  ]

[construct:73-1] Simple 12

  $ $MERLIN server construct -depth 4 -position 328:59 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 328,
        "col": 59
      },
      "end": {
        "line": 328,
        "col": 60
      }
    },
    [
      "(fun ~v float mytype mytype_1 -> 0)"
    ]
  ]

[construct:74-1] Simple 13

  $ $MERLIN server construct -position 333:18 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(_, _, _)"
  ]

[construct:75-1] Simple 14

  $ $MERLIN server construct -position 338:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(`B _)",
    "`A"
  ]

[construct:76-1] Simple 15

  $ $MERLIN server construct -with-values local -position 338:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(`B _)",
    "`A",
    "some_v",
    "(Compl_kind.funnyny _)",
    "Compl_1575_1.something",
    "Compl_1575_2.something",
    "Compl_1575_3.something",
    "Compl_1575_4.something",
    "(C4.x ~int:_)",
    "C8._"
  ]

[construct:77-1] Simple 16

  $ $MERLIN server construct -position 347:6 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(Int _)",
    "(Float _)",
    "(Eq (_, _))"
  ]

[construct:78-1] Simple 17

  $ $MERLIN server construct -depth 2 -position 347:6 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 347,
          "col": 6
        },
        "end": {
          "line": 347,
          "col": 7
        }
      },
      [
        "(Int 0)",
        "(Float 0.0)",
        "(Eq ((Eq (_, _)), (Float _)))",
        "(Eq ((Float _), (Float _)))",
        "(Eq ((Int _), (Int _)))",
        "(Eq ((Eq (_, _)), (Int _)))",
        "(Eq ((Float _), (Eq (_, _))))",
        "(Eq ((Int _), (Eq (_, _))))",
        "(Eq ((Eq (_, _)), (Eq (_, _))))"
      ]
    ],
    "notifications": []
  }

[construct:79-1] Simple 18

  $ $MERLIN server construct -position 350:6 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "(Int _)",
    "(Eq (_, _))"
  ]

[construct:80-1] Simple 19

  $ $MERLIN server construct -with-values local -position 361:6 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "(Int _)",
    "(Float _)",
    "Add",
    "(App (_, _))",
    "v1",
    "v2",
    "(Compl_kind.funnyny _)",
    "Compl_1575_1.something",
    "Compl_1575_2.something",
    "Compl_1575_3.something",
    "Compl_1575_4.something",
    "(C4.x ~int:_)",
    "C8._"
  ]


[construct:81-1] Simple 20

  $ $MERLIN server construct -with-values local -position 363:6 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "(Int _)",
    "(App (_, _))",
    "v1",
    "x",
    "(Compl_kind.funnyny _)",
    "Compl_1575_1.something",
    "Compl_1575_2.something",
    "Compl_1575_3.something",
    "Compl_1575_4.something",
    "(C4.x ~int:_)",
    "C8._"
  ]

[construct:82-1] Simple 21

  $ $MERLIN server construct -position 367:14 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  []

[construct:83-1] Simple 22

  $ $MERLIN server construct -position 370:30 \
  > -filename ctxt.ml < ctxt.ml | jq '.value'
  [
    {
      "start": {
        "line": 370,
        "col": 8
      },
      "end": {
        "line": 370,
        "col": 31
      }
    },
    [
      "[]",
      "(_ :: _)"
    ]
  ]

[construct:84-1] Simple 23

  $ $MERLIN server construct -position 373:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "0"
  ]

  $ $MERLIN server construct -position 374:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "0n"
  ]

  $ $MERLIN server construct -position 375:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "0l"
  ]

  $ $MERLIN server construct -position 376:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "0L"
  ]

  $ $MERLIN server construct -position 377:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "0.0"
  ]

  $ $MERLIN server construct -position 378:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "'c'"
  ]

  $ $MERLIN server construct -position 379:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "\"\""
  ]

  $ $MERLIN server construct -position 380:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "false"
  ]

  $ $MERLIN server construct -position 381:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "()"
  ]

  $ $MERLIN server construct -position 382:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "exn"
  ]

  $ $MERLIN server construct -position 383:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "[||]"
  ]

  $ $MERLIN server construct -position 384:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "(lazy _)"
  ]

[destruct:85-1] complete 1

  $ $MERLIN server case-analysis -start 419:8 -end 419:8 \
  > -filename ctxt.ml < ctxt.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 419,
          "col": 18
        },
        "end": {
          "line": 419,
          "col": 18
        }
      },
      "|Some _ -> _"
    ],
    "notifications": []
  }


;;; RE-RUNNING FOR CHEAP STRESS-TESTING


[complete-prefix:1-2] Application context

  $ $MERLIN server complete-prefix -position 4:17 \
  > -filename ctxt.ml < ctxt.ml \
  > | tr '\n' ' ' | jq ".value.context"
  [
    "application",
    {
      "argument_type": "int",
      "labels": [
        {
          "name": "~j",
          "type": "int"
        }
      ]
    }
  ]


[complete-prefix:2-2] Disambiguation 1

  $ $MERLIN server complete-prefix -position 12:22 -prefix Foo -doc n \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "Foobar",
          "kind": "Constructor",
          "desc": "T.t",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }

[complete-prefix:3-2] Disambiguation 2

  $ $MERLIN server complete-prefix -position 20:21 -prefix T.f -doc n \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "foobar",
          "kind": "Label",
          "desc": "T.t -> int",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }


[complete-prefix:4-2] Disambiguation 3

  $ $MERLIN server complete-prefix -position 22:22 -prefix foo -doc n \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "foobar",
          "kind": "Label",
          "desc": "T.t -> int",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }

[complete-prefix:5-1] Disambiguation 4

  $ $MERLIN server complete-prefix -position 26:35 -prefix tes -doc n \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "test_other",
          "kind": "Label",
          "desc": "T.t -> float",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }

[expand-prefix:1-2] Expansion 1

  $ $MERLIN server expand-prefix -position 32:15 -prefix L.m -doc n \
  > -filename ctxt.ml < ctxt.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "Lazy.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "Lazy.map_val",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    }
  ]

[expand-prefix:2-2] Expansion 2

  $ $MERLIN server expand-prefix -position 35:17 -prefix Lsi.m -doc n \
  > -filename ctxt.ml < ctxt.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "List.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "List.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.map2",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mapi",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assoc",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.mem_assq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.memq",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    },
    {
      "name": "ListLabels.merge",
      "kind": "Value",
      "desc": "",
      "info": "",
      "deprecated": false
    }
  ]

[complete-prefix:6-2] Infix 1

  $ $MERLIN server complete-prefix -position 49:12 -prefix "Z." -doc n \
  > -filename ctxt.ml < ctxt.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "(>>)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(>>=)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(>>|)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(|+)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(|-)",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    }
  ]


[complete-prefix:7-2] Kind 1

  $ $MERLIN server complete-prefix -position 54:12 -prefix fu \
  > -filename ctxt.ml < ctxt.ml | jq ".value.entries[].name"
  "funnyny"


[complete-prefix:8-2] Kind 2

  $ $MERLIN server complete-prefix -position 54:12 -prefix fu \
  > -kind k -filename ctxt.ml < ctxt.ml | jq ".value.entries[].name"
  "function"
  "fun"
  "functor"

[complete-prefix:9-2] Kind 3

  $ $MERLIN server complete-prefix -position 54:12 -prefix fu \
  > -kind keyword -kind value -filename ctxt.ml < ctxt.ml | jq ".value.entries[].name"
  "funnyny"
  "function"
  "fun"
  "functor"

[complete-prefix:10-2] Kind 4

  $ $MERLIN server complete-prefix -position 52:4 -prefix f \
  > -kind k -extension lwt -filename ctxt.ml < ctxt.ml | jq ".value.entries[].name"
  "function"
  "false"
  "fun"
  "for"
  "functor"

[complete-prefix:11-2] Kind 5 [FIXME from single to server]

  $ $MERLIN single complete-prefix -position 56:16 \
  > -filename ctxt.ml -prefix List.f < ctxt.ml | jq ".value.entries[].name"
  "fast_sort"
  "filter"
  "filter_map"
  "filteri"
  "find"
  "find_all"
  "find_index"
  "find_map"
  "find_mapi"
  "find_opt"
  "flatten"
  "fold_left"
  "fold_left2"
  "fold_left_map"
  "fold_right"
  "fold_right2"
  "for_all"
  "for_all2"


[complete-prefix:12-2] Kind 6

  $ $MERLIN server complete-prefix -position 56:16 \
  > -filename ctxt.ml -kind k -prefix List.f < ctxt.ml | jq ".value.entries"
  []


[complete-prefix:12-2] Parenthesize 1

  $ $MERLIN server complete-prefix -position 70:17 -prefix MyList. \
  > -filename ctxt.ml < ctxt.ml | jq ".value.entries | sort_by(.name)"
  [
    {
      "name": "(())",
      "kind": "Constructor",
      "desc": "MyList.u",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(::)",
      "kind": "Constructor",
      "desc": "'a * 'a MyList.t -> 'a MyList.t",
      "info": "",
      "deprecated": false
    },
    {
      "name": "([])",
      "kind": "Constructor",
      "desc": "'a MyList.t",
      "info": "",
      "deprecated": false
    },
    {
      "name": "(mod)",
      "kind": "Value",
      "desc": "MyList.u",
      "info": "",
      "deprecated": false
    },
    {
      "name": "random",
      "kind": "Value",
      "desc": "int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "t",
      "kind": "Type",
      "desc": "type 'a t = (::) of 'a * 'a MyList.t | []",
      "info": "",
      "deprecated": false
    },
    {
      "name": "u",
      "kind": "Type",
      "desc": "type u = ()",
      "info": "",
      "deprecated": false
    }
  ]



[complete-prefix:13-2] issue1575 1

  $ $MERLIN server complete-prefix -position 80:20 -prefix go \
  > -filename ctxt.ml < ctxt.ml | jq '.value.entries'
  [
    {
      "name": "goo",
      "kind": "Value",
      "desc": "< bar : int -> int; bazs : 'a -> string >",
      "info": "",
      "deprecated": false
    }
  ]

[complete-prefix:14-2] issue1575 2

  $ $MERLIN server complete-prefix -position 89:22 -prefix "" \
  > -filename ctxt.ml < ctxt.ml | jq '.value.entries'
  [
    {
      "name": "bar",
      "kind": "#",
      "desc": "int -> int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "bazs",
      "kind": "#",
      "desc": "'a -> string",
      "info": "",
      "deprecated": false
    }
  ]

[complete-prefix:15-2] issue1575 3

  $ $MERLIN server complete-prefix -position 98:25 -prefix baz \
  > -filename ctxt.ml < ctxt.ml | jq '.value.entries'
  [
    {
      "name": "bazs",
      "kind": "#",
      "desc": "'a -> string",
      "info": "",
      "deprecated": false
    }
  ]


[complete-prefix:16-2] issue1575 4

  $ $MERLIN server complete-prefix -position 109:26 -prefix "ba" \
  > -filename ctxt.ml < ctxt.ml | jq '.value.entries'
  [
    {
      "name": "bar",
      "kind": "#",
      "desc": "int -> int",
      "info": "",
      "deprecated": false
    },
    {
      "name": "bazs",
      "kind": "#",
      "desc": "'a -> string",
      "info": "",
      "deprecated": false
    }
  ]


[complete-prefix:17-1] issue-lsp-503 [FIXME from single to server]

  $ $MERLIN single complete-prefix -position 113:16 -prefix "List.ma" \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": {
      "entries": [
        {
          "name": "map",
          "kind": "Value",
          "desc": "('a -> 'b) -> 'a list -> 'b list",
          "info": "",
          "deprecated": false
        },
        {
          "name": "mapi",
          "kind": "Value",
          "desc": "(int -> 'a -> 'b) -> 'a list -> 'b list",
          "info": "",
          "deprecated": false
        },
        {
          "name": "map2",
          "kind": "Value",
          "desc": "('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list",
          "info": "",
          "deprecated": false
        }
      ],
      "context": null
    },
    "notifications": []
  }

[construct:18-2] Module 1

  $ $MERLIN server construct -position 167:20 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 167,
          "col": 19
        },
        "end": {
          "line": 167,
          "col": 20
        }
      },
      [
        "struct
    type t = private b
    and b =
      | A 
      | B of t 
    type (-!'a, +!'b) t' =
      | T of ('a -> 'b) 
    type t2 =
      | A 
      | B of string 
      | C of t 
    type nonrec r = {
      lbl1: t ;
      lbl2: float list }
    type nonrec n = r
    and m = float
    type t_ext = ..
    type t_ext +=  
      | Str of string 
    type t_ext +=  
      | A 
    type v = [ `A of t_ext ]
    let i = _
    let f = _
    module Sub = struct let y = _ end
    [@@@ocaml.text
      \"Construct does not handle class types yet. Please replace this comment by [room]'s definition.\"]
    [@@@ocaml.text
      \"Construct does not handle classes yet. Please replace this comment by [croom]'s definition.\"]
    module type Another  = sig val i : int end
    module type Sig  =
      sig
        type t
        and b
        val f : int -> float
        module type STyp  = sig  end
        module D : Another
      end
    module Submod =
      struct
        type t
        and b
        let f = _
        module type STyp  = sig  end
        module D = struct let i = _ end
      end
    module SubFunc(M:Sig) = struct let g = _ end
  end"
      ]
    ],
    "notifications": []
  }


[construct:19-2] Module 2

  $ $MERLIN server construct -position 169:30 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 169,
          "col": 29
        },
        "end": {
          "line": 169,
          "col": 30
        }
      },
      [
        "((module struct type t = int end) : (module Small))"
      ]
    ],
    "notifications": []
  }

[construct:20-2] Module 3

  $ $MERLIN server construct -position 171:21 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 171,
          "col": 20
        },
        "end": {
          "line": 171,
          "col": 21
        }
      },
      [
        "struct type t = int end"
      ]
    ],
    "notifications": []
  }

[construct:21-2] Functor Application 1

  $ $MERLIN server construct -position 124:26 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 124,
          "col": 25
        },
        "end": {
          "line": 124,
          "col": 26
        }
      },
      [
        "struct let x = _ end"
      ]
    ],
    "notifications": []
  }


[construct:22-2] Depth 1.1

  $ $MERLIN server construct -depth 1 -position 177:32 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "None",
    "(Some _)"
  ]


[construct:23-2] Depth 1.2

  $ $MERLIN server construct -depth 2 -position 177:32 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "None",
    "(Some None)",
    "(Some (Some _))"
  ]

[construct:24-2] Depth 1.3

  $ $MERLIN server construct -depth 3 -position 177:32 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "None",
    "(Some None)",
    "(Some (Some 0))"
  ]


[construct:25-2] Depth 1.4

  $ $MERLIN server construct -depth 4 -position 177:32 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "None",
    "(Some None)",
    "(Some (Some 0))"
  ]

[construct:26-2] Depth 2.1

  $ $MERLIN server construct -depth 1 -position 181:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "{ a = _; b = _ }"
  ]

[construct:27-2] Depth 2.2

  $ $MERLIN server construct -depth 2 -position 181:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "{ a = (Some _); b = None }",
    "{ a = None; b = None }",
    "{ a = None; b = (Some _) }",
    "{ a = (Some _); b = (Some _) }"
  ]


[construct:28-2] Depth 2.3

  $ $MERLIN server construct -depth 3 -position 181:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "{ a = (Some (Some _)); b = None }",
    "{ a = None; b = None }",
    "{ a = (Some None); b = None }",
    "{ a = (Some None); b = (Some 0.0) }",
    "{ a = None; b = (Some 0.0) }",
    "{ a = (Some (Some _)); b = (Some 0.0) }"
  ]


[construct:29-2] Depth 2.4

  $ $MERLIN server construct -depth 4 -position 181:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "{ a = (Some (Some 0)); b = None }",
    "{ a = None; b = None }",
    "{ a = (Some None); b = None }",
    "{ a = (Some None); b = (Some 0.0) }",
    "{ a = None; b = (Some 0.0) }",
    "{ a = (Some (Some 0)); b = (Some 0.0) }"
  ]


[construct:30-2] Depth 3.1

  $ $MERLIN server construct -depth 2 -position 185:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "((Some _), None)",
    "(None, None)",
    "(None, (Some _))",
    "((Some _), (Some _))"
  ]

[construct:31-2] Error 1

  $ $MERLIN server construct -position 190:20 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "error",
    "value": "Construct only works on holes.",
    "notifications": []
  }

[construct:32-2] Error 2

  $ $MERLIN server construct -position 192:18 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "error",
    "value": "Construct only works on holes.",
    "notifications": []
  }

[construct:33-2] Error 3

  $ $MERLIN server construct -position 198:8 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "error",
    "value": "Construct only works on holes.",
    "notifications": []
  }

[construct:34-2] Error 4

  $ $MERLIN server construct -position 202:20 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "error",
    "value": "Construct only works on holes.",
    "notifications": []
  }

[construct:35-2] Error 5

  $ $MERLIN server construct -position 205:19 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "error",
    "value": "Could not find a module type to construct from. Check that you used a correct constraint.",
    "notifications": []
  }

[construct:36-2] Error 6

  $ $MERLIN server construct -position 208:15 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "error",
    "value": "Could not find a module type to construct from. Check that you used a correct constraint.",
    "notifications": []
  }



[construct:37-1] Fun 1

  $ $MERLIN server construct -position 217:6 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 217,
        "col": 6
      },
      "end": {
        "line": 217,
        "col": 7
      }
    },
    [
      "(fun the_type the_type_1 -> _)"
    ]
  ]

[construct:38-1] Fun 2

  $ $MERLIN server construct -position 223:6 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 223,
        "col": 6
      },
      "end": {
        "line": 223,
        "col": 7
      }
    },
    [
      "(fun int int_1 -> _)"
    ]
  ]


[construct:39-1] Fun 3

  $ $MERLIN server construct -position 233:6 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  []

[construct:40-1] Fun 4 (the result diverge since we have a richer context)

  $ $MERLIN server construct -position 233:6 -with-values local \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "x",
    "(Compl_kind.funnyny _)",
    "Compl_1575_1.something",
    "Compl_1575_2.something",
    "Compl_1575_3.something",
    "Compl_1575_4.something",
    "(Mymod.f _)",
    "Mymod.x"
  ]

[construct:41-1] Fun 5

  $ $MERLIN server construct -position 235:6 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "0.0"
  ]

[construct:42-1] Fun 6

  $ $MERLIN server construct -position 235:6 -with-values local \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "0.0",
    "(g _)",
    "(Compl_kind.funnyny _)",
    "Compl_1575_1.something",
    "Compl_1575_2.something",
    "Compl_1575_3.something",
    "Compl_1575_4.something"
  ]



[construct:43-2] Inline record 1

  $ $MERLIN server construct -position 245:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(Foo _)"
  ]

[construct:44-2] Inline record 2

  $ $MERLIN server construct -position 246:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(Bar _)"
  ]

[construct:45-2] Object 1

  $ $MERLIN server construct -position 252:8 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 252,
        "col": 8
      },
      "end": {
        "line": 252,
        "col": 9
      }
    },
    [
      "object method get = _ method a = _ end"
    ]
  ]

[construct:46-2] Object 2

  $ $MERLIN server construct -depth 2 -position 252:8 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "object method get int = _ method a = \"\" end"
  ]

[construct:47-2] Object 3

  $ $MERLIN server construct -depth 3 -position 252:8 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "object method get int = None method a = \"\" end",
    "object method get int = Some _ method a = \"\" end"
  ]


[construct:48-2] Object 4

  $ $MERLIN server construct -position 256:8 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "object method x = _ end"
  ]

[construct:49-2] Object 5

  $ $MERLIN server construct -position 258:8 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "object method x = _ end"
  ]

[construct:50-2] Object 6

  $ $MERLIN server construct -position 261:8 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "object method y = _ method x = _ end"
  ]

[construct:51-2] Parenthesis on sum

  $ $MERLIN server construct -position 266:36 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 266,
        "col": 35
      },
      "end": {
        "line": 266,
        "col": 38
      }
    },
    [
      "None",
      "(Some _)"
    ]
  ]

[construct:52-2] Prefix 1

  $ $MERLIN server construct -position 274:23 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 274,
        "col": 23
      },
      "end": {
        "line": 274,
        "col": 24
      }
    },
    [
      "(A _)",
      "B"
    ]
  ]


[construct:53-2] Prefix 2

  $ $MERLIN server construct -position 281:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 281,
        "col": 16
      },
      "end": {
        "line": 281,
        "col": 17
      }
    },
    [
      "(A _)",
      "B"
    ]
  ]

[construct:54-2] Prefix 3

  $ $MERLIN server construct -position 288:23 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(A _)",
    "B"
  ]

[construct:55-2] Prefix 4

  $ $MERLIN server construct -position 289:23 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "{ a = _ }"
  ]

[construct:56-2] Prefix 5

  $ $MERLIN server construct -position 291:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(A _)",
    "B"
  ]

[construct:57-2] Prefix 6

  $ $MERLIN server construct -position 292:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "{ a = _ }"
  ]

[construct:58-2] Prefix 7

  $ $MERLIN server construct -position 288:23 \
  > -filename ctxt.ml < ctxt.ml -w +disambiguated-name | jq ".value[1]"
  [
    "(Prefix.A _)",
    "Prefix.B"
  ]

[construct:59-2] Prefix 8

  $ $MERLIN server construct -position 289:23 \
  > -filename ctxt.ml < ctxt.ml -w +disambiguated-name | jq ".value[1]"
  [
    "{ Prefix.a = _ }"
  ]


[construct:60-2] Prefix 9

  $ $MERLIN server construct -position 291:16 \
  > -filename ctxt.ml < ctxt.ml -w +disambiguated-name | jq ".value[1]"
  [
    "(A _)",
    "B"
  ]

[construct:61-2] Prefix 10

  $ $MERLIN server construct -position 292:16 \
  > -filename ctxt.ml < ctxt.ml -w +disambiguated-name | jq ".value[1]"
  [
    "{ a = _ }"
  ]



[construct:62-2] Simple 1

  $ $MERLIN server construct -position 302:25 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 302,
        "col": 25
      },
      "end": {
        "line": 302,
        "col": 26
      }
    },
    [
      "None",
      "(Some _)"
    ]
  ]

[construct:63-2] Simple 2

  $ $MERLIN server construct -depth 2 -position 302:25 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 302,
        "col": 25
      },
      "end": {
        "line": 302,
        "col": 26
      }
    },
    [
      "None",
      "(Some 0)"
    ]
  ]

[construct:64-2] Simple 3

  $ $MERLIN server construct -with-values local -position 302:25 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 302,
        "col": 25
      },
      "end": {
        "line": 302,
        "col": 26
      }
    },
    [
      "None",
      "(Some _)",
      "nice_candidate",
      "(nice_candidate_with_arg _)",
      "(nice_candidate_with_labeled_arg ~x:_)",
      "(Compl_kind.funnyny _)",
      "Compl_1575_1.something",
      "Compl_1575_2.something",
      "Compl_1575_3.something",
      "Compl_1575_4.something"
    ]
  ]

[construct:65-2] Simple 4
FIXME: Results looks suspicious

  $ $MERLIN server construct -with-values local -depth 2 -position 302:25 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 302,
        "col": 25
      },
      "end": {
        "line": 302,
        "col": 26
      }
    },
    [
      "None",
      "(Some Compl_1575_4.something)",
      "(Some Compl_1575_2.something)",
      "(Some (Compl_kind.funnyny _))",
      "(Some (Compl_app_ctxt.foo ~j:_ ~i:_))",
      "(Some y)",
      "(Some 0)",
      "(Some Compl_app_ctxt.bar)",
      "(Some (Compl_app_ctxt.y ~j:_))",
      "(Some Compl_1575_1.something)",
      "(Some Compl_1575_3.something)",
      "(Some Construct_c_errors_a.x)",
      "nice_candidate",
      "(nice_candidate_with_arg _)",
      "(nice_candidate_with_labeled_arg ~x:_)",
      "(Compl_kind.funnyny _)",
      "Compl_1575_1.something",
      "Compl_1575_2.something",
      "Compl_1575_3.something",
      "Compl_1575_4.something"
    ]
  ]


[construct:66-2] Simple 5

  $ $MERLIN server construct -position 305:23 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 305,
        "col": 23
      },
      "end": {
        "line": 305,
        "col": 24
      }
    },
    [
      "[]",
      "(_ :: _)"
    ]
  ]

[construct:67-2] Simple 6

  $ $MERLIN server construct -position 308:22 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 308,
        "col": 22
      },
      "end": {
        "line": 308,
        "col": 23
      }
    },
    [
      "[]",
      "(_ :: _)"
    ]
  ]

[construct:68-2] Simple 7

  $ $MERLIN server construct -position 311:23 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 311,
        "col": 23
      },
      "end": {
        "line": 311,
        "col": 24
      }
    },
    [
      "(lazy _)"
    ]
  ]

[construct:69-2] Simple 8

  $ $MERLIN server construct -position 315:22 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 315,
        "col": 22
      },
      "end": {
        "line": 315,
        "col": 23
      }
    },
    [
      "One",
      "Another"
    ]
  ]


[construct:70-2] Simple 9

  $ $MERLIN server construct -position 320:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 320,
        "col": 16
      },
      "end": {
        "line": 320,
        "col": 17
      }
    },
    [
      "{ a = _; b = _ }"
    ]
  ]

[construct:71-2] Simple 10

  $ $MERLIN server construct -position 324:28 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 324,
        "col": 28
      },
      "end": {
        "line": 324,
        "col": 29
      }
    },
    [
      "(fun string -> _)"
    ]
  ]

[construct:72-2] Simple 11

  $ $MERLIN server construct -position 328:59 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 328,
        "col": 59
      },
      "end": {
        "line": 328,
        "col": 60
      }
    },
    [
      "(fun ~v float mytype mytype_1 -> _)"
    ]
  ]

[construct:73-2] Simple 12

  $ $MERLIN server construct -depth 4 -position 328:59 \
  > -filename ctxt.ml < ctxt.ml | jq ".value"
  [
    {
      "start": {
        "line": 328,
        "col": 59
      },
      "end": {
        "line": 328,
        "col": 60
      }
    },
    [
      "(fun ~v float mytype mytype_1 -> 0)"
    ]
  ]

[construct:74-2] Simple 13

  $ $MERLIN server construct -position 333:18 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(_, _, _)"
  ]

[construct:75-2] Simple 14

  $ $MERLIN server construct -position 338:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(`B _)",
    "`A"
  ]

[construct:76-2] Simple 15

  $ $MERLIN server construct -with-values local -position 338:16 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(`B _)",
    "`A",
    "some_v",
    "(Compl_kind.funnyny _)",
    "Compl_1575_1.something",
    "Compl_1575_2.something",
    "Compl_1575_3.something",
    "Compl_1575_4.something",
    "(C4.x ~int:_)",
    "C8._"
  ]

[construct:77-2] Simple 16

  $ $MERLIN server construct -position 347:6 \
  > -filename ctxt.ml < ctxt.ml | jq ".value[1]"
  [
    "(Int _)",
    "(Float _)",
    "(Eq (_, _))"
  ]

[construct:78-2] Simple 17

  $ $MERLIN server construct -depth 2 -position 347:6 \
  > -filename ctxt.ml < ctxt.ml
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 347,
          "col": 6
        },
        "end": {
          "line": 347,
          "col": 7
        }
      },
      [
        "(Int 0)",
        "(Float 0.0)",
        "(Eq ((Eq (_, _)), (Float _)))",
        "(Eq ((Float _), (Float _)))",
        "(Eq ((Int _), (Int _)))",
        "(Eq ((Eq (_, _)), (Int _)))",
        "(Eq ((Float _), (Eq (_, _))))",
        "(Eq ((Int _), (Eq (_, _))))",
        "(Eq ((Eq (_, _)), (Eq (_, _))))"
      ]
    ],
    "notifications": []
  }

[construct:79-2] Simple 18

  $ $MERLIN server construct -position 350:6 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "(Int _)",
    "(Eq (_, _))"
  ]

[construct:80-2] Simple 19

  $ $MERLIN server construct -with-values local -position 361:6 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "(Int _)",
    "(Float _)",
    "Add",
    "(App (_, _))",
    "v1",
    "v2",
    "(Compl_kind.funnyny _)",
    "Compl_1575_1.something",
    "Compl_1575_2.something",
    "Compl_1575_3.something",
    "Compl_1575_4.something",
    "(C4.x ~int:_)",
    "C8._"
  ]


[construct:81-2] Simple 20

  $ $MERLIN server construct -with-values local -position 363:6 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "(Int _)",
    "(App (_, _))",
    "v1",
    "x",
    "(Compl_kind.funnyny _)",
    "Compl_1575_1.something",
    "Compl_1575_2.something",
    "Compl_1575_3.something",
    "Compl_1575_4.something",
    "(C4.x ~int:_)",
    "C8._"
  ]

[construct:82-2] Simple 21

  $ $MERLIN server construct -position 367:14 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  []

[construct:83-1] Simple 22

  $ $MERLIN server construct -position 370:30 \
  > -filename ctxt.ml < ctxt.ml | jq '.value'
  [
    {
      "start": {
        "line": 370,
        "col": 8
      },
      "end": {
        "line": 370,
        "col": 31
      }
    },
    [
      "[]",
      "(_ :: _)"
    ]
  ]

[construct:84-2] Simple 23

  $ $MERLIN server construct -position 373:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "0"
  ]

  $ $MERLIN server construct -position 374:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "0n"
  ]

  $ $MERLIN server construct -position 375:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "0l"
  ]

  $ $MERLIN server construct -position 376:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "0L"
  ]

  $ $MERLIN server construct -position 377:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "0.0"
  ]

  $ $MERLIN server construct -position 378:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "'c'"
  ]

  $ $MERLIN server construct -position 379:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "\"\""
  ]

  $ $MERLIN server construct -position 380:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "false"
  ]

  $ $MERLIN server construct -position 381:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "()"
  ]

  $ $MERLIN server construct -position 382:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "exn"
  ]

  $ $MERLIN server construct -position 383:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "[||]"
  ]

  $ $MERLIN server construct -position 384:26 \
  > -filename ctxt.ml < ctxt.ml | jq '.value[1]'
  [
    "(lazy _)"
  ]


[destruct:85-2] complete 1

  $ $MERLIN server case-analysis -start 419:8 -end 419:8 \
  > -filename ctxt.ml < ctxt.ml | \
  > sed -e 's/, /,/g' | sed -e 's/ *| */|/g' | tr -d '\n' | jq '.'
  {
    "class": "return",
    "value": [
      {
        "start": {
          "line": 419,
          "col": 18
        },
        "end": {
          "line": 419,
          "col": 18
        }
      },
      "|Some _ -> _"
    ],
    "notifications": []
  }


Some cleanup.

  $ $MERLIN server stop-server
