<!-- NOTE: README.md is generated from mode/README.md -->

# @codemirror/legacy-modes [![NPM version](https://img.shields.io/npm/v/@codemirror/legacy-modes.svg)](https://www.npmjs.org/package/@codemirror/legacy-modes)

[ [**WEBSITE**](https://codemirror.net/6/) | [**ISSUES**](https://github.com/codemirror/codemirror.next/issues) | [**FORUM**](https://discuss.codemirror.net/c/next/) | [**CHANGELOG**](https://github.com/codemirror/legacy-modes/blob/main/CHANGELOG.md) ]

This package implements a collection of ported [stream
language](https://codemirror.net/6/docs/ref#stream-parser) modes for
the [CodeMirror](https://codemirror.net/6/) code editor. Each mode is
available as a separate script file, under
`"@codemirror/legacy-modes/mode/[name]"`, and exports the values
listed below.

The [project page](https://codemirror.net/6/) has more information, a
number of [examples](https://codemirror.net/6/examples/) and the
[documentation](https://codemirror.net/6/docs/).

This code is released under an
[MIT license](https://github.com/codemirror/legacy-modes/tree/main/LICENSE).

We aim to be an inclusive, welcoming community. To make that explicit,
we have a [code of
conduct](http://contributor-covenant.org/version/1/1/0/) that applies
to communication around the project.

## Example

Using modes from this package works like this:

 - Install this package and the
   [`@codemirror/stream-parser`](https://codemirror.net/6/docs/ref/#stream-parser)
   package.

 - Find the `StreamParser` instance you need in the reference below.

 - Add `StreamLanguage.define(theParser)` to your editor's
   configuration.

For example, to load the Lua mode, you'd do something like...

```javascript
import {StreamLanguage} from "@codemirror/stream-parser"
import {lua} from "@codemirror/legacy-modes/mode/lua"

import {EditorView, EditorState, basicSetup} from "@codemirror/basic-setup"

let view = new EditorView({
  state: EditorState.create({
    extensions: [basicSetup, StreamLanguage.define(lua)]
  })
})
```

## API Reference
