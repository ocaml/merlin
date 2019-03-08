import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/rename", () => {
  let languageServer = null;

  async function openDocument(source) {
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "txt",
        0,
        source
      )
    });
  }

  async function query(position) {
    return await languageServer.sendRequest("textDocument/rename", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position,
      newName: "new_num"
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("rename value in a file", async () => {
    await openDocument(outdent`
      let num = 42
      let num = num + 13
      let num2 = num
    `);

    let result = await query(Types.Position.create(0, 4));

    expect(result).toMatchObject(
      {
        "documentChanges": [
          {
            "textDocument": {
              "version": 0,
              "uri": "file:///test.ml"
            },
            "edits": [
              {
                "range": {
                  "start": {
                    "line": 0,
                    "character": 4
                  },
                  "end": {
                    "line": 0,
                    "character": 7
                  }
                },
                "newText": "new_num"
              },
              {
                "range": {
                  "start": {
                    "line": 1,
                    "character": 10
                  },
                  "end": {
                    "line": 1,
                    "character": 13
                  }
                },
                "newText": "new_num"
              }
            ]
          }
        ],
        "changes": null
      });
  });
});
