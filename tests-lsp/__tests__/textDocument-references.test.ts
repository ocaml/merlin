import outdent from "outdent";
import * as LanguageServer from "../src/LanguageServer";

import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/references", () => {
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
    return await languageServer.sendRequest("textDocument/references", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position,
      context: { includeDeclaration: false }
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("finds references in a file", async () => {
    await openDocument(outdent`
      let num = 42
      let sum = num + 13
      let sum2 = sum + num
    `);

    let result = await query(Types.Position.create(0, 4));

    expect(result).toMatchObject([
      {
        range: {
          end: {
            character: 7,
            line: 0
          },
          start: {
            character: 4,
            line: 0
          }
        },
        uri: "file:///test.ml"
      },
      {
        range: {
          end: {
            character: 13,
            line: 1
          },
          start: {
            character: 10,
            line: 1
          }
        },
        uri: "file:///test.ml"
      },
      {
        range: {
          end: {
            character: 20,
            line: 2
          },
          start: {
            character: 17,
            line: 2
          }
        },
        uri: "file:///test.ml"
      }
    ]);
  });
});
