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

  async function query() {
    return await languageServer.sendRequest("textDocument/codeLens", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml")
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("returns codeLens for a module", async () => {
    await openDocument(outdent`
      let num = 42
      let string = "Hello"

      module M = struct
        let m a b = a + b
      end
    `);

    let result = await query();

    expect(result).toMatchObject([
      {
        command: { command: "", title: "int -> int -> int" },
        range: {
          end: { character: 19, line: 4 },
          start: { character: 2, line: 4 }
        }
      },
      {
        command: { command: "", title: "string" },
        range: {
          end: { character: 20, line: 1 },
          start: { character: 0, line: 1 }
        }
      },
      {
        command: { command: "", title: "int" },
        range: {
          end: { character: 12, line: 0 },
          start: { character: 0, line: 0 }
        }
      }
    ]);
  });
});
