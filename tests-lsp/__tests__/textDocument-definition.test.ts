import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";

import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/definition", () => {
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

  async function queryDefinition(position) {
    return await languageServer.sendRequest("textDocument/definition", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("returns location of a definition", async () => {
    await openDocument(outdent`
      let x = 43

      let () =
        print_int x
    `);

    let result = await queryDefinition(Types.Position.create(3, 12));

    expect(result).toMatchObject([
      {
        range: {
          end: { character: 4, line: 0 },
          start: { character: 4, line: 0 }
        },
        uri: "file://test.ml"
      }
    ]);
  });
});
