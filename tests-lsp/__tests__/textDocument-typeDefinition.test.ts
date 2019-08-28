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
    return await languageServer.sendRequest("textDocument/typeDefinition", {
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

  it("returns location of a type definition", async () => {
    await openDocument(outdent`
      (* type we are jumping on *)
      type t = T of int

      let x = T 43
    `);

    let result = await queryDefinition(Types.Position.create(3, 4));

    expect(result).toMatchObject([
      {
        range: {
          end: { character: 0, line: 1 },
          start: { character: 0, line: 1 }
        },
        uri: "file://test.ml"
      }
    ]);
  });

  it("ignores names in values namespace", async () => {
    await openDocument(outdent`
      (* type we are jumping on *)
      type t = T of int

      let t = T 42
      let x = T 43
    `);

    let result = await queryDefinition(Types.Position.create(4, 4));

    expect(result).toMatchObject([
      {
        range: {
          end: { character: 0, line: 1 },
          start: { character: 0, line: 1 }
        },
        uri: "file://test.ml"
      }
    ]);
  });

  it("ignores names in values namespace (cursor on same named value)", async () => {
    await openDocument(outdent`
      (* type we are jumping on *)
      type t = T of int

      let t = T 42
    `);

    let result = await queryDefinition(Types.Position.create(3, 4));

    expect(result).toMatchObject([
      {
        range: {
          end: { character: 0, line: 1 },
          start: { character: 0, line: 1 }
        },
        uri: "file://test.ml"
      }
    ]);
  });
});
