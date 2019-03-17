import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";

import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/completion", () => {
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

  async function queryCompletion(position) {
    let result = await languageServer.sendRequest("textDocument/completion", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position
    });
    return result.items.map(item => {
      return {
        label: item.label,
        sortText: item.sortText,
        textEdit: item.textEdit,
      };
    });
  }

  beforeEach(async () => {
    languageServer = await LanguageServer.startAndInitialize();
  });

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
    languageServer = null;
  });

  it("can start completion at arbitrary position (after the dot)", async () => {
    openDocument(outdent`
      Strin.func
    `);

    let items = await queryCompletion(Types.Position.create(0, 5));
    expect(items).toMatchObject([
      { label: "String", sortText: "0000" },
      { label: "StringLabels", sortText: "0001" }
    ]);
  });

  it("can start completion at arbitrary position", async () => {
    openDocument(outdent`
      StringLabels
    `);

    let items = await queryCompletion(Types.Position.create(0, 6));
    expect(items).toMatchObject([
      { label: "String", sortText: "0000" },
      { label: "StringLabels", sortText: "0001" }
    ]);
  });

  it("can start completion at arbitrary position 2", async () => {
    openDocument(outdent`
      StringLabels
    `);

    let items = await queryCompletion(Types.Position.create(0, 7));
    expect(items).toMatchObject([
      { label: "StringLabels", sortText: "0000" }
    ]);
  });

  it("completes identifier at top level", async () => {
    openDocument(outdent`
      let somenum = 42
      let somestring = "hello"

      let () =
        some
    `);

    let items = await queryCompletion(Types.Position.create(4, 6));
    expect(items).toMatchObject([
      { label: "somestring", sortText: "0000" },
      { label: "somenum", sortText: "0001" }
    ]);
  });

  it("completes without prefix", async () => {
    openDocument(outdent`
      let somenum = 42
      let somestring = "hello"

      let plus_42 (x:int) (y:int) =
        somenum + 
    `);

    let items = await queryCompletion(Types.Position.create(4, 12));
    let items_top5 = items.slice(0, 5);
    expect(items_top5).toMatchObject([
      { label: "y", sortText: "0000", textEdit: undefined },
      { label: "x", sortText: "0001", textEdit: undefined },
      { label: "somenum", sortText: "0002", textEdit: undefined },
      { label: "max_int", sortText: "0003", textEdit: undefined },
      { label: "min_int", sortText: "0004", textEdit: undefined },
    ]);
  });
});
