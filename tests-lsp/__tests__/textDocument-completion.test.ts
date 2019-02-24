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
    return await languageServer.sendRequest("textDocument/completion", {
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

  it("can start completion at arbitrary position (after the dot)", async () => {
    openDocument(outdent`
      Strin.func
    `);

    let result = await queryCompletion(Types.Position.create(0, 5));
    let items = result.items.map(item => item.label);
    items.sort();
    expect(items).toMatchObject(["String", "StringLabels"]);
  });

  it("can start completion at arbitrary position", async () => {
    openDocument(outdent`
      StringLabels
    `);

    let result = await queryCompletion(Types.Position.create(0, 6));
    let items = result.items.map(item => item.label);
    items.sort();
    expect(items).toMatchObject(["String", "StringLabels"]);
  });

  it("completes identifier at top level", async () => {
    openDocument(outdent`
      let somenum = 42
      let somestring = "hello"

      let () =
        some
    `);

    let result: any = await queryCompletion(Types.Position.create(4, 6));
    let items = result.items.map(item => item.label);
    items.sort();
    expect(items).toMatchObject(["somenum", "somestring"]);
  });

});
