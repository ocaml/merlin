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

  it("completes identifier at top level", async () => {
    await openDocument(outdent`
      Strin
    `);

    let result = await queryCompletion(Types.Position.create(0, 5));

    expect(result).toMatchObject({
      isIncomplete: false,
      items: [
        { label: "StringLabels", detail: "" },
        { label: "String", detail: "" }
      ]
    });
  });

  it("completes identifier at top level", async () => {
    openDocument(outdent`
      String.
    `);

    let result: any = await queryCompletion(Types.Position.create(0, 7));
    let items = result.items.map(item => item.label);
    expect(items).toMatchObject([
      "blit",
      "capitalize",
      "capitalize_ascii",
      "compare",
      "concat",
      "contains",
      "contains_from",
      "copy",
      "create",
      "equal",
      "escaped",
      "fill",
      "get",
      "index",
      "index_from",
      "index_from_opt",
      "index_opt",
      "init",
      "iter",
      "iteri",
      "length",
      "lowercase",
      "lowercase_ascii",
      "make",
      "map",
      "mapi",
      "rcontains_from",
      "rindex",
      "rindex_from",
      "rindex_from_opt",
      "rindex_opt",
      "set",
      "split_on_char",
      "sub",
      "trim",
      "uncapitalize",
      "uncapitalize_ascii",
      "unsafe_blit",
      "unsafe_fill",
      "unsafe_get",
      "unsafe_set",
      "uppercase",
      "uppercase_ascii",
      "t"
    ]);
  });

  it("can start completion at arbitrary position (after the dot)", async () => {
    openDocument(outdent`
      Strin.func
    `);

    let result = await queryCompletion(Types.Position.create(0, 5));
    expect(result).toMatchObject({
      isIncomplete: false,
      items: [
        { label: "StringLabels" },
        { label: "String" }
      ]
    });
  });

  it("can start completion at arbitrary position", async () => {
    openDocument(outdent`
      StringLabels
    `);

    let result = await queryCompletion(Types.Position.create(0, 6));
    expect(result).toMatchObject({
      isIncomplete: false,
      items: [
        { label: "StringLabels" },
        { label: "String" }
      ]
    });
  });
});
