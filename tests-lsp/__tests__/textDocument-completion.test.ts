import outdent from "outdent";
import * as LanguageServer from "./../src/LanguageServer";

import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/completion", () => {
  it("completes identifier at top level", async () => {
    let languageServer = await LanguageServer.startAndInitialize();
    let source = outdent`
      Strin
    `;
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "txt",
        0,
        source
      )
    });

    let result = await languageServer.sendRequest("textDocument/completion", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(0, 5)
    });
    expect(result).toMatchObject({
      isIncomplete: false,
      items: [
        { label: "StringLabels", detail: "" },
        { label: "String", detail: "" }
      ]
    });
    await LanguageServer.exit(languageServer);
  });

  it("completes identifier at top level", async () => {
    let languageServer = await LanguageServer.startAndInitialize();
    let source = outdent`
      String.
    `;
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "txt",
        0,
        source
      )
    });

    let result: any = await languageServer.sendRequest(
      "textDocument/completion",
      {
        textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
        position: Types.Position.create(0, 7)
      }
    );
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
    await LanguageServer.exit(languageServer);
  });
});
