import * as LanguageServer from "./../src/LanguageServer";

import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("textDocument/hover", () => {
  let languageServer;

  afterEach(async () => {
    await LanguageServer.exit(languageServer);
  });

  it("returns type inferred under cursor", async () => {
    languageServer = await LanguageServer.startAndInitialize();
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "txt",
        0,
        "let x = 1\n"
      )
    });

    let result = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(0, 4)
    });

    expect(result).toMatchObject({
      contents: { kind: "plaintext", value: "int" },
      range: {end: {character: 5, line: 0}, start: {character: 4, line: 0}},
    });
  });

  it("returns type inferred under cursor (markdown formatting)", async () => {
    languageServer = await LanguageServer.startAndInitialize({
      textDocument: {
        hover: {
          dynamicRegistration: true,
          contentFormat: ["markdown", "plaintext"]
        }
      }
    });
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test.ml",
        "txt",
        0,
        "let x = 1\n"
      )
    });

    let result = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(0, 4)
    });

    expect(result).toMatchObject({
      contents: { kind: "markdown", value: "```ocaml\nint\n```" },
      range: {end: {character: 5, line: 0}, start: {character: 4, line: 0}},
    });
  });
});
