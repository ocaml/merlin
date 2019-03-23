import outdent from "outdent";
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

  it("returns type inferred under cursor with documentation", async () => {
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
        "(** This function has a nice documentation *)\nlet id x = x\n"
      )
    });

    let result = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(1, 4)
    });

    expect(result).toMatchObject({
      contents: {
        kind: "markdown",
        value: "```ocaml\n'a -> 'a\n(** This function has a nice documentation *)\n```" }
    });
  });

  it("returns good type when cursor is between values", async () => {
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
        outdent`
          let f i f = float_of_int i +. f
          let i = 10
          let f = 10.
          let sum = f i f
       `
      )
    });

    let result = await languageServer.sendRequest("textDocument/hover", {
      textDocument: Types.TextDocumentIdentifier.create("file:///test.ml"),
      position: Types.Position.create(3, 13)
    });

    expect(result).toMatchObject({
      contents: {
        kind: "markdown",
        value: "```ocaml\nint\n```"
      },
      range: {
        start: {"character": 12, "line": 3},
        end: {"character": 13, "line": 3},
      }
    });
  });
});
