import * as LanguageServer from "./../src/LanguageServer";

import * as Protocol from "vscode-languageserver-protocol";
import * as Types from "vscode-languageserver-types";

describe("TextDocument: incremental sync", () => {
  async function getDoc(languageServer) {
    let result = await languageServer.sendRequest("debug/textDocument/get", {
      textDocument: Types.TextDocumentIdentifier.create(
        "file:///test-document.txt"
      ),
      position: Types.Position.create(0, 0)
    });
    return result;
  }

  it("updates in the middle of the line", async () => {
    let languageServer = await LanguageServer.startAndInitialize();
    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test-document.txt",
        "txt",
        0,
        "let x = 1;\n\nlet y = 2;"
      )
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y = 2;");

    await languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1
      ),
      contentChanges: [
        {
          range: {
            start: { line: 2, character: 5 },
            end: { line: 2, character: 5 }
          },
          rangeLength: 0,
          text: "1"
        }
      ]
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y1 = 2;");

    await languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1
      ),
      contentChanges: [
        {
          range: {
            start: { line: 2, character: 5 },
            end: { line: 2, character: 6 }
          },
          rangeLength: 1,
          text: ""
        }
      ]
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y = 2;");
    await LanguageServer.exit(languageServer);
  });

  it("updates in at the start of the line", async () => {
    let languageServer = await LanguageServer.startAndInitialize();

    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test-document.txt",
        "txt",
        0,
        "let x = 1;\n\nlet y = 2;"
      )
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y = 2;");

    await languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1
      ),
      contentChanges: [
        {
          range: {
            start: { line: 1, character: 0 },
            end: { line: 1, character: 0 }
          },
          rangeLength: 0,
          text: "s"
        }
      ]
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\ns\nlet y = 2;");
    languageServer = await LanguageServer.startAndInitialize();
  });

  it("update when inserting a line", async () => {
    let languageServer = await LanguageServer.startAndInitialize();

    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test-document.txt",
        "txt",
        0,
        "let x = 1;\n\nlet y = 2;"
      )
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y = 2;");

    await languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1
      ),
      contentChanges: [
        {
          range: {
            start: { line: 0, character: 10 },
            end: { line: 0, character: 10 }
          },
          rangeLength: 0,
          text: "\nlet x = 1;"
        }
      ]
    });

    expect(await getDoc(languageServer)).toEqual(
      "let x = 1;\nlet x = 1;\n\nlet y = 2;"
    );
    languageServer = await LanguageServer.startAndInitialize();
  });

  it("update when inserting a line at the end of the doc", async () => {
    let languageServer = await LanguageServer.startAndInitialize();

    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test-document.txt",
        "txt",
        0,
        "let x = 1;\n\nlet y = 2;"
      )
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y = 2;");

    await languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1
      ),
      contentChanges: [
        {
          range: {
            start: { line: 2, character: 10 },
            end: { line: 2, character: 10 }
          },
          rangeLength: 0,
          text: "\nlet y = 2;"
        }
      ]
    });

    expect(await getDoc(languageServer)).toEqual(
      "let x = 1;\n\nlet y = 2;\nlet y = 2;"
    );
    languageServer = await LanguageServer.startAndInitialize();
  });

  it("update when deleting a line", async () => {
    let languageServer = await LanguageServer.startAndInitialize();

    await languageServer.sendNotification("textDocument/didOpen", {
      textDocument: Types.TextDocumentItem.create(
        "file:///test-document.txt",
        "txt",
        0,
        "let x = 1;\n\nlet y = 2;"
      )
    });

    expect(await getDoc(languageServer)).toEqual("let x = 1;\n\nlet y = 2;");

    await languageServer.sendNotification("textDocument/didChange", {
      textDocument: Types.VersionedTextDocumentIdentifier.create(
        "file:///test-document.txt",
        1
      ),
      contentChanges: [
        {
          range: {
            start: { line: 0, character: 0 },
            end: { line: 1, character: 0 }
          },
          rangeLength: 11,
          text: ""
        }
      ]
    });

    expect(await getDoc(languageServer)).toEqual("\nlet y = 2;");
    languageServer = await LanguageServer.startAndInitialize();
  });
});

describe("TextDocument", () => {
  describe("didOpen", () => {
    it("stores text document", async () => {
      let languageServer = await LanguageServer.startAndInitialize();
      await languageServer.sendNotification("textDocument/didOpen", {
        textDocument: Types.TextDocumentItem.create(
          "file:///test-document.txt",
          "txt",
          0,
          "Hello, World!"
        )
      });

      let result = await languageServer.sendRequest("debug/textDocument/get", {
        textDocument: Types.TextDocumentIdentifier.create(
          "file:///test-document.txt"
        ),
        position: Types.Position.create(0, 0)
      });

      expect(result).toEqual("Hello, World!");
      await LanguageServer.exit(languageServer);
    });
  });

  describe("didChange", () => {
    it("updates text document", async () => {
      let languageServer = await LanguageServer.startAndInitialize();
      await languageServer.sendNotification("textDocument/didOpen", {
        textDocument: Types.TextDocumentItem.create(
          "file:///test-document.txt",
          "txt",
          0,
          "Hello, World!"
        )
      });

      await languageServer.sendNotification("textDocument/didChange", {
        textDocument: Types.VersionedTextDocumentIdentifier.create(
          "file:///test-document.txt",
          1
        ),
        contentChanges: [{ text: "Hello again!" }]
      });

      let result = await languageServer.sendRequest("debug/textDocument/get", {
        textDocument: Types.TextDocumentIdentifier.create(
          "file:///test-document.txt"
        ),
        position: Types.Position.create(0, 0)
      });

      expect(result).toEqual("Hello again!");
      await LanguageServer.exit(languageServer);
    });
  });

  describe("hover", () => {
    it("rerurns the type of the identifier at position", async () => {
      let languageServer = await LanguageServer.startAndInitialize();
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

      expect(result).toMatchObject({ contents: ["int"] });
      await LanguageServer.exit(languageServer);
    });
  });
});
