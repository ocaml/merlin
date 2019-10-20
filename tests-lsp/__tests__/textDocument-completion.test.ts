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

  it("can start completion at arbitrary position (before the dot)", async () => {
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

  it("completes from a module", async () => {
    openDocument(outdent`
      let f = List.m
    `);

    let items = await queryCompletion(Types.Position.create(0, 14));
    expect(items).toMatchObject([
      { label: "map", sortText: "0000" },
      { label: "map2", sortText: "0001" },
      { label: "mapi", sortText: "0002" },
      { label: "mem", sortText: "0003" },
      { label: "mem_assoc", sortText: "0004" },
      { label: "mem_assq", sortText: "0005" },
      { label: "memq", sortText: "0006" },
      { label: "merge", sortText: "0007" },
    ]);
  });

  it("completes a module name", async () => {
    openDocument(outdent`
      let f = L
    `);

    let items = await queryCompletion(Types.Position.create(0, 9));
    let items_top5 = items.slice(0, 5);
    expect(items_top5).toMatchObject([
      { label: "LargeFile", sortText: "0000" },
      { label: "Lazy", sortText: "0001" },
      { label: "Lexing", sortText: "0002" },
      { label: "List", sortText: "0003" },
      { label: "ListLabels", sortText: "0004" },
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
      { label: "y", sortText: "0000", textEdit: null },
      { label: "x", sortText: "0001", textEdit: null },
      { label: "somenum", sortText: "0002", textEdit: null },
      { label: "max_int", sortText: "0003", textEdit: null },
      { label: "min_int", sortText: "0004", textEdit: null },
    ]);
  });

  it("completes with invalid prefix", async () => {
    openDocument(outdent`
      let f = Li.ma
    `);

    let items = await queryCompletion(Types.Position.create(0, 13));
    expect(items).toMatchObject([
      {
        label: "ListLabels.map",
        sortText: "0000",
        textEdit: {
          range: {
            start: { "line": 0, "character": 8 },
            end: { "line": 0, "character": 13 }
          },
          newText: "ListLabels.map"
        }
      },
      {
        label: "ListLabels.map2",
        sortText: "0001",
        textEdit: {
          range: {
            start: { "line": 0, "character": 8 },
            end: { "line": 0, "character": 13 }
          },
          newText: "ListLabels.map2"
        }
      },
      {
        label: "ListLabels.mapi",
        sortText: "0002",
        textEdit: {
          range: {
            start: { "line": 0, "character": 8 },
            end: { "line": 0, "character": 13 }
          },
          newText: "ListLabels.mapi"
        }
      },
      {
        label: "List.map",
        sortText: "0003",
        textEdit: {
          range: {
            start: { "line": 0, "character": 8 },
            end: { "line": 0, "character": 13 }
          },
          newText: "List.map"
        }
      },
      {
        label: "List.map2",
        sortText: "0004",
        textEdit: {
          range: {
            start: { "line": 0, "character": 8 },
            end: { "line": 0, "character": 13 }
          },
          newText: "List.map2"
        }
      },
      {
        label: "List.mapi",
        sortText: "0005",
        textEdit: {
          range: {
            start: { "line": 0, "character": 8 },
            end: { "line": 0, "character": 13 }
          },
          newText: "List.mapi"
        }
      }
    ]);
  });

  it("completes with invalid prefix is buggy, it gives suggestions for LL instead of L", async () => {
    openDocument(outdent`
      let f = L.
    `);

    let items = await queryCompletion(Types.Position.create(0, 10));
    let items_top5 = items.slice(0, 5);
    expect(items_top5).toMatchObject([
      {
        label: "ListLabels.append",
        sortText: "0000",
        textEdit: {
          range: {
            start: { "line": 0, "character": 8 },
            end: { "line": 0, "character": 10 }
          },
          newText: "ListLabels.append"
        }
      },
      {
        label: "ListLabels.assoc",
        sortText: "0001",
        textEdit: {
          range: {
            start: { "line": 0, "character": 8 },
            end: { "line": 0, "character": 10 }
          },
          newText: "ListLabels.assoc"
        }
      },
      {
        label: "ListLabels.assoc_opt",
        sortText: "0002",
        textEdit: {
          range: {
            start: { "line": 0, "character": 8 },
            end: { "line": 0, "character": 10 }
          },
          newText: "ListLabels.assoc_opt"
        }
      },
      {
        label: "ListLabels.assq",
        sortText: "0003",
        textEdit: {
          range: {
            start: { "line": 0, "character": 8 },
            end: { "line": 0, "character": 10 }
          },
          newText: "ListLabels.assq"
        }
      },
      {
        label: "ListLabels.assq_opt",
        sortText: "0004",
        textEdit: {
          range: {
            start: { "line": 0, "character": 8 },
            end: { "line": 0, "character": 10 }
          },
          newText: "ListLabels.assq_opt"
        }
      },
    ]);
  });

  it("completes with invalid prefix is buggy", async () => {
    openDocument(outdent`
      let f = LL.
    `);

    let items = await queryCompletion(Types.Position.create(0, 11));
    expect(items).toMatchObject([]);
  });

  it("completes labels", async () => {
    openDocument(outdent`
      let f = ListLabels.map 
    `);

    let items = await queryCompletion(Types.Position.create(0, 23));
    let items_top5 = items.slice(0, 5)
    expect(items_top5).toMatchObject([
      {label: "~f", sortText: "0000", textEdit: null},
      {label: "::", sortText: "0001", textEdit: null},
      {label: "[]", sortText: "0002", textEdit: null},
      {label: "!", sortText: "0003", textEdit: null},
      {label: "exit", sortText: "0004", textEdit: null}
    ]);
  });

});
