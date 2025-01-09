unreleased
==========

  + merlin binary
    - Support for OCaml 5.3
    - Use new 5.3 features to improve locate behavior in some cases. Merlin no
      longer confuses uids from interfaces and implementations. (#1857)
    - Perform less merges in the indexer (#1881)
    - `locate` can now disambiguate between files with identical names and contents
  + vim plugin
    - Added support for search-by-type (#1846)
      This is exposed through the existing `:MerlinSearch` command, that
      switches between search-by-type and polarity search depending on the
      first character of the query.

merlin 5.3
==========
Tue Nov 26 17:30:42 CET 2024

  + merlin binary
    - Respect the `EXCLUDE_QUERY_DIR` configuration directive when looking for cmt
      files (#1854)
    - Fix occurrences bug in which relative paths in index files are resolved against the
      PWD rather than the SOURCE_ROOT (#1855)
    - Fix exception in polarity search (#1858 fixes #1113)
    - Fix jump to `fun` targets not working (#1863, fixes #1862)
    - Fix type-enclosing results instability. This reverts some overly
      aggressive deduplication that should be done on the client side. (#1864)
    - Fix occurrences not working when the definition comes from a hidden source
      file (#1865)

merlin 5.2.1
============
Fri Sep 27 12:02:42 CEST 2024

  + merlin binary
    - A new `WRAPPING_PREFIX` configuration directive that can be used to tell Merlin
      what to append to the current unit name in the presence of wrapping (#1788)
    - Add `-unboxed-types` and `-no-unboxed-types` as ocaml ignored flags (#1795, fixes #1794)
    - destruct: Refinement in the presence of optional arguments (#1800 #1807, fixes #1770)
    - Implement new expand-node command for expanding PPX annotations (#1745)
    - Implement new inlay-hints command for adding hints on a sourcetree (#1812)
    - Implement new search-by-type command for searching values by types (#1828)
    - Canonicalize paths in occurrences. This helps deduplicate the results and
      show more user-friendly paths. (#1840)
    - Fix dot-merlin-reader ignoring `SOURCE_ROOT` and `STDLIB` directives
      (#1839, #1803)
  + editor modes
    - vim: fix python-3.12 syntax warnings in merlin.py (#1798)
    - vim: Dead code / doc removal for previously deleted MerlinPhrase command (#1804)
    - emacs: Improve the way that result of polarity search is displayed (#1814)
    - emacs: Add `merlin-search-by-type`, `merlin-search-by-polarity` and change the
	  behaviour of `merlin-search` to switch between `by-type` or `by-polarity`
	  depending on the query (#1828)

merlin 5.1
==========
Tue Jun 18 12:00:42 CEST 2024

  + merlin binary
    - Support project-wide occurrences queries using index files (#1766)
      - The file format is described in library `Merlin_lib.index_format`
      - Two new configuration directives are introduced:
        - `SOURCE_ROOT` that is used to resolve relative paths found in the
          indexes.
        - `INDEX` that is used to declare the list of index files Merlin should
          use when looking for occurrences.
    - A new `UNIT_NAME` configuration directive that can be used to tell Merlin
      the correct name of the current unit in the presence of wrapping (#1776)
    - Perform incremental indexation of the buffer when typing. (#1777)
    - `merlin-lib.commands`: Add a `find_command_opt`` alternative to
      `find_command` that does not raise (#1778)
    - Prevent uid clashes by not returning PWO for defs located in the current
      interface file (#1781)
    - Reset uid counters when restoring the typer cache so that uids are stable
      across re-typing (#1779)
    - Improve the behavior on occurrences when the cursor is on a label /
      constructor declaration (#1785)
  + editor modes
    - emacs: add basic support for project-wide occurrences (#1766)
    - vim: add basic support for project-wide occurrences (#1767, @Julow)

merlin 5.0
==========
Fri May 17 19:59:42 CET 2024

  + merlin binary
    - Support for OCaml 5.2 (#1757)
    - destruct: Removal of residual patterns (#1737, fixes #1560)
    - Do not erase fields' names when destructing punned record fields (#1734,
      fixes #1661)
    - Ignore SIGPIPE in the Merlin server process (#1746)
    - Fix lexing of quoted strings in comments (#1754, fixes #1753)
    - Improve cursor position detection in longidents (#1756)
    - Addition of a `merlin-lib.commands` library which disassociates the
      execution of commands from the `new_protocol`, from the binary, allowing
      it to be invoked from other projects (#1758)
    - New occurrences backend: Don't index occurrences when `merlin.hide`
      attribute is present. (#1768)
    - Use the new `uid_to_decl` table in 5.2's cmt files to get documentation.
      (#1773)

merlin 4.14
===========
Thu Feb 22 14:00:42 CET 2024

  + merlin binary
    - Preliminary support for OCaml 5.2
    - Some regressions in recovery and destruct are present.
    - Add a "heap_mbytes" field to Merlin server responses to report heap usage (#1717)
    - Add cache stats to telemetry (#1711)
    - Add new SyntaxDocument command to find information about the node under the cursor (#1706)
    - Fix `FLG -pp ppx.exe -as-pp/-dump-ast` use of invalid shell redirection when
    direct process launch on Windows. (#1723, fixes #1722)
    - Add a query_num field to the `ocamlmerlin` responses to detect server crashes (#1716)
    - Jump to cases within a match statement (#1726)
    - Jump to `module-type` (#1728, partially fixes #1656)
    - Exposes stable functions for configuration handling and pattern variable
      destruction. (#1730)
    - Add `signature-help` command (#1720)
  + editor modes
    - vim: load merlin under the ocamlinterface and ocamllex filetypes (#1340)
    - Fix merlinpp not using binary file open (#1725, fixes #1724)

merlin 4.13.1
=============
Fri Dec  1 15:00:42 CET 2023

  + merlin binary
    - Fix a follow-up issue to the preference of non-ghost nodes introduced in #1660 (#1690, fixes #1689)
    - Add `-cache-lifespan` flag, that sets cache invalidation period. (#1698,
      #1705)
    - Ignore the new 5.1 `cmi-file` flag instead of rejecting it (#1710, fixes
      #1703)
    - Fix Merlin locate not fallbacking on the correct file in case of ambiguity
      (@goldfirere, #1699)
    - Fix Merlin reporting errors provoked by the recovery itself (#1709, fixes
      #1704)
    - Add support for OCaml 5.1.1 (#1714)
  + editor modes
    - vim: load merlin when Vim is compiled with +python3/dyn (e.g. MacVim)
    - emacs: highlight only first error line by default (#1693, fixes #1663)

merlin 4.12
===========
Tue Sep 26 17:45:42 CEST 2023

  + merlin binary
    - Fix issue with let operators and tuples (#1684, fixes #1683, fixes
      ocaml/ocaml-lsp#1182)
    - Fix an issue causing Merlin locate queries to hang (#1686,
      fixes ocaml/ocaml-lsp#1192)

merlin 4.11
===========
Thu Sep 24 18:01:42 CEST 2023

  + merlin binary
    - Add support for OCaml 5.1
    - Improve error messages for missing configuration reader (#1669)
    - Fix regression causing crash when using ppxes under Windows (#1673)
    - Fix confusion between aliased modules and module types (#1676,
      fixes #1667)
    - Ignore hidden branches when listing occurrences (#1677, fixes #1671)
  + editor modes
    - emacs: fix/improve keybindings (#1668, fixes #1386):
      Unbind <kbd>C-c C-r</kbd> (to avoid shadowing `tuareg-eval-region`)
      and bind <kbd>C-c C-v</kbd> instead to `merlin-error-check`;
      rebind <kbd>C-c C-d</kbd> to `merlin-document`
      and bind <kbd>C-c M-d</kbd> and <kbd>C-c |</kbd> instead to `merlin-destruct`;
      bind <kbd>C-u C-c C-t</kbd> to `merlin-type-expr`.
      See also <https://github.com/ocaml/merlin/issues/1386#issuecomment-1701567716>
    - emacs: remove use of obsolete `defadvice` macro (#1675)

merlin 4.10
===========
Thu Aug 24 17:17:42 CEST 2023

  + merlin binary
    - Constrain socket path buffer size to avoid build warnings (#1631)
    - Handle concurrent server start (#1622)
    - Omit module prefixes for constructors and record fields in the
      `construct` command (#1618).  Prefixes are still produced when
      warning 42 (disambiguated name) is active.
    - Correctly invalidate PPX cache when pipeline ran partially (#1650,
      fixes #1647)
    - Prevent `short-path` from looping in some cases related to recursive type
      definitions (#1645)
    - Support parsing negative numbers in sexps (#1655)
    - Fix construct not working with inline records (#1658)
    - Improve behavior of `type-enclosing` on let/and operators (#1653)
    - Fix occurrences of extension constructors (#1662)
    - Improve node selection when ghosts are present (#1664, fixes #1660)
  + editor modes
    - emacs: call merlin-client-logger with "interrupted" if the
      merlin binary itself is interrupted, not just the parsing of the
      result (#1626).
    - emacs: merlin-construct, with a prefix argument, now includes
      local values in the completion options.  Alternatively, this
      behavior can be enabled permanently by customizing
      `merlin-construct-with-local-values` (#1644)
    - emacs: add support for opam-switch-mode (#1654, fixes #1591).
      See <https://github.com/ProofGeneral/opam-switch-mode>

merlin 4.9
==========
unreleased

  + merlin binary
    - Preview support for OCaml 5.1-alpha1. Short path is temporary disabled and
      inline records might not behave as expected.
    - Allow monadic IO in dot protocol (#1581)
    - Add a `scope` option to the `occurrences` command in preparation for
      the upcoming `project-wide-occurrences` feature (#1596)
    - Construct bool-typed holes as `false` instead of `true` in the
      `construct` command, for consistency (#1599).
    - Add a hook to configure system command for spawning ppxes when Merlin is
      used as a library. (#1585)
    - Implement an all-or-nothing cache for the PPX phase (#1584)
    - Cleanup functors caches when backtracking, to avoid memory leaks
      (#1609, fixes #1529 and ocaml-lsp#1032)
    - Fix `construct` results ordering for sum types sand poly variants (#1603)
    - Fix object method completion not working (#1606, fixes #1575)
    - Improve context detection for package types (#1608, fixes #1607)
    - Fix incorrect locations for string literals (#1574)
    - Fixed an issue that caused `errors` to erroneously alert about missing
      `cmi` files (#1577)
    - Prevent destruct from crashing on closed variant types (#1602,
      fixes #1601)
    - Improve longident parsing (#1612, fixes #945)
  + editor modes
    - emacs: call the user's configured completion UI in
      `merlin-construct` (#1598)
  + test suite
    - Add missing dependency to a test using ppxlib (#1583)
    - Add tests for the new PPX phase cache (#1584)
    - Add and update tests for `construct` ordering (#1603)

merlin 4.8
==========
Fri Feb 24 16:55:42 CEST 2023

  + merlin binary
    - Recognize OCaml 5.0 cmi magic number in compiler version mismatch message
      (#1554, fixes #1553)
    - Upgrade Merlin from the RC2 to the stable 5.0.0 compiler release (#1559,
      fixes #1558)
    - Improve type-enclosing behaviour when used on records' labels (#1565,
      fixes #1564)
    - Restore compatibility with the compiler's command line by accepting the
      `-safe-string` flag as a no-op instead of rejecting it (#1544, fixes
      #1518)
    - Traverse aliases when jumping to declaration. This matches
      jump-to-definition's behavior (#1563)
    - Improve locate's behavior in various ill-typed expressions (#1546, fixes
      #1567 and partially #1543)
    - Correctly traverse patterns when looking for docs in the typedtree (#1572)
    - Get documentation when the declaration or definition is selected (#1542,
      fixes #1540)
    - On Windows, change to a harmless directory when launching server to avoid
      locking down current directory (#1569, fixes #1474)
  + editor modes
    - emacs: Fix misuse of `eq` comparison (#1549, @mattiase)
    - emacs: xref works from context menus; better highlighting of xref matches;
      xref recognises operators and binding operators at the cursor position;
      bad locations are filtered out (#1385, fixes #1410, @mattiase)
  + test suite
    - Add multiple tests for locate over ill-typed expressions (#1546)
    - Add non-regression tests for other fixes in this release
    - Add a test for incorrect alert defaults (#1559)

merlin 4.7.1
============
Thu Dec 13 11:49:42 CEST 2022

  + merlin binary
    - Restore compatibility with the compiler's command line by accepting
      the `-safe-string` flag as a no-op instead of rejecting it. (#1544,
      fixes #1518)
    - Mark some C variables as unused to remove warnings (#1541, @antalsz)

merlin 4.7
==========
Thu Nov 24 17:49:42 CEST 2022

  + merlin binary
    - Replace custom "holes" AST nodes by extensions. This restores binary
      compatibility and fixes issues with PPXs when using typed-holes.
      (#1503)
    - Do not change temporarily Merlin's cwd when starting a PPX (#1521,
      fixes #1420)
    - Fix a parsing issue when declaring the `(??)` custom prefix operator.
      (#1507, fixes #1506)
    - Fix variant constructors' comments grouping (#1516, @mheiber, fixes #1513)
    - Filter-out duplicates from the `enclosing` command result (#1512)
    - Add a new `verbosity=smart` mode for type enclosing that only expand
      modules' types (#1374, @ulugbekna)
    - Improve locate for labels' declarations in the current buffer.
      (#1505, fixes #1524)
    - Fix locate on module without implementation (#1522, fixes #1519)
    - Allow program name customization when merlin is used as a library. (#1532)
  + editor modes
    - vim: load the plugin when necessary if it wasn't loaded before (#1511)
    - emacs: xref works from context menus; better highlighting of xref
      matches; xref recognises operators and binding operators at the
      cursor position; bad locations are filtered out (#1385, fixes
      #1410)
    - emacs: update CI for newer releases and fix some warnings (#1454,
      @mattiase)
  + test suite
    - Add tests for constructors' documentation (#1511)
    - Add test cases for label comment documentation (#1526, @mheiber)
    - Add a test for the `enclosing` command (#1512)
    - Add tests for interactions between locate and record labels (#1505)
    - Add test showing an issue with locate and implicit transitive deps

merlin 4.6
==========
Thu Jun 30 14:51:42 CEST 2022

  + merlin binary
    - make most library public and split merlin in two packages: the
      `merlin-lib` package that exposes merlin's internals and the `merlin`
      package with the frontend. (#1448, #1455, #1457, #1497, @rgrinberg,
      @tmattio, @kit-ty-kate)
    - Type printing: use best_module_path for paths from Mty_alias (#1470)
    - Attempt at finding the 'real' capitalization of files on windows (#1462 by
      @mlasson)
    - Use newer `Seq`-based API of Yojson 2.0, avoiding the need for the
      deprecated `Stream` module (#1475 by @Leonidas-from-XIV)
    - unify parsing of `MERLIN_LOG` (#1480 by @ulugbekna)
    - Fix type deduplication in `type-enclosing` results (#1483, fixes #1477)
    - Only weakly reduce the shapes to speed up the new Merlin locate
      implementation. (#1488)
    - Ignore unknown configuration tags from dune configuration provider but not
      from dot-merlin-reader (#1486)
    - typing recovery: recover at the granularity of `core_type` (#1484)
  + editor modes
    - add method imenu items for emacs (#1481, @mndrix)
    - emacs: Make the prefix argument to `merlin-locate` optional, both for
      consistency with Emacs convention and for backwards compatibility. (#1476,
      @antalsz)
    - emacs: fix duplicated prefix path in imenu entries (#1495, @bcc32)

merlin 4.5
==========
Tue Apr  5 20:51:42 CEST 2022

  + merlin binary
    - don't reset the environment when running merlin in single mode so that the
      parent environment is forwarded the the child processes (#1425)
    - filter dups in source paths (#1218)
    - improve load path performance (#1323)
    - fix handlink of ppx's under Windows (#1413)
    - locate: look for original source files before looking for preprocessed
      files (#1219 by @ddickstein, fixes #894)
    - handle `=` syntax in compiler flags (#1409)
    - expose all destruct exceptions in the api (#1437)
    - fix superfluous break in error reporting (#1432)
    - recognise binding operators in locate and occurrences (#1398, @mattiase)
    - remove dependency on Result (#1441, @kit-ty-kate)
    - use the new "shapes" generated by the compiler to perform precise
      jump-to-definition (#1431)
  + editor modes
    - fix an issue in Neovim where the current line jumps to the top of the
      window on repeated calls to `MerlinTypeOf` (#1433 by @ddickstein, fixes
      #1221)
    - add module, module type, and class imenu items for emacs (#1244, @ivg)
    - add prefix argument to force or prevent opening in a new buffer in locate
      command (#1426, @panglesd)
    - add type-on-hover functionality for vim (#1439, @nilsbecker)
    - add a dedicated buffer `*merlin-errors*` containing the last viewed error
      (#1414, @panglesd)
  + test suite
    - make `merlin-wrapper` create a default `.merlin` file  only when there is
      no `dune-project` to let tests use `dune ocaml-merlin` reader. (#1425)
    - cover locate calls on module aliases with and without dune
    - Add a test expliciting the interaction between locate and Dune's generated
      source files (#1444)

merlin 4.4
==========
Mon Jul 26 11:12:21 PM CET 2021

  + ocaml support
    - add support for 4.13
    - stopped actively supporting version older than 4.12
  + merlin binary
    - Mbrowse.select_leaf: correctly ignore merlin.hide (#1376)
    - enable `occurences` to work when looking for locally abstract types
      (#1382)
    - handle `-alert` compiler flag (#1401)
    - avoid a race condition when the process started to read a configuration
      file crashes/is not found (#1378, @antalsz)
    - log the backtrace even when the exception is a Failure (#1377, @antalsz)
    - ignore `-error-style` compiler flag (#1402, @nojb)
    - fix handling of record field expressions (#1375)
    - allow -pp to return an AST (#1394)
    - fix merlin crashing due to short-paths (#1334, fixes #1322)
  + editor modes
    - update quick setup instructions for emacs (#1380, @ScriptDevil)
  + test suite
    - improve record field destruction testing (#1375)

merlin 4.3.1
============
Mon Jul 26 04:45:37 PM CET 2021

  + merlin binary
    - recover ill-typed patterns (#1317, #1342)
    - more accurate type-enclosing for methods (#1328, fixes #1124)
    - fix location of patterns in Occurrences (#1324, fixes ocaml/ocaml-lsp#375)
    - fix location of module definitions done via functors (#1329, fixes #1199)
    - fix -cmt-path dirs mistakenly added to build path (#1330)
    - add new module holes that can replace module expressions (#1333)
    - add a new command `construct` that builds a list of possible terms when
      called on a typed hole (#1318)
    - `refactor-open` improvements (#1313, #1314, #1366, #1372)
      - do not make paths absolute, simply prefix with the identifier under
      the cursor
        ```ocaml
        open Foo (* calling refactor-open qualify on this open *)
        let _ = Foo.bar (* previously could result in [Dune__exe.Foo.bar] *)
        ```
      - do not return identical (duplicate) edits
      - do not return unnecessary edits that when applied do not change
        the document
      - handle record fields properly
      - handle multi-line paths
      - `unqualify` should not qualify
    - Handle `Persistent_env.Error` in `Typemod.initial_env` (#1355)
    - locate: reset global state from all entry points (#1364)
    - Windows: replace user name by its SID in socketnames (#1345, @ttamttam)
  + editor modes
    - vim: add a simple interface to the new `construct` command:
      `MerlinConstruct`. When several results are suggested, `<c-i>`
      and `<c-u>` can be use to change the depth of the recursive
      construction. (#1318)
    - vim: add support for the `merlin-locate-type` command:
      `MerlinLocateType` (#1359)
    - emacs: add a simple interface to the new `construct` command:
      `merlin-construct`. (#1352)
    - emacs: add support for the `merlin-locate-type` command. (#1359)
    - emacs: fix issue with `merlin--highlight` and  various minor improvements
        (#1367, @mattiase)
  + test suite
    - cover the new `construct` command (#1318)
    - disable tests failing in Opam's CI due to nested dune projects (#1373)

merlin 4.2
==========
Tue Apr 12 11:44:22 AM CET 2021

  + merlin binary
    - external configuration reading:
      + use relative paths to communicate with Dune when possible. This solves
        issues related to symlinks on Unix and improve Windows support (#1271,
        fixes #1288)
      + make the `workdir` configuration value when using the
        `dune ocaml-merlin` configuration provider the same as when using
        `dot-merlin-reader` so that ppxes behaves in the same way as before
        (#1284, fixes ocaml/dune#4479, discussion in #1292)
    - destruct:
      + improve prefixing of generated constructors in Destruct by filtering
        opened modules (#1277)
      + make the destruct command more resilient to ill-typed expressions and
        when called without nodes (#1304, fixes #1300)
    - reintroduce some record recovery and improve completion (#1276)
    - introduce a new AST node for holes (`_`), allow correct typing of these
      holes and add a new `holes` command that returns the locations of all
      holes in the current file along with their types (#1242, #1289)
    - Mppx: don't restore cookies after invocation. Ppx are invoked only once
      so there is no need to manage cookies. This small change should increase
      performance and should not change any other behavior (#1309)
    - Windows: system command variant: do not open a window console when
      launching a ppx (#1270, fixes #714)
    - fix same file documentation bug (#1265 by @ulugbekna, fixes #1261)
  + editor modes
    - vim: Add `MerlinNextHole` and `MerlinPreviousHole` commands to navigate
      between holes. Jump to the first hole after destruct (#1287, #1303)
    - emacs: Add `merlin-next-hole` and `merlin-previous-hole` commands to
      navigate holes. Jump to the first hole after calling destruct. (#1291)
    - emacs: modernization of the elisp code and conformance with coding
      guidelines (#1247, #1310 by Steve Purcell )
    - vim & emacs : new client-side "merlin use package" commands, restoring
      previous behavior (#1272, fixes #1191)
  + test suite
    - cover constructor disambiguation and record fields (#1276)
    - cover the new `holes` command and AST node (#1242, #1289)
    - cover the document fix (#1265, #1315)

merlin 4.1
==========
Tue Feb 16 10:33:11 AM CET 2021

  + merlin binary:
    - fix windows paths canonicalization (#1254)
    - fix hanging on windows (#1256, #1263)

merlin 4.0
==========
Tue Feb  2 03:13:37 PM CET 2021

  + ocaml support
    Detailed list of changes on
    https://tarides.com/blog/2021-01-26-recent-and-upcoming-changes-to-merlin#dropping-support-for-old-versions-of-ocaml
    Summary:
    - any revision of Merlin now only supports one version of OCaml. Support for
      other versions will be found in other branches
    - stopped actively supporting version older than 4.11
    - add support for 4.12
  + merlin binary
    - add keyword completion (disabled by default) (#1243)
    - fix a bug which caused type-enclosing to sometimes look at an incorrect
      node (#1232, fixes #1226)
    - properly report leaked parsing error (#1223, fixes #1222)
    - wrap `merlin_analysis` and `merlin_utils` library
  + editor modes
    - emacs: add missing mandatory argument for define-obsolete-function-alias
      (#1250, by Atharva Shukla, fixes #1234)
    - emacs: use "opam var" instead of "opam config var" (#1249, by Raja Boujbel)
    - vim: fix CursorMoved semantics (#1213, by @ddickstein)
    - vim: add :MerlinLocateImpl and :MerlinLocateIntf (#1208 by Matthew Ryan)
  + test suite
    - replace mdx usage by dune's cram mechanism

merlin 3.4.2
============
Fri Nov 13 12:16:42 CEST 2020

  + merlin binary
    - simplify local store implementation and API (#1188, #1184)
    - fix a destruct issue allowing ill-typed match completions (#1194)

merlin 3.4.1
============
Thu Oct  1 15:31:42 CEST 2020

  + dot-merlin-reader
    - fix issue when multiple packages with pxxes are declared in the
    configuration. (#1181, fixes #1179)

merlin 3.4.0
============
Wed Sep 16 15:00:42 CEST 2020

  + merlin binary
    - fix completion of pattern matchings with exception patterns (#1169)
    - delegate configuration reading to external programs via a simple protocol
    and create a new package `dot-merlin-reader` with a binary that reads
    `.merlin` files. (#1123, #1152)

merlin 3.3.8
============
Thu Aug 27 14:48:42 CEST 2020

  + merlin binary
    - dune: restore compatibility with dune 1.8.0 (#1157, #1153)

merlin 3.3.7
============
Tue Aug 25 15:13:42 CEST 2020

  + ocaml support
    - full support from OCaml 4.02 to OCaml 4.11 (#1153)

merlin 3.3.6
============
Fri Jun 12 10:51:42 CEST 2020

  + merlin binary
    - dune: remove duplicated rules for profile=release (#1143)
  + test suite
    - fix a test that required Dune 2.5 (#1146)
    - fix another test that lacked reproducibility (#1146)

merlin 3.3.5
============
Tue Jun  9 15:13:42 CEST 2020

  + ocaml support
    - alerts are no-more ignored and are reported as warnings (#1138)
  + merlin binary
    - fix completion of names containing `-` (#1142)
    - fix several type-enclosing bugs by performing context-analysis (#1108)
    - lsp: add deprecation flag to outline items (#1087)
    - lsp: add go-to typedef (`Locate_type`) (#1067)

merlin 3.3.4
============
Tue Apr 14 15:25:05 CEST 2020

  + ocaml support
    - full support from OCaml 4.02 to OCaml 4.10 (#1117, #1127)
    - fix desynchronized cache (#1120)
    - short path for OCaml 4.09 and OCaml 4.10 (#1082, #1117)
    - catch and test environment initialization errors (#1083, #1130)
    - restore type levels after recovery (#1092)
  + merlin binary
    - fix syntax errors in 4.08 and 4.09 (#1081)
    - complete-prefix command accepts -kind option to filter results (#1071)
    - code cleanup (#1093, #1079, #1112)
    - better handling of expression and pattern extra nodes during browse tree
      traversal (#1091, #1121)
    - improve context detection (e.g. appropriate namespace for lookup) for
      various queries (#1104, #1110)
    - add stdlib to locate source path (#1085)
  + editor modes
    - vim: tweak heuristic to select python version (#1111)
    - emacs: marlin/call
    - lsp: move server to its own repository (#1069),
      https://github.com/ocaml/ocaml-lsp
  + test suite
    - dune rules for the test suite are now generated, deterministic and
      can be run individually (#1068, #1070, #1072)
    - fix incorrect command-line arguments in tests (#1073)
    - better coverage of frontend features (#1075, #1078, #1088, #1089, #1126)

Build no longer relies on implicit transitive_deps (#1065).

merlin 3.3.4~4.10preview1
=========================
Mon Mar  2 14:26:32 CET 2020

This is a preview release that adds support for OCaml 4.10.
Short-path is disabled.  Other versions of OCaml are not supported.

merlin 3.3.3
============
Fri Nov 29 17:35:58 CET 2019

  + backend
    - support OCaml 4.09 (#1055)
    - fix parse errors in 4.08 (#1037)
    - update 4.08 support to OCaml 4.08.1 (#1053)
    - support `without_cmis`
    - separate reading from caching in file-cache, use caching in
      `Env.check_state_consistency` (#1044)
    - simplify compiler state management (#1056, #1059)
    - fix creation of initial environment, improve compatibility with
      upstream 4.08 (#1052)
  + frontend
    - code re-organization (#1042)
    - error command: select which kind of errors to show (#995)
    - print value types in outline (#1014)
    - fix process handling in windows (#1005)
  + editor modes
    - emacs
      + bugfixes in merlin-imenu, merlin-xref (#1000, #1021, #1001)
      + show types in merlin-imenu (#1013)
      + reset buffer local configurations when resetting server (#1004)
      + remove merlin-use-tuareg-imenu
      + fix stack overflow (#1024)
      + fix merlin-occurrence (#1043)
    - vim
      + display warn-error warnings as errors (#1009)
  + testsuite
    - cover file-cache and `check_state_consistency` (#1044)
    - check inconsistent assumptions, test server versus single modes (#1047)

merlin 3.3.2
============
Mon Jul 15 11:10:35 CEST 2019

  + backend
    - `**` globbing in .merlin won't look into hidden directories
      (starting with a '.') (by Daniel Bünzli, #990)
    - fallback to "/dev/null" configuration for findlib
    - better 4.08 support:
      + support for letop (let+, and+, ...) (#986)
      + fix parsing recovery for 4.08 constructions (#987)
      + catch an exception raised by 4.08 Printtyp trying to rename a
        persistent identifier (#991)
    - locate: treat local locations differently from external locations (coming
      from a cmi), this fixes "jump to definition" on mutually recursive
      bindings (#984)
    - when completing an infix operator in a sub-module, wrap with () (#992)
    - disable arity checks on externals (for Bucklescript compatibility)
    - remove parser preprocessing (simplify compilation for OCaml < 4.08) (#997)
  + editor modes
    - emacs
      + fix position computation in presence of tabs or multi-byte characters (#981)
      + log arguments in "merlin-debug-last-commands" (#981)
    - vim
      + install reason.vim file (by Hezekiah M. Carty, #974)

merlin 3.3.1
============
Mon Jun 17 17:13:33 CEST 2019

  + backend
    - catch findlib initialization failures and keep going on

merlin 3.3.0
============
Fri May 31 11:09:08 BST 2019

  + backend
    - interpret `-pp` flag
    - backtrack warnings in all versions, not just 4.06
    - silence C compiler warnings (by David Allsopp and Bernhard Schommer)
    - remove sturgeon support
    - allow to select sections to log
    - better error message on ocaml version mismatch
    - locate:
      + handle functors and functor applications
      + do not use the location coming from the environment
    - tweaked caching policy
    - fix environment when a file disappears
    - fix -short-paths handling of classes and class types (by Leo White)
    - don't select deprecated paths in -short-paths (by Leo White)
    - return type info in outline query (by Andrey Popp)
    - properly handle new lines in the lexer
    - better tracking of errors reported by the parser and by preprocessors
    - add support for OCaml 4.08
    - tweaked the recovery strategy in presence of syntax errors
    - timing information in replies now includes wall clock time.
    - dump command can new dump the parsetree post preprocessing

  + editors modes
    - emacs
      + fix merlin-xref.el install (by Emilio Jesus Gallego Arias)
      + keep labels matching the prefix the user has typed rather than
        dropping them (by Mitchell Plamann)
      + remove unused `merlin--overlay` function (by Wilfred Hughes)
      + show the number of errors in the modline (by Wilfred Hughes)
      + call a logger on the client side if one is defined
      + allow user to disable completion inside comments and strings
      + show errors and types even when buffer is narrowed (by Wilfred Hughes)
      + make sure PATH is updated when merlin-command is 'opam

    - vim
      + better FindBinary
      + make the log buffer a scratch buffer (by Tom Johnson)
      + execute buffer switching silently (by Fabian)
      + restore view after updating merlin type buffer (by Fabian)

  + testsuite
    - Switched to mdx with cram syntax.

Special thanks to Rudi Grinberg for helping us in reviewing and merging
pull-requests.

merlin 3.2.2
============
Tue Oct  9 11:25:12 BST 2018

Update cmt magic number for 4.07.1

merlin 3.2.1
============
Mon Oct  8 11:44:16 BST 2018

Fix build on OCaml 4.02 to 4.04

merlin 3.2.0
============
Mon Oct  8 10:41:24 BST 2018

Switched build to dune (thanks to @nojb).
Added support for 4.07.1
Various bugfixes in the backend and in the editor modes.


merlin 3.1.0
============
Wed Jun 20 14:05:04 BST 2018

  + backend
    - new "polarity search" feature: provides a Hoogle-like type-based search
      for libraries that are in merlin's scope.
      See https://github.com/ocaml/merlin/blob/master/doc/features.md#polarity-search
    - new "open refactoring" feature: helps cleaning the code in the scope of an
      open statement.
      See https://github.com/ocaml/merlin/blob/master/doc/features.md#open-refactoring
    - spell-checking: a simple spell-checker has been added to suggest
      corrections when nothing can be directly completed.
    - type-driven record completion: merlin will now make use of type
      information from the context for narrowing and refining completion
      candidates.
    - support for `#require` directive in a source file, and will treat it as a
      package use
    - Add support for OCaml 4.07
    - locate: various minor bugfixes, as well as the following general
      improvements:
      + improved context detection
      + better tracking of namespaces
      + fixed support for local bindings
      + fixed support of disambiguated record fields and variant constructors
      + improved support for functors: merlin will now jump through functor
        application to the functor definitions and in some cases go back to the
        argument that was given (if it is simply reexported).
    - backport fixes of OCaml 4.06.1 to the 4.06 backend
    - various minor bugfixes

  + editor modes
    - emacs
      + proper handling of multibyte strings (by @Chris00)
      + bind "q" to close type buffer (by @MiloDavis)
      + make goto-point encoding independent
      + add reason-mode to the guessed favorite mode list (by @Khady)
      + sped up some tight loops (by @rgrinberg)
      + add support for x-ref backend (by @rgrinberg)
    - vim
      + fix support for Neomake (by @bobbypriambodo and @statianzo)
      + fix encoding issues in filepaths (by @Thelyria)
      + fix handling of enclosing-type cache (by @ELLIOTTCABLE)
      + add <silent> to prevent flashing when highlighting an enclosing (by @bluddy)

Thanks to the people who contributed to this release: ELLIOTTCABLE, Louis Roché,
Rudi Grinberg, Yotam Barnoy, Leo White, Daniel Below, Andreas Hauptmann,
Christophe Troestler, Bobby Priambodo, Milo Davis.

merlin 3.0.5
============
Mon Nov 13 18:30:02 CET 2017

Fix magic numbers for 4.06 (issue #749, reported by @Fourchaux).

merlin 3.0.4
============
Sun Nov 12 10:14:03 CET 2017

Add support for 4.06.
Use Leo White's short-path for 4.05.
Various bug fixes (in locate, in emacs serialization).

merlin 3.0.3
============
Mon Oct  2 12:56:23 CEST 2017

The major change Windows support is contributed by David Allsopp.

Other changes are a bunch of fixes:
- compilation on FreeBSD contributed by Malcolm Matalka
- improvement to emacs mode contributed by Olivier Andrieu, Christophe
  Troestler and Steve Purcell
- improvement to vim mode by Fabian Hemmer and Gregory Nisbet
- fixes to ppx invocation by Keigo Imai
- fixes to Merlin s-expr dialect to bring UTF-8 compatibility with Emacs (WIP)

merlin 3.0.2
============
Wed Aug  2 15:09:07 CEST 2017

Bug fix after 3.0.1:
- CMT magic number for 4.05 was wrong
- handle merlin.focus, merlin.ignore, merlin.loc/merlin.relaxed-loc and merlin.syntax-error
- missing include preventing build on some platforms contributed by Bernhard Schommer

merlin 3.0.1
============
Wed Jul 26 18:25:23 CEST 2017

Bug fix release after 3.0.0 major release:
- portability fixes by David Allsop in configure script and vim mode
  (tough Windows support is not ready yet)
- preliminary support for findlib toolchains with FINDLIB_TOOLCHAIN .merlin
  directive
- make ocamlmerlin.c frontend more portable
- various fixes to the frontend

merlin 3.0.0
============
Mon Jul 24 11:21:58 CEST 2017

The major change is a new protocol that moves process management inside Merlin
codebase, saving a lot of pain in Emacs and Vim. There are not much new user
facing features.

Windows support is not yet available.

In editor configuration is now done with merlin-flags, merlin-extensions and
merlin-use in Emacs and :MerlinFlags, :MerlinExtensions and :MerlinPackages in
Vim.
In previous versions, enabled extensions, flags and packages were retained
while now only the last command is remembered.

"M-x merlin-use a", "M-x merlin-use b" should be replaced by "M-x merlin-use a,b".
":MerlinUse a", ":MerlinUse b" should be replaced by ":MerlinUse a b".

The old protocol is still supported, so existing editor modes should not be
affected (tested with Atom, Visual Studio and Sublime-text).

Other main changes:
- Support for OCaml 4.05 was added
- Merlin uses a new implementation of short-path by Leo White which addresses
  performance problems
- Merlin now works with the upstream version of Menhir
- numerous cleanup and refactoring to decrease the amount of changes to
  upstream typechecker
- emacs-imenu feature was contributed by tddsg. It is similar the "outline"
  feature in vim for navigating in a buffer.

Thanks to the many contributors (Jochen Bartl, tddsg, Ximin Luo, Jason Staten,
Leo White, Leandro Ostera, Jacob Bass, Xavier Guérin, Yotam Barnoy, Jacques
Pascal Deplaix, David Allsopp, ...).

merlin 2.5.5
============
Wed Jun 14 14:54:32 CEST 2017

Minor release:
- fix flag parsing in .merlin (#661)

merlin 2.5.4
============
Tue Apr 25 15:07:18 CEST 2017

Minor release:
- handle hole in 4.04
- bug fixes in emacs mode
- introduce merlin-imenu

merlin 2.5.3
============
Mon Nov 28 09:54:57 CET 2016

Minor release:
- fix Windows build with MSVC (#605).
- fix module level errors escaping

merlin 2.5.2
============
Wed Nov 16 14:44:19 CET 2016

This release mainly brings support for OCaml 4.04.
Internal code was simplified and bugs were fixed in the meantime (cache
invalidation, ast traversal, type error recovery, certain cases of completion,
ppx working directory, locate, ...).

merlin 2.5.1
============
Tue Oct 18 12:04:19 CEST 2016

Bug fix release before major version.

  - reintroduce lazy substitution to fix performance issue
  - add "FINDLIB_PATH" directive to .merlin (contributed by Gerd Stolpmann)
  - relax arity checks on externals (harmless, requested by Hongbo Zang)
  - handle case insensitivity of OS X (fix longstanding bug)
  - fix build under Cygwin
  - minor cleanup, portability and usability improvements in build system and
    editor modes

merlin 2.5.0
============
Mon Jun 13 22:26:33 CEST 2016

  + frontend:
    - now all commands can take a context, this reduce the amount of state
      in the command interpreter. Long term goal is to make protocol stateless
    - merlin now supports customizable "readers": processes responsible for
      parsing and pretty-printing. Main use-case is Reason, cppo/optcomp support
      might be added later
  + backend:
    - drop support for 4.00 / 4.01
    - support for 4.03 has been added
    - new implementation of type recovery, should diverge less from upstream
    - support for 4.02 was reimplemented to use the same design
    - menhir's fork has been synchronized with upstream, recovery algorithm
      is completely new
  + vim: add support for python3, update to new protocol
  + emacs: update to new protocol, bug fixes

merlin 2.3.1
============
Wed Nov 25 15:01:47 CET 2015

Bug fix release, fix builds under Mac OS X and Windows.

  + backend:
    - improve support for module aliases in completion, locate and short-path
    - change management of flags
    - Cuillère ou Dorade
    - fix grammar for 4.02.3, support attributes on core_types

  + emacs & vim: minor fixes

merlin 2.3
==========
Wed Oct 28 14:32:48 CET 2015

  + backend:
    - locate: fix assert failure on first class modules inclusion
    - outline: add support for classes and object types
    - nonrec: enable by default for OCaml >= 4.02.2
    - error reporting: less aggressive filtering on ghost locs
    - finer-grained tracking of usage (values, opened modules, etc)
    - significant improvement in the handling of PPX extensions:
        + fix shell commandline and working directory
        + normalize parsetree locations
        + implement caching of intermediate rewriting
    - merged support for MetaOCaml
    - path to the standard library can now be specified with STDLIB command
      in .merlin
    - BrowseT: split into Browse_node (OCaml version specific) and
      Merlin_browse, extract recursion scheme
    - add Jump command, contributed by Tomasz Kołodziejski
    - contextual-commands: optionnally specify the context (file, project)
      in which each command is interpreted
    - better support for trunk
    - many bugfixes

  + documentation:
    - update ARCHITECTURE and PROTOCOL documentations

  + emacs:
    - make use of contextual-commands, non backward compatible protocol change
    - new merlin-set-flags command
    - split into multiple files
    - cleanup symbol namespaces:
        + merlin- for user targeted definitions
        + merlin-- for internal definitions,
        + merlin/ for API definitions
    - usability tweaks, notably on error display and navigation
    - general cleanup and bugfixes

  + vim:
    - expose custom .merlin loading through buffer variable
    - cleanup and bugfixes, notably process liveness check and restart

This release also contains contributions from: Rudi Grinberg, Fourchaux,
Christopher Reichert, David Allsopp, Nick Borden, Mario Rodas, @Twinside,
Pierre Chambart, Philipp Haselwarter, Tomasz Kołodziejski and Syohei Yoshida.

merlin 2.2
==========
Wed May 20 09:44:55 EDT 2015

  + backend:
    - completion
        + return the type of the expected argument when completing an
          application.
          This allows us to offer completion for named and optional parameters,
          as well as polymorphic variants
        + optionally associates ocamldoc comments to candidates
        + adds field completion inside records ( #296 )
    - locate:
        + partially rewritten, introduces a new kind of cache
          (so potentially noticeably bigger memory consumption)
        + better handling of functors
        + handle local modules
        + fix occasional "inconsistent assumptions"
    - error reporting:
        + handle environment errors (inconsistent assumptions, …)
        + filter duplicated messages
        + fix type error reporting:
            "this expression has type t = t but an expression was expected of
            type u = u"
          we now only print the equality when it adds some information
        + less noisy pattern recovery: when every pattern is recovered, consider
          that the matched expression is the the source of the problem, and
          retry typing with "'a" as the type of the matched expression.
    - add support for trunk
    - add a "document" command: takes an ident and return its documentation (if
      any)
    - destruct: use more precise environments ( #389 )
    - warnings:
        + check signature inclusion to prevent spurious warnings about unused
          declarations
        + backport 4.02-style warning management
        + add a dump command
    - nonrec: update implementation to more-or-less match the upstream one
      (upstream >= 4.02.2)
    - parser: improve marking heuristic in presence of ;; or toplevel
      directives.
    - typeof: during verbose expansion, also print the type declaration if we
      have a type constructor

  + emacs:
    - fix bindings of every completion backend
    - bind ocamldoc comments to company (optional)
    - detect race conditions when running synchronous commands
    - cleanup "merlin-process-started-p"
    - locate error messages were silently ignored, they are now printed
    - drop text properties from commands sent to merlin (pull request #383 by
      milanst)
    - Tell merlin the content of the buffer when opening a new buffer.
      This allows merlin idle-job to preload content if nothing else is
      requested.
    - remove call to merlin from the lighter

  + vim:
    - fix ctrlp binding for locate
    - add (dwim) completion on :TypeOf
    - while completing, candidates documentation can be displayed in the
      "preview" window
    - prefix every command name by "Merlin" ( #379 )
    - Tell merlin the content of the buffer when opening a new buffer.
      This allows merlin idle-job to preload content if nothing else is
      requested.

merlin 2.1.2
============
Tue Mar  3 12:20:08 UTC 2015

  Main new feature is a faster short-path, and also a lot of buxfixes.

  + backend:
    - merge new implementation of short-path
    - infrastructure for doing background computations
    - fix exhaustivity checking of GADTs
    - fix Typecore error reporting in 4.00.1 & 4.01.0
    - delayed checks are now enabled (e.g warnings)
    - special handling of "myocamlbuild.ml" (issue #363)
    - better sharing/caching of global modules
    - more customizable .merlin loading
    - minor fixes (better error messages, typos, "fake" extensions)

  + build system:
    - allow bytecode builds, support OpenBSD (pull request #364 by madroach)
    - Refuse/Resist... environment variables redefinition

  + vim:
    - fix charset/encoding detection (pull request #352 by rgrinberg)
    - minor fixes and simplification

  + emacs:
    - better integration with emacs error management and asynchronous handling
    - expose custom .merlin loading in merlin-grouping-function
    - fixes, printf-debugging cleanup

merlin 2.1.1
============
Wed Jan 28 08:59:20 GMT 2015

  + backend:
    - locate: merlin refused to locate things when it had no context (happens
      when the buffer didn't parse for example) claiming it was at the
      "definition point". Fixed.
    - locate: use the cmt path when no ml file was found in the source path
      (this might not be such a good idea, the cases when this work are the ones
      where the user configuration is wrong...)
    - destruct: qualify introduced constructors
    - destruct: eliminate "impossible" GADT branches
    - parser: handle '%' as an operator for 4.00 and 4.01 ( #345 )

  + fake:
    - add typerep support
    - never generate `'_` type variables.

  + vim:
    - show duplicated outlines in CtrlP
    - sort outlines by name length in CtrlP
    - when split method is set to 'tab' *always* open a new tab.

merlin 2.1
===========
Sun Jan 11 22:20:23 CET 2015

  + backend:
    - add PPX support
    - make use of context before locating (#308, #316, #318).
    - generate match patterns for arbitrary expressions and missing patterns
      for incomplete matchings (#123).
    - reintroduce type expansion (asking the times of the same expression
      several times will resolve type aliases).
    - "smart" (a.k.a "do what I mean") completion:
          `L.m` will expand to `List.map ; List.m... ; ListLabels.map ; ...` if
          `L` doesn't exist.
    - simplify incremental parser and typer interfaces
    - locate: better handling of packed modules (supposedly)
    - more precise recovery on patterns (before the recovery was done at the
      expression level, so the whole match was discarded, now only the pattern
      is)

  + emacs:
    - don't use fringe in emacs23 (broken)
    - remove obsolete aliases:
        merlin-occurences => merlin-occurrences
        merlin-to-end => merlin-error-check
    - disable merlin-mode on type buffer
    - require caml-types (needed for highlighting) (#331).

  + misc:
    - update README (#301).

  + vim:
    - add a type history buffer (#313, #322) -- only available for vim > 7.3.
    - highlight types when displaying them in the command line -- only for vim >
      7.3
    - add tab completion for the argument of the `:Locate` command
    - add support for text objects based on type enclosing
    - introduce an interactive version of `:Rename`
    - locate results can now be shown in a new or existing tab (#335)
    - use `fileencoding` where necessary (#332)
    - ctrlp bindings for outline and locate

This release also contains contributions from: Geoff Gole, Rudi Grinberg, Markus
Mottl, Roman Vorobets and Arthur Wendling.

merlin 2.0
==========
Fri Oct 31 11:04:21 CET 2014

This is a major release which we worked on for several months, rewriting many
parts of the codebase. An exhaustive list of changes is therefore impossible to
give, but here are some key points (from an user perspective):

  - support for OCaml 4.02.{0,1}
  - more precise recovery in presence of syntax errors
  - more user-friendly messages for syntax errors
  - locate now works on MLI files
  - automatic reloading of .merlin files (when they are update or created), it
    is no longer necessary to restart merlin
  - introduced a small refactoring command: rename, who renames all occurrences
    of an identifier. See: http://yawdp.com/~def/rename.webm


This release also contains contributions from: Yotam Barnoy, Jacques-Pascal
Deplaix, Geoff Gole, Rudi Grinberg, Steve Purcell and Jan Rehders.

merlin 1.7.1
============
Fri Aug 22 10:01:58 CEST 2014

Minor update to installation procedure

merlin 1.7
==========
Mon Aug 18 17:08:00 BST 2014

This release also marks the apparition of a proper opam install script.

  + backend:
    - fixes on locate
    - print manifests even when -short-paths is set
    - add an "occurrences" command to list every occurrence of an identifier ( #156 )
    - new "version" command ( #180 )
    - add CPU time to log files ( #192 )
    - better error reporting from locate ( #190 )

  + documentation:
    - update vim doc file ( #204 )
    - typo correction in the README by Philippe Wang ( #195 )

  + emacs:
    - fix most byte compilation warnings, by Geoff Gole ( #209 )
    - numerous fixes

  + vim:
    - add error list independent from syntastic
    - fix completion for vim<=703 (#223)

merlin 1.6
==========
Tue Mar 11 14:33:55 CET 2014

  + backend:
    - small memory leak fix
    - major improvements and bugfixes for locate (i.e. "jump to definition")

  + emacs:
    - fixed bug preventing merlin restart ( #167 )
    - removed keybindings reserved to users ( #170 )
      the full list is:
        + `C-c l` previously bound to `merlin-use`
        + `C-c r` previously bound to `merlin-restart-process`
        + `C-c t` previously bound to `merlin-type-expr`
    - removed keybindings on `C-<up>` and `C-<down>` as these already have a
      meaning in emacs ( #129 )
      They were bound to `merlin-type-enclosing-go-up` and
      `merlin-type-enclosing-go-down` respectively.
    - the emacs mode is now compiled (contribution from Jacques-Pascal Deplaix
      #158 , with a follow up from Rudy Grinberg #165 )
    - improved efficiency of completion at point

  + extensions:
    - added support for variantslib ( #132 )
    - updated fieldslib support ( #169 , #185 )
    - fix pa_lwt translation ( #182 )
    - added support for pa_enumerate ( #187 )

  + vim:
    - the split method for locate can now be configured

merlin 1.5
==========
Sat Dec 14 19:45:06 CET 2013

  + backend:
    - better handling of paths (both sources and build)
    - split build path into cmi and cmt path.
      New directives "CMI" and "CMT" are now available in .merlin files ("B"
      still works as previously)
    - doesn't get confused anymore when the user switch between buffers (the
      state is cleaned)

  + emacs:
    - adds ability to enable/disable extensions manually
    - adds a command to clear all the errors from a buffer
    - displaying of errors can now be disabled

  + extensions
    - updated bin_prot for version >= 109.45.00
    - bugfix for [with compare] in presence of parametrized types
    - added support for "here" (when activated adds
      [val _here_ : Lexing.position])
    - added support for [assert_lwt]
    - fixed typing of [while_lwt]

  + vim:
    - vim plugin can be installed into a custom directory and has its own
      makefile target (contribution from Vsevolod Velichko)
    - added "ClearEnclosing" command to remove merlin's overlay after a call to
      TypeOf.

merlin 1.4.1
============
Thu Sep 26 21:29:56 BST 2013

  + documentation:
    - updates of the emacs section of the readme by Ronan Lehy.

  + emacs:
    - bugfix for ac-mode: merlin-ac-prefix wasn't called.
    - better formatting for completion suggestions.

  + vim:
    - bugfix for the "selectPhrase" command, an overflow on 32b plateform was
      causing complete desynchronisation between vim and merlin.
    - better formatting for completion suggestions.

merlin 1.4.0
============
Tue Sep 24 23:02:04 BST 2013

  + backend:
    - lazy processing of open directives makes merlin much faster
    - simplified buffer management
    - tweak signal handling to improve windows compatibility
    - track verbosity of query: repeated queries are considered more "verbose"
    - type expansion: expand type aliases for verbose query
    - add support for OUnit-like Benchmark extension
    - more tolerant type checker, to provide completion on ill-typed
      expressions
    - proper version reporting with git revision
    - refactored logging subsystem
    - add support "val constructs" in implementation: report errors but add
      definition to current environment
    - add FLG, EXT and PRJ in .merlin
    - "locate" command now works in much more situations
    - one distribution for 4.00 and 4.01, introduced common interface between
      both, typers now live in https://github.com/def-lkb/merlin-typers
    - new implementation of the main merlin state tracking ast & types
    - better error reporting thanks to a contribution from Ronan Lehy

  + documentation:
    - started a wiki (https://github.com/def-lkb/merlin/wiki)
    - wrote 'from-scratch' guides to ease setting-up merlin in your editor

  + emacs
    - during completion with auto-complete, you can hit C-c C-l on a candidate
      to jump on its definition
    - made communication with merlin asynchronous (using transfer queues) hence
      improving responsiveness
    - when running a merlin command, the errors present in all phrases but the
      current one are displayed
    - fixed buffer cleaning
    - successive call to C-c C-t do not climb the typed tree but improve merlin's
      verbosity. To move inside the tree use C-down and C-up (which implements
      phrase movement if there is no enclosing started).
    - customize data for merlin
    - refactoring and numerous bugfixes


merlin 1.3.1
============


  Minor release, but merlin is now compatible with ocaml versions > 4.00.1.
  The only noticeable changes since 1.3 is the use of short paths even with
  version 4.00.1 of ocaml.

merlin 1.3
==========

  + backend:
    - added a "locate" command to find the definition location of the given
      identifier
      works on the local buffer out of the box and at project level if it the
      build directories contain cmt files

  + emacs:
    - various bugfixes
    - aesthetic changes for highlighting
    - introduced "semantic movements":
      add commands (and keybindings) to go to the next/previous phrase
    - support for completion in emacs 23
    - reporting of syntax errors
    - removed "idle-typing" feature
    - asynchronous fetching of types so that long signatures
      won't make emacs hang

  + extensions:
    - add support for "type nonrec" declaration
    - add support for "with compare" from type-conv
    - add partial support for "with fields" from type-conv

  + misc:
    - added specific support for omake's polling mode

  + vim:
    - ':TypeOf' command now accepts an (optional) argument and tries to type it
      in the current context (i.e. at cursor position)
    - better catching of errors

merlin 1.2
==========

  + emacs:
    - add ML, MLI and merlin-goto-project-file commands
    - prints the type of completed entry on completion
    - various bugfixes

  + extensions:
    - merged support for ignoring P4_QUOTATION
    - merged support for js_of_ocaml syntax
    - support top-level lwt binding
    - merged support for oUnit

  + misc:
    - introduced 'REC' flag in .merlin:
        tells merlin to concatenate the current .merlin with the ones present in
        parents directories
    - added specific support for omake's polling mode.

  + vim:
    - bugfix for omnicompletion on versions <= 703

merlin 1.1
==========

  + emacs:
    - Ported the completion to the usual `completion-at-point' system, disabled
      auto-complete-mode-by-default
    - reset now tells merlin about the current buffer name
    - merlin-mode comes with a menu
    - improved documentation of the mode

  + backend:
    - code cleanup
    - method completion

  + vim plugin: refactored synchronization code out

merlin 1.0
==========
First release
