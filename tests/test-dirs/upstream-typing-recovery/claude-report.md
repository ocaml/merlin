# Merlin vs. compiler reference — divergences

I compared all 82 tests by flattening every message (main **and** `sub`) on both
sides and normalizing whitespace, so line-wrapping/grouping/sub-vs-inline
differences are neutralized. **Of the 82, only 9 have genuinely different error
messages.** The other ~11 that a naive diff flags are pure line-wrapping (the
compiler wraps at a fixed column, Merlin doesn't — same text).

## Tests where Merlin ≠ compiler (genuine)

| Test | Difference |
|---|---|
| **modular_explicit_examples** | **Biggest.** Merlin returns a *fatal* error (`class: "error"` — "The module F is not a functor…") and recovers **nothing**: 0 diagnostics vs the compiler's 19. Recovery aborts entirely on this modular-explicit file. |
| **invalid_format** | Merlin is **missing `Warning 14` (illegal-backslash escape)** — the two format errors and the constant error match. |
| **missing_constructor_and_invalid_types** | Merlin says generic *"This constant has type int/char…"*; compiler names the literal: *"The constant 3 / 'a' has type…"*. |
| **instance_variable_not_mutable** | Merlin: *"This value has type int…"*; compiler names it: *"The value y has type int…"*. |
| **too_many_arguments** | Type variable printed differently: Merlin *"The function f has type **'b** -> int"* vs compiler *"…**'a** -> int"*. Rest matches. |
| **apply_wrong_label** | Warning text: Merlin *"Warning 5:"* vs compiler *"Warning 5 **[ignored-partial-application]**:"*. |
| **optional_poly_param** | Same warning-tag issue, ×4: *"Warning 20:"* vs *"Warning 20 **[ignored-extra-argument]**:"*. |
| **virtual_class** | Same: *"Warning 20:"* vs *"Warning 20 [ignored-extra-argument]:"*. |
| **not_subtype_nonrec** | *Expected* — the one upstream test run without recovery. Compiler emits 1 error with un-expanded type synonyms (`Type u -> unit …`); Merlin recovers 3 and prints expanded types. |

## Themes

- **Warning mnemonic tags** (`[ignored-extra-argument]`, `[illegal-backslash]`, …)
  are absent from Merlin's warnings (apply_wrong_label, optional_poly_param,
  virtual_class, invalid_format). This looks systematic — quite possibly a
  **vendored-compiler-version** artifact (mnemonic tags are a newer feature)
  rather than a recovery bug.
- **Generic vs. named wording**: in recovery Merlin sometimes loses the
  identifier/literal and says *"This value/constant…"* instead of
  *"The value y"/"The constant 3"* (instance_variable_not_mutable,
  missing_constructor_and_invalid_types).
- **Fatal-vs-recover**: modular_explicit_examples is the only case where Merlin
  bails with a hard error and recovers nothing — the most worth investigating.

Everything else (e.g. `bind_existentials`, `oop_label_tuples`,
`wrong_expected_kind`, `ctor_arity_mismatch`, `unbound_modules`,
`undefined_method`, `wrong_name`, `less_general`, `function_arity_type_clash`,
`inside_check`, `unbound_existential`) is **message-identical** once
wrapping/sub-message placement is accounted for — Merlin even emits the
*"Hint: Did you mean …?"* suggestions, just in the JSON `sub` field rather than
inlined.
