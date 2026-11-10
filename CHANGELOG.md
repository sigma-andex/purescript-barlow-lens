# Changelog

## Unreleased
- Update to VTAs
- Add indexed traversals with `#` and `ibarlow`; nested indices are combined into tuples
- Add `[key]` for `ix` (arrays, lists, maps, objects, ...) and `[?key]` for `at`
- Allow `[field]` on records to select fields with special characters
- Add `{a,b}` to zoom into a sub-record
- Allow `+` (and `#`) on records whose fields all have the same type
- Add type-changing `%1` / `%2` for `Tuple`
- Add custom compile errors for common mistakes (unknown field, wrong type for `?`, `<`, `>`, `+`, invalid index, unterminated `[`/`{`, ...)
- Add helpers `toListOf`, `firstOf`, `lastOf`, `lengthOf`, `has`, `hasn't`, `traverseOf`, `iover`, `itoListOf`, `ifoldMapOf`, `itraverseOf`
- Breaking: `#`, `[` and `{` now end a field name in a path

## v1.0.0
- Update to Purescript `0.15.10`
- Use visible type applications instead of proxy syntax ([#26](https://github.com/sigma-andex/purescript-barlow-lens/pull/26))

## v0.9.0
- Update to Purescript `v0.15.0`

## v0.8.0
- Move from `Optic'` to `Optic` to allow changing types when modifying a value.
- Add helpers to further simplify lens construction
- Add some more interesting test cases
- Refactoring tests

## v0.7.2
- Bugfix: Remove unnecessary constraints resulting in type errors when explicitely adding the type annotations.

## v0.7.1
- Bugfix: Remove unnecessary constraint on Newtype instance causing exclamation mark not working with view.

## v0.7.0
- Refactor support for algebraic data types
- Remove `<` and `>` for ADTs 
- Introduce `%Name` syntax for sum types
- Introduce `%i` syntax for product types

## v0.6.0
- Add experimental support for algebraic data types 
- Refactoring: Break up code into different modules

## v0.5.0
- Simplified parsing 
- Removed requirement for records. A lens can now start e.g. with `?` 

## v0.4.0
- Support for `Newtype` using `!`

## v0.3.0
- Support for `Either` using `<` and `>`
- Support for `Array` and other `Traversal` using `+`

## v0.2.0
- Support for `Maybe` using `?`

## v0.1.0
- Support for records
