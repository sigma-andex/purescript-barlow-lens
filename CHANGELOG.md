# Changelog

## Unreleased
- Update to VTAs

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
