# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## 0.0.3

### Added

- Bindings for `children` and `childAt` Enzyme functions.
- New helper function, `spy`.

## 0.0.2

### Changed

- **Breaking**: tracking element wrapper multiplicity at type-level.
  - `ElementWrapper` renamed to `Wrapper` and given a type parameter, either
    `SingleNode` or `ManyNodes`, reflecting possible multiplicity of any given
    wrapper.
  - `EnzymeM` given the same type parameter reflecting possible multiplicity of
    the "current context" wrapper.
  - Every function in the API annotated with the expected and resulting (where
    appropriate) wrapper multiplicity. For example, `text` only works for
    `SingleNode` wrappers.
  - This ensures that mistakes related to accidentally selecting multiple
    elements instead of one (or vice versa) get discovered early, at time of
    selection, and thus can produce a more informative error message.
- **Breaking**: renamed `find` to `findAll`; renamed `findSingle` to `find`.

## 0.0.1

The first draft version.
