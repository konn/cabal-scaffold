# Changelog for cabal-scaffold

## Unreleased

- Supports cabal-install's [external command mechanism](https://cabal.readthedocs.io/en/3.12/external-commands.html) (introduced since `cabal-install-3.12.0.0`), which allows user to call `cabal scaffold new` or `cabal help scaffold`.
- Introduced `project-file` field in config file and deleted `noProject` field.
  + Use `project-file: false` instead of `noProject: true` in config file.

## 0.1.0.0

- Initial Release!
