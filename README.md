# cabal-scaffold - Scaffolding Tool for Cabal-based Projects with Stackage Snapshots

`cabal-scaffold` provides a cabal-analogue of `stack new`.
It automatically downloads `cabal.project.freeze` corresponding to the specified Stackage Snapshot.

## Usage

```sh
Usage: cabal-scaffold new [--resolver ARG] [-p|--param KEY:VALUE] 
                          [-w|--with-compiler COMPILER] PROJECT_NAME [TEMPLATE]

  Create New Project

Available options:
  --resolver ARG           Stackage resolver (default: lts)
  -p,--param KEY:VALUE     Parameters to specify explicitly
  -w,--with-compiler COMPILER
                           The compiler to use in the project
  PROJECT_NAME             Project name
  TEMPLATE                 Template Name or path
  -h,--help                Show this help text
```

### Example Usage

Create a new project `my-new-package` with the default template, resolver, and compiler:

```sh
cabal-scaffold new my-new-package
```

Create a package by specifying the template name:

```sh
cabal-scaffold new my-new-package my-template
```

Create a package by specifying the path to the template:

```sh
cabal-scaffold new my-new-package ./path/to/my-template.hsfiles
```

Create a package with specified template, resolver, compiler, and custom params:

```sh
cabal-scaffold new -p category:Network --resolver lts-20.1 --with-compiler ghc-9.2.6 my-new-package default
```

Resolvers can be specified partially (resolved by querying to stackage):

```sh
cabal-scaffold new --resolver lts my-new-package

cabal-scaffold new --resolver lts-20 my-new-package

cabal-scaffold new --resolver nightly my-new-package
```

## Configuration

Save the following as `${XDG_CONFIG_HOME}/cabal-scaffold.yaml` (by default, it is equivalent to `~/.config/cabal-scaffold.yaml`):

```yaml
templateDirs: []
hpack: true

defaults:
  template: default
  snapshot: lts-20

params: 
  author-name: Your Name
  author-email: your_name@example.com
  # github-username: octcat
```

## Templates

Templates must be placed under `${XDG_DATA_HOME}/cabal-scaffold` (defaulted to `~/.local/share/cabal-scaffold`).
The format is the same as the one used by `stack`; i.e. `hsfiles` format.
You can just copy templates from `~/.stack/templates`, or adding `~/.stack/templates` to `templateDirs` config item.

Templates in the scope can be inspected by `cabal-scaffold expand` command:

```sh
Usage: cabal-scaffold expand TEMPLATE [DIR]

  Search and expand the template to the specified directory

Available options:
  TEMPLATE                 The template to expand
  DIR                      The directory to put the expanded template
  -h,--help                Show this help text
```

Directory or hsfiles can be imported by `cabal-scaffold import` command:

```sh
Usage: cabal-scaffold import [-S|--override] PATH [TEMPLATE_NAME]

  Import directory or .hsfiles as a new preset template

Available options:
  -S,--override            If on, this will overrides the existing templates
  PATH                     The path to the hsfiles tempalte-file or directory to
                           import as a template
  TEMPLATE_NAME            The name of the template. If omitted, uses the base
                           name (without extension) of input template path.
  -h,--help                Show this help text
```
