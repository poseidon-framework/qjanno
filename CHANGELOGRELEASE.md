### V 1.0.0.1

This minor release replaces the pipeline to produce static `qjanno` executables for every release.

Just like `trident` and `xerxes`, `qjanno` will now be available in the following pre-compiled versions:

- `qjanno-conda-linux` (for bioconda)
- `qjanno-Linux` (build on Ubuntu 20.04 for X64 architecture)
- `qjanno-macOS-ARM64` (build on macOS 14 for ARM64 architecture)
- `qjanno-macOS-X64` (build on macOS 13 for X64 architecture)
- `qjanno-Windows`

`qjanno` now also depends on a new stackage resolver version LTS 21.21.

### V 1.0.0.0

This release marks the switch to [Haskell's Package Versioning Policy](https://pvp.haskell.org/). Under the hood we also switched to a new GHC version (9.4.7) and a new Stackage resolver version (21.17). 

Feature-wise in this version we changed the semantics of the `d()` pseudo-function to load `.janno` files, introduced a set of additional mechanisms to load specific `.janno` files more conveniently and finally added automatically generated columns (`package_title`, `package_version` and `source_file`) for the SQL tables to distinguish source files in derived queries. We also implemented sorting of the `.janno` columns according to the suggested order in the Poseidon schema.

#### New and modified pseudo-functions to crawl `.janno` files

In previous versions `qjanno` included a single method to specify `.janno` files for loading and merging in the `FROM` instruction of the SQL query: The `d(<path_to_directory1>,<path_to_directory2>,...)` pseudo-function. When used in a query, `qjanno` crawled all directories for files with the extension `.janno`, to read them, row-bind them and load them as a table into the SQLite database for querying. This specific functionality is now accessible with a new pseudo-function `j()`. Beyond that, various additional methods are available now for searching and selecting `.janno` files.

Here is how the updated `FROM` field gets parsed and interpreted:

- `d(<path_to_directory1>,<path_to_directory2>,...)`: With `d()`, qjanno (recursively) searches all package-defining `POSEIDON.yml` files in all listed directories and reads them to determine the latest package version. It then reads the `.janno` files associated with these latest package versions.
- `da(<path_to_directory1>,<path_to_directory2>,...)`: `da()` behaves just as `d()`, but it does not filter for the latest package version: It loads all packaged `.janno` files.
- `j(<path_to_directory1>,<path_to_directory2>,...)`: `j()` simply searches for files with the extension `.janno` in all listed directories and loads them regardless of whether they are part of a Poseidon package or not.
- `<path_to_one_janno_file>.janno`: Specific `.janno` files can be listed individually.

Multiple of these methods can be combined in the `FROM` field as a comma-separated list. Each respective mechanism then yields a list of `.janno` files, and the list of lists is flattened to a simple list of `.janno` files. `qjanno` then reads all `.janno` files in this combined list, merges them and makes them available for querying in the in-memory SQLite database.

This means the following syntax is now valid:

```bash
qjanno "SELECT Poseidon_ID,Country FROM d(2018_Lamnidis_Fennoscandia,2012_MeyerScience),2010_RasmussenNature/2010_RasmussenNature.janno"
```

This loads the `.janno` files in `2018_Lamnidis_Fennoscandia` and `2012_MeyerScience`, and the additional file `2010_RasmussenNature/2010_RasmussenNature.janno`.

#### Additional columns to distinguish source files

From this version onwards `qjanno` prepends information about the source of a given observation in the form of three additional columns `package_title`, `package_version` and `source_file` to each SQL table row.

`package_title`: The title of the source package of a given `.janno` row.
`package_version`: The package version of the source package.
`source_file`: The relative path to the source file.

The former two can only be added for `.janno` files loaded via the `d()` and `da()` mechanisms, because only they have the necessary information about the source package of a given `.janno` file.

`source_file` works for all files, including `.janno` files loaded directly or via `d()`, `da()` or `j()`.

#### Sorting of the `.janno` table columns

In the process of reading `.janno` files, `qjanno` now not only row-binds them, but also orders their columns according to the specification in the Poseidon schema [here](https://github.com/poseidon-framework/poseidon-schema/blob/master/janno_columns.tsv).

The just introduced source columns `package_title`, `package_version` and `source_file` are kept at the beginning in this order. Additional columns not specified in the Poseidon schema are appended at the end in alphabetical order.
