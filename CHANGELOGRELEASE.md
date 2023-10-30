### V 1.0.0.0

This release marks the switch to [Haskell's Package Versioning Policy](https://pvp.haskell.org/). Under the hood we also switched to a new GHC version (9.4.7) and a new Stackage resolver version (21.17). 

Feature-wise in this version we changed the semantics of the `d()` pseudo-function to load `.janno` files, introduced a set of additional mechanisms to load specific `.janno` files more conveniently and finally added an automatically generated `source_file` column for the SQL tables to distinguish source files in derived queries.

#### New and modified pseudo-functions to crawl .janno files

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

#### A source_file column to distinguish files

From this version onwards `qjanno` prepends the relative path to the source file for a given observation to each SQL table row. This works for all files, including `.janno` files loaded directly or via `d()`, `da()` or `j()`. It allows to include the source in a query.
