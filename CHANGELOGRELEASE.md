### V 1.0.0.0

This release marks the switch to [Haskell's Package Versioning Policy](https://pvp.haskell.org/). It changes the semantics of the `d()` pseudo-function to load .janno files and introduces a set of additional mechanisms to do that more conveniently.

In previous versions `qjanno` supported a single method to specify .janno files for loading and merging in the `FROM` instruction of the SQL query: The `d(<path_to_directory1>,<path_to_directory2>,...)` pseudo-function. This caused qjanno to crawl all directories for files with the extension `.janno`, read them, row-bind them and load them as a table into the SQLite database for querying. This specific functionality is now accessible with a new pseudo-function `j()`. Beyond that, various additional methods are available now.

Here is how the `FROM` field gets parsed and interpreted:

- `d(<path_to_directory1>,<path_to_directory2>,...)`: With `d()`, qjanno searches all package-defining `POSEIDON.yml` files in all listed directories and reads them to determine the latest package version. It finally reads the `.janno` files of these latest package versions.
- `da(<path_to_directory1>,<path_to_directory2>,...)`: `da()` behaves just as `d()`, but it does not filter for the latest package version. It loads all packaged .janno files.
- `j(<path_to_directory1>,<path_to_directory2>,...)`: `j()` simply searches for files with the extension `.janno` in all listed directories and loads them regardless of whether they are part of a Poseidon package or not.
- `<path_to_one_janno_file>.janno`: Specific `.janno` files can be listed individually.

Multiple of these methods can be combined (separated by comma) in the `FROM` field. Each respective mechanism then determines a list of `.janno` files, which is concatenated with the others. Each `.janno` file in the combined list is finally read, merged with the others and made available for querying in the in-memory SQLite database.