[![CI](https://github.com/poseidon-framework/qjanno/actions/workflows/main.yml/badge.svg?branch=main)](https://github.com/poseidon-framework/qjanno/actions/workflows/main.yml)
[![Coverage Status](https://img.shields.io/codecov/c/github/poseidon-framework/qjanno/main.svg)](https://app.codecov.io/github/poseidon-framework/qjanno?branch=main)
[![GitHub release (latest by date including pre-releases)](https://img.shields.io/github/v/release/poseidon-framework/qjanno?include_prereleases) ![GitHub all releases](https://img.shields.io/github/downloads/poseidon-framework/qjanno/total)](https://github.com/poseidon-framework/qjanno/releases)

# qjanno

A command line tool to run SQL queries on .janno (and arbitrary .csv and .tsv) files. This is an adjusted version and hard fork of the qsh package (https://github.com/itchyny/qhs).

**Detailed user documentation can be found on our [website](https://poseidon-framework.github.io/#/qjanno).**

***

## For (Haskell) developers

To install the development version of qjanno you can follow these steps:

1. Install the Haskell build tool [Stack](https://docs.haskellstack.org/en/stable/README/)
2. Clone this repository
3. Execute `stack install` inside the repository to build the tool and copy the executables to `~/.local/bin` (which you may want to add to your path). This will install the compiler and all dependencies into folders that won't interfere with any installation you might already have.
4. To run the tests, execute `stack test` inside the repository to build and run tests.

qjanno includes a set of golden tests defined by input data files, shell scripts and output files in `test/tests`.

### Preparing a new stable release

The Github Actions script in `.github/workflows/release.yml` registers a new draft release and automatically builds and uploads qjanno binaries when a new Git tag with the prefix `v*` is pushed. 

```bash
# locally register a new tag (e.g. 0.3.1)
git tag -a v0.3.1 -m "see CHANGELOG.md"
# push tag
git push origin v0.3.1
```

In case of a failing build delete the tag and the release draft on Github and then delete the tag locally with

```bash
git tag -d v0.3.1
```

before rerunning the procedure above.
