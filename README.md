# codeforces-cli

[![Hackage](https://img.shields.io/hackage/v/codeforces-cli?color=green)](https://hackage.haskell.org/package/lens)
[![Build Status](https://img.shields.io/github/workflow/status/farbodsz/codeforces-cli/Haskell%20CI)](https://github.com/farbodsz/codeforces-cli/actions?query=workflow%3AHaskell-CI)

Command line interface to interact with Codeforces.

<img src="https://github.com/farbodsz/codeforces-cli/blob/master/docs/demo.gif?raw=true" alt="demo-gif" width="700" height="550">

## Features

- View/filter contests and problems
- Watch contest standings and submissions
- Calculate rating after a virtual contest
- List rating changes for each contest
- And much more!

## Installation

The pre-compiled binary file can be found
[on the releases page](https://github.com/farbodsz/codeforces-cli/releases).

Download it and place it in a directory in your `PATH`. For example,
`~/.local/bin/cf`.

Alternatively, you can get it through Haskell Stack, by installing the
[`codeforces-cli`](https://hackage.haskell.org/package/codeforces-cli) package:

```
stack install codeforces-cli
```

## Usage

```
Codeforces CLI v0.1.0

Usage: cf COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  agenda                   Upcoming contests. Alias for contests --upcoming
  contests                 List of contests
  info                     Show the problems and your problem results of a
                           contest
  friends                  List your friends (must be authenticated)
  open                     Open a contest in the browser
  problems                 View and filter problem sets
  ratings                  Rating changes of a user
  setup                    Setup your configuration file
  standings                Standings table of a contest
  status                   Recent submissions of a user
  user                     Information about a user
  virtual                  Calculate your rating after a virtual contest, to
                           find what it would be if you competed live
```

## Documentation

A help page for the CLI is available via `cf --help`.

The package's documentation is
[available on Hackage](https://hackage.haskell.org/package/codeforces-cli).
