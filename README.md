# codeforces-cli

Command line interface to interact with Codeforces.

![demo](https://github.com/farbodsz/codeforces-cli/blob/master/docs/demo.gif?raw=true)

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
