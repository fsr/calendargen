# calendargen - Make your own fancy [iFSR](https://ifsr.de) themed calendar

## Installation

1. Clone the repository
2. Install with your favorite Haskell build tool
    - **stack**: `stack install`
    - **cabal**: `cabal install`
3. Run it
    - `calendargen` on the command line

## Options

Per default it creates a calendar from an internal template and saves it to "generated.svg" but this can be configured using command line options.

To see the available options run `calendargen --help`.

    calendargen -- make your own iFSR calendar

    Usage: calendargen [-t|--template PATH] [-o|--output PATH]

    Available options:
    -h,--help                Show this help text
    -t,--template PATH       path to the mustache template to use (default:
                            internal template)
    -o,--output PATH         where to write the generated svg
                            to (default: "generated.svg")
