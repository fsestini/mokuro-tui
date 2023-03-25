# mokuro-tui

Simple Terminal User Interface (TUI) for the [mokuro](https://github.com/kha-white/mokuro) manga OCR tool.

## Build & Install

The project uses the `cabal` tool to build, run, and install.

    $ cabal build
    $ cabal install

## Usage

The program is quite simple: given a file system tree, it shows a list of all (sub)directories containing
image files, and that are therefore amenable to be processed with `mokuro`.
An "image file" is any file with extension among `jpg, jpeg, png`.

From the TUI, you can

* toggle-select any of the listed subdirectories, by pressing `ENTER`.
* send all selected directories to be batch-processed by `mokuro`, by pressing `p`.
* exit the program, by pressing `ESC`

Just point `mokuro-tui` to the root of your manga library and follow the instructions on screen.

    $ mokuro-tui [your library dir here]
