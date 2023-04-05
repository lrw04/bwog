# bwog

bwog is a static blog generator written in Common Lisp. Its highlights include:

- character replacement
- customizable KaTeX macros
- embeddable LaTeX figures

## Installation

bwog is dependant on `tectonic`, `cairo`, and `roswell`. After cloning the repository, run:

```sh
ros build bwog.ros
```

Then copy the resulting `bwog` file (or `bwog.exe` on Windows) into a folder in your `PATH`.

## Usage

Prepare a folder with `blog.lisp` in it. You should redefine global variables and functions in it, as it will be loaded at runtime.

Read `bwog.lisp` for more details. Then, run `bwog <repo>` to build it. Output will be written to the same directory.
