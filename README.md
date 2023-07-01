# bwog

bwog is a static blog generator written in Chez Scheme. Its highlights include:

- character replacement
- customizable KaTeX macros
- embeddable LaTeX figures
- full configurability

## Installation

bwog is dependent on TeX Live. Copy \*.scm to your blog repository.

## Usage

Prepare a folder with `blog.lisp` in it. You should redefine global variables and functions in it, as it will be loaded at runtime.

Read `bwog.lisp` for more details. Then, run `bwog <repo>` to build it. Output will be written to the same directory.
