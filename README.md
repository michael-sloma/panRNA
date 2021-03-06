The RNA secondary structure world has an annoyingly large number of different formats for describing sequences and structures. This is a simple command line utility to interconvert between formats. It accepts the input format from standard in, and emits the new format to standard out, for convenient incorporation in unix pipelines.

Usage: panRNA -i [input format] -o [output format] [options]

Currently supported formats:
* plain (text file with one sequence per line)
* fasta (everyone knows this one)
* seq (mfold/RNAstructure legacy .seq format)
* ct (mfold/RNAstructure connectivity table format)
* vienna (viennaRNA style dot-bracket structures)

Options:
* -s [n], --select=[n]: only emit the structure at index n (1-indexed)
* -r, --removenoncanonical: remove pairs that are not A-U, G-C, or G-U
* --commentchar=[c]: character c is treated as the beginning of a comment, and the rest of the line is ignored
* --convertambiguous=[c]: convert all IUPAC ambiguity codes (e.g. R, Y, X) to character c
  

Installation with cabal on linux:

If you don't have it, install the Haskell Platform from the website or with your package manager.

Then,


    git clone git@github.com:michael-sloma/panRNA.git && cd panRNA
    cabal sandbox init # optional, keeps your global packages from being polluted
    cabal configure --bindir=/location/for/executable
    cabal install --only-dependencies
    cabal build


Please send any comments or bug reports to michael_sloma@urmc.rochester.edu

In particular, please tell me if it fails on valid input!
