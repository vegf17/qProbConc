# qProbConc

This README aims to instruct how to use the tool developed for ICTAC25 submission

## How to load the library into GHCi

The tool in question corresponds to a Haskell library. To load this library's modules into GHCi and use them, refer to the following instructions. 
A prerequisite for these instructions is the installation of [GHC](https://www.haskell.org/ghc/) and [cabal-install](https://cabal.readthedocs.io/en/stable/).

These tools can be installed separetly, however it is possible to install both simultaneously using [GHCUP](https://www.haskell.org/ghcup/).

The GHC version we are using is **9.6.7** (this seems to be important to not create any conflict between the libraries we are using)


1. Download the qProbConc Cabal package.
2. Open a terminal within the downloaded package.
3. Run ``cabal build`` and then ``cabal repl qProbConc``.


## How to use the tool

After executing the steps detailed in "How to load the library into GHCi", look at the following instructions, which aim to aid the user using the tool.

1. Inside the downloaded folder, create a txt file with one or more programs with the following format
(to see some examples, take a look at the txt files we have developed inside the folder "examples")
   >``---ProgramName---``
   >
   >``rep: int_value``
   >
   >``k: int_value``
   >
   >``< C, cs, l, qs >``
   >
   >``---ProgramName---``
    - ProgramName: is the name of the program without spaces
    - rep: is the number of samples for building the histogram
    - k: is the number of computational steps the $k$-step semantics performs
    - C: is the command to be evaluated
    - cs: is the classical state
    - l: is the linking function
    - qs: is the quantum state
2. Load the module Run.hs by executing ``:l Run``
3. To obtain a histogram run ``runHist "path"``
  - ``path`` is the path to the file with the programs to be evaluated
  - this will show a histogram for each program inside the file 
4. To obtain results given by the k-step semantics run ``runSem "path" sch``
  - ``path`` is the path to the file with the programs to be evaluated
  - ``sch`` is a scheduler that needs to be defined inside the file ``KStep.hs``
  - this shows the results obtained for each program inside the file
  - the schedulers that are currently available are:
    - undSch: undefined scheduler
    - initSch: scheduler that chooses always the first element in the list of the possible transitions
    - lasSch: scheduler that chooses always the last element in the list of the possible transitions
    - middleSch: scheduler that chooses always the middle element in the list of the possible transitions
    - probSch: scheduler, which assigns the same probability to all the elements in the list of the possible transitions
    - fairSch: scheduler that checks the history of the computation to sse if the current command and the classical state already occurred; if yes, then the scheduler tries another option

For example, if we wish to evaluate the commands inside the **prob.txt** file we write 
- ``runHist "./examples/prob.txt"``, to obtain a histogram from each program inside **prob.txt**
- ``runSem "./examples/prob.txt" initSch``, to obtain the evaluation of the k-step semantics from each program inside **prob.txt**

