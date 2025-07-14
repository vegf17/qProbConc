# qProbConc

This README aims to instruct how to use the tool developed for ICTAC25 submission

## How to load the library into GHCi

The tool in question corresponds to a Haskell library. To load this library's modules into GHCi and use them, refer to the following instructions. A prerequisite for these instructions is the installation of [GHC](https://www.haskell.org/ghc/) and [cabal-install](https://cabal.readthedocs.io/en/stable/).

1. Download the qProbConc Cabal package (==TODO: add link to qProbConc cabal package, which will be in a separate git repo==).
2. Open a terminal within the downloaded package.
3. Run ``cabal build`` and then ``cabal repl qProbConc``.


## How to use the tool

1. Inside the downloaded folder, create a txt file with one or more programs with the following format
(to see some examples, take a look at the txt files we have developed inside the folder "examples")
   >``---ProgramName---``
   >
   >``hist: (int_value, vars_to_plot)``
   >
   >``k: int_value``
   >
   >``< C, cs, l, qs >``
   >
   >``---ProgramName---``
    - ProgramName: is the name of the program without spaces
    - hist: holds the information to build the histogram
      - int_value: is the number of samples for building the histogram
      - vars_to_plot: is a list or list of lists of variables to be plotted
        - e.g: [x1,...,xn] builds one histogram with the information of the variables x1,...,xn
        - e.g: [[x1,...,xn],[y1,...,ym]] builds two histograms: one for [x1,...,xn] and the other for [y1,...,ym]
    - k: is the number of computational steps the $k$-step semantics performs
    - C: is the command to be evaluated
    - cs: is the classical state
    - l: is the linking function
    - qs: is the quantum state
2. Open the terminal inside the downloaded folder and run ``cabal repl``
3. Load the module Run.hs by executing ``:l Run``
4. To obtain a histogram run ``runHist "path"``
  - ``path`` is the path to the file with the programs to be evaluated
  - this will show a histogram for each program inside the file 
5. To obtain results given by the k-step semantics run ``runSem "filename" sch``
  - ``path`` is the path to the file with the programs to be evaluated
  - ``sch`` is a scheduler that needs to be defined inside the file ``KStep.hs``
  - this shows the results obtained for each program inside the file

For example, if we wish to evaluate the commands inside the **prob.txt** file we write 
- ``runHist "./examples/prob.txt"``, to obtain a histogram from each program inside **prob.txt**
- ``runSem "./examples/prob.txt" initSch``, to obtain the evaluation of the k-step semantics from each program inside **prob.txt**

