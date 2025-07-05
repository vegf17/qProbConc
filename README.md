# qProbConc

This README aims to instruct how to use the tool developed for ICTAC25 submission

## How to install

1. Download this repository 
2. Within the downloaded folder, open the terminal
3. Run cabal init


## How to use the tool

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

