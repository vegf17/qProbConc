# qProbConc

This README aims to instruct how to use the tool developed for ICTAC25 submission

## How to install

1. Download this repository 
2. Within the downloaded folder, open the terminal
3. Run cabal init


## How to use the tool

1. Inside the downloaded folder, create a txt file with one or more programs with the following format
(to see some examples, have a look at the txt files we have developed)
   	  ``---ProgramName---
	  rep: int_value
	  k: int_value
	  < C, cs, l, qs >
	  ---ProgramName---``
    - ProgramName: is the name of the program without spaces
    - rep: is the number of samples for building the histogram
    - k: is the number of computational steps the $k$-step semantics performs
    - C: is the command to be evaluated
    - cs: is the classical state
    - l: is the linking function
    - qs: is the quantum state
2. Open the terminal inside the downloaded folder and run ``cabal repl``
3. Load the module Run.hs by executing ``:l Run``
4. Write ``run``, press enter, and then choose one of the following:
   - ``FileName -n (-v|-d|fd)``
     - FileName: is the name of the file to be parsed
     - -k : outputs the results produced by the $k$-step operational semantics
     - -v : produces the results using the runNStepConf	 
     - -d : produces the results using the runNStepSch
     - cd : produces the results using the runNStepSchFullConv     
   - ``FileName -h``
     - FileName: is the name of the file to be parsed
     - -h : outputs a histogram


