module RunIO where

import KStepIO
import HistogramSem
import ParserQ_Left
import Syntax
import Examples
import Beautify

import System.Exit

{-
Receives a file and a flag choosing the output (-n,-h); if the previous flag is "-n", it is
necessary another flag (-v|-sd|-sp) to indicate the method used in the k-step

General input: filename -n (-v|-sd|-sp)  OR filename -h 

· filename : is the name of the file to be parsed
· -n : outputs the results produced by the k-step operational semantics
· -h : outputs a histogram

· -v : produces the results using runNStepConf with a built-in scheduler
· -sd : produces the results using runKStepSch with a pre-defined determinstic scheduler
· -sp : produces the results using runKStepSch with a pre-defined probabilistic scheduler
-}

run :: IO()
run = do
  putStrLn "For NStep insert: Filename -n (-v|-sd|-sp) \n For histogram insert: Filename -h"
  input <- getLine
  let inputs = words input
  fileContent <- readFile ("./examples/" ++ (head inputs) ++ ".txt")
  case length inputs of
    2 -> do
      runHist fileContent
    3 -> do
      runDist fileContent (inputs!!2) --inputs!!2 tells us the method to use in the NStep

--Receives the content from fileName and the method to be used
runDist :: String -> String -> IO()
runDist fileContent flagMethod = do
    case parseRun fileContent of
      Left err -> print err  -- Print error if parsing fails
      Right configs -> runDistAuxIIIO configs flagMethod
      --Right configs -> runDistAux configs flagMethod

--Executes the k-step semantics accordingly to the flagMethod received
runDistAuxIO :: [((String, Int, Int), (C,SC,L,SQ))] -> String -> IO()
runDistAuxIO [] _ = return ()
runDistAuxIO (((name, rep, k),(c,sc,l,sq)):t) flagMethod =
  case flagMethod of
    "-v" -> do
      result <- runSample c (sc,l,sq) k -- (Mem,Double)
      putStrLn $ showRun (name, [result])
      runDistAuxIO t flagMethod
    "-sd" -> do
      result <- runKStepSchIO detSch c (sc,l,sq) k -- [([(Mem,Double)], 1.0)]
      putStrLn $ showRunK (name, limPrecKStep 10 result)
      runDistAuxIO t flagMethod
    "-sp" -> do
      result <- runKStepSchIO probSch c (sc,l,sq) k -- [([(Mem,Double)], Double)]
      putStrLn $ showRunK (name, limPrecKStep 10 result)
      runDistAuxIO t flagMethod

runDistAuxIIIO :: [((String, Int, Int), (C,SC,L,SQ))] -> String -> IO()
runDistAuxIIIO [] _ = return ()
runDistAuxIIIO (((name, rep, k),(c,sc,l,sq)):t) flagMethod =
  case flagMethod of
    "-v" -> do
      result <- runSample c (sc,l,sq) k -- (Mem,Double)
      putStrLn $ showRun (name, [result])
      runDistAuxIIIO t flagMethod
    "-sd" -> do
      result <- runKStepSchIIIO detSch c (sc,l,sq) k -- [(Mem,Double)]
      putStrLn $ showRun (name, limitPrecAux 5 $ result)
      runDistAuxIIIO t flagMethod
    "-sp" -> do
      result <- runKStepSchIIIO probSch c (sc,l,sq) k -- [(Mem,Double)]
      putStrLn $ showRun (name, limitPrecAux 5 $ result)
      runDistAuxIIIO t flagMethod      

--Receives the content from fileName and the method to be used
runHist :: String -> IO()
runHist fileContent = do
    case parseRun fileContent of
      Left err -> print err  -- Print error if parsing fails
      Right configs -> (runHistAux configs) >> return ()

--Executes the NStep semantics accordingly to the flagMethod received
runHistAux :: [((String, Int, Int), (C,SC,L,SQ))] -> IO ExitCode
runHistAux [] = return (ExitFailure 1)
runHistAux (((name, rep, k),(c,sc,l,sq)):t) = do
  showHist name rep c (sc,l,sq) k
  runHistAux t

