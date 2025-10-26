module Run where

import KStep
import HistogramSem
import ParserCom
import ParserFile
import Syntax
import Examples
import Beautify

import System.Exit

--Receives a filename and a scheduler, defined by the user, and executes runKStepSch for each
--program inside the filename
runSem :: String -> Sch -> IO()
runSem path sch = do
  fileContent <- readFile path
  case parseRun fileContent of
    Left err -> print err  -- Print error if parsing fails
    Right configs -> runSemAux configs sch

runSemAux :: [((String, (Int, [[String]]), Int), (C,SC,L,SQ))] -> Sch -> IO()
runSemAux [] _ = return ()
runSemAux (((name, rep, k),(c,sc,l,sq)):t) sch = do
  let result = runKStepSch sch c (sc, l, sq) k
  putStrLn $ showRun (name, result)
  runSemAux t sch


--Receives a filename and shows a Histogram for each program inside the filename
runHist :: String -> IO()
runHist path = do
  fileContent <- readFile path
  case parseRun fileContent of
    Left err -> print err  -- Print error if parsing fails
    Right configs -> (runHistAux configs) >> return ()

runHistAux :: [((String, (Int, [[String]]), Int), (C,SC,L,SQ))] -> IO ExitCode
runHistAux [] = return (ExitFailure 1)
runHistAux (((name, rep, k),(c,sc,l,sq)):t) = do
  showHist name rep c (sc,l,sq) k
  runHistAux t
  
