module Run where

import SemOp
import ParserQ_Left
import Syntax
import Examples
import Beautify

import System.Exit

{-
runNStepQCSch :: C -> LMem -> Int -> IO [(Mem,Double)]
showNStepQCSch :: C -> LMem -> Int -> IO()
histogramNStep :: Int -> C -> LMem -> Int -> IO ExitCode

parseInputC :: String -> Either ParseError C
-}

{-
Receives a file, a flag choosing the output (-n,-h), and if the previous flag is "-n", it is
necessary another flag (-v|-d|-cd) to indicate the method used in the NStep

General input: filename -n (-v|-d|-cd)  OR filename -h 

· filename : is the name of the file to be parsed
· -n : outputs the results produced by the NStep operational semantics
· -h : outputs a histogram

· -v : produces the results using the runNStepConf
· -d : produces the results using the runNStepSch
· cd : produces the results using the runNStepSchFullConv
-}


run :: IO()
run = do
  putStrLn "For NStep insert: Filename -n (-v|-d|-cd) \n For histogram insert: Filename -h"
  input <- getLine
  let inputs = words input
  fileContent <- readFile ((head inputs) ++ ".txt")
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
      Right configs -> runDistAux configs flagMethod

--Executes the NStep semantics accordingly to the flagMethod received
runDistAux :: [((String, Int, Int), (C,SC,L,SQ))] -> String -> IO()
runDistAux [] _ = return ()
runDistAux (((name, rep, k),(c,sc,l,sq)):t) flagMethod =
  case flagMethod of
    "-v" -> do
      result <- runNStepConf c (sc,l,sq) k -- (Mem,Double)
      putStrLn $ showRun (name, [result])
      runDistAux t flagMethod
    "-d" -> do
      result <- runNStepSch c (sc,l,sq) k -- [(Mem,Double)]
      putStrLn $ showRun (name, limitPrecAux 10 $ result)
      runDistAux t flagMethod
    "-cd" -> do
      result <- runNStepSchFullConv c (sc,l,sq) k -- [(Mem,Double)]
      putStrLn $ showRun (name, limitPrecAux 10 $ result)
      runDistAux t flagMethod

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
  histogramNStepConf name rep c (sc,l,sq) k
  runHistAux t
    

{-PREVIOUS VERSIONS
run :: IO ()
run = do
  putStrLn "Filename  (-n|-h)  (-v|-d|-cd):"
  input <- getLine
  let inputs = words input
  case inputs of
    (fileName:flag) -> do
      fileContent <- readFile (fileName ++ ".txt")
      let newInputs = fileContent : flag
      runDistHist newInputs
    [] -> putStrLn "Error: No filename provided."


runDistHist :: [String] -> IO()
runDistHist l =
  if length l == 1
  then do
    case parseRun (l!!0) of
      Left err -> print err  -- Print error if parsing fails
      Right configs -> (runHistAux configs "") >> (runDistAux configs "")
  else do
    case parseRun (l!!0) of
      Left err -> print err  -- Print error if parsing fails
      Right configs -> (runHistAux configs (l!!1)) >> (runDistAux configs (l!!1))
      
runDistAux :: [((String, Int, Int), (C,SC,L,SQ))] -> String -> IO()
runDistAux [] _ = return ()
runDistAux (((name, rep, k),(c,sc,l,sq)):t) flag =
  case flag of
    "" -> do
      result <- runNStepSch c (sc,l,sq) k -- [(Mem,Double)]
      putStrLn $ showRun (name, limitPrecAux 5 $ result)
      runDistAux t flag
    "-fc" -> do
      result <- runNStepSchFullConv c (sc,l,sq) k -- [(Mem,Double)]
      putStrLn $ showRun (name, limitPrecAux 5 $ result)
      runDistAux t flag
    "-c" -> do
      result <- runNStepCom c (sc,l,sq) k -- [(Mem,Double)]
      putStrLn $ showRun (name, result)
      runDistAux t flag


runHistAux :: [((String, Int, Int), (C,SC,L,SQ))] -> String -> IO ExitCode
runHistAux [] _ = return (ExitFailure 1)
runHistAux (((name, rep, k),(c,sc,l,sq)):t) flag =
  case flag of
    "" -> do
      histogramNStep name rep c (sc,l,sq) k
      runHistAux t flag
    "-fc" ->  do
      histogramNStepFullConv name rep c (sc,l,sq) k
      runHistAux t flag
    "-c" -> do
      histogramNStepCom name rep c (sc,l,sq) k
      runHistAux t flag
-}

--REMARK: when using the method funcFullConv there may exist a disparity between the probabilities
--shown in the terminal (obtained by runNStepSchFullConv) and the histograms generated; this is
--because the former is only executed one time, while the histograms performs "k" executions of the
--program, which entails calculating "k" times a new convex closure



--PREVIOUS VERSIONS
-- run = do
--   putStrLn "Name of the file to be computed: "
--   fileName <- getLine                      
--   text <- readFile (fileName ++ ".txt")
--   runDist text
--   runHist text

-- runDist :: String -> IO()
-- runDist s = do
--   case parseRun s of
--     Left err -> print err  -- Print error if parsing fails
--     Right configs -> runDistAux configs

-- runHist :: String -> IO()
-- runHist text = do
--   case parseRun text of
--     Left err -> print err  -- Print error if parsing fails
--     Right configs -> (runHistAux configs) >> return ()


--


-- run = do
--   putStrLn "input a configuration"
--   conf <- getLine
--   result <- runAux conf
--   case result of
--     Left ioAction -> ioAction
--     Right ioExitCode -> ioExitCode >> return ()

-- showSchC :: String -> LMem -> Int -> IO()
-- showSchC s l n = case parseInputC s of
--   Left err -> print err
--   Right c -> showNStepQCSch c l n

-- histogramC :: Int -> String -> LMem -> Int -> IO ExitCode
-- histogramC rep s l n = case parseInputC s of
--   Left err -> return (ExitFailure 1)
--   Right c -> histogramNStep rep c l n

-- showSchConf :: String -> Int -> IO()
-- showSchConf s n = case parseConf s of
--   Left err -> print err
--   Right (c,sc,l,sq) -> showNStepQCSch c (sc,l,sq) n

-- histogramConf :: Int -> String -> Int -> IO ExitCode
-- histogramConf rep s n = case parseConf s of
--   Left err -> return (ExitFailure 1)
--   Right (c,sc,l,sq) -> histogramNStep rep c (sc,l,sq) n  

-- runAux :: String -> IO (Either (IO ()) (IO ExitCode))
-- runAux s = do
--   putStrLn "write: \n'-s' for showing a distribution of states \n'-h' for a histogram"
--   flag <- getLine
--   case flag of
--     "-s" -> do
--       putStrLn "introduce the number of executions"
--       n <- getLine
--       return $ Left (showSchConf s (read n :: Int))
--     "-h" -> do
--       putStrLn "introduce the number of repetitions"
--       rep <- getLine
--       putStrLn "introduce the number of executions"
--       n <- getLine
--       return $ Right (histogramConf (read rep :: Int) s (read n :: Int))
--     _ -> do
--       putStrLn "Invalid flag."
--       return $ Left (return ())



--some examples of configuration--
-- <skip, [(x,0)], [(q,1)], (1.0 + i0.0)|0><0|>
-- <x:=1 +(0.5) x:=2, [(x,0)], [(q,1)], (1.0 + i0.0)|0><0|>
-- <H(q1); CNOT(q1,q2); Meas(x1,q1); Meas(x2,q2), [(x1,0),(x2,0)], [(q1,1),(q2,2)], (1.0 + i0.0)|00><00|>
--some examples of configuration--

