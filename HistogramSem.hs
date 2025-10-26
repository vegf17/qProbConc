module HistogramSem where

import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Identity
import System.Random
import Data.Complex -- module for complex numbers
import Data.Matrix -- module for matrix datatype and operations
import Data.Char -- intToDigit function
import Data.List -- nub function
import Data.Ord -- sortBy, comparing functions
import Graphics.Histogram
import qualified Graphics.Gnuplot.Frame.OptionSet as Opt
import qualified Graphics.Gnuplot.Frame.Option as Option
import System.Exit -- in order to get type ExitCode 
import Numeric.Probability.Game.Event

import Syntax
import SmallStep
import Beautify
import DistTMonad
--import KStepIO
import KStep
import Examples

--START: sampling from the k-step with a built-in memoryless scheduler--
--The scheduler used here chooses a configuration by first selecting a distribution from a set of
--distributions (using a uniform distribution), and a configuration from the distribution based on
--the probabilities associated to the configurations; the scheduler is memoryless, i.e. it does not
--save the history of the computation

--nstep where after a small-step a configuration is chosen to be executed next
sample :: ((C,LMem),Int) -> DistT IO LMem
sample ((c,s),0) = DistT $ return [(s, 0)] --From the original semantics
sample ((c,s),n) = do
  nextDist <- lift $ memorylessSch (c,s) -- [(Either S (C,S), Double)]
  nextConf <- lift $ eventNextStep nextDist -- (Either S (C,S), Double)
  case nextConf of
    (Left lmemi, pi) -> DistT $ return [(lmemi, pi)]
    (Right (ci,si), pi) -> do
      let ppR = [(((ci,si), n-1), pi)] 
          --transStep = (DistT $ return ppR) >>= nstepSch -- DistT IO S
      (DistT $ return ppR) >>= sample

--IO [(Either S (C,S), Double)]
--Given a configuration, returns the next distribution to be evaluated according to a uniform
--distribution
memorylessSch :: (C,LMem)  -> IO [(Either LMem (C,LMem), Double)]
memorylessSch ((c,s)) = do
  --let list = runDistT $ runExceptT $ runStateT (small c) s -- [[(Either S (C,S), Double)]]
  let list = evalAtom c s -- [[(Either S (C,S), Double)]]
  next <- nextStep list -- [(Either LMem (C,LMem), Double)]
  return next

eventNextStep :: [(Either LMem (C,LMem), Double)] -> IO (Either LMem (C,LMem), Double)
eventNextStep [] = error "eventNextStep: list should be non-empty"
eventNextStep l = do
  n <- enact $ makeEventProb list -- int (the index of l that should be returned)
  return $ l!!n
  where list = zip [i | i <-[0 .. length l]] [p | (a,p) <- l]

--Given a list of distributions selects the next distribution to be evaluated according to a
--uniform distribution
nextStep :: (Eq a) => [a] -> IO a
nextStep [] = error "nextStep: list should be non-empty"
nextStep l = do
  prob <- randNumber $ fromIntegral (length l)
  --let index = floor $ prob * fromIntegral (length l)
  return $ l !! (floor prob)  --  $ (l !! max 0 (min (length l - 1) index))

--uniform distribution
--given a double d, generates a random number between 0 and d, based on a uniform distribution
randNumber :: Double -> IO Double
randNumber n = do
  gen <- getStdGen
  let (p,newgen) = randomR (0, n) gen
  setStdGen newgen
  return p

--Returns the result of the n-step operational semantics for a given command C and state s given by
--a memoryless scheduler, which selects a configuration according to a uniform distribution
runSample :: C -> LMem -> Int -> IO (Mem,Double)
runSample c s n = do
  memList <- runDistT $ sample ((c,s),n) --the probabilities associated to the configurations in the distribution
  return $ (\((a,b,c),p) -> ((a,c),p) ) $ head memList

--improved display of the results of runNStepConf
--for example states are shown in bra-ket notation
showSample :: C -> LMem -> Int -> IO()
showSample c s n = do
  s' <- runSample c s n -- [(Mem,Double)]
  --putStrLn $ showProbMemList $ (limitPrecAux 10 s')
  putStrLn $ showProbMem s'  
--END: sampling from the k-step with a built-in memoryless scheduler--


--START: setting up data for building the Histogram--
--Building the histogram using the method that returns a configuration
-- showHist :: FileName -> (Nº Samples, ListVar, "ind" or "join") -> Command -> Memory -> k-step -> IO ExitCode
-- where "ind" creates a histogram for each variable in ListVar and "join" creates a histogram for
-- the variables in ListVar
showHist :: String -> (Int, [[String]]) -> C -> LMem -> Int -> IO ExitCode
showHist name (k, lvars) c s n = do
  listMem <- sampleCollect k c s n
  processData name lvars listMem

processData :: String -> [[String]] -> [Mem] -> IO ExitCode
processData _ [] _ = exitSuccess
processData name [lvar] listMem = processDataAux name lvar listMem
processData name (lvar:t) listMem = (processDataAux name lvar listMem) >> (processData name t listMem)

processDataAux :: String -> [String] -> [Mem] -> IO ExitCode
processDataAux name lvar listMem = do
  let listSC = map fst listMem
      --nameHist = name ++ " " ++ unwords lvar
      nameHist = name
      xlabel = unwords lvar
      lvarSC = [concat [filter (\(var',val) -> if var'==var then True else False) sc | var <- lvar] | sc <- listSC] -- [SC]
      valVarSC = [ (map snd varSC) | varSC <- lvarSC] -- [[Int]]
      sortValVarSC = sort valVarSC
      strSorted = [concat $ (map show) l | l <- sortValVarSC]
  buildHist (confIntoDouble strSorted) nameHist xlabel
  
--Executes runSample k times and stores the results obtained in each iteration
--If the probability of the result obtained is zero, the result is discarded; Furthermore, if after
--executing listNStepConf we obtain an empty list, then the plotting of the histogram will give an
--error
sampleCollect :: Int -> C -> LMem -> Int -> IO [Mem]
sampleCollect 0 c s n = return []
sampleCollect k c s n = do
    ((sc,sq),p) <- runSample c s n -- (Mem,Double)
    case p of
      0 -> sampleCollect (k-1) c s n
      otherwise -> do
        as <- sampleCollect (k-1) c s n
        return ((sc, limitPrecisionS 5 sq):as)
    

--functions for the indexes of the x-axis of the histogram 
confIntoInt :: [(String,Double)] -> [(Int,Double)]
confIntoInt l = zip [i | i <- [1 .. length l]] [p | (mem,p) <- l]

-- confIntoDouble [c_1,...,c_n] = [d_1,...,d_n], where d_m = integer corresponding to configuration
-- c_m (there is an integer corresponding to each different configuration in [c_1,...,c_n])
-- e.g. confIntoDouble [c1,c2,c3,c1,c3] = [(1.0,c1),(2.0,c2),(3.0,c3),(1.0,c1),(3.0,c3)]
confIntoDouble :: [String] -> [(Double, String)]
confIntoDouble l = zip (indexes l ldiff) l
  where ldiff = nub l

-- (indexes [x_1,...,x_n] l) = [i_1,...,i_n], where i_m = index of the first occurence of x_m in
-- list l, if all elements of [x1,...,xn] belong to l
indexes :: [String] -> [String] -> [Double]
indexes [] l = []
indexes (h1:t1) l2 = (listIndex h1 l2) : (indexes t1 l2)

-- listIndex x l = index of the first occurence of x in list l, if x is part of l. If l is not empty
-- and x is not part of it, an error occurs. The index of the first element of l is considered to be
-- 1.
listIndex :: String -> [String] -> Double
listIndex x [] = 0
listIndex x (h:t)
    | not (elem x (h:t)) = error (x ++ " does not belong to " ++ (show (h:t)))
    | (x==h) = 1
    | otherwise = 1 + (listIndex x t)
--END: setting up data for building the Histogram--





--START: Building the histogram--
-- Building a histogram with integer data, with one column for each integer, whose caption is the
-- respective state for each integer (x-1)

-- (histogramInt dataSet t) plots an histogram whose input is dataSet and whose title is t, with the following features:
---- dataSet is a list formed by tuples (i, mem_i), where 1 <= i <= n, with n being the number of different memories in dataSet
---- the bin size is 1, so there is one column for each integer; 
---- the range of the x-axis is from -1 to (n+1);
---- in the x-axis only integers from 0 to (n-1) are labeled;
---- for each x, there is an associated mem_i which is then used as the caption

-- The elements of dataSet are all expected to be integers.
--(title++xlabel++".png")
buildHist :: [(Double, String)] -> String -> String -> IO ExitCode
buildHist [] title _ = error "Empty input."
buildHist dataSet title xlabel = plotAdv "" options hist -- the filename (1st argument of plotAdv) is empty, so the histogram will appear on a new window
    where newDataSet = [(d-1,st) | (d,st) <- dataSet] -- moving the x coordinates of the labels to the left by one
          doubles =  [d | (d,st) <- newDataSet]
          max = round (maximum doubles) :: Int -- (maximum doubles) is of type Double, but max is of type Int; max is n
          hist = histogramBinSize 1 doubles -- (histogramBinSize 1 doubles) creates a histogram with bin size 1 and with doubles as its input
          options = Opt.title title $
                    Opt.xLabel xlabel $
                    Opt.xRange2d (-1,max+1) $
                    Opt.xTicks2d (xTicksData newDataSet) $
                    Opt.yLabel "Frequency" $
                    Opt.add (Option.yRange "bounds") ["[0:]"] $
                    Opt.gridYTicks True $
                    defOpts hist -- options for the histogram (title, range of the x-axis and labels of the x-axis)

-- xTicksData [(0.0, mem_1),(1.0, mem_2), ..., (n-1, mem_n)] = [("mem_1",0), ("mem_2",1), ... ("mem_n", n-1)]
xTicksData :: [(Double, String)] -> [(String, Int)]
xTicksData [] = []
xTicksData ((d,str):t) = (str, round d :: Int) : xTicksData t

-- -- The elements of dataSet are all expected to be integers.
-- buildHist :: [(Double,Mem)] -> String -> IO ExitCode
-- buildHist [] title = error "Empty input."
-- buildHist dataSet title = plotAdv "" options hist -- the filename (1st argument of plotAdv) is empty, so the histogram will appear on a new window
--     where newDataSet = [(d-1,st) | (d,st) <- dataSet] -- moving the x coordinates of the labels to the left by one
--           doubles =  [d | (d,st) <- newDataSet]
--           max = round (maximum doubles) :: Int -- (maximum doubles) is of type Double, but max is of type Int; max is n
--           hist = histogramBinSize 1 doubles -- (histogramBinSize 1 doubles) creates a histogram with bin size 1 and with doubles as its input
--           options = Opt.title title $ Opt.xRange2d (-1,max+1) $ Opt.xTicks2d (xTicksData newDataSet) (defOpts hist) -- options for the histogram (title, range of the x-axis and labels of the x-axis)

-- -- xTicksData [(0.0, mem_1),(1.0, mem_2), ..., (n-1, mem_n)] = [("mem_1",0), ("mem_2",1), ... ("mem_n", n-1)]
-- xTicksData :: [(Double, Mem)] -> [(String, Int)]
-- xTicksData [] = []
-- xTicksData ((d,mem):t) = (memToString mem, round d :: Int) : xTicksData t
--END: Building the histogram--


-- Consultei, para fazer este código:

-- http://learnyouahaskell.com/
-- https://hackage.haskell.org/
-- especificamente:
-- https://hackage.haskell.org/package/Histogram-0.1.0.2/docs/Graphics-Histogram.html (contains a useful example)
-- https://hackage.haskell.org/package/gnuplot-0.5.7/src/src/Demo.hs (also contains some useful examples)

-- https://wiki.haskell.org/Converting_numbers (mentions useful functions for converting numbers)
