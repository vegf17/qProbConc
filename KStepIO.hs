module KStepIO where

--Haskell imports--
import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
--import Control.Monad
--import Control.Monad.Identity
import System.Random
-- import Data.Either
-- import Data.List
-- import Data.Ord
-- import Data.Complex
--import Data.Ratio
--import Data.Char
--import Data.Fixed
--import Data.Matrix
--Haskell imports--

--my imports--
import Syntax
import SemBE_Brookes
import SmallStep
import Examples
import ProbTMonad
import QuantumCalc
import Beautify
--my imports--


--START: k-step convex coefficients explicit with IO--
--The scheduler here needs to be given as an input, contrarily to what happens in runNStepSch,
--runNStepSchFullConv, and runNStepConf, in which we consider a pre-defined scheduler

--SchedulerIO: ProbPath --> V(V(S + X)) + {*}  ==> ProbPath --> Maybe V(V(S+X))
--Allows the creation of schedulers that use random
--if to use, the definition needs to be changed
type SchIO = ProbPath -> MaybeT IO [([(Either LMem (C, LMem), Double)], Double)]


--Returns the result of the k-step operational semantics for a given scheduler Sch, command C, state
--s, and empty history
runKStepSchIO :: SchIO -> C -> LMem -> Int -> IO [([(Mem, Double)],Double)]
runKStepSchIO sch c lmem k = rmvIOLII $ runProbT $ runProbT $ kStepSchIO (sch,([],(c,lmem)),k)

--improved display of the results of runNStepSch
--for example quantum states are shown in bra-ket notation
showKStepSchIO :: SchIO -> C -> LMem -> Int -> IO()
showKStepSchIO sch c s n = do
  s' <- runKStepSchIO sch c s n -- [([(Mem, Double)],Double)]
  putStrLn $ showProbProbMemList $ (limPrecKStep 5 s')

--codifies the k-step semantics
kStepSchIO :: (SchIO, ProbPath, Int) -> ProbT (ProbT IO) LMem
kStepSchIO (_, path, 0) = ProbT $ ProbT $ return [([(snd $ snd path, 0)],0)]
kStepSchIO (sch, l@(path, (c, s)), k) = do
  appSch <- lift.lift $ runMaybeT $ sch l
  case appSch of
    Nothing -> error "Scheduler undefined"
    Just convDist -> do 
      let ppL = [(projL dist, q) | (dist, q) <- convDist] -- [([(S, Double)], Double)]
          --next_eval = [([((sch, (path ++ [((c, s), dist)], cs), k-1), p) | (cs, p) <- (projR dist)] ,q) | (dist, q) <- convDist]
          next_eval = [([((sch, ([], cs), k-1), p) | (cs, p) <- (projR dist)] ,q) | (dist, q) <- convDist]
          transStep = (ProbT $ ProbT $ return next_eval) >>= kStepSchIO
      --joinProbTProbTIO transStep (ProbT $ ProbT $ return ppL)
      joinProbTProbTIO transStep (ProbT $ ProbT $ return ppL)


-- joinProbTProbTIO :: (Eq x) => ProbT (ProbT IO) x  -> ProbT (ProbT IO) x -> ProbT (ProbT IO) x
-- joinProbTProbTIO (ProbT probtiopsi) (ProbT probtiophi) = let dist = addProbTIOG probtiopsi probtiophi
--                                                          in ProbT $ dist
joinProbTProbTIO :: (Eq x) => ProbT (ProbT IO) x  -> ProbT (ProbT IO) x -> ProbT (ProbT IO) x
joinProbTProbTIO (ProbT (ProbT iopsi)) (ProbT (ProbT iophi)) = do
  psi <- lift.lift $ iopsi -- [([(x,Double)], Double)]
  phi <- lift.lift $ iophi -- [([(x,Double)], Double)]
  ProbT $ ProbT $ return $ (filtro psi) ++ (filtro phi)
    where filtro = filter (\(dist,_) -> not (null dist))


-- addProbTProbT :: (Eq x) => [([(x, Double)], Double)] -> [([(x, Double)], Double)] -> [([(x, Double)], Double)]
-- addProbTProbT [] [] = []
-- addProbTProbT [([],_)] l = filtro l
--   where filtro = filter (\(dist,_) -> not (null dist))
-- addProbTProbT l [([],_)] = filtro l
--   where filtro = filter (\(dist,_) -> not (null dist))
-- addProbTProbT l1 l2 = (filtro l1) ++ (filtro l2)
--   where filtro = filter (\(dist,_) -> not (null dist))

joinProbTProbTIOII :: (Eq x) => ProbT (ProbT IO) x  -> ProbT (ProbT IO) x -> ProbT (ProbT IO) x
joinProbTProbTIOII (ProbT (ProbT iopsi)) (ProbT (ProbT iophi)) = do
  psi <- lift.lift $ iopsi -- [([(x,Double)], Double)]
  phi <- lift.lift $ iophi -- [([(x,Double)], Double)]
  ProbT $ ProbT $ return (psi++phi)
--END: k-step convex coefficients explicit with IO--

--START: k-step with IO--
--The scheduler here needs to be given as an input, contrarily to what happens in runNStepSch,
--runNStepSchFullConv, and runNStepConf, in which we consider a pre-defined scheduler

--Returns the result of the k-step operational semantics for a given scheduler Sch, command C, state
--s, and empty history
runKStepSchIIIO :: SchIO -> C -> LMem -> Int -> IO [(Mem, Double)]
runKStepSchIIIO sch c lmem k = rmvIOL $ runProbT $ kStepSchIIIO (sch,([],(c,lmem)),k)

--improved display of the results of runNStepSch
--for example quantum states are shown in bra-ket notation
showKStepSchIIIO :: SchIO -> C -> LMem -> Int -> IO()
showKStepSchIIIO sch c s n = do
  s' <- runKStepSchIIIO sch c s n -- [([(Mem, Double)],Double)]
  --putStrLn $ showProbMemList $ (limitPrecAux 5 s')
  putStrLn $ showProbMemList s'

--codifies the k-step semantics
kStepSchIIIO :: (SchIO, ProbPath, Int) -> ProbT IO LMem
kStepSchIIIO (_, path, 0) = ProbT $ return [(snd $ snd path, 0)]
kStepSchIIIO (sch, l@(path, (c, s)), k) = do
  appSch <- lift $ runMaybeT $ sch l 
  case appSch of
    Nothing -> error "Scheduler undefined"
    Just convDist -> do 
      let ppL = [ [ (s, p*q) | (s, p) <- (projL dist)] | (dist, q) <- convDist] -- [([(S, Double)], Double)]
          next_eval = [[((sch, (path ++ [((c, s), dist)], cs), k-1), p*q) | (cs, p) <- (projR dist)] | (dist, q) <- convDist]
          --next_eval = [[((sch, ([], cs), k-1), p*q) | (cs, p) <- (projR dist)] | (dist, q) <- convDist]
          transStep = (ProbT $ return $ concat next_eval) >>= kStepSchIIIO
      addProbTIOG transStep (ProbT $ return $ concat ppL)
--END: k-step with IO--


--START: definition of schedulers with IO--
detSch :: SchIO
detSch path = do
  dist <- lift $ pathSch path --[(Either LMem (C,LMem), Double)]
  MaybeT $ return $ Just [(dist,1)]

probSch :: SchIO
probSch (path,(c,s)) = do
  let list = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  l <- lift $ convClosure list -- [([(Either LMem (C,LMem), Double)],Double)]
  MaybeT $ return $ Just l
--END: definition of schedulers with IO--

--START: auxiliary functions for schedulers with IO--
--IO [(Either S (C,S), Double)]
--Given a path/history, returns the next distribution to be evaluated according to a uniform
--distribution
pathSch :: ProbPath -> IO [(Either LMem (C,LMem), Double)]
pathSch (path,(c,s)) = do
  let list = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either S (C,S), Double)]]
  next <- nextStep list -- [(Either LMem (C,LMem), Double)]
  return next

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

--Does the convex closure of a list by normalizing the convex coefficients obtained from
--listConvCoef
convClosure :: [a] -> IO [(a, Double)]
convClosure [] = return []
convClosure l = do
  coefs <- listConvCoef (length l) 1.0
  let norm = sum coefs
      coefsNorm = map (/norm) coefs 
  return $ zip l coefsNorm

--Creates a list of size n with convex coefficients
listConvCoef :: Int -> Double -> IO [Double]
listConvCoef 0 _ = return []
listConvCoef n d = do
  q <- randNumber d
  l <- listConvCoef (n-1) 1.0
  return $ q : l  
--END: auxiliary functions for schedulers with IO--
