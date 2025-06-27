module KStep where

--Haskell imports--
import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Except
--import Control.Monad.Trans.Maybe
--import Control.Monad
--import Control.Monad.Identity
--import System.Random
--import Data.Either
--import Data.List
--import Data.Ord
--import Data.Complex
--import Data.Ratio
--import Data.Char
--import Data.Fixed
--import Data.Matrix
--Haskell imports--

--my imports--
import Syntax
import SmallStep
import Examples
import ProbTMonad
import Beautify
--my imports--


--START: k-step--
--Scheduler: ProbPath --> V(V(S + (C x S))) + {*}  ==> ProbPath --> Maybe V(V(S + (C x S)))
type Sch = ProbPath -> Maybe [([(Either LMem (C, LMem), Double)], Double)]

--Returns the result of the k-step operational semantics for a given scheduler Sch, command C, state
--s, and empty history
runKStepSch :: Sch -> C -> LMem -> Int -> [(Mem, Double)]
runKStepSch sch c lmem k = cleanL $ getProb $ kStepSch (sch,([],(c,lmem)),k)
  where cleanL = map (\((sc,l,sq),p) -> ((sc,sq),p))

--improved display of the results of runNStepSch
--for example quantum states are shown in bra-ket notation
showKStepSch :: Sch -> C -> LMem -> Int -> IO()
showKStepSch sch c s n = let s' = runKStepSch sch c s n -- [([(Mem, Double)],Double)]
                         in putStrLn $ showProbMemList s'
                         --in putStrLn $ showProbMemList $ (limitPrecAux 5 s')

--codifies the k-step semantics
kStepSch :: (Sch, ProbPath, Int) -> MyDist LMem
kStepSch (_, path, 0) = MyDist [(snd $ snd path, 0)]
kStepSch (sch, l@(path, (c, s)), k) = 
  case (sch l) of
    Nothing -> error "Scheduler undefined"
    Just convDist -> do 
      let ppL = [ [ (s, p*q) | (s, p) <- (projL dist)] | (dist, q) <- convDist] -- [([(S, Double)], Double)]
          next_eval = [[((sch, (path ++ [((c, s), dist)], cs), k-1), p*q) | (cs, p) <- (projR dist)] | (dist, q) <- convDist]
          transStep = (MyDist $ concat next_eval) >>= kStepSch
      addMyDist transStep (MyDist $ concat ppL)
--END: k-step--



--START: definition of schedulers without IO--
--undefined scheduler
undScheduler :: Sch
undScheduler _ = Nothing

--scheduler that chooses always the first element in the list of the possible transitions
initScheduler :: Sch
initScheduler (path,(c,s)) =
  let listDist = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  in Just $ [(head listDist, 1)]

--scheduler that chooses always the last element in the list of the possible transitions
lastScheduler :: Sch
lastScheduler (path,(c,s)) =
  let listDist = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  in Just $ [(last listDist, 1)]

--scheduler that chooses always the middle element in the list of the possible transitions
middleScheduler :: Sch
middleScheduler (path,(c,s)) =
  let listDist = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
      ind = fromIntegral $ floor ((fromIntegral $ length listDist)/2)
  in Just $ [(listDist!!ind, 1)]  

--probabilistic scheduler, which attributes the same probability to all the elements in the list of
--the possible transitions
probScheduler :: Sch
probScheduler (path,(c,s)) =
  let listDist = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
      len = fromIntegral $ length listDist
  in Just $ [(dist, 1/len) | dist <- listDist]
--END: definition of schedulers without IO--
