module KStep where

--Haskell imports--
import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Except
--import Control.Monad.Trans.Maybe
--import Control.Monad
--import Control.Monad.Identity
--import System.Random
import Data.Either
import Data.List
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
undSch :: Sch
undSch _ = Nothing

--scheduler that chooses always the first element in the list of the possible transitions
initSch :: Sch
initSch (path,(c,s)) =
  let listDist = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  in Just $ [(head listDist, 1)]

--scheduler that chooses always the last element in the list of the possible transitions
lastSch :: Sch
lastSch (path,(c,s)) =
  let listDist = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  in Just $ [(last listDist, 1)]

--scheduler that chooses always the middle element in the list of the possible transitions
middleSch :: Sch
middleSch (path,(c,s)) =
  let listDist = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
      ind = fromIntegral $ floor ((fromIntegral $ length listDist)/2)
  in Just $ [(listDist!!ind, 1)]  

--probabilistic scheduler, which attributes the same probability to all the elements in the list of
--the possible transitions
probSch :: Sch
probSch (path,(c,s)) =
  let listDist = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
      len = fromIntegral $ length listDist
  in Just $ [(dist, 1/len) | dist <- listDist]



--fair scheduler that checks the history path to see if the current command and the classical state
--already occurred; if yes, then the scheduler tries another option

--TODO: TEST THIS 
fairSch :: Sch
fairSch hist@(path,(c,s)) = 
  let listDist = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
      nextDist = notRepeated hist (head listDist, tail listDist)
  in Just $ [(nextDist, 1)]

--Receives a probabilistic path, a tuple composed of the scheduled next step and all the unscheduled
--next steps, and returns a next step that has not occur yet; in the case where all the possible
--next steps, scheduled or unscheduled, already appeared in the probabilistic path, the next step
--chosen is the last one unscheduled
notRepeated :: ProbPath -> ([(Either LMem (C,LMem), Double)],[[(Either LMem (C,LMem), Double)]]) -> [(Either LMem (C,LMem), Double)]
notRepeated _ (next,[]) = next
notRepeated (path,(c,s)) (next,(h:t)) = if (allLeft || null intersection ) -- if next is composed only by final states or it did not appear in the history --  || allIn listComProbPath nextR
                                        then next -- then return next
                                        else notRepeated (path,(c,s)) (h,t) -- else repeat the procedure by giving as next step the head of the list of the unscheduled next steps
  where allLeft = and $ map (\x -> isLeft x) (map fst next) --verifies if all the elements of next are final states
        nextL = lefts (map fst next) --collects the final states from next
        nextR = rights (map fst next) --collects the computations that did not finish
        nextRClassic = map (\(c,(sc,l,sq)) -> (c,sc)) nextR --collects the computations that did not finish only considering the command and the classical state
        listComProbPath = map fst path --list with all the commands and states that occurred during the computation
        listComProbPathClassic = map (\(c,(sc,l,sq)) -> (c,sc)) listComProbPath --list with all the commands and states that occurred during the computation only considering the command and the classical state
        intersection = intersect listComProbPathClassic nextRClassic --checks if the unfinished computations in nextR are present in the history
--END: definition of schedulers without IO--
