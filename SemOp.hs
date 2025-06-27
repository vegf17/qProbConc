module SemOp where

--Haskell imports--
import Control.Monad.State
import Control.Monad.Trans 
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Identity
import System.Random
import Data.Either
import Data.List
import Data.Ord
import Data.Complex
import Data.Ratio
import Data.Char
import Data.Fixed
import Data.Matrix
--import Data.Random.Normal --cabal install normaldistribution
--import Data.Random -- :set -package random-fu
--import Data.Random.Distribution.Normal
--import Numeric (showFFloat)
--import Numeric.Probability.Distribution hiding (map, lift, filter)
  --for the use we make of probabilities, what we have defined is sufficient
  --later versions can make use of this package
--Haskell imports--

--import for graphics--
import HistogramSem
import Numeric.Probability.Game.Event
import System.Exit
--import for graphics--

--my imports--
import Syntax
import SemBE_Brookes
import Examples
import ProbTMonad
import QuantumCalc
import Beautify
--my imports--

{-NEED TO UPDATE THIS
Important functions of this module:
· showNStepQCSch :: C -> LMem -> Int -> IO()
  (showNStepQCSch com lmem n) where:
  - com is a command
  - lmem is something like (classical state, linking function, density operator)
  - n is the number of steps the n-step is going to execute
 output:
  a beautified form of a probability distribution composed pairs of classical and quantum states

· histogramNStep :: String -> Int -> C -> LMem -> Int -> IO ExitCode
  (histogramNStep name rep com lmem n) where:
  - name is the title of the histogram
  - rep is the number of times the histogram is going to repeat the execution of com
  - com is a command
  - lmem is something like (classical state, linking function, density operator)
  - n is the number of steps the n-step is going to execute
 output:
  a histogram
-}


--precision for doubles
precision = 5

--START: semantics for the await command--
--Type definition for the semantics of the await command
--StTP C '=' S -> [(Either S (C,S), Prob)]
type StTP a = StateT LMem (ExceptT LMem MyDist) a

--Codifies the behavior of the small-step operational semantics for commands inside await
smallAwait :: CAwait -> StTP CAwait
smallAwait SkipA = StateT $ \s -> throwE s
smallAwait (AsgA var e) = StateT $ \(sc,l,sq) -> throwE $ (changeSt var (bigStepExp e sc) sc, l, sq)
smallAwait (ResetA q) = StateT $ \(sc,l,sq) -> throwE $ (sc, l, resetOpDen (qNumsAux q l) sq) 
smallAwait (UA g qvar) = StateT $ \(sc,l,sq) -> throwE $ (sc, l, appGateOpDen g (qNums qvar l) sq)
smallAwait (MeasA (x,q)) = do
  (sc,l,sqt) <- get
  let sq = zeroIfSmallS sqt
      p0 = probOpDen 0 ((qNumsAux q l)) sq -- probability of measuring qubit q to be in state |0>
      p1 = probOpDen 1 ((qNumsAux q l)) sq -- probability of measuring qubit q to be in state |1>
      sq0 = stateMeasOpDen 0 ((qNumsAux q l)) sq -- state of the system of qubits after measuring qubit q in state |0>
      sq1 = stateMeasOpDen 1 ((qNumsAux q l)) sq -- state of the system of qubits after measuring qubit q in state |1>
      sc0 = changeSt x 0 sc -- assigning the value 0 to the variable x
      sc1 = changeSt x 1 sc -- assigning the value 1 to the variable x
  if (p0==0.0)
    then StateT $ \_ -> ExceptT $ MyDist $ [(Left (sc1,l,sq1),p1)]
    else if (p1==0.0)
    then StateT $ \_ -> ExceptT $ MyDist $ [(Left (sc0,l,sq0),p0)]
    else StateT $ \_ -> ExceptT $ MyDist $ [(Left (sc0,l,sq0),p0), (Left (sc1,l,sq1),p1)]
smallAwait (SeqA c1 c2) = do 
    s <- get 
    let cp = getProb $ runExceptT $ runStateT (smallAwait c1) s  -- :: [(Either LMem (Com, LMem), Rational)]
        seqC = compSeqA cp c2
    StateT $ \_ -> ExceptT $ MyDist $ seqC
smallAwait (IfCA bExp c1 c2) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
  if b == True
    then StateT $ \_ -> ExceptT $ MyDist $ [(Right (c1,(sc,l,sq)),1.0)]
    else StateT $ \_ -> ExceptT $ MyDist $ [(Right (c2,(sc,l,sq)),1.0)]

--auxiliary functions for sequential composition inside await
compSeqA :: [(Either LMem (CAwait,LMem), Double)] -> CAwait -> [(Either LMem (CAwait,LMem), Double)]
compSeqA [] _ = []
compSeqA ((Left s, p) : t) c = (Right (c,s), p) : compSeqA t c
compSeqA ((Right (cc, s),p) : t) c = (Right (SeqA cc c, s), p) : compSeqA t c    

--Codifies the behavior of the big-step semantics for await commands
bigAwait :: (CAwait,LMem) -> MyDist LMem
bigAwait (c,s) = do
  let small = getProb $ runExceptT $ runStateT (smallAwait c) s --[(Either LMem (CAwait, LMem), Double)]
      sts = projL small -- [(LMem, Double)]
      confs = projR small -- [((CAwait, LMem), Double)]
      result = (MyDist $ confs) >>= bigAwait
  MyDist $ addDistG sts (getProb result)

--improved display of the results of bigAwait
--for example quantum states are shown in bra-ket notation
showBigAwait :: CAwait -> LMem -> IO()
showBigAwait c s = putStrLn $ showProbMemList $ f $ getProb $ bigAwait (c,s)
  where f :: [((LMem, Double))] -> [(Mem, Double)]
        f l = [((sc,sq),p) | ((sc,l,sq),p) <- l]
--END: semantics for the await command--

--START: small-step semantics--
-- StTQC C '=' S -> [[(Either S (C,S), Prob)]]
type StTQC a = StateT LMem (ExceptT LMem (ProbT [])) a --small

--Codifies the behavior of the small-step operational semantics
small :: C -> StTQC C
small Skip = StateT $ \s -> throwE s
small (Asg var e) = StateT $ \(sc,l,sq) -> throwE $ (changeSt var (bigStepExp e sc) sc, l, sq)
small (Reset q) = StateT $ \(sc,l,sq) -> throwE $ (sc, l, resetOpDen (qNumsAux q l) sq) 
small (U g qvar) = StateT $ \(sc,l,sq) -> throwE $ (sc, l, appGateOpDen g (qNums qvar l) sq) 
small (P p c1 c2) = do
  s <- get
  let cc1 = runProbT $ runExceptT $ runStateT (small c1) s  -- :: [[(Either S (Com, S), Rational)]]
      cc2 = runProbT $ runExceptT $ runStateT (small c2) s  -- :: [[(Either S (Com, S), Rational)]]
      pc1 = [[(c,p*p')| (c,p') <- lcc1] | lcc1 <- cc1]
      pc2 = [[(c,(1-p)*p')| (c,p') <- lcc2] | lcc2 <- cc2]
      pc1c2 = concat [[ec1++ec2 | ec2 <- pc2] | ec1 <- pc1]
  StateT $ \_ -> ExceptT $ ProbT $ pc1c2
small (Meas (x,q)) = do
  (sc,l,sqt) <- get
  let sq = zeroIfSmallS sqt
      p0 = probOpDen 0 ((qNumsAux q l)) sq -- probability of measuring qubit q to be in state |0>
      p1 = probOpDen 1 ((qNumsAux q l)) sq -- probability of measuring qubit q to be in state |1>
      sq0 = stateMeasOpDen 0 ((qNumsAux q l)) sq -- state of the system of qubits after measuring qubit q in state |0>
      sq1 = stateMeasOpDen 1 ((qNumsAux q l)) sq -- state of the system of qubits after measuring qubit q in state |1>
      sc0 = changeSt x 0 sc -- assigning the value 0 to the variable x
      sc1 = changeSt x 1 sc -- assigning the value 1 to the variable x
  if (p0==0.0)
    then StateT $ \_ -> ExceptT $ ProbT $ [[(Left (sc1,l,sq1),p1)]]
    else if (p1==0.0)
    then StateT $ \_ -> ExceptT $ ProbT $ [[(Left (sc0,l,sq0),p0)]]
    else StateT $ \_ -> ExceptT $ ProbT $ [[(Left (sc0,l,sq0),p0), (Left (sc1,l,sq1),p1)]]
small (Seq c1 c2) = do 
    s <- get 
    let cp = runProbT $ runExceptT $ runStateT (small c1) s  -- :: [[(Either LMem (Com, LMem), Rational)]]
        seqC = [compSeq dist c2| dist <- cp]
    StateT $ \_ -> ExceptT $ ProbT $ seqC
small (Or c1 c2) = do 
    s <- get 
    let cp1 = runProbT $ runExceptT $ runStateT (small c1) s -- :: [[(Either LMem (Com, LMem), Rational)]]
        cp2 = runProbT $ runExceptT $ runStateT (small c2) s -- :: [[(Either LMem (Com, LMem), Rational)]]
    StateT $ \_ -> ExceptT $ ProbT $ (cp1++cp2) 
small (Paral c1 c2) = do 
    s <- get 
    let cp1 = runProbT $ runExceptT $ runStateT (small c1) s -- :: [[(Either LMem (Com, LMem), Rational)]]
        cp2 = runProbT $ runExceptT $ runStateT (small c2) s -- :: [[(Either LMem (Com, LMem), Rational)]]
        par1 = [compParR dist c2| dist <- cp1]
        par2 = [compParL dist c1| dist <- cp2]
    StateT $ \_ -> ExceptT $ ProbT $ par1++par2
small (IfC bExp c1 c2) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
  if b == True
    then StateT $ \_ -> ExceptT $ ProbT $ [[(Right (c1,(sc,l,sq)),1.0)]]
    else StateT $ \_ -> ExceptT $ ProbT $ [[(Right (c2,(sc,l,sq)),1.0)]]
small (Whl bExp c) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
  if b == True
    then StateT $ \_ -> ExceptT $ ProbT $ [[(Right (Seq c (Whl bExp c),(sc,l,sq)),1)]]
    else StateT $ \_ -> ExceptT $ ProbT $ [[(Right (Skip,(sc,l,sq)),1)]]
small (Await bExp c) = do
  (sc,l,sq) <- get
  let b = bigStepBExp bExp sc
  if b == False
    then StateT $ \_ -> ExceptT $ ProbT $ [[(Right (Await bExp c,(sc,l,sq)),1)]]
    else StateT $ \_ -> ExceptT $ ProbT $ [[(Left (sci, li, sqi), pi) | ((sci,li,sqi),pi) <- getProb $ bigAwait (c,(sc,l,sq))]]

--auxiliary functions for sequential and parallel composition
compSeq :: [(Either LMem (C,LMem), Double)] -> C -> [(Either LMem (C,LMem), Double)]
compSeq [] _ = []
compSeq ((Left s, p) : t) c = (Right (c,s), p) : compSeq t c
compSeq ((Right (cc, s),p) : t) c = (Right (Seq cc c, s), p) : compSeq t c

compParR :: [(Either LMem (C,LMem), Double)] -> C -> [(Either LMem (C,LMem), Double)]
compParR [] _ = []
compParR ((Left s, p) : t) c = (Right (c,s), p) : compParR t c
compParR ((Right (cc, s),p) : t) c = (Right (Paral cc c, s), p) : compParR t c

compParL :: [(Either LMem (C,LMem), Double)] -> C -> [(Either LMem (C,LMem), Double)]
compParL [] _ = []
compParL ((Left s, p) : t) c = (Right (c,s), p) : compParL t  c
compParL ((Right (cc, s),p) : t) c = (Right (Paral c cc, s), p) : compParL t c

--Evaluates the results of the small-step operational semantics for a given command C and state s
runSmall :: C -> LMem -> [[(Either Mem (C,Mem), Double)]]
runSmall c s = rmvL $ runProbT $ runExceptT $ runStateT (small c) s
--END: small-step semantics--

--START: k-step--
--ProbPath: (X x V(S+X))* x X ==> ([(X, V(S + X))], X)
-- X = (C,LMem)
-- V(S+X) = [(Either S X, Double)]
-- ([((C,LMem), [(Either LMem (C,LMem), Double)])], (C,LMem))
type ProbPath = ([((C,LMem), [(Either LMem (C,LMem),Double)])], (C,LMem))

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



--START: nstep that chooses a distribution to be evaluated next--
--The scheduler used here chooses the next distribution to be evaluated from a set of distributions;
--the scheduler has memory, following the approach of the article; note that this may entail loss of
--efficiency

--Returns the result of the n-step operational semantics for a given command C, state s, and empty
--history, given by a scheduler, which selects a distribution according to a uniform distribution
runNStepSch :: C -> LMem -> Int -> IO [(Mem,Double)]
runNStepSch c s n = rmvIOL $ runProbT $ nstepSch (([],(c,s)),n)

--improved display of the results of runNStepSch
--for example quantum states are shown in bra-ket notation
showNStepSch :: C -> LMem -> Int -> IO()
showNStepSch c s n = do
  s' <- runNStepSch c s n -- [(Mem,Double)]
  putStrLn $ showProbMemList $ (limitPrecAux 5 s')

--codify the n-step semantics, for which a distribution is selected according to a uniform
--distribution
nstepSch :: (ProbPath,Int) -> ProbT IO LMem
nstepSch (l,0) = ProbT $ return [(snd $ snd l, 0)] --From the original semantics
nstepSch (l@(path,(c,s)),n) = do
  nextDist <- lift $ pathSch l -- [(Either S (C,S), Double)] 
  let pL = projL nextDist -- [(S, Double)]
      pR = projR nextDist -- [((C,S), Double)]
      ppR = [ (((path ++ [((c, s), nextDist)], cs), n-1), p) | (cs,p) <- pR, p/=0] --removing elements with probability 0
      transStep = (ProbT $ return ppR) >>= nstepSch -- ProbT IO S
  addProbTIOG transStep (ProbT $ return pL)
--END: nstep that chooses a distribution to be evaluated next--


--START: nstep with a scheduler that does the convex closure of a set of distributions--
--Incorporating the probabilities from the convex closure in the distributions
--Returns the result of the n-step operational semantics for a given command C, state s, and empty
--history, given by a scheduler, which multiplies the convex coefficients by the probabilities in
--the distributions
runNStepSchFullConv :: C -> LMem -> Int -> IO [(Mem,Double)]
runNStepSchFullConv c s n = rmvIOL $ runProbT $ nstepSchFullConv (([],(c,s)),n)

--improved display of the results of runNStepSchConv
--for example states are shown in bra-ket notation
showNStepSchFullConv :: C -> LMem -> Int -> IO()
showNStepSchFullConv c s n = do
  s' <- runNStepSchFullConv c s n -- [(Mem,Double)]
  putStrLn $ showProbMemList $ (limitPrecAux 5 s')

--codify the n-step semantics, in which convex coefficients are incorporated into distributions
nstepSchFullConv :: (ProbPath,Int) -> ProbT IO LMem
nstepSchFullConv (l,0) = ProbT $ return [(snd $ snd l, 0)]
nstepSchFullConv (l@(path,(c,s)),n) = do
  nextDist <- lift $ pathSchFullConv l -- [(Either S (C,S), Double)] 
  let pL = projL nextDist -- [(S, Double)]
      pR = projR nextDist -- [((C,S), Double)]
      ppR = [ (((path ++ [((c, s), nextDist)], cs), n-1), p) | (cs,p) <- pR, p/=0] --removing elements with probability 0
      transStep = (ProbT $ return ppR) >>= nstepSchFullConv -- ProbT IO S
  addProbTIOG transStep (ProbT $ return pL)

--Given a path/history, returns the a distribution that incorporates the convex coefficients into
--the distributions
pathSchFullConv :: ProbPath -> IO [(Either LMem (C,LMem), Double)]
pathSchFullConv (path,(c,s)) = do
  let list = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  l <- convClosure list -- [([(Either LMem (C,LMem), Double)],Double)]
  return $ concat [ [ (cs, p*q) | (cs, p) <- dist] | (dist, q)  <- l]
--END: nstep with a scheduler that does the convex closure of a set of distributions--


--START: nstep with a scheduler that chooses the next command to be executed--
--The scheduler used here chooses a configuration by first selecting a distribution from a set of
--distributions (using a uniform distribution), and a configuration from the distribution based on
--the probabilities associated to the configurations; the scheduler is memoryless, i.e. it does not
--save the history of the computation

--Returns the result of the n-step operational semantics for a given command C and state s given by
--a memoryless scheduler, which selects a configuration according to a uniform distribution and then
runNStepConf :: C -> LMem -> Int -> IO (Mem,Double)
runNStepConf c s n = do
  memList <- runProbT $ nstepConf ((c,s),n) --the probabilities associated to the configurations in the distribution
  return $ (\((a,b,c),p) -> ((a,c),p) ) $ head memList


--runNStepConf :: C -> LMem -> Int -> IO [(Mem,Double)]
--runNStepConf c s n = rmvIOL $ runProbT $ nstepConf ((c,s),n)

--improved display of the results of runNStepConf
--for example states are shown in bra-ket notation
showNStepConf :: C -> LMem -> Int -> IO()
showNStepConf c s n = do
  s' <- runNStepConf c s n -- [(Mem,Double)]
  --putStrLn $ showProbMemList $ (limitPrecAux 10 s')
  putStrLn $ showProbMem s'

--nstep where after a small-step a configuration is chosen to be executed next
nstepConf :: ((C,LMem),Int) -> ProbT IO LMem
nstepConf ((c,s),0) = ProbT $ return [(s, 0)] --From the original semantics
nstepConf ((c,s),n) = do
  nextDist <- lift $ memorylessSch (c,s) -- [(Either S (C,S), Double)]
  nextStep <- lift $ eventNextStep nextDist -- (Either S (C,S), Double)
  case nextStep of
    (Left lmemi, pi) -> ProbT $ return [(lmemi, pi)]
    (Right (ci,si), pi) -> do
      let ppR = [(((ci,si), n-1), pi)] 
          --transStep = (ProbT $ return ppR) >>= nstepSch -- ProbT IO S
      (ProbT $ return ppR) >>= nstepConf

--IO [(Either S (C,S), Double)]
--Given a configuration, returns the next distribution to be evaluated according to a uniform
--distribution
memorylessSch :: (C,LMem)  -> IO [(Either LMem (C,LMem), Double)]
memorylessSch ((c,s)) = do
  let list = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either S (C,S), Double)]]
  next <- nextStep list -- [(Either LMem (C,LMem), Double)]
  return next

eventNextStep :: [(Either LMem (C,LMem), Double)] -> IO (Either LMem (C,LMem), Double)
eventNextStep [] = error "eventNextStep: list should be non-empty"
eventNextStep l = do
  n <- enact $ makeEventProb list -- int (the index of l that should be returned)
  return $ l!!n
  where list = zip [i | i <-[0 .. length l]] [p | (a,p) <- l]
--END: nstep with a scheduler that chooses the next command to be executed--


--START: Histogram--
--Building the histogram using the method that returns a configuration
histogramNStepConf :: String -> Int -> C -> LMem -> Int -> IO ExitCode
histogramNStepConf name k c s n = do
  input <- listNStepConf k c s n -- [Mem]
  let sortInput = sortBy (comparing fst) input
  histogramInt (confIntoDouble sortInput) name
  
--Executes runNStepConf k times and stores the results obtained in each iteration
--If the probability of the result obtained is zero, the result is discarded; Furthermore, if after
--executing listNStepConf we obtain an empty list, then the plotting of the histogram will give an
--error
listNStepConf :: Int -> C -> LMem -> Int -> IO [Mem]
listNStepConf 0 c s n = return []
listNStepConf k c s n = do
    ((sc,sq),p) <- runNStepConf c s n -- (Mem,Double)
    case p of
      0 -> listNStepConf (k-1) c s n
      otherwise -> do
        as <- listNStepConf (k-1) c s n
        return ((sc, limitPrecisionS 5 sq):as)
    

--functions for the indexes of the x-axis of the histogram 
confIntoInt :: [(Mem,Double)] -> [(Int,Double)]
confIntoInt l = zip [i | i <- [1 .. length l]] [p | (mem,p) <- l]

-- confIntoDouble [c_1,...,c_n] = [d_1,...,d_n], where d_m = integer corresponding to configuration
-- c_m (there is an integer corresponding to each different configuration in [c_1,...,c_n])
-- e.g. confIntoDouble [c1,c2,c3,c1,c3] = [(1.0,c1),(2.0,c2),(3.0,c3),(1.0,c1),(3.0,c3)]
confIntoDouble :: [Mem] -> [(Double,Mem)]
confIntoDouble l = zip (indexes l ldiff) l
  where ldiff = nub l

-- (indexes [x_1,...,x_n] l) = [i_1,...,i_n], where i_m = index of the first occurence of x_m in
-- list l, if all elements of [x1,...,xn] belong to l
indexes :: [Mem] -> [Mem] -> [Double]
indexes [] l = []
indexes (h1:t1) l2 = (listIndex h1 l2) : (indexes t1 l2)

-- listIndex x l = index of the first occurence of x in list l, if x is part of l. If l is not empty
-- and x is not part of it, an error occurs. The index of the first element of l is considered to be
-- 1.
listIndex :: Mem -> [Mem] -> Double
listIndex x [] = 0
listIndex x (h:t)
    | not (elem x (h:t)) = error ((show x) ++ " does not belong to " ++ (show (h:t)))
    | (x==h) = 1
    | otherwise = 1 + (listIndex x t)
--END: Histogram--


--START: auxiliary functions--
--auxiliary functions for small
projL :: [((Either a b), Double)] -> [(a, Double)]
projL [] = []
projL ((Left a,p):t) = (a,p) : projL t
projL ((Right b ,p):t) = projL t

projR :: [((Either a b), Double)] -> [(b, Double)]
projR [] = []
projR ((Left a, p):t) = projR t
projR ((Right b, p):t) = (b,p) : projR t

rmvL :: [[(Either LMem (a,LMem), Double)]] -> [[(Either Mem (a,Mem), Double)]]
rmvL [] = []
rmvL (h:t) = rmvLAux h : rmvL t

rmvLAux :: [(Either LMem (a,LMem), Double)] -> [(Either Mem (a,Mem), Double)]
rmvLAux [] = []
rmvLAux (((Left (sc,l,sq)),p) :t) = ((Left (sc,sq)),p) : rmvLAux t
rmvLAux (((Right(c, (sc,l,sq))),p) :t) = ((Right (c,(sc,sq))),p) : rmvLAux t

--auxiliary function for runNStepSch
--Remove the linking function
-- rmvLII :: [([(LMem, Double)],Double)] -> [([(Mem, Double)],Double)]
-- rmvLII ll = [([ ((sc,sq),p) | ((sc,l,sq),p) <- dist],q) | (dist,q) <- ll]

rmvIOLII :: IO [([(LMem, Double)],Double)] -> IO [([(Mem, Double)],Double)]
rmvIOLII ioll = do
  ll <- ioll
  let l = [([ ((sc,sq),p) | ((sc,l,sq),p) <- dist],q) | (dist,q) <- ll]
  return l

--Remove the linking function 
rmvIOL :: IO [(LMem,Double)] -> IO [(Mem,Double)]
rmvIOL iol = do
  l <- iol -- [(LMem,Double)]
  let l' = map (\((a,b,c),p) -> ((a,c),p) ) l
  return l'

--auxiliary functions to add elements of ProbT IO x
addMyDist :: (Eq x) => MyDist x -> MyDist x -> MyDist x
addMyDist (MyDist psi) (MyDist phi) = MyDist (addDistG psi phi)

addProbTIOG :: (Eq x) => ProbT IO x -> ProbT IO x -> ProbT IO x
addProbTIOG (ProbT iopsi) (ProbT iophi) = do
  psi <- lift $ iopsi -- [(x,Double)]
  phi <- lift $ iophi -- [(x,Double)]
  ProbT $ return (addDistG psi phi)

addDistG :: (Eq x) => [(x,Double)] -> [(x,Double)] -> [(x,Double)]
addDistG psi [] = psi
addDistG [] phi = phi
addDistG ((v1,p1):t1) phi = addDistAuxG (v1,p1) phi : addDistG t1 (rmvG v1 phi)

addDistAuxG :: (Eq x) => (x,Double) -> [(x,Double)] -> (x,Double)
addDistAuxG (v,p) [] = (v,p)
addDistAuxG (v1,p1) ((v2,p2):t) = if v1==v2
                                 then (v1,p1+p2)
                                 else addDistAuxG (v1,p1) t

rmvG :: (Eq x) => x -> [(x,Double)] -> [(x,Double)]
rmvG x [] = []
rmvG x ((y,p):t) = if x==y
                  then t
                  else (y,p) : rmvG x t
--END: auxiliary functions--

--Consider numbers very small to be zero; necessary because the transport of these numbers may lead
--to states arising from measurements that shouldn't exist
zeroIfSmall :: Double -> Double
zeroIfSmall x = if abs x < threshold
                then 0
                else x
  where threshold = 1e-15

zeroIfSmallC :: Complex Double -> Complex Double
zeroIfSmallC (r :+ i) = zeroIfSmall r :+ zeroIfSmall i

zeroIfSmallS :: SQ -> SQ
zeroIfSmallS st = fromLists [ f l | l <- lst]
  where lst = toLists st
        f = map (\e -> zeroIfSmallC e)



{- Another of implementing nstepSch
--START: nstep with scheduler--
--Using convexity
--Returns the result of the n-step operational semantics for a given command C, state s, and empty
--history, given by a scheduler, which selects a distribution according to convex coefficients; This
--seems very similar to runNStepSch because we are choosing based on a uniform distribution also
runNStepSchConv :: C -> LMem -> Int -> IO [(Mem,Double)]
runNStepSchConv c s n = rmvIOL $ runProbT $ nstepSchConv (([],(c,s)),n)

--improved display of the results of runNStepSchConv
--for example states are shown in bra-ket notation
showNStepSchConv :: C -> LMem -> Int -> IO()
showNStepSchConv c s n = do
  s' <- runNStepSchConv c s n -- [(Mem,Double)]
  putStrLn $ showProbMemList $ (limitPrecAux 5 s')

--codify the n-step semantics, for which a distribution is selected according to convex coefficients
nstepSchConv :: (ProbPath,Int) -> ProbT IO LMem
nstepSchConv (l,0) = ProbT $ return [(snd $ snd l, 0)]
nstepSchConv (l@(path,(c,s)),n) = do
  nextDist <- lift $ pathSchConv l -- [(Either S (C,S), Double)] 
  let pL = projL nextDist -- [(S, Double)]
      pR = projR nextDist -- [((C,S), Double)]
      ppR = [ (((path ++ [((c, s), nextDist)], cs), n-1), p) | (cs,p) <- pR, p/=0] --removing elements with probability 0
      transStep = (ProbT $ return ppR) >>= nstepSchConv -- ProbT IO S
  addProbTIOG transStep (ProbT $ return pL)

--IO [(Either S (C,S), Double)]
--Given a path/history, returns the next distribution to be evaluated according to convex
--coefficients
pathSchConv :: ProbPath -> IO [(Either LMem (C,LMem), Double)]
pathSchConv (path,(c,s)) = do
  let list = runProbT $ runExceptT $ runStateT (small c) s -- [[(Either LMem (C,LMem), Double)]]
  l <- convClosure list -- [([(Either LMem (C,LMem), Double)],Double)]
  next <- eventConv l -- [(Either LMem (C,LMem), Double)]
  return next

--Given a list of distributions selects the next distribution to be evaluated according to the
--probabilities in the distribution
eventConv :: [([(Either LMem (C,LMem), Double)], Double)] -> IO [(Either LMem (C,LMem), Double)]
eventConv [] = error "eventConv: list should be non-empty"
eventConv l = do
  n <- enact $ makeEventProb list -- int (the index of l that should be returned)
  return $ fst $ l!!n
  where list = zip [i | i <-[0 .. length l]] [p | (a,p) <- l]
--END: nstep with scheduler--
-}



{-OLDER VERSIONS OF THE HISTOGRAM
--START: histogram--
--Building the histogram using the uniform distribution or the method that uses convex coefficients
--to choose the next step of the computation
histogramNStep :: String -> Int -> C -> LMem -> Int -> IO ExitCode
histogramNStep name k c s n = do
    input <- listNStepMem k c s n -- [Mem]
    histogramInt (confIntoDouble input) name

--This function can be built using either runNStepSch or runNStepSchConv
--It uses the probabilities associated in the distributions to return a final result
listNStepMem :: Int -> C -> LMem -> Int -> IO [Mem]
listNStepMem 0 c s n = return []
listNStepMem k c s n = do
    a <- runNStepSch c s n -- [(Mem,Double)]
    --a <- runNStepSchConv c s n -- [(Mem,Double)]
    (sc,sq) <- eventMem a -- Mem = (sc,sq)
    as <- listNStepMem (k-1) c s n
    return ((sc, limitPrecisionS 5 sq):as)

--Building the histogram using the method that multiplies the convex coefficients with the
--probabilities of the distributions 
histogramNStepFullConv :: String -> Int -> C -> LMem -> Int -> IO ExitCode
histogramNStepFullConv name k c s n = do
    input <- listNStepMemFullConv k c s n -- [Mem]
    histogramInt (confIntoDouble input) name

--Similar to listNStepMem, but having in mind that the convex coefficients are multiplied by the
--probabilities of the distributions
listNStepMemFullConv :: Int -> C -> LMem -> Int -> IO [Mem]
listNStepMemFullConv 0 c s n = return []
listNStepMemFullConv k c s n = do
    a <- runNStepSchFullConv c s n -- [(Mem,Double)]
    (sc,sq) <- eventMem a -- Mem = (sc,sq)
    as <- listNStepMemFullConv (k-1) c s n
    return ((sc, limitPrecisionS 5 sq):as)    

--normalize the distribution (e.g: a (+)(0.5) b;c --k=1--> 0.5 a, which is then normalized to 
-- 0.5 a + 0.5 empty)
normDistMem :: [(Mem,Double)] -> [(Mem,Double)]
normDistMem [] = []
normDistMem l = if (probs==1.0)
                then l
                else (([],fromLists [[1]]),1-probs):l
  where probs = sum [p | (mem, p) <- l]

--normalize a distribution by dividing each probability by the sum of all the probabilities
normDist :: [(Mem,Double)] -> [(Mem,Double)]
normDist l = let probs = sum $ map snd l
             in [(mem,p/probs) | (mem,p) <- l]
--Returns a Mem based on the probabilities in the distribution given as input
eventMem :: [(Mem,Double)] -> IO Mem
eventMem l = do
  n <- enact $ makeEventProb dist -- int
  return $ [mem | (mem,p) <- normD]!!(n-1) 
  where normD = normDist l --normDistMem l
        dist = confIntoInt normD
--END: histogram--

OLDER VERSIONS OF THE HISTOGRAM-}
