module ProbTMonad where

import Control.Monad.State
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Except
import Control.Monad
import Control.Applicative

{-
To build a monad in Haskell we need:
(1) a data type (let us call it DT)
(2) Instantiate to Functor
    · fmap :: (a->b) -> (DT a) -> (DT b)
(3) Instantiate to Applicative
    · pure :: a -> (DT a)
    · (<*>) :: (DT (a->b)) -> (DT a) -> (DT b)
(4) Instantiate to Monad
    · return :: a -> (DT a)
    · (>>=) :: (DT a) -> (a-> (DT b)) -> (DT b)

To build a monad transformer in Haskell we need:
(1) Define a type of kind (* -> *) -> * -> * (let us call it MonT)
(2) Instantiate to Functor
    · fmap :: (Monad m) => (a->b) -> (MonT m a) -> (MonT m b)
(3) Instantiate to Applicative
    · pure :: (Monad m) => a -> (MonT m a)
    · (<*>) :: (Monad m) => (MonT m (a->b)) -> (MonT m a) -> (MonT m b)
(4) Instantiate to Monad
    · return :: (Monad m) => a -> (MonT m a)
    · (>>=) :: (Monad m) => (MonT m a) -> (a -> (MonT m b)) -> (MonT m b)
(5) Instantiate to MonadTrans (need to import Control.Monad.Trans)
    · lift :: (Monad m) => (m a) -> (MonT m a)
-}


--START: Prob Monad--
newtype MyDist x = MyDist { getProb :: [(x, Double)]} deriving (Show,Eq)

instance Functor MyDist where
  --fmap :: (a->b) -> (MyDist a) -> (MyDist b) 
  fmap f (MyDist dist) = MyDist $ [(f x,p) | (x,p) <- dist] 
  --fmap f (MyDist x) = MyDist $ map (\(y,p) -> (f y, p)) x

instance Applicative MyDist where
    pure x = MyDist $ [(x,1)]
    (MyDist distf) <*> (MyDist dist) = MyDist $ [(f x, pf*px) | (x,px)<-dist, (f, pf) <- distf]

instance Monad MyDist where
    return x = pure x 
    (MyDist x) >>= f = MyDist $ [(x'', px'*px) | (x',px)<-x, (x'', px')<- getProb $ f x']
--END: Prob Monad--

--START: ProbT-- 
newtype ProbT m a = ProbT { runProbT :: m [(a, Double)] } 

joinProb :: (Monad m) => (a -> ProbT m b) -> [(a, Double)] -> m [[(b, Double)]]
joinProb f dist = forM dist $ \(x, px) -> do
  let ProbT mdist = f x
  dist' <- mdist
  return [(y, px * py) | (y, py) <- dist']

instance (Monad m) => Functor (ProbT m) where
  fmap f (ProbT mdist) = ProbT $ do
    dist <- mdist
    return [(f x, p) | (x, p) <- dist]

instance (Monad m) => Applicative (ProbT m) where
  pure x = ProbT $ return [(x, 1)]
  (ProbT mdistf) <*> (ProbT mdist) = ProbT $ do
    distf <- mdistf
    dist <- mdist
    return [(f x, pf * px) | (f, pf) <- distf, (x, px) <- dist]

instance (Monad m) => Monad (ProbT m) where
  return x = ProbT $ return [(x, 1)]
  (ProbT mdist) >>= f = ProbT $ do
    dist <- mdist
    appf <- joinProb f dist
    return (concat appf)
    
instance MonadTrans ProbT where
  lift ma = ProbT $ liftM (\x -> [(x, 1)]) ma

{- Alternative definition of the instance MonadTrans ProbT
instance MonadTrans ProbT where
  lift ma = ProbT $ do
    a <- ma
    return [(a,1)]
-}
--END: ProbT--
