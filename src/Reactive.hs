module Reactive where

import Data.Monoid

newtype Future t a = Future { unFuture :: (t,a) }

instance Functor (Future t) where
  fmap f (Future (t,a)) = Future $ (t,f a)

instance (Bounded t,Ord t) => Applicative (Future t) where
  pure a = Future (minBound,a)
  Future (t0,f) <*> Future (t1,x) = Future (max t0 t1,f x)

instance (Bounded t,Ord t) => Monad (Future t) where
  return = pure
  Future (t,a) >>= f = let (t',b) = unFuture (f a) in Future (max t t',b)

instance (Bounded t,Ord t) => Monoid (Future t a) where
  mempty = Future (maxBound,undefined)
  Future (t0,a) `mappend` Future (t1,b) =
    Future (min t0 t1,if t0 <= t1 then a else b)

data Reactive t a = Stepper a (Event t a)

instance Functor (Reactive t) where
  fmap f (Stepper a e) = Stepper (f a) (fmap f e)

instance (Bounded t,Ord t) => Applicative (Reactive t) where
  pure a = Stepper a mempty
  rf@(Stepper f (Event e0)) <*> rx@(Stepper x (Event e1)) = Stepper (f x) (Event e')
    where
      e' = fmap (<*> rx) e0 <> fmap (rf <*>) e1








newtype Event t a = Event (Future t (Reactive t a))

instance Functor (Event t) where
  fmap f (Event fut) = Event (fmap (fmap f) fut)

instance (Bounded t,Ord t) => Monoid (Event t a) where
  mempty = Event mempty
  Event a `mappend` Event b = Event (merge a b)
    where
      merge a b = (inFutR (`merge` b) <$> a) <> (inFutR (a `merge`) <$> b)
        where
          inFutR f (Stepper r (Event c)) = Stepper r (Event $ f c)




stepperR :: a -> Event t a -> Reactive t a
stepperR = Stepper

type Sink a = a -> IO ()

sinkR :: Sink a -> Reactive t a -> IO ()
sinkR snk (Stepper a e) = snk a >> sinkE snk e

-- TODO: wait for t
sinkE :: Sink a -> Event t a -> IO ()
sinkE snk (Event (Future (t,r))) = sinkR snk r

accumR :: a -> Event t (a -> a) -> Reactive t a
accumR
