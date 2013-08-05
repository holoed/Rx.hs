module Observables where

import Prelude hiding (map, takeWhile)
import System.IO
import Data.Char
import System.Console.ANSI
import Control.Monad
import Control.Monad.Trans
import qualified Data.List 
import Data.IORef
import Coroutine

(|>) :: a -> (a -> b) -> b
x |> f = f x

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _        = False

data Observer a = Observer (a -> Trampoline IO ())

data Observable a = Observable (Observer a -> Trampoline IO ()) 

onNext :: a -> Observer a -> Trampoline IO ()
onNext x (Observer f) = f x

unit :: a -> Observable a
unit x = Observable (\o -> o |> onNext x)

subscribe ::  Observer a -> Observable a -> Trampoline IO ()
subscribe f (Observable m) = m f

merge :: Observable (Observable a) -> Observable a
merge xss = Observable (\o -> xss |> subscribe (Observer (\xs -> xs |> subscribe o)))

instance Functor Observable where
    fmap :: (a -> b) -> Observable a -> Observable b
    fmap f m = Observable (\o -> m |> subscribe(Observer(\x -> o |> onNext (f x)))) 

instance Monad Observable where
    (>>=) :: Observable a -> (a -> Observable b) -> Observable b
    m >>= f = merge (fmap f m)
    return :: a -> Observable a
    return = unit 

instance MonadPlus Observable where
	mzero :: Observable a
	mzero = Observable (\o -> return ())
	mplus :: Observable a -> Observable a -> Observable a
	mplus xs ys = Observable (\o -> sequence_ [xs |> subscribe (Observer (\x -> o |> onNext x)), 
                                               ys |> subscribe (Observer (\x -> o |> onNext x))])

filter :: (a -> Bool) -> Observable a -> Observable a
filter p xs = Observable (\o -> xs |> subscribe (Observer (\x -> if (p x) then 
                                                                    (o |> onNext x) 
                                                                    else pause)))

takeWhile :: (a -> Bool) -> Observable a -> Observable a
takeWhile p xs = do x <- xs;
                    guard (p x);
                    return x

skipWhile :: (a -> Bool) -> Observable a -> Observable a
skipWhile p xs = takeWhile (not . p) xs 

combine :: Observable a -> Observable b -> Observable (Either a b)
combine xs ys = Observable (\o -> do interleave [xs |> subscribe(Observer (\x -> do o |> onNext (Left x)
                                                                                    pause)),
                                                 ys |> subscribe(Observer (\x -> do o |> onNext (Right x)
                                                                                    pause))]
                                     return ())

takeUntil :: Observable b -> Observable a -> Observable a
takeUntil sig xs = Observable (\o ->  do ref <- liftIO $ newIORef False
                                         (combine sig xs) |> subscribe (obs ref o))
                   where obs ref o = Observer (\x -> do b <- liftIO $ readIORef ref
                                                        if (b) then return () 
                                                        else if (isLeft x) then liftIO $ writeIORef ref True
                                                        else let (Right v) = x in o|> onNext v)  

skipUntil :: Observable b -> Observable a -> Observable a
skipUntil sig xs = Observable (\o ->  do ref <- liftIO $ newIORef False
                                         (combine sig xs) |> subscribe (obs ref o))
                   where obs ref o = Observer (\x -> do b <- liftIO $ readIORef ref
                                                        if (b) then let (Right v) = x in o|> onNext v
                                                        else if (isLeft x) then liftIO $ writeIORef ref True
                                                        else return())

window :: Observable a -> Observable b -> Observable (Observable a)
window xs close = error "Not implemented yet"

toObservable :: [a] -> Observable a
toObservable xs = Observable (\o -> (mapM_ (\ x -> o |> onNext x) xs))

keys :: Observable Char
keys = Observable loop
	   where loop o = do x <- liftIO $ getChar;
                             o |> onNext x;
                             loop o



