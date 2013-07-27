module Observables where

import Prelude hiding (map, takeWhile)
import System.IO
import Data.Char
import System.Console.ANSI
import Control.Monad
import qualified Data.List 

(|>) :: a -> (a -> b) -> b
x |> f = f x

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

data Observer a = Observer (a -> IO())

data Observable a = Observable (Observer a -> IO ()) 

onNext :: a -> Observer a -> IO()
onNext x (Observer f) = f x

unit :: a -> Observable a
unit x = Observable (\o -> o |> onNext x)

subscribe ::  Observer a -> Observable a -> IO()
subscribe f (Observable m) = m f

map :: (a -> b) -> Observable a -> Observable b
map f m = Observable (\o -> m |> subscribe(Observer(\x -> o |> onNext (f x))))

merge :: Observable (Observable a) -> Observable a
merge xss = Observable (\o -> xss |> subscribe (Observer (\xs -> xs |> subscribe o)))

instance Functor Observable where
    fmap :: (a -> b) -> Observable a -> Observable b
    fmap f m = Observable (\o -> m |> subscribe (Observer (\x -> o |> onNext (f x)))) 

instance Monad Observable where
    (>>=) :: Observable a -> (a -> Observable b) -> Observable b
    m >>= f = merge (fmap f m)
    return :: a -> Observable a
    return = unit 

combine :: Observable a -> Observable b -> Observable (Either a b)
combine xs ys = Observable (\o -> do xs |> subscribe(Observer (\x -> o |> onNext (Left x)));
 			             ys |> subscribe(Observer (\x -> o |> onNext (Right x))))

instance MonadPlus Observable where
	mzero :: Observable a
	mzero = Observable (\o -> return ())
	mplus :: Observable a -> Observable a -> Observable a
	mplus xs ys = error "Not implemented yet"

takeWhile :: (a -> Bool) -> Observable a -> Observable a
takeWhile p xs = do x <- xs;
		    guard (p x);
		    return x

takeUntil :: Observable a -> Observable b -> Observable a
takeUntil xs sig = combine xs sig |> takeWhile isLeft |> map (\(Left x) -> x) 

toObservable :: [a] -> Observable a
toObservable xs = Observable (\o -> (mapM_ (\ x -> o |> onNext x) xs))

keys :: Observable Char
keys = Observable loop
	   where loop o = do x <- getChar;
                              o |> onNext x;
                              loop o


