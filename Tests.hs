import Observables
import Control.Monad
import System.Console.ANSI
import Prelude hiding (takeWhile, filter)
import System.IO
import Data.IORef
import Test.HUnit

ys = [1..10] |> toObservable

assertSubscribe :: (Eq b, Show b) => [b] -> Observable b -> IO()
assertSubscribe expected xs = do ret <- newIORef []
                                 xs |> subscribe (Observer (\x -> modifyIORef ret (++ [x]) ))
                                 actual <- readIORef ret
                                 assertEqual "" expected actual 


tests = TestList ["Subscribe"  ~: assertSubscribe [1..10] ys, 
                  "Unit"       ~: assertSubscribe [42] (unit 42),
                  "Map"        ~: assertSubscribe ([1..10] |> map (show)) (ys |> fmap (show)),
                  "Take While" ~: assertSubscribe [1..4] (ys |> takeWhile (<5)),
                  "Merge"      ~: assertSubscribe [1..10] ([[1..5], [6..10]] |> map toObservable |> toObservable |> merge),
                  "Bind"       ~: assertSubscribe [(x,y) | x <- [1..5], y <-[5..10]] (do x <- [1..5] |> toObservable
                                                                                         y <- [5..10] |> toObservable
                                                                                         return (x,y)),
                  "mplus"      ~: assertSubscribe [1..10] (([1..5] |> toObservable) `mplus` ([6..10] |> toObservable)),
                  "skipWhile"  ~: assertSubscribe [5..10] (ys |> skipWhile (<5)),
                  "combine"    ~: assertSubscribe (concat [[Left x, Right x] | x <- [1..10]]) (combine ys ys),
                  "takeUntil"  ~: assertSubscribe [1..4] (ys |> takeUntil (ys |> filter (==5))),
                  "skipUntil"  ~: assertSubscribe [5..10] (ys |> skipUntil (ys |> filter (==5)))]

main = runTestTT tests

{-
main = ys |> skipUntil (ys |> mfilter (== 5))
          |> subscribe (Observer print)
-}

{-
main = ys |> skipWhile (< 5)
          |> subscribe (Observer print)
-}

{-
main = ys |> takeUntil (ys |> mfilter (< 5))
          |> subscribe (Observer print)
-}

{-
main =  do  hSetEcho stdin False
            hSetBuffering stdin NoBuffering
            hSetBuffering stdout NoBuffering
            hideCursor
            keys |> map (\x -> read [x] :: Float) 
		         |> map (\x -> x * x)
		         |> subscribe (Observer (\x -> do clearLine
		       	                                  x |> show |> putStr;
		       		                              setCursorColumn 0;
		       		                              return ()))


-}