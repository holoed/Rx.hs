import Observables
import Control.Monad
import System.Console.ANSI
import Prelude hiding (map, takeWhile)
import System.IO
import Data.IORef
import Test.HUnit
import qualified Data.List

ys = [1..10] |> toObservable

assertSubscribe :: (Eq b, Show b) => [b] -> Observable b -> IO()
assertSubscribe expected xs = do ret <- newIORef []
                                 xs |> subscribe (Observer (\x -> modifyIORef ret (++ [x]) ))
                                 actual <- readIORef ret
                                 assertEqual "" expected actual 


tests = TestList ["Subscribe"  ~: assertSubscribe [1..10] ys, 
                  "Unit"       ~: assertSubscribe [42] (unit 42),
                  "Map"        ~: assertSubscribe ([1..10] |> Data.List.map (show)) (ys |> map (show)),
                  "Take While" ~: assertSubscribe [1..4] (ys |> takeWhile (<5)),
                  "Merge"      ~: assertSubscribe [1..10] ([[1..5], [6..10]] |> Data.List.map toObservable |> toObservable |> merge),
                  "Bind"       ~: assertSubscribe [(x,y) | x <- [1..5], y <-[5..10]] (do x <- [1..5] |> toObservable
                                                                                         y <- [5..10] |> toObservable
                                                                                         return (x,y)),
                  "Combine"    ~: assertSubscribe ((Data.List.map (Left) [1..4]) ++ 
                                                   (Data.List.map (Right) ['a'..'z'])) (combine ([1..4] |> toObservable)
                                                                                                (['a'..'z'] |> toObservable))]

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