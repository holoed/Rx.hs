import Observables
import Control.Monad
import System.Console.ANSI
import Prelude hiding (map, takeWhile)
import System.IO

ys = [1..10] |> toObservable


main = ys |> takeUntil (ys |> mfilter (< 5))
	  |> subscribe (Observer print)

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

