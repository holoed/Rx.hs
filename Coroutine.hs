{-# Language FlexibleContexts, Rank2Types, ScopedTypeVariables #-}

module Coroutine where
 
import Control.Monad 
import Control.Monad.Trans 
 
newtype Trampoline m r = Trampoline {
    bounce :: m (Either (Trampoline m r) r)
}
 
instance Monad m => Monad (Trampoline m) where
    return = Trampoline . return . Right
    t >>= f = Trampoline (bounce t >>= either (return . Left. (>>= f)) (bounce . f))
 
instance MonadTrans Trampoline where
    lift = Trampoline . liftM Right
 
instance (MonadIO m) => MonadIO (Trampoline m) where
	liftIO = lift . liftIO 

pause :: Monad m => Trampoline m ()
pause = Trampoline (return $ Left $ return ())
 
run :: Monad m => Trampoline m r -> m r
run t = bounce t >>= either run return

mzipWith :: Monad m => (a -> b -> c) -> Trampoline m a -> Trampoline m b -> Trampoline m c
mzipWith f t1 t2 = Trampoline (liftM2 bind (bounce t1 ) (bounce t2 ))
                   where
                    bind (Left a) (Left b) = Left (mzipWith f a b)
                    bind (Left a) (Right b) = Left (mzipWith f a (return b))
                    bind (Right a) (Left b) = Left (mzipWith f (return a) b)
                    bind (Right a) (Right b) = Right (f a b)

interleave :: Monad m => [Trampoline m r] -> Trampoline m [r]
interleave = foldr (mzipWith (:)) (return [])