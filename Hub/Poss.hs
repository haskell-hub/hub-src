--
-- >>> Hub.Poss <<<
--
-- This module reestablishes an unvandalised monadic Either type.
--
-- (c) 2011-2015 Chris Dornan


module Hub.Poss
    ( Poss(..)
    , poss
    , ei2ps
    ) where

import Control.Applicative

data Poss e a = NOPE e | YUP a
                                                                deriving (Show)
instance Monad (Poss e) where
    (>>=) ps f = poss NOPE f ps
    return     = YUP

instance Applicative (Poss e) where
	(<*>) ps ps' = do f <- ps; x<-ps'; return $ f x
	pure         = return

instance Functor (Poss e) where
    fmap f p = poss NOPE (YUP . f) p

poss :: (e->b) -> (a->b) -> Poss e a -> b
poss n _ (NOPE e) = n e
poss _ y (YUP  x) = y x

ei2ps :: Either e a -> Poss e a
ei2ps = either NOPE YUP
