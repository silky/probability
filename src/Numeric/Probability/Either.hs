module Numeric.Probability.Either where

import Control.Monad.Instances ()

import Control.Applicative (Applicative, pure, (<*>), liftA2, )


newtype EitherT a m b = EitherT (m (Either a b))

instance Functor m => Functor (EitherT a m) where
   fmap f (EitherT m) = EitherT $ fmap (fmap f) m

instance Applicative m => Applicative (EitherT a m) where
   pure a = EitherT $ pure $ Right a
   EitherT af <*> EitherT am =
      EitherT $
      liftA2 (\ef em ->
         case ef of
            Left b -> Left b
            Right f ->
               case em of
                  Left b -> Left b
                  Right m -> Right $ f m) af am

instance Monad m => Monad (EitherT a m) where
   return a = EitherT $ return $ Right a
   EitherT m >>= f  =  EitherT $ do
      e <- m
      case e of
         Left b -> return $ Left b
         Right a ->
            case f a of
               EitherT n -> n
   fail s = EitherT $ fail s

throw :: Applicative m => a -> EitherT a m b
throw a = EitherT $ pure $ Left a
