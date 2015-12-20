{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module Heisenberg
    ( newExperiment
    , runExperiment
    -- ^ Types
    , ExperimentName(..)
    , experimentName
    , Experiment
    , eName
    , _eName
    , eStats
    , _eStats
    ) where


-------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Exception.Enclosed
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Data.Either
import           System.Clock
import           System.Random.MWC
-------------------------------------------------------------------------------
import           Heisenberg.Types
-------------------------------------------------------------------------------

newExperiment :: ExperimentName -> IO Experiment
newExperiment n = do
  stats <- newTVarIO (ExperimentStats 0 0 0 0)
  rng <- createSystemRandom
  return (Experiment n rng stats)


-------------------------------------------------------------------------------
-- list of named candidates?
runExperiment
    :: ( MonadBaseControl IO m
       , Eq a
       )
    => Experiment
    -> m a
    -- ^ Control
    -> m a
    -- ^ Candidate
    -> m a
runExperiment Experiment {..} ctrl cand = do
  controlFirst <- liftIO' (uniform _eRNG)
  if controlFirst
     then do ctrlRes <- runAction ctrl
             candRes <- runAction cand
             report' ctrlRes candRes
             result ctrlRes
     else do candRes <- runAction cand
             ctrlRes <- runAction ctrl
             report' ctrlRes candRes
             result ctrlRes
  where result = either (liftIO' . throw) (return . fst) . observation
        report' ctrl cand = liftIO' (atomically (report _eStats ctrl cand))


-------------------------------------------------------------------------------
--TODO: newtype over result?
report
    :: (Eq a)
    => TVar ExperimentStats
    -> Observation a
    -- ^ Control observation
    -> Observation a
    -- ^ Candidate observation
    -> STM ()
report v (Observation ctrl) (Observation cand) = modifyTVar' v go
  where go s = s & esRuns +~ 1
                 & esMismatches +~ mismatch
                 & esCtrlExceptions +~ ctrlException
                 & esCandExceptions +~ candException
        mismatch = case (ctrl, cand) of
                     (Right a, Right b)
                       | a == b -> 0
                     _ -> 1
        ctrlException = if isLeft ctrl then 1 else 0
        candException = if isLeft cand then 1 else 0


-------------------------------------------------------------------------------
runAction
    :: ( MonadBaseControl IO m
       )
    => m a
    -> m (Observation a)
runAction f = Observation <$> tryAny go
  where go = do t1 <- liftIO' getTime'
                res <- f
                t2 <- liftIO' getTime'
                let tDelta = NanoSeconds (timeSpecAsNanoSecs (diffTimeSpec t2 t1))
                return (res, tDelta)
        getTime' = getTime Monotonic


-------------------------------------------------------------------------------
-- | I don't actually know if this is right
liftIO' :: MonadBaseControl IO m => IO a -> m a
liftIO' f = liftBaseWith (const f)
