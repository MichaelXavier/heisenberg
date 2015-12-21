{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module Heisenberg
    ( -- * Setup
      newExperiment
    , runExperiment
    -- * Reporters
    , nullReporter
    , sequenceReporters
    , parallelReporters
    -- * Types
    , ExperimentName(..)
    , experimentName
    , Experiment
    , eName
    , _eName
    , eReporter
    , _eReporter
    , Outcome
    , _oControlObservation
    , _oCandidateObservation
    , oControlObservation
    , oCandidateObservation
    , Observation(..)
    , _FailedObservation
    , _Observation
    , SuccessfulObservation
    , _oResult
    , _oDuration
    , oResult
    , oDuration
    , NanoSeconds(..)
    , nanoSeconds
    ) where


-------------------------------------------------------------------------------
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Exception
import           Control.Exception.Enclosed
import           Control.Monad
import           Control.Monad.Trans.Control
import           System.Clock
import           System.Random.MWC
-------------------------------------------------------------------------------
import           Heisenberg.Internal.Types
-------------------------------------------------------------------------------


newExperiment :: ExperimentName -> ExperimentReporter m a -> IO (Experiment m a)
newExperiment n reporter = do
  rng <- createSystemRandom
  return (Experiment n rng reporter)


-------------------------------------------------------------------------------
-- TODO: list of named candidates?
runExperiment
    :: ( MonadBaseControl IO m
       , Eq a
       )
    => Experiment m a
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
             _ <- report ctrlRes candRes
             result ctrlRes
     else do candRes <- runAction cand
             ctrlRes <- runAction ctrl
             _ <- report ctrlRes candRes
             result ctrlRes
  where result (FailedObservation e)                                 = liftIO' (throw e)
        result (Observation (SuccessfulObservation { _oResult = r})) = return r
        report ctrlRes candRes = liftBaseWith $ \runInIO ->
          forkIO (void (runInIO (_eReporter (Outcome ctrlRes candRes))))


-------------------------------------------------------------------------------
runAction
    :: ( MonadBaseControl IO m
       )
    => m a
    -> m (Observation a)
runAction f = do
  res <- tryAny go
  return $ case res of
             Left e -> FailedObservation e
             Right so -> Observation so
  where go = do t1 <- liftIO' getTime'
                res <- f
                t2 <- liftIO' getTime'
                let tDelta = NanoSeconds (timeSpecAsNanoSecs (diffTimeSpec t2 t1))
                return (SuccessfulObservation res tDelta)
        getTime' = getTime Monotonic


-------------------------------------------------------------------------------
-- | I don't actually know if this is right
liftIO' :: MonadBaseControl IO m => IO a -> m a
liftIO' f = liftBaseWith (const f)


-------------------------------------------------------------------------------
nullReporter :: Monad m => ExperimentReporter m a
nullReporter = const (return ())


-------------------------------------------------------------------------------
-- | Combine 2 experiment reporters to run in sequence. Eats synchronous exceptions for each reporter
sequenceReporters
    :: (MonadBaseControl IO m)
    => ExperimentReporter m a
    -> ExperimentReporter m a
    -> ExperimentReporter m a
sequenceReporters a b o = eatExceptions (a o) >> eatExceptions (b o)


-------------------------------------------------------------------------------
parallelReporters
  :: (MonadBaseControl IO m)
  => ExperimentReporter m a
  -> ExperimentReporter m a
  -> ExperimentReporter m a
parallelReporters a b o = restoreM =<< liftBaseWith (\runInIO ->
    let doConc = Concurrently . runInIO . eatExceptions
    in runConcurrently (doConc (a o) *> doConc (b o)))


-------------------------------------------------------------------------------
eatExceptions :: (MonadBaseControl IO m) => m () -> m ()
eatExceptions = void . tryAny
