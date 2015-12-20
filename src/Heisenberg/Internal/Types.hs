{-# LANGUAGE TemplateHaskell #-}
module Heisenberg.Internal.Types where


-------------------------------------------------------------------------------
import           Control.Exception
import           Control.Lens
import           Data.Text         (Text)
import           System.Random.MWC
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
newtype ExperimentName = ExperimentName {
      _experimentName :: Text
    } deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
data Experiment m a = Experiment {
      _eName     :: !ExperimentName
    , _eRNG      :: !GenIO
    , _eReporter :: !(ExperimentReporter m a)
    }


-------------------------------------------------------------------------------
type ExperimentReporter m a = Outcome a -> m ()


-------------------------------------------------------------------------------
data Outcome a = Outcome {
      _oControlObservation   :: !(Observation a)
    , _oCandidateObservation :: !(Observation a)
    } deriving (Show)


-------------------------------------------------------------------------------
newtype NanoSeconds = NanoSeconds {
      _nanoSeconds :: Integer
    } deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
data Observation a = FailedObservation !SomeException
                   | Observation !(SuccessfulObservation a) deriving (Show)


-------------------------------------------------------------------------------
data SuccessfulObservation a = SuccessfulObservation {
      _oResult   :: !a
    , _oDuration :: !NanoSeconds
    } deriving (Show, Eq)


-------------------------------------------------------------------------------
makeLenses ''ExperimentName
makeLenses ''Experiment
makeLenses ''NanoSeconds
makePrisms ''Observation
makeLenses ''SuccessfulObservation
