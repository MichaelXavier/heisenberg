{-# LANGUAGE TemplateHaskell #-}
module Heisenberg.Types where


-------------------------------------------------------------------------------
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Lens
import           Data.Text              (Text)
import           System.Random.MWC
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
newtype ExperimentName = ExperimentName {
      _experimentName :: Text
    } deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
data Experiment = Experiment {
      _eName  :: !ExperimentName
    , _eRNG   :: !GenIO
    , _eStats :: !(TVar ExperimentStats)
    }


-------------------------------------------------------------------------------
data ExperimentStats = ExperimentStats {
      _esRuns           :: !Integer
    , _esMismatches     :: !Integer
    , _esCtrlExceptions :: !Integer
    , _esCandExceptions :: !Integer
    } deriving (Show, Eq)


-------------------------------------------------------------------------------
newtype NanoSeconds = NanoSeconds {
      _nanoSeconds :: Integer
    } deriving (Show, Eq, Ord)


-------------------------------------------------------------------------------
newtype Observation a = Observation {
      observation :: (Either SomeException (a, NanoSeconds))
    }

-------------------------------------------------------------------------------
makeLenses ''ExperimentName
makeLenses ''Experiment
makeLenses ''NanoSeconds
makeLenses ''ExperimentStats
