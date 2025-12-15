module Core.Api.ServerContext (
  runCavefishMonad,
  CavefishServerM (..),
  CavefishServices (..),
) where

import Cardano.Api (
  MonadError,
  MonadIO,
 )
import Control.Monad.Reader (MonadReader, ReaderT, runReaderT)
import Core.Services.TxBuilding (TxBuilding)
import Core.Services.WBPS (WBPS)
import Servant.Server (Handler)
import Servant.Server.Internal.ServerError (ServerError)

data CavefishServices = CavefishServices
  { wbpsService :: WBPS
  , txBuildingService :: TxBuilding
  }

newtype CavefishServerM a = CavefishServerM {unServerM :: ReaderT CavefishServices Handler a}
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader CavefishServices
    , MonadError ServerError
    )

runCavefishMonad :: CavefishServices -> CavefishServerM a -> Handler a
runCavefishMonad cavefishServices (CavefishServerM m) = runReaderT m cavefishServices
