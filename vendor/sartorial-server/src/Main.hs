import Web.Scotty                           (scotty, middleware)
import Network.Wai.Middleware.Static        (staticPolicy, addBase, noDots, (>->))
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Control.Concurrent.STM.TVar          (newTVarIO)
import Server                               (gameServer, GameState (..))
import Sartorial.World                      (freshWorld, addRoom)
import Sartorial.Room                       (freshRoom)
import Data.Map                             (empty)

main = do
  let (newWorld, startingRoom) = addRoom freshWorld freshRoom
  gameState <- newTVarIO (GameState startingRoom newWorld empty empty)
  scotty 8334 $ do
    middleware logStdoutDev
    middleware $ staticPolicy $ noDots >-> addBase "vendor"
    gameServer gameState
