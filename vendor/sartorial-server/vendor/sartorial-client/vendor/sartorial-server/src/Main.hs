import Web.Scotty                  (scotty)
import Control.Concurrent.STM.TVar (newTVarIO)
import Server                      (gameServer, GameState (..))
import Sartorial.World             (freshWorld, addRoom)
import Sartorial.Room              (freshRoom)
import Data.Map                    (empty)

main = do
  let (newWorld, startingRoom) = addRoom freshWorld freshRoom
  gameState <- newTVarIO (GameState startingRoom newWorld empty)
  scotty 8334 (gameServer gameState)
