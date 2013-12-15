{-# LANGUAGE OverloadedStrings #-}

module Server (gameServer, handleCommand, GameState (..)) where

import Web.Scotty                  (get, body, text)
import Data.Text.Lazy              (pack)
import Data.ByteString.Lazy.Char8  (unpack)
import Safe                        (readMay)
import Control.Concurrent.STM      (atomically)
import Control.Concurrent.STM.TVar (readTVar, writeTVar)
import Control.Monad.IO.Class      (liftIO)
import Protocol                    (Request (..), Response (..))
import Sartorial.World             (World (), freshWorld, placePlayerInRoom)
import Sartorial.Room              (Room ())
import Sartorial.Player            (Player (..), freshPlayer)

import qualified Data.Map as M

data GameState = GameState { startingRoom :: Room
                           , world        :: World
                           , players      :: M.Map String Player
                           }

gameServer gameState = do
  get "/thing" $ do
    command  <- body
    response <- liftIO $ atomically $ do
      currentState <- readTVar gameState
      let (newGameState, response) = handleCommand currentState $ bytestringToString command
      writeTVar gameState newGameState
      return response
    text $ stringToText $ show response

handleCommand gameState commandText = case readMay commandText of
                                        Just command -> process gameState command
                                        Nothing      -> (gameState, InvalidRequestResponse)

process gameState PlayerListRequest        = (gameState, PlayerListResponse $ M.keys $ players gameState)
process gameState (AddPlayerRequest name)  = (newGameState, SuccessResponse)
  where newGameState = gameState { players = newPlayers }
        player       = freshPlayer { playerName = name }
        (newPlayers, response) = case placePlayerInRoom (world gameState) player (startingRoom gameState) of
                                   Just player -> (M.insert name player $ players gameState, SuccessResponse)
                                   Nothing     -> (players gameState, ErrorResponse "Room does not exist")

bytestringToString = unpack
stringToText       = pack
