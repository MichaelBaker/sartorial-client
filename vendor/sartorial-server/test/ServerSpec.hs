{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (spec) where

import Test.Hspec      (describe, it, shouldBe)
import Data.Map        (empty)
import Server          (handleCommand, GameState (..))
import Protocol        (Request (..), Response (..))
import Sartorial.World (freshWorld, addRoom)
import Sartorial.Room  (freshRoom)

freshGameState = GameState startingRoom world empty
  where (world, startingRoom) = addRoom freshWorld freshRoom

spec = describe "Server" $ do
  describe "handleCommand" $ do
    it "returns an InvalidRequest if the request is garbled" $ do
      snd (handleCommand freshGameState "bogus command") `shouldBe` InvalidRequestResponse

    it "returns an empty player list when no players have been added" $ do
      snd (handleCommand freshGameState $ show PlayerListRequest) `shouldBe` PlayerListResponse []

    it "returns a list of player names if players have been added" $ do
      let (gameStateOne, _) = handleCommand freshGameState $ show $ AddPlayerRequest "Hamilton"
          (gameStateTwo, _) = handleCommand gameStateOne   $ show $ AddPlayerRequest "Jefferson"
      snd (handleCommand gameStateTwo $ show PlayerListRequest) `shouldBe` PlayerListResponse ["Hamilton", "Jefferson"]
