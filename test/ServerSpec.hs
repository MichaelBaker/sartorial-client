{-# LANGUAGE OverloadedStrings #-}

module ServerSpec (spec) where

import Test.Hspec      (describe, it, shouldBe)
import Data.Map        (empty, elems)
import Server          (handleCommand, GameState (..))
import Protocol        (Request (..), Response (..))
import Sartorial.World (freshWorld, addRoom)
import Sartorial.Room  (freshRoom)

freshGameState = GameState startingRoom world empty empty
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

  describe "chat" $ do
    it "adds an entry to the chat list for every player when a chat request gets sent" $ do
      let (gameStateOne, _)   = handleCommand freshGameState $ show $ AddPlayerRequest "Hamilton"
          (gameStateTwo, _)   = handleCommand gameStateOne   $ show $ AddPlayerRequest "Jefferson"
          (gameStateThree, _) = handleCommand gameStateTwo   $ show $ ChatMessageRequest "Hamilton" "Hello"
      elems (chats gameStateThree) `shouldBe` [["Hamilton: Hello"], ["Hamilton: Hello"]]

    it "adds multiple entries in chronological order with the newest last" $ do
      let (gameStateOne, _)   = handleCommand freshGameState $ show $ AddPlayerRequest "Hamilton"
          (gameStateTwo, _)   = handleCommand gameStateOne   $ show $ AddPlayerRequest "Jefferson"
          (gameStateThree, _) = handleCommand gameStateTwo   $ show $ ChatMessageRequest "Hamilton" "Hello"
          (gameStateFour, _)  = handleCommand gameStateThree $ show $ ChatMessageRequest "Jefferson" "Ohai"
      elems (chats gameStateFour) `shouldBe` [["Hamilton: Hello", "Jefferson: Ohai"], ["Hamilton: Hello", "Jefferson: Ohai"]]

    it "only sends the messages received since you last checked for a player" $ do
      let (gameStateOne, _)               = handleCommand freshGameState $ show $ AddPlayerRequest "Hamilton"
          (gameStateTwo, _)               = handleCommand gameStateOne   $ show $ AddPlayerRequest "Jefferson"
          (gameStateThree, _)             = handleCommand gameStateTwo   $ show $ ChatMessageRequest "Hamilton" "Hello"
          (gameStateFour, responseOne)    = handleCommand gameStateThree $ show $ ChatUpdatesRequest "Hamilton"
          (gameStateFive, _)              = handleCommand gameStateFour  $ show $ ChatMessageRequest "Hamilton" "Again"
          (gameStateSix, responseTwo)     = handleCommand gameStateFive  $ show $ ChatUpdatesRequest "Hamilton"
          (gameStateSeven, responseThree) = handleCommand gameStateFive  $ show $ ChatUpdatesRequest "Jefferson"
      responseOne   `shouldBe` ChatUpdatesResponse ["Hamilton: Hello"]
      responseTwo   `shouldBe` ChatUpdatesResponse ["Hamilton: Again"]
      responseThree `shouldBe` ChatUpdatesResponse ["Hamilton: Hello", "Hamilton: Again"]
