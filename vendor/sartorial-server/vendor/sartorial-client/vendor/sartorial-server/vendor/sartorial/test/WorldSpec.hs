module WorldSpec where

import Test.Hspec            (describe, it, shouldBe)
import Test.Hspec.QuickCheck (property)
import Data.List             (foldl')

import Sartorial.Player (freshPlayer)
import Sartorial.Room   (freshRoom)
import Sartorial.World  ( LinkDirections (..)
                        , roomCount
                        , freshWorld
                        , addRoom
                        , accessibleRooms
                        , roomsAccessibleToPlayer
                        , linkRooms
                        , placePlayerInRoom
                        )

spec = describe "World" $ do
  describe "roomCount" $ do
    it "is the total number of rooms in the world" $ do
      let populateWorld numRooms = foldl' (\w r -> fst $ addRoom w r) freshWorld $ replicate numRooms freshRoom
      roomCount (populateWorld 0)     `shouldBe` 0
      roomCount (populateWorld 100)   `shouldBe` 100
      roomCount (populateWorld 1000)  `shouldBe` 1000
      roomCount (populateWorld 10000) `shouldBe` 10000

  describe "linkRooms" $ do
    it "makes one room accessible from another room" $ do
      let (worldOne, roomOne) = addRoom freshWorld freshRoom
      let (worldTwo, roomTwo) = addRoom worldOne   freshRoom
      let world               = linkRooms worldTwo (roomOne, West) (roomTwo, East)
      accessibleRooms world roomOne `shouldBe` Just [(roomTwo, West)]
      accessibleRooms world roomTwo `shouldBe` Just [(roomOne, East)]

    it "only allows one way to get from one room to another in the same direction" $ do
      let (worldOne, roomOne) = addRoom freshWorld freshRoom
      let (worldTwo, roomTwo) = addRoom worldOne   freshRoom
      let worldThree          = linkRooms worldTwo   (roomOne, West) (roomTwo, East)
      let world               = linkRooms worldThree (roomOne, West) (roomTwo, East)
      accessibleRooms world roomOne `shouldBe` Just [(roomTwo, West)]
      accessibleRooms world roomTwo `shouldBe` Just [(roomOne, East)]

  describe "roomsAccessibleToPlayer" $ do
    it "is empty if the player isn't in a room" $ do
      roomsAccessibleToPlayer freshWorld freshPlayer `shouldBe` Just []

    it "is the rooms connected to the player's room otherwise" $ do
      let (worldOne, roomOne) = addRoom freshWorld freshRoom
      let (worldTwo, roomTwo) = addRoom worldOne   freshRoom
      let world               = linkRooms worldTwo (roomOne, West) (roomTwo, East)
      let Just player         = placePlayerInRoom world freshPlayer roomOne
      roomsAccessibleToPlayer world player `shouldBe` accessibleRooms world roomOne
      let Just player         = placePlayerInRoom world freshPlayer roomTwo
      roomsAccessibleToPlayer world player `shouldBe` accessibleRooms world roomTwo
