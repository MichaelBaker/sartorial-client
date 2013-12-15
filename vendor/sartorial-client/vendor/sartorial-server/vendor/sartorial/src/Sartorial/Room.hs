module Sartorial.Room where

data Room = Room { name        :: String
                 , description :: String
                 , roomId      :: Int
                 } deriving (Show, Eq)

freshRoom = Room { name        = "A Room"
                 , description = "An empty room"
                 , roomId      = 0
                 }
