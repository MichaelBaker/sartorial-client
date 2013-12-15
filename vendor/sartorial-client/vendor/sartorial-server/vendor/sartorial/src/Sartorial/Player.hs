module Sartorial.Player where

data Player = Player { playerName :: String
                     , location   :: Maybe Int
                     }

freshPlayer = Player { location   = Nothing
                     , playerName = "No one"
                     }
