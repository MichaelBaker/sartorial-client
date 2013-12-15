module Protocol where

data Request = AddPlayerRequest  { addPlayerName :: String }
             | PlayerListRequest
             deriving (Show, Read, Eq)

data Response = SuccessResponse
              | ErrorResponse String
              | PlayerListResponse [String]
              | InvalidRequestResponse
              deriving (Show, Read, Eq)
