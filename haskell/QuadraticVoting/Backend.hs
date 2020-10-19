
module QuadraticVoting.Backend where

import Text.Read

import System.IO

import QuadraticVoting.Base
import QuadraticVoting.Poll

data Backend
  = File     { filepath :: FilePath }
  | TCP      { url :: String }
  | Database { url :: String }
  | GRPC     { url :: String }

commit :: Backend -> Poll -> IO (Maybe Error)
commit File { filepath = p } poll' = do
  content <- readFile p
  case (readMaybe content :: Maybe Poll) of
    Nothing -> return $ Just Error { errmsg = "could not parse state" }
    Just poll -> do
      if isValidTransition poll poll'
         then do
           handle <- openFile p WriteMode
           hPutStr handle (show poll')
           hClose handle
           return Nothing
         else return $ Just Error { errmsg = "state transition is invalid" }
commit _ _ = do return $ Just Error { errmsg = "not implemented" }

