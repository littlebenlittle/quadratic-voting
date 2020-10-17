
import System.IO
import System.Directory(getTemporaryDirectory)
import System.Environment
import System.Directory
import System.Console.GetOpt

import Text.Read

data Error = Error { errmsg :: String }
data Nat = Zero | Suc Nat
  deriving (Show, Read)

data Backend
  = File     { path :: FilePath }
  | TCP      { url  :: String }
  | Database { url  :: String }
  | GRPC     { url  :: String }

-- shared secret: pubKey is also private key
data SignatureAlgorithm = SharedSecret
  deriving (Show, Read)

data VoterCryptographicInformation = VoterCryptographicInformation {
  algo   :: SignatureAlgorithm,
  pubKey :: String
} deriving (Show, Read)

data Voter = Voter {
  voterId            :: Nat,
  voiceCreditBalance :: Nat,
  crypto             :: VoterCryptographicInformation
} deriving (Show, Read)

data Alternative = Alternative {
  alternativeDescription  :: String,
  votesFor                :: Integer,
  votesAgainst            :: Integer
} deriving (Show, Read)

data Issue = Issue {
  issueDescription  :: String,
  alternatives      :: [Alternative]
} deriving (Show, Read)

data PollState = PollState {
  voters :: [Voter],
  issues :: [Issue]
} deriving (Show, Read)

data Poll = Poll {
  state :: PollState
}

newPoll :: IO Poll
newPoll = do
  return Poll {
    state = PollState {
      voters = [],
      issues = []
    }
  }

send :: Backend -> PollState -> IO (Maybe Error)
send File { path = p } state' = do
  content <- readFile p
  case (readMaybe content :: Maybe PollState) of
    Nothing -> return $ Just Error { errmsg = "could not parse state" }
    Just state ->
      if isValidTransition state state'
         then do
           writeFile p (show state')
           return Nothing
         else return $ Just Error { errmsg = "state transition is invalid" }
send _ _ = do return $ Just Error { errmsg = "not implemented" }

isValidTransition :: PollState -> PollState -> Bool
isValidTransition state state' = error "not implemented"
  --the square of the number of voice credits spent
  --is equal to the sums of the squares of all the votes cast
  --
  --if a voice credit balance changes, the transaction must be
  --signed by all voters whose balance changed

main :: IO ()
main = do
  dir <- getTemporaryDirectory
  (file, handle) <- openTempFile dir "poll-state"
  putStrLn $ "using temp file " ++ file
  poll <- newPoll
  putStrLn "writing initial state to file"
  hPutStr handle $ show (state poll)
  let backend = File { path = file }
   in do
     putStrLn "applying user transformation"
     applyUserTransformation backend poll
     removeFile file
     return ()

applyUserTransformation :: Backend -> Poll -> IO ()
applyUserTransformation backend poll = do
  putStrLn "? for help"
  poll' <- promptUser poll
  err   <- send backend (state poll')
  case err of
    Nothing -> return ()
    Just Error {errmsg = msg} -> do
      putStrLn msg
      putStrLn "Select one of the following:"
      putStrLn " [1] Start over"
      putStrLn " [2] Edit with current poll state"
      putStrLn " [0] Exit"
      sel <- getLine
      case sel of
        "1" -> applyUserTransformation backend poll
        "2" -> applyUserTransformation backend poll'
        "0" -> return ()
      

promptUser :: Poll -> IO Poll
promptUser poll = do
  sel <- getLine
  case sel of
    "?" -> do
      putStrLn " [1] Display issues"
      putStrLn " [2] Display alternatives for an issue"
      putStrLn " [3] Vote FOR     an alternative"
      putStrLn " [4] Vote AGAINST an alternative"
      putStrLn " [5] Raise a new issue"
      putStrLn " [6] Propose a new alternative for an issue"
      putStrLn " [7] Register a new voter"
      putStrLn " [8] List all voters"
      putStrLn " [9] Show voter detail"
      putStrLn " [0] Exit"
      promptUser poll
    "1" -> do
      poll <- displayIssues poll
      promptUser poll
    "2" -> do
      poll <- displayIssues poll
      promptUser poll
    "5" -> do
      (poll, newIssue) <- raiseAnIssue poll
      putStrLn $ "Issue created: " ++ (issueDescription newIssue)
      promptUser poll
    "0" -> return poll
    _   -> do
      putStrLn $ "not implemented"
      promptUser poll

displayIssues :: Poll -> IO Poll
displayIssues poll = do
  putStrLn $ show $ issues $ state poll
  return poll

raiseAnIssue :: Poll -> IO (Poll, Issue)
raiseAnIssue poll = do
  putStrLn "Description of issue:"
  desc <- getLine
  let newIssue = Issue {
        issueDescription = desc,
        alternatives     = []
      } 
      poll' = Poll {
        state = PollState {
          voters = voters $ state poll,
          issues = issues (state poll) ++ [newIssue]
        }
      }
   in do return (poll', newIssue)

