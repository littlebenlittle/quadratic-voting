
import Control.Monad.Except

import System.IO
import System.Directory(getTemporaryDirectory)
import System.Environment
import System.Console.GetOpt

data Error = Error { msg :: String }
data Nat = Zero | Suc Nat

data Backend
  = File     {path :: String}
  | TCP      {url  :: String}
  | Database {url  :: String}

connect :: Backend -> IO ()
connect File     {path = p} = error "not implemented"
connect TCP      {url  = p} = error "not implemented"
connect Database {url  = p} = error "not implemented"

data Options = Options
  {
  backend :: Backend
  }

data Voter = Voter
  {
  id           :: Nat,
  voiceCredits :: Nat
  }

data Alternative = Alternative
  {
  alternativeDescription  :: String,
  votesFor                :: Integer,
  votesAgainst            :: Integer
  }
  deriving Show

data Issue = Issue
  {
  issueDescription  :: String,
  alternatives      :: [Alternative]
  }
  deriving Show

data PollState = PollState
  {
  voters :: [Voter],
  issues :: [Issue]
  }

data Poll = Poll
  {
  opts  :: Options,
  state :: PollState
  }

newPoll :: Options -> IO Poll
newPoll options = do
  return Poll
    {
    opts  = options,
    state = PollState
      {
      voters = [],
      issues = []
      }
    }

send :: Backend -> PollState -> Maybe Error
send _ _ = error "not implemented"

recv :: Backend -> Either PollState Error
recv _ = error "not implemented"

main :: IO ()
main = do
  opts <- do
    dir <- getTemporaryDirectory
    (file, handle) <- openTempFile dir "poll-state"
    putStrLn $ "using temp file " ++ file
    return Options {backend = File {path = file}}
  poll <- newPoll opts
  putStrLn "? for help"
  poll <- promptUser poll
  return ()

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
        opts  = opts poll,
        state = PollState {
          voters = voters $ state poll,
          issues = issues (state poll) ++ [newIssue]
        }
      }
   in do return (poll', newIssue)

