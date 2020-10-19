
import Text.Read

import System.IO
import System.Directory(getTemporaryDirectory, removeFile)
import System.Environment
import System.Console.GetOpt

import QuadraticVoting.Base
import QuadraticVoting.Poll
import QuadraticVoting.Backend

main :: IO ()
main = do
  dir <- getTemporaryDirectory
  (file, handle) <- openTempFile dir "poll-state"
  putStrLn $ "using temp file " ++ file
  let poll = newPoll
      backend = File { filepath = file }
   in do
      putStrLn "writing initial state to file"
      hPutStr handle $ show poll
      hClose handle
      putStrLn "applying user transformation"
      applyUserTransformation backend poll
      removeFile file
      return ()

applyUserTransformation :: Backend -> Poll -> IO ()
applyUserTransformation backend poll = do
  putStrLn "? for help"
  poll' <- promptUser poll
  err   <- commit backend poll'
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
      displayIssues poll
      promptUser poll
    "2" -> do
      displayAlternatives poll
      promptUser poll
    "5" -> do
      (poll, issDescription) <- raiseAnIssue poll
      putStrLn $ "Issue created: " ++ issDescription
      promptUser poll
    "0" -> return poll
    _   -> do
      putStrLn $ "not implemented"
      promptUser poll

displayIssues :: Poll -> IO ()
displayIssues poll = displayDescriptions issues poll

displayAlternatives :: Poll -> IssueLedgerEntry -> IO ()
displayAlternatives entry =
  let altIds     = issalts entry
      altsLedger = alts poll
   in displayDescriptions $ mapfilter altIds altsLedger

displayDescriptions :: [(a,b)]  -> String
displayDescriptions pairs =
  let pprint (id, desc) = "# " ++ show id ++ ": " ++ desc + "\n"
      lines = map pprint pairs
   in foldl "" (++) lines

raiseAnIssue :: Poll -> IO (Poll, IssueDescription)
raiseAnIssue poll = do
  putStrLn "Description of issue:"
  desc <- getLine
  let is     = issues poll
      nextId = nextIssueId $ keys $ issues poll
   in do
    return ( Poll {
      voters = voters poll,
      issues = appendMap is nextId (newIssue desc),
      alts   = alts poll
    }, desc)

nextIssueId :: [IssueId] -> IssueId
nextIssueId issues = Suc (foldl max Zero issues)

displayAlternativesPrompt :: Poll -> IO ()
displayAlternativesPrompt poll = do
  putStrLn "Issue id?"
  sel <- getLine
  case (readMaybe sel :: Maybe IssueId) of
    Nothing -> do
      putStrLn $ "could not parse input " ++ sel
      displayAlternativesPrompt poll
    Just issid  -> do
      let issueLedgerEntry = mapLookUp (issues poll) issid
       in displayAlternatives poll $ issueLedgerEntry

