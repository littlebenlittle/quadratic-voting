
import System.IO
import System.Directory(getTemporaryDirectory)
import System.Environment
import System.Directory
import System.Console.GetOpt

import Text.Read

data Error = Error { errmsg :: String }

data Nat = Zero | Suc Nat
  deriving (Eq, Show, Read)

instance Num Nat where
  Zero  + a = a
  Suc a + b = Suc (a + b)
  Zero  * a  = a
  Suc a * b  = b + a * b

instance Ord Nat where
  Zero    <= a = True
  (Suc a) <= b = a <= Suc (Suc b)

data Map a b = Map {
  keys :: [a],
  vals :: [b]
} deriving (Show, Read)

newMap :: Map a b
newMap = Map { keys=[], vals=[] }

appendMap :: Map a b -> a -> b -> Map a b
appendMap Map { keys=ks, vals=vs } key val
  = Map { keys = (key:ks), vals = (val:vs) }

entries :: Map a b -> [b]
entries Map { keys=_, vals=vs } = vs

type Id = Nat

type VoterId       = Id
type IssueId       = Id
type AlternativeId = Id

type VoiceCreditsAllocated = Nat
type VoiceCreditBalance    = Nat

type VotesFor     = Nat
type VotesAgainst = Nat

type IssueDescription       = String
type AlternativeDescription = String

data VoterLedgerEntry = VoterLedgerEntry {
  vcAllocated  :: VoiceCreditsAllocated,
  vcBalance    :: VoiceCreditBalance
} deriving (Show, Read)

data IssueLedgerEntry = IssueLedgerEntry {
  issdesc :: IssueDescription,
  issalts :: [AlternativeId]
} deriving (Show, Read)

data AltLedgerEntry = AltLedgerEntry {
  altdesc      :: AlternativeDescription,
  votesfor     :: VotesFor,
  votesAgainst :: VotesAgainst
} deriving (Show, Read)

data Poll = Poll {
  voters :: Map VoterId        VoterLedgerEntry,
  issues :: Map IssueId        IssueLedgerEntry,
  alts   :: Map AlternativeId  AltLedgerEntry
} deriving (Show, Read)

newPoll :: Poll
newPoll = Poll {
  voters = newMap,
  issues = newMap,
  alts   = newMap
}

newIssue :: IssueDescription -> IssueLedgerEntry
newIssue desc = IssueLedgerEntry { issdesc=desc, issalts=[] }

commit :: Backend -> Poll -> IO (Maybe Error)
commit File { filepath = p, handle = h } state' = do
  content <- readFile p
  case (readMaybe content :: Maybe Poll) of
    Nothing -> return $ Just Error { errmsg = "could not parse state" }
    Just state ->
      if isValidTransition state state'
         then do
           hPutStr h (show state')
           return Nothing
         else return $ Just Error { errmsg = "state transition is invalid" }
commit _ _ = do return $ Just Error { errmsg = "not implemented" }

--for each issue, the square of the number of voice credits spent
--is equal to the sums of the squares of all the votes cast
--
--if a voice credit balance changes, the transaction must be
--signed by all voters whose balance changed

data Backend
  = File     { filepath :: FilePath, handle :: Handle }
  | TCP      { url :: String }
  | Database { url :: String }
  | GRPC     { url :: String }

main :: IO ()
main = do
  dir <- getTemporaryDirectory
  (file, handle) <- openTempFile dir "poll-state"
  putStrLn $ "using temp file " ++ file
  let poll = newPoll
      backend = File { filepath = file, handle = handle }
   in do
      putStrLn "writing initial state to file"
      hPutStr handle $ show poll
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
      poll <- displayIssues poll
      promptUser poll
    "2" -> do
      poll <- displayIssues poll
      promptUser poll
    "5" -> do
      (poll, issDescription) <- raiseAnIssue poll
      putStrLn $ "Issue created: " ++ issDescription
      promptUser poll
    "0" -> return poll
    _   -> do
      putStrLn $ "not implemented"
      promptUser poll

displayIssues :: Poll -> IO Poll
displayIssues poll = do
  putStrLn $ show $ issues $ poll
  return poll

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

inBalance :: VoterLedgerEntry -> Bool
inBalance v = (vcAllocated v) <= (vcBalance v)

noAllocationExceedsBalance :: Poll -> Bool
noAllocationExceedsBalance poll
  = let ledgerEntries :: [VoterLedgerEntry]
        ledgerEntries = entries $ voters poll
     in all inBalance ledgerEntries

allocatedVoiceCreditsAndCostsBalance :: Poll -> Bool
allocatedVoiceCreditsAndCostsBalance poll
  = totalVoiceCreditsAllocated poll == totalCostOfAllIssues poll

isValidState :: Poll -> Bool
isValidState state
  = allocatedVoiceCreditsAndCostsBalance state
 && noAllocationExceedsBalance           state

totalVoiceCreditsAllocated :: Poll -> Nat
totalVoiceCreditsAllocated state
  = let ledgerEntry = entries (voters state)
     in sum (map vcAllocated ledgerEntry)

totalCostOfAllIssues :: Poll -> Nat
totalCostOfAllIssues state
  = error "not implemented"

isValidTransition :: Poll -> Poll -> Bool
isValidTransition poll poll'
  = let ledgerEntries :: [VoterLedgerEntry]
        ledgerEntries = entries $ voters poll
        sumOfAllBalances = \x -> sum ( map vcBalance ledgerEntries )
     in isValidState poll'
     && (sumOfAllBalances poll) == (sumOfAllBalances poll')

