
module QuadraticVoting.Poll where

import QuadraticVoting.Base

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
  votesFor     :: VotesFor,
  votesAgainst :: VotesAgainst
} deriving (Show, Read)

data Poll = Poll {
  voters :: Map VoterId        VoterLedgerEntry,
  issues :: Map IssueId        IssueLedgerEntry,
  alts   :: Map AlternativeId  AltLedgerEntry
} deriving (Show, Read)

newPoll :: Poll
newPoll = Poll { voters=newMap, issues=newMap, alts=newMap }

newIssue :: IssueDescription -> IssueLedgerEntry
newIssue desc = IssueLedgerEntry { issdesc=desc, issalts=[] }

entries :: Map a b -> [b]
entries Map { keys=_, vals=vs } = vs

isValidTransition :: Poll -> Poll -> Bool
isValidTransition poll poll'
  = let voterEntries = entries $ voters poll
        sumOfAllBalances = \x -> sum ( map vcBalance voterEntries )
     in isValidState poll'
     && (sumOfAllBalances poll) == (sumOfAllBalances poll')

isValidState :: Poll -> Bool
isValidState state
  = allocatedVoiceCreditsAndCostsBalance state
 && noAllocationExceedsBalance           state

noAllocationExceedsBalance :: Poll -> Bool
noAllocationExceedsBalance poll
  = let voterEntries = entries $ voters poll
     in all inBalance voterEntries

inBalance :: VoterLedgerEntry -> Bool
inBalance v = (vcAllocated v) <= (vcBalance v)

allocatedVoiceCreditsAndCostsBalance :: Poll -> Bool
allocatedVoiceCreditsAndCostsBalance poll
  = totalVoiceCreditsAllocated poll == totalCostOfAllIssues poll

totalCostOfAllIssues :: Poll -> Nat
totalCostOfAllIssues poll
  = let issueEntries = keys $ issues poll
     in sum (map (costOfIssue poll) issueEntries)

costOfIssue :: Poll -> IssueId -> Nat
costOfIssue poll issueId
  = let issue = mapLookUp (issues poll) issueId 
        alts  = issalts issue
     in sum $ map (costOfAlternative poll) alts

totalVoiceCreditsAllocated :: Poll -> Nat
totalVoiceCreditsAllocated state
  = let voterEntries = entries (voters state)
     in sum (map vcAllocated voterEntries)

costOfAlternative :: Poll -> AlternativeId -> Nat
costOfAlternative poll altId
 = let alt     = mapLookUp (alts poll) altId 
       for     = votesFor alt
       against = votesAgainst alt
    in for*for + against*against

