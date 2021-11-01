data Grade = F | D | C | B | A
  deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS | BA | MS | PhD
  deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  {candidateId :: Int
  ,codeReview :: Grade
  ,cultureFit :: Grade
  ,education :: Degree }
  deriving Show

viable :: Candidate -> Bool
viable candidate =
  passedCoding && passedCultureFit && educationMin
  where
    passedCoding = codeReview candidate == A
    passedCultureFit = cultureFit candidate > C
    educationMin = education candidate >= MS

readM :: Read a => String -> IO a
readM txt = do
  putStr $ txt ++ ": "
  read <$> getLine

testCandidate :: Candidate
testCandidate = Candidate
  { candidateId = 1
  , codeReview = A
  , cultureFit = A
  , education = PhD }

readCandidate :: IO Candidate
readCandidate = do
  cId <- readM "enter id"
  codeGrade <- readM "enter code grade"
  cultureGrade <- readM "enter culture fit grade"
  degree <- readM "enter education"
  return $ Candidate cId codeGrade cultureGrade degree

viableCandidateIO :: IO Bool
viableCandidateIO = viable <$> readCandidate

candidate1 :: Candidate
candidate1 = Candidate
  { candidateId = 1
  , codeReview = A
  , cultureFit = A
  , education = BA }

candidate2 :: Candidate
candidate2 = Candidate
  { candidateId = 2
  , codeReview = C
  , cultureFit = A
  , education = PhD }

candidate3 :: Candidate
candidate3 = Candidate
  { candidateId = 3
  , codeReview = A
  , cultureFit = B
  , education = MS }

candidateDB :: [(Int, Candidate)]
candidateDB =
  map (\c -> (candidateId c, c))
  [candidate1, candidate2, candidate3]

viableCandidateMaybe :: Int -> Maybe Bool
viableCandidateMaybe cId =
  viable <$> lookup cId candidateDB

candidates :: [Candidate]
candidates = [candidate1,candidate2,candidate3]

viableCandidateList :: [Candidate] -> [Bool]
viableCandidateList = map viable

viableCandidate :: Monad m => m Candidate -> m Bool
viableCandidate mcandidates = do
  -- viable <$> mcandidates
  candidate <- mcandidates
  return $ viable candidate
