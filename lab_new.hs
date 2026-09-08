type Name = String
type Hunger = Int
type Mood = Int
type Lives = Int
type Pickyness = Int
data Breed = Yard | Siamese | MaineCoon | Sphynx | British | Ginger deriving (Show, Eq)

data CatData = CatData
  { name :: Name
  , hunger :: Hunger
  , mood :: Mood
  , lives :: Lives
  , pickyness :: Pickyness
  , breed :: Breed
  } deriving (Show)
  
type Cat = CatData  

cat :: CatData -> Cat
cat = id  

feedCat :: Cat -> Hunger -> Cat
feedCat cd amountOfFood =
  if hunger cd + amountOfFood > 100
  then cd { hunger = 100 }
  else cd { hunger = hunger cd + amountOfFood }

strokeCat :: Cat -> Mood -> Cat
strokeCat cd strokeForce =
  if mood cd + strokeForce > 100
  then cd { mood = 100 }
  else cd { mood = mood cd + strokeForce }

printCatInfo :: Cat -> String
printCatInfo cd =
  "Name: " ++ name cd ++ 
  ", Hunger: " ++ show (hunger cd) ++ 
  ", Mood: " ++ show (mood cd) ++ 
  ", Live: " ++ show (lives cd) ++ 
  ", Pickyness: " ++ show (pickyness cd) ++ 
  ", Breed: " ++ show (breed cd)

isHere :: Cat -> Bool
isHere cd = hunger cd > 0 && mood cd > 0

spendTime :: Cat -> Cat
spendTime cd =
  cd { hunger = max 0 (hunger cd - pickyness cd)
     , mood = max 0 (mood cd - pickyness cd)
     , lives = lives cd + 1 }

battleRound (catA, catB) = (newA, newB)
  where
    newA = let (newH, newM) = if hunger catB > 0 && mood catB > 0
                              then (max 0 (hunger catA - pickyness catA), 
                                    max 0 (mood catA - pickyness catA))
                              else (hunger catA, mood catA)
           in catA { hunger = newH, mood = newM, lives = lives catA + 1 }
    
    newB = let (newH, newM) = if hunger catA > 0 && mood catA > 0
                              then (max 0 (hunger catB - pickyness catB), 
                                    max 0 (mood catB - pickyness catB))
                              else (hunger catB, mood catB)
           in catB { hunger = newH, mood = newM, lives = lives catB + 1 }

getWinner :: Cat -> Cat -> Cat
getWinner catA catB =
  if lives catA > lives catB
  then catA
  else catB