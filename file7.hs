data Track = Track
  { title :: String
  , artist :: String
  , duration :: Int 
  , genre :: Genre
  , platform :: Platform
  } deriving (Show, Eq)

data Platform = YandexMusic | VKMusic | Zvuk deriving (Show, Eq)
data Genre = Rap | Pop | Indie | Phonk
  deriving (Show, Eq)
data MusicStyle = ClassicRap
                | TrapRap String
                | FolkRegional String
                | PopVariant Int 
  deriving (Show, Eq)
-- TrapRap "memphis"
-- Folkregional "Ural"
-- PopVariant 2020

style

platformFee :: Platform -> Double
platformFee YandexMusic = 0.3
platformFee VKMusic = 0.35
platformFee Zvuk = 0.25

popolam :: Track
popolam = Track
  { title = "popolam"
  , artist = "Macan"
  , duration = 183
  , genre = Rap
  , platform = YandexMusic
  }