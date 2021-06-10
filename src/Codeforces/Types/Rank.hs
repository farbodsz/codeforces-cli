--------------------------------------------------------------------------------

module Codeforces.Types.Rank where

import           Codeforces.Types.Common

import           Data.Text                      ( Text )

--------------------------------------------------------------------------------

data RankColor = Gray | Green | Cyan | Blue | Violet | Orange | Red
    deriving (Eq, Show)

type RatingBounds = (Rating, Rating)

data Rank = Rank
    { rankName   :: Text
    , rankColor  :: RankColor
    , rankBounds :: RatingBounds
    }
    deriving (Eq, Show)

-- | A list of all Codeforces ranks.
ranks :: [Rank]
ranks =
    [ Rank "Newbie"                    Gray   (0   , 1199)
    , Rank "Pupil"                     Green  (1200, 1399)
    , Rank "Specialist"                Cyan   (1400, 1599)
    , Rank "Expert"                    Blue   (1600, 1899)
    , Rank "Candidate Master"          Violet (1900, 2099)
    , Rank "Master"                    Orange (2100, 2299)
    , Rank "International Master"      Orange (2300, 2399)
    , Rank "Grandmaster"               Red    (2400, 2599)
    , Rank "International Grandmaster" Red    (2600, 2899)
    , Rank "Legendary Grandmaster"     Red    (2900, 9999)
    ]

-- | Finds the 'Rank' that matches the supplied rating.
getRank :: Rating -> Rank
getRank x = head $ filter withinRankBounds ranks
  where
    withinRankBounds Rank {..} = fst rankBounds <= x && x <= snd rankBounds

--------------------------------------------------------------------------------