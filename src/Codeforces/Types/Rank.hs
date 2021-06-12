--------------------------------------------------------------------------------

module Codeforces.Types.Rank where

import           Codeforces.Types.Common

import           Data.Text                      ( Text )

--------------------------------------------------------------------------------

data RankColor
    = RankGray
    | RankGreen
    | RankCyan
    | RankBlue
    | RankViolet
    | RankOrange
    | RankRed
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
    [ Rank "Newbie"                    RankGray   (0   , 1199)
    , Rank "Pupil"                     RankGreen  (1200, 1399)
    , Rank "Specialist"                RankCyan   (1400, 1599)
    , Rank "Expert"                    RankBlue   (1600, 1899)
    , Rank "Candidate Master"          RankViolet (1900, 2099)
    , Rank "Master"                    RankOrange (2100, 2299)
    , Rank "International Master"      RankOrange (2300, 2399)
    , Rank "Grandmaster"               RankRed    (2400, 2599)
    , Rank "International Grandmaster" RankRed    (2600, 2899)
    , Rank "Legendary Grandmaster"     RankRed    (2900, 9999)
    ]

-- | Finds the 'Rank' that matches the supplied rating.
getRank :: Rating -> Rank
getRank x = head $ filter withinRankBounds ranks
  where
    withinRankBounds Rank {..} = fst rankBounds <= x && x <= snd rankBounds

--------------------------------------------------------------------------------
