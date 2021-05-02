module Codeforces.Rank where

data RankColor = Gray | Green | Cyan | Blue | Violet | Orange | Red

type RatingBounds = (Int, Int)

data Rank = Rank
    { rankName   :: String
    , rankColor  :: RankColor
    , rankBounds :: RatingBounds
    }

-- | `ranks` is list of all Codeforces ranks
ranks :: [Rank]
ranks =
    [ Rank "Newbie"                    Gray   (0   , 1199)
    , Rank "Pupil"                     Green  (1200, 1399)
    , Rank "Specialist"                Cyan   (1400, 1599)
    , Rank "Expert"                    Blue   (1600, 1899)
    , Rank "Candidate Master"          Violet (1900, 2199)
    , Rank "Master"                    Orange (2200, 2299)
    , Rank "International Master"      Orange (2300, 2399)
    , Rank "Grandmaster"               Red    (2400, 2599)
    , Rank "International Grandmaster" Red    (2600, 2899)
    , Rank "Legendary Grandmaster"     Red    (2900, 9999)
    ]

-- | `getRank` @rating@ returns the @Rank@ that matches the user's @rating@
getRank :: Int -> Rank
getRank x = head $ filter withinRankBounds ranks
  where
    withinRankBounds r =
        let bounds = rankBounds r in x >= fst bounds && x <= snd bounds
