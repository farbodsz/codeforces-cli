--------------------------------------------------------------------------------

module Main where

import Codeforces.App

--------------------------------------------------------------------------------

main :: IO ()
main = do
    command <- parseCommands
    config  <- loadConfig

    case command of
        -- List/tabulate data
        AgendaCmd                -> contestList (ContestOpts False False True)
        ContestsCmd opts         -> contestList opts
        InfoCmd cId opts         -> contestInfo cId config opts
        ProblemsCmd opts         -> problemList opts
        StandingsCmd cId opts    -> standingsList cId config opts

        -- User-related commands
        UserCmd    h             -> userInfo h
        RatingsCmd h             -> userRatings h
        StatusCmd h opts         -> userStatus h opts
        FriendsCmd               -> userFriends config
        VirtualCmd cId h pts pen -> virtualRating cId h pts pen

        -- Miscellaneous
        SetupCmd                 -> setupConfig
        OpenCmd cId              -> openContest cId

--------------------------------------------------------------------------------
