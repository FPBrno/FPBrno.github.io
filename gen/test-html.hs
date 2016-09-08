{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import qualified Data.Foldable as Foldable
import qualified Text.Blaze.Html.Renderer.String as R
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
-- import qualified Text.Blaze.Html.Renderer.Pretty as R

import Control.Monad (sequence, unless, zipWithM_)
import Data.Default.Class (Default(def))
import Data.List (intercalate, intersperse)
import Data.Monoid
import Data.Time
import Data.Time.ISO8601 (formatISO8601)

data Tag
    = Agda
    | Compiler
    | Concurrent
    | Elm
    | Erlang
    | Theory
    | Haskell
    | HotCodeSwap
    | HoTT
    | Reliability
    | Types
    | Web
    deriving Show

data Language
    = Cz
    | En
    | Sk
    deriving Show

type URL = String

data Presentation = Presentation
    { title :: String
    , author :: String
    , language :: [Language]
    -- ^ In what language(s) talk can\ be/was.
    , tags :: [Tag]
    , slides :: Maybe URL
    -- ^ URL to slides of this presentation. 'Nothing' in case that there are
    -- no slides.
    , audio :: Maybe URL
    -- ^ URL to audio recording of this presentation or 'Nothing' if there is
    -- not any recording.
    , player :: Maybe URL
    -- ^ URL to web player able to handle audio recording of this presentation.
    -- Set to 'Nothing' in case that wab player is not provided.
    } deriving Show

instance Default Presentation where
    def = Presentation
        { title = "Unknown title"
        , author = "Unknown author"
        , language = [Cz, En]
        , tags = []
        , slides = Nothing
        , audio = Nothing
        , player = Nothing
        }

data Meetup = Meetup
    { indexM :: Integer
    -- ^ Meetups form a total order in time and is mapped in to linear ordering
    -- of 'Integer' numbers starting from 0.
    , presentations :: [Presentation]
    -- ^ List of presentations presented during meetup.
    , time :: Maybe ZonedTime
    -- ^ Time when the meetup occurred
    , participants :: Maybe Integer
    -- ^ Number of participats, for future, or meetups currently being held,
    -- this is set, obviously, to 'Nothing'.
    } deriving Show

futureMeetup :: Integer -> Meetup
futureMeetup idx = Meetup
    { indexM = idx
    , presentations = []
    , time = Nothing
    , participants = Nothing
    }

-- | Assign index to a 'Meetup'. It expects them to be in reverse order, i.e.
-- newest/future first and oldest as last.
checkMeetupsIndex :: [Meetup] -> [Meetup]
checkMeetupsIndex ms = case areCorrectlyOrdered ms of
    Right ()  -> ms
    Left  msg -> error msg
  where
    areCorrectlyOrdered = zipWithM_ checkIndex [0 ..] . reverse

    checkIndex expectedIndex m@Meetup{indexM = gotIndex} =
        unless (expectedIndex == gotIndex) . Left $ concat
            [ "Expected index ", show expectedIndex
            , ", but got ", show gotIndex
            , " in ", show m
            ]

-- | List of all meetups, those that already occurred and those that are
-- planned. Meetups are ordered from newest/future down to the oldest;
-- so please add new meetups on top.
meetups :: [Meetup]
meetups = checkMeetupsIndex
    [ Meetup
        { indexM = 6
        , presentations =
            [ Presentation
                { title = "Introduction to Agda"
                , author = "Adam Krupicka"
                , language = [En, Sk]
                , tags = [Agda, Theory]
                , slides = Nothing
                , audio = Nothing
                , player = Nothing
                }
            ]
        , time = Nothing
        , participants = Nothing
        }
    , Meetup
        { indexM = 5
        , presentations =
            [ Presentation
                { title = "Types and Higher Groupoids"
                , author = "John Bourke"
                , language = [En]
                , tags = [HoTT, Theory]
                , slides = Nothing
                , audio = Nothing
                , player = Nothing
                }
            ]
        , time = Just $ read "2016-07-27 18:00:00 +02:00"
        , participants = Just 12
        }
    , Meetup
        { indexM = 4
        , presentations =
            [ Presentation
                { title = "Elm - the Best of Functional Programming in Your Browser"
                , author = "Adam Kövári"
                , language = [En]
                , tags = [Elm, Web]
                , slides = Just "fpb-4/elm_best_of_fp_in_browser.pdf"
                , audio = Nothing
                , player = Nothing
                }
            ]
        , time = Just $ read "2016-06-28 18:30:00 +02:00"
        , participants = Just 8
        }
    , Meetup
        { indexM = 3
        , presentations =
            [ Presentation
                { title = "Erlang for Haskellers"
                , author = "Hynek Vychodil"
                , language = [Cz]
                , tags = [Erlang, Concurrent, Reliability, HotCodeSwap]
                , slides = Just "fpb-3/erlang_for_haskellers.pdf"
                , audio = Just "fpb-3/fpb-3.ogg"
                , player = Just "fpb-3/player.html"
                }
            ]
        , time = Just $ read "2015-11-25 18:30:00 +01:00"
        , participants = Just 28
        }
    , Meetup
        { indexM = 2
        , presentations =
            [ Presentation
                { title =
                    "Types as values: Derive correctness from practicality"
                , author = "Peter"
                , language = [Sk]
                , tags = [Haskell, Types]
                , slides = Just "fpb-2/types-as-values.html"
                , audio = Nothing -- I forgot to start recording
                , player = Nothing -- Does not make sense without audio
                }
            ]
        , time = Just $ read "2015-09-30 19:00:00 +02:00"
        , participants = Just 14
        }
    , Meetup
        { indexM = 1
        , presentations =
            [ Presentation
                { title = "Apples and Oranges"
                , author = "Matej"
                , language = [Sk]
                , tags = [Haskell, Types]
                , slides = Just "fpb-1/fpb-1.html"
                , audio = Just "fpb-1/fpb-1.ogg"
                , player = Just "fpb-1/player.html"
                }
            ]
        , time = Just $ read "2015-05-12 18:00:00 +02:00"
        , participants = Just $ 4 + 18
        }
    , Meetup
        { indexM = 0
        , presentations =
            [ Presentation
                { title = "There Is No Compiler"
                , author = "Matej"
                , language = [Sk]
                , tags = [Haskell, Compiler]
                , slides = Just "fpb-0/fpb-0.html"
                , audio = Just "fpb-0/fpb-0.ogg"
                , player = Just "fpb-0/player.html"
                }
            ]
        , time = Just $ read "2015-02-16 19:00:00 +01:00"
        , participants = Just 6
        }
    ]

localjs :: String
localjs = concatMap (dropWhile (' ' ==))
    [ "$(document).ready(function(){"
    , "  $.timeago.settings.allowFuture = true;"
    , "  $(\"time.timeago\").timeago();"
    , "  $.getJSON( \"https://api.github.com/orgs/FPBrno/members\", function(data) {"
    , "    var items = [];"
    , "    $.each(data, function(item) {"
    , "      items.push($(\"<a>\", {href: data[item].html_url}).append($(\"<img>\", {"
    , "        src: data[item].avatar_url,"
    , "        alt: data[item].login,"
    , "        title: data[item].login,"
    , "      })));"
    , "    });"
    , "    $(\".members\").before($(\"<h2>\", {html: \"Public members\"})).empty().append(items);"
    , "  });"
    , "});"
    ]

time2Html :: ZonedTime -> H.Html
time2Html t = H.time H.! A.class_ "timeago" H.! A.datetime (H.toValue . formatISO8601 $ zonedTimeToUTC t) $ H.toHtml (show t)

presentation2html :: Presentation -> H.Html
presentation2html Presentation{..} = H.div H.! A.class_ "presentation" $ do
    classed "presentation_title" title
    " (by "
    H.toHtml author
    ", "
    sequence . intersperse ", " $ map (classedShow "lang") language
    ")"
    mapM_ ((" " >>) . classedShow "tag") tags
    H.div H.! A.class_ "pres_goodies" $ do
        h "Slides" slides
        h "Audio" audio
        h "Player" player
    where
    classed c x = H.span H.! A.class_ c $ H.toHtml x
    classedShow c = classed c . show
    g t u = H.a H.! A.href (H.toValue u) $ t
    h = Foldable.mapM_ . g

presentations2html :: [Presentation] -> H.Html
presentations2html [] = "No presentations"
presentations2html [x] = do
    "Presentation: "
    presentation2html x
presentations2html s = do
    "Presentatios: "
    mapM_ presentation2html s

meetup2html :: Meetup -> H.Html
meetup2html Meetup{..} = H.div H.! A.class_ "meetup" $ do
    H.h3 . H.toHtml $ "Meetup " ++ show indexM
    H.ul $ do
        H.li $ do
            "Time: "
            maybe "To be determined" time2Html time
        maybe mempty (\ n -> H.li ("Participants: " >> H.toHtml (show n))) participants
        H.li $ presentations2html presentations

fpbTitle :: H.Html
fpbTitle = "Functional Programming Brno"

cdns :: H.Html
cdns = mconcat
    [ js "https://code.jquery.com/jquery-2.1.3.min.js"
    , js "https://cdn.rawgit.com/rmm5t/jquery-timeago/master/jquery.timeago.js"
    -- , css "https://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css"
    -- , css "https://fonts.googleapis.com/css?family=Roboto:300,400"
    ] where
        -- css u = H.link H.! A.rel "stylesheet" H.! A.href u
        js  u = H.script H.! A.src u $ mempty

timeago :: H.Html
timeago = H.script $ H.preEscapedToHtml localjs

fa :: String -> H.Html
fa x = H.i H.! A.class_ (H.toValue $ "fa " ++ x) $ mempty

site :: UTCTime -> H.Html
site t = H.html $ do
    H.head $ do
        H.meta H.! A.charset "UTF-8"
        H.title H.! A.class_ "head-title" $ fpbTitle
        H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "style.css"
        cdns
        timeago
    H.body $ H.div H.! A.class_ "wrapper" $ do
        H.header $ do
            H.h1 fpbTitle
            H.img H.! A.src "images/FPB.svg" H.! A.alt "Functional Programming Brno"
        -- H.menu $ do
        --     fa "fa-rss fa-fw" >> "All"
        --     fa "fa-rss fa-fw" >> "Articles"
        --     fa "fa-rss fa-fw" >> "Meetups"
        --     fa "fa-rss fa-fw" >> "Talks"
        H.div H.! A.class_ "main" $ do
            H.p . H.preEscapedToHtml $ intercalate (" " :: String)
                [ "Functional Programming Brno (FPBrno or FPB for short) is for everyone interested in functional programming who happens to be in <a href=\"https://goo.gl/maps/MIRi3\">Brno</a> or nearby areas."
                , "Activities include but are not limited to talks and discussions."
                ]
            H.p . H.preEscapedToHtml $ intercalate (" " :: String)
                [ "We have a mailing list (<a href=\"https://groups.google.com/d/forum/fpbrno\">online archive</a>) that you can sign-up to simply by sending an email to fpbrno+subscribe@googlegroups.com (even an empty email will do)."
                ]
            H.p "More to come."
            H.h2 "Upcoming events"
            let fe = filter (maybe True ((t <=) . zonedTimeToUTC) . time) meetups
            if null fe then
                H.p "There are no planned events now. In the ideal case next meetup will occur between 1 and 2 months after the last meetup."
            else
                mapM_ meetup2html fe
            H.h2 "Past events"
            mapM_ meetup2html . take 10 $ filter (maybe False ((t >) . zonedTimeToUTC) . time) meetups
            H.div H.! A.class_ "members" $ mempty
            H.p $ do
                "Would you like to be on the list?"
                " "
                "It's simple: get a GitHub account and send us a pull request."
                " "
                "If nothing else, add yourself to PEOPLE.md :-)."
        H.footer $ do
            H.a H.! A.href "https://github.com/FPBrno" $ "FPBrno on GitHub"
            " "
            H.a H.! A.href "https://groups.google.com/d/forum/fpbrno" $ "FPBrno mailing list"
            H.div "© 2015 Functional Programming Brno"

main :: IO ()
main = do
    t <- getCurrentTime
    writeFile "index.html" . R.renderHtml $ H.docType >> site t
