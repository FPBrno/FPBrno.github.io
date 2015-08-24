{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
import qualified Data.Foldable as Foldable
import qualified Text.Blaze.Html.Renderer.String as R
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
-- import qualified Text.Blaze.Html.Renderer.Pretty as R

import Data.Default.Class (Default(def))
import Data.List (intercalate)
import Data.Monoid
import Data.Time
import Data.Time.ISO8601 (formatISO8601)

data Tag
    = Compiler
    | Haskell
    | Types
    deriving Show

type URL = String

data Presentation = Presentation
    { title :: String
    , author :: String
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

instance Default Meetup where
    def = Meetup
        { indexM = -1
        , presentations = []
        , time = Nothing
        , participants = Nothing
        }

-- | Assign index to a 'Meetup'. It expects them to be in reverse order, i.e.
-- newest/future first and oldest as last.
indexMeetups :: [Meetup] -> [Meetup]
indexMeetups = reverse . zipWith (\idx m -> m{indexM = idx}) [0..] . reverse

-- | List of all meetups, those that already occurred and those that are
-- planned. Meetups are ordered from newest/future down to the oldes; so please
-- add new meetups on top.
meetups :: [Meetup]
meetups = indexMeetups
    [ def
        { presentations =
            [ def
                { title =
                    "Types as values: Derive correctness from practicality"
                , author = "Peter"
                , tags = [Haskell, Types]
                }
            ]
        }
    , def
        { presentations =
            [ Presentation
                { title = "Apples and Oranges"
                , author = "Matej"
                , tags = [Haskell, Types]
                , slides = Just "fpb-1/fpb-1.html"
                , audio = Just "fpb-1/fpb-1.ogg"
                , player = Just "fpb-1/player.html"
                }
            ]
        , time = Just $ read "2015-05-12 18:00:00 +02:00"
        , participants = Just $ 4 + 18
        }
    , def
        { presentations =
            [ Presentation
                { title = "There Is No Compiler"
                , author = "Matej"
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
presentation2html p = H.div H.! A.class_ "presentation" $ do
    H.span H.! A.class_ "presentation_title" $ H.toHtml (title p)
    " (by "
    H.toHtml (author p)
    ")"
    mapM_ (\ t -> " " >> ((H.span H.! A.class_ "tag") $ H.toHtml (show t))) (tags p)
    H.div H.! A.class_ "pres_goodies" $ do
        h "Slides" slides
        h "Audio" audio
        h "Player" player
    where
    g t u = H.a H.! A.href (H.toValue u) $ t
    h a b = Foldable.mapM_ (g a) (b p)

presentations2html :: [Presentation] -> H.Html
presentations2html [] = "No presentations"
presentations2html [x] = do
    "Presentation: "
    presentation2html x
presentations2html s = do
    "Presentatios: "
    mapM_ presentation2html s

meetup2html :: Meetup -> H.Html
meetup2html x = H.div H.! A.class_ "meetup" $ do
    H.h3 . H.toHtml $ "Meetup " ++ show (indexM x)
    H.ul $ do
        H.li $ do
            "Time: "
            maybe "To be determined" time2Html $ time x
        maybe mempty (\ n -> H.li ("Participants: " >> H.toHtml (show n))) $ participants x
        H.li $ presentations2html (presentations x)

fpbTitle :: H.Html
fpbTitle = "Functional Programming Brno"

cdns :: H.Html
cdns = mconcat
    [ js "https://code.jquery.com/jquery-2.1.3.min.js"
    , js "https://cdn.rawgit.com/rmm5t/jquery-timeago/master/jquery.timeago.js"
    -- , css "https://maxcdn.bootstrapcdn.com/font-awesome/4.3.0/css/font-awesome.min.css"
    -- , css "https://fonts.googleapis.com/css?family=Roboto:300,400"
    ] where
        css u = H.link H.! A.rel "stylesheet" H.! A.href u
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
                [ "Functional Programming Brno (FPBrno or FPB for short) is for all people with interest in functional programming that happen to be in <a href=\"https://goo.gl/maps/MIRi3\">Brno</a> or nearby areas."
                , "Activities include but are not limited to talks and discussions."
                ]
            H.p . H.preEscapedToHtml $ intercalate (" " :: String)
                [ "We have a mailing list (<a href=\"https://groups.google.com/d/forum/fpbrno\">online archive</a>) that you can sign-up to simply by sending mail to fpbrno+subscribe@googlegroups.com (even empty mail will do)."
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
        H.footer $ do
            H.a H.! A.href "https://github.com/FPBrno" $ "FPBrno on GitHub"
            " "
            H.a H.! A.href "https://groups.google.com/d/forum/fpbrno" $ "FPBrno mailing list"
            H.div "© 2015 Functional Programming Brno"

main :: IO ()
main = do
    t <- getCurrentTime
    writeFile "index.html" . R.renderHtml $ H.docType >> site t
