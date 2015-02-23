{-# LANGUAGE OverloadedStrings #-}
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.String as R
-- import qualified Text.Blaze.Html.Renderer.Pretty as R

import Control.Monad
import Data.Time

data Presentation = Presentation
    { title :: String
    , author :: String
    , slides :: Maybe String
    , audio :: Maybe String
    , player :: Maybe String
    } deriving Show

data Meetup = Meetup
    { indexM :: Integer
    , presentations :: [Presentation]
    , time :: UTCTime
    , participants :: Integer
    } deriving Show

meetups =
    [ Meetup
        { indexM = 0
        , presentations =
            [ Presentation
                { title = "There is no Compiler"
                , author = "Matej"
                , slides = Just "fpb-0/fpb-0.html"
                , audio = Just "fpb-0/fpb-0.ogg"
                , player = Just "fpb-0/player.html"
                }
            ]
        , time = read "2014-02-16 19:00:00 CET"
        , participants = 6
        }
    ]

mFromMaybe_ :: Monad m => (a -> m ()) -> Maybe a -> m ()
mFromMaybe_ = maybe (return ())

presentation2html :: Presentation -> H.Html
presentation2html p = H.div H.! A.class_ "presentation" $ do
    H.toHtml (title p)
    " (by "
    H.toHtml (author p)
    ")"
    H.div H.! A.class_ "pres_goodies" $ do
        h "Slides" slides
        h "Audio" audio
        h "Player" player
    where
    g t u = H.a H.! A.href (H.toValue u) $ t
    h a b = mFromMaybe_ (g a) (b p)

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
    H.h2 . H.toHtml $ "Meetup " ++ show (indexM x)
    H.ul $ do
        H.li ("Time: " >> H.toHtml (show $ time x))
        H.li ("Participants: " >> H.toHtml (show $ participants x))
        H.li $ presentations2html (presentations x)

fpbTitle = "Functional Programming Brno"

site = H.html $ do
    H.head $ do
        H.meta H.! A.charset "UTF-8"
        H.title fpbTitle
        H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "style.css"
    H.body $ H.div H.! A.class_ "wrapper" $ do
        H.header $ do
            H.h1 fpbTitle
            H.img H.! A.src "images/FPB.svg" H.! A.alt "Functional Programming Brno"
        H.div H.! A.class_ "main" $ do
            H.p "More to come"
            mapM_ meetup2html . take 10 $ reverse meetups
        H.footer $ do
            H.a H.! A.href "https://github.com/FPBrno" $ "FPBrno on GitHub"
            H.div "© 2015 Functional Programming Brno"

main = writeFile "index.html" . R.renderHtml $ H.docType >> site
