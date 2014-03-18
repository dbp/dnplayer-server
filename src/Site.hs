{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Applicative
import           Control.Monad (when)
import           Control.Monad.Trans (liftIO)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (toStrict)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import qualified Data.ByteString.Char8 as B8
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import           Text.Parsec
import           Network.HTTP.Client
import           Network.HTTP.Types.Header
import           Data.Maybe
import           Text.HTML.TagSoup
------------------------------------------------------------------------------
import           Application
import           State
import           Lib

routes :: [(ByteString, AppHandler ())]
routes = [ ("/show_desc/:datestring", handleShowDesc)
         , ("/shows_desc_month/:shows", handleShowManyMonth)
         , ("/media/:datestring", handleMedia)
         , ("",          serveDirectory "static")
         ]



dateParser = do year <- count 4 digit
                char '-'
                month <- count 2 digit
                day <- count 2 digit
                return (read year, read month, read day)

handleShowManyMonth :: AppHandler ()
handleShowManyMonth = do showsstr <- fmap fromJust (getParam "shows")
                         modifyResponse $ addHeader "Content-Type" "text/plain; charset=utf-8"
                         let shows = case parse showsParser "" showsstr of
                                       Left err -> error "Could not parse shows"
                                       Right res -> res
                         res <- loadShows shows
                         writeText (T.intercalate "\n" res)
  where showsParser = dateParser `sepBy` (char ',')

-- NOTE(dbp 2014-03-13): This looks odd, but it captures the pattern -
-- we load from database until we hit the first that isn't there, this
-- triggers an http request, but if we hit another failure, we no
-- longer make requests, as these are all supposed to be dates within
-- one month, and an entire month is loaded at once.
loadShows :: [(Int,Int,Int)] -> AppHandler [Text]
loadShows [] = return []
loadShows allShows@((y,m,d):xs) = do mdesc <- getDesc y m d
                                     case mdesc of
                                       Nothing -> lookupShows allShows
                                       Just s -> do rest <- loadShows xs
                                                    return (s:rest)

lookupShows :: [(Int,Int,Int)] -> AppHandler [Text]
lookupShows [] = return []
lookupShows ((y,m,d):xs) = do ms <- lookupShow y m d
                              case ms of
                                Nothing -> do rest <- lookupShows' xs
                                              return ("":rest)
                                Just (s,_) -> do rest <- lookupShows' xs
                                                 return (s:rest)
  where lookupShows' [] = return []
        lookupShows' ((y,m,d):xs) = do mdesc <- getDesc y m d
                                       case mdesc of
                                         Nothing -> do rest <- lookupShows' xs
                                                       return ("":rest)
                                         Just desc -> do rest <- lookupShows' xs
                                                         return (desc:rest)

lookupShow :: Int -> Int -> Int -> AppHandler (Maybe (Text, Text))
lookupShow year month day =
  do m <- use man
     url <- liftIO $ parseUrl $ "http://www.democracynow.org/shows/" ++ show year ++ "/" ++ show month
     resp <- fmap (T.decodeUtf8 . toStrict . responseBody) $ liftIO $ httpLbs url m
     let tags = parseTags resp
     let descs = getShowDescriptionsUrls tags
     mapM_ (\((yr,mn,dy), (desc, url)) ->
              do md <- getDesc yr mn dy
                 when (isNothing md) (newDesc yr mn dy desc)
                 mm <- getMedia yr mn dy
                 when (isNothing mm) (newMedia yr mn dy url)) descs
     return $ lookup (year,month,day) descs

handleShowDesc :: AppHandler ()
handleShowDesc =
  do date <- fmap fromJust (getParam "datestring")
     let (year,month,day) = case parse dateParser "" date of
                               Left err -> error "Could not parse date"
                               Right res -> res
     mdesc <- getDesc year month day
     modifyResponse $ addHeader "Content-Type" "text/plain; charset=utf-8"
     case mdesc of
       Nothing -> do s <- lookupShow year month day
                     case s of
                       Nothing -> writeText ""
                       Just (desc, _) -> writeText desc
       Just desc -> writeText desc


{-
Present to May 4, 2012 - dvlabs
Nov 10th 2008 to May 3, 2012 -
  http://blip.tv/file/get/Demnow-DemocracyNowMondayNovember102008592.mp4
  PROBLEM: What is the three digit number at the end? Do we need to scrape all of these urls?
Sept 2nd 2008 to Nov 7th, 2008 -
  http://gfx.dvlabs.com/democracynow/ipod/dn2008-1105.mp4
July 28th 2008 to Sept 1st, 2008 - dvlabs
July 27th 2008 to September 17th, 2001 -
  http://www.archive.org/download/dn2008-0130_vid/dn2008-0130_512kb.mp4
September 16th, 2001 to February 19th, 1996 -
  http://www.archive.org/download/dn2000-1229/dn2000-1229-1_64kb.mp3
-}

handleMedia :: AppHandler ()
handleMedia = do datestring <- fmap fromJust (getParam "datestring")
                 let (year,month,day) = case parse dateParser "" datestring of
                               Left err -> error "Could not parse date"
                               Right res -> res
                 if year > 2012 ||
                    (year == 2012 && month > 5) ||
                    (year == 2012 && month == 5 && day > 3)
                    then redirect $ B.concat ["http://dncdn.dvlabs.com/ipod/dn", datestring, ".mp4"]
                    else doLookup year month day
  where doLookup y m d =
          do mm <- getMedia y m d
             case mm of
               Just url -> redirect (T.encodeUtf8 url)
               Nothing -> do mr <- lookupShow y m d
                             case mr of
                               Nothing -> pass
                               Just (_, url) -> redirect (T.encodeUtf8 url)

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db pgsInit
    m <- liftIO $ newManager defaultManagerSettings
    addRoutes routes
    onUnload (closeManager m)
    return $ App h d m
