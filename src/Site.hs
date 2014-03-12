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
import           Data.ByteString.Lazy (toStrict)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Util.FileServe
import           Heist
import qualified Heist.Interpreted as I
import           Text.Parsec
import           Network.HTTP.Client
import           Data.Maybe
import           Text.HTML.TagSoup
------------------------------------------------------------------------------
import           Application
import           State
import           Lib

routes :: [(ByteString, AppHandler ())]
routes = [ ("/show_desc/:datestring", handleShowDesc)
         , ("",          serveDirectory "static")
         ]


handleShowDesc :: AppHandler ()
handleShowDesc =
  do date <- fmap fromJust (getParam "datestring")
     let (year,month,day) = case parse dateParser "" date of
                               Left err -> error "Could not parse date"
                               Right res -> res
     mdesc <- getDesc year month day
     modifyResponse $ addHeader "Content-Type" "text/plain; charset=utf-8"
     case mdesc of
       Nothing ->
         do m <- use man
            url <- liftIO $ parseUrl $ "http://www.democracynow.org/shows/" ++ show year ++ "/" ++ show month
            resp <- fmap (T.decodeUtf8 . toStrict . responseBody) $ liftIO $ httpLbs url m
            liftIO $ T.writeFile "/tmp/month.html" resp
            let tags = parseTags resp
            let descs = getShowDescriptions tags
            mapM_ (\((yr,mn,dy), desc) -> do md <- getDesc yr mn dy
                                             when (isNothing md) (newDesc yr mn dy desc)) descs
            case lookup (year,month,day) descs  of
              Nothing -> writeText ""
              Just desc -> writeText desc
       Just desc -> writeText desc
  where dateParser = do year <- count 4 digit
                        char '-'
                        month <- count 2 digit
                        day <- count 2 digit
                        return (read year, read month, read day)


app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db pgsInit
    m <- liftIO $ newManager defaultManagerSettings
    addRoutes routes
    onUnload (closeManager m)
    return $ App h d m
