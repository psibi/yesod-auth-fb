{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import Yesod
import Yesod.Auth
import Yesod.Facebook
import Yesod.Auth.Facebook.ServerSide
import Facebook (Credentials(..))

fbclientId :: Text
fbclientId = "sample_fb_client_id"

fbclientSecret :: Text
fbclientSecret = "sample_fb_secret"

data App =
  App

mkYesod
  "App"
  [parseRoutes|
/ HomeR GET
/auth AuthR Auth getAuth
|]

instance Yesod App where
  approot = ApprootStatic "http://localhost:3000"

instance YesodFacebook App where
  fbCredentials _ = Credentials "yesod" fbclientId fbclientSecret

instance YesodAuth App where
  type AuthId App = Text
  getAuthId = return . Just . credsIdent
  loginDest _ = HomeR
  logoutDest _ = HomeR
  authPlugins _ = [authFacebook ["user_about_me", "email"]]
  -- The default maybeAuthId assumes a Persistent database. We're going for a
  -- simpler AuthId, so we'll just do a direct lookup in the session.
  maybeAuthId = lookupSession "_ID"

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

getHomeR :: Handler Html
getHomeR = do
  maid <- maybeAuthId
  defaultLayout
    [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
                    <a href=@{AuthR facebookLogout}>Facebook logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

main :: IO ()
main = do
  warp 3000 $ App
