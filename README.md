yesod-auth-fb
---------------

[![Build Status](https://dev.azure.com/psibi2000/Haskell%20Projects/_apis/build/status/psibi.yesod-auth-fb?branchName=master)](https://dev.azure.com/psibi2000/Haskell%20Projects/_build/latest?definitionId=18&branchName=master)
[![Hackage](https://img.shields.io/hackage/v/yesod-auth-fb.svg)](https://hackage.haskell.org/package/yesod-auth-fb)
[![Stackage
Nightly](http://stackage.org/package/yesod-auth-fb/badge/nightly)](http://stackage.org/nightly/package/yesod-auth-fb)
[![Stackage
LTS](http://stackage.org/package/yesod-auth-fb/badge/lts)](http://stackage.org/lts/package/yesod-auth-fb)

Authentication backend for Yesod using Facebook

# Demo

Sample code showing Facebook authentication in action:

```
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
main = warp 3000 App
```
