module Api(getVault, saveVault) where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (message, Error)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just))
import Data.String.Base64 (encode)
import Network.HTTP.Affjax as AX
import Network.HTTP.RequestHeader (RequestHeader(..))
import Vault (Vault)

getVault :: forall eff1. String -> Aff (ajax :: AX.AJAX | eff1) (Either String Vault)
getVault password = do
  res <- attempt $ AX.affjax $ AX.defaultRequest
    { url = "http://localhost:3000/db/1"
    , headers = [RequestHeader "Authorization" ("Basic " <> (encode $ "vault:" <> password)) ]
    }
  let decode r = decodeJson r.response :: Either String Vault
  let vault = either (Left <<< message) decode res
  pure vault

saveVault :: forall eff1. Vault -> String -> Aff (ajax :: AX.AJAX | eff1) (Either String String)
saveVault vault password = do
  res <- attempt $ AX.affjax $ AX.defaultRequest
    { method = Left PUT
    , url = "http://localhost:3000/db/1"
    , content = Just $ encodeJson vault
    , headers = [RequestHeader "Authorization" ("Basic " <> (encode $ "vault:" <> password)) ]
    }
  let resp = map _.response res :: Either Error String
  pure $ lmap message resp
