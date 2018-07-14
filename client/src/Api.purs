module Api(getVault, saveVault) where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (message, error)
import Data.Argonaut (decodeJson, encodeJson)
import Data.Bifunctor (lmap)
import Data.Either (Either(Left, Right), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just))
import Data.String.Base64 (encode)
import Network.HTTP.Affjax as AX
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.StatusCode (StatusCode(..))
import Vault (Vault)

getVault :: forall eff1. String -> Aff (ajax :: AX.AJAX | eff1) (Either String Vault)
getVault password = do
  res <- attempt $ AX.affjax $ AX.defaultRequest
    { url = "/db"
    , headers = [RequestHeader "Authorization" ("Basic " <> (encode $ "vault:" <> password)) ]
    }
  let decode r = decodeJson r.response :: Either String Vault
  let vault = either (Left <<< message) decode res
  pure vault

saveVault :: forall eff1. Vault -> String -> Aff (ajax :: AX.AJAX | eff1) (Either String String)
saveVault vault password = do
  res <- attempt $ AX.affjax $ AX.defaultRequest
    { method = Left PUT
    , url = "/db"
    , content = Just $ encodeJson vault
    , headers = [RequestHeader "Authorization" ("Basic " <> (encode $ "vault:" <> password)) ]
    }

  let resp = do
              r <- res
              case r.status of
                (StatusCode code) | code >= 400 -> Left $ error $ "Error " <> show code
                _ -> Right r.response
  pure $ lmap message resp
