module Vault (Vault(..), Credential(..), Note(..)) where

import Prelude
import Data.Argonaut (class DecodeJson, decodeJson, (.?))

newtype Credential = Credential
  { id :: String
  , username :: String
  , password :: String }

newtype Note = Note
  { title :: String
  , content :: String
  }

newtype Vault = Vault
  { credentials :: Array Credential
  , notes :: Array Note
  }

instance decodeJsonCredential :: DecodeJson Credential where
  decodeJson json = do
    obj <- decodeJson json
    id <- obj .? "id"
    username <- obj .? "username"
    password <- obj .? "password"
    pure $ Credential { id: id, username: username, password: password }

instance decodeJsonNote :: DecodeJson Note where
  decodeJson json = do
    obj <- decodeJson json
    title <- obj .? "title"
    content <- obj .? "content"
    pure $ Note { title: title, content: content }

instance decodeJsonVault :: DecodeJson Vault where
  decodeJson json = do
    obj <- decodeJson json
    credentials <- obj .? "credentials"
    notes <- obj .? "notes"
    pure $ Vault { credentials: credentials, notes: notes }
