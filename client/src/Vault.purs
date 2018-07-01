module Vault (Vault(..), Credential(..), Note(..), VaultEntry(..)) where

import Prelude
import Data.Newtype (class Newtype)
import Data.Argonaut (class DecodeJson, decodeJson, class EncodeJson, jsonEmptyObject, (~>), (:=), (.?))

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

data VaultEntry = CredentialEntry Credential | NoteEntry Note

derive instance newtypeCredential :: Newtype Credential _
derive instance newtypeNote :: Newtype Note _

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

instance encodeJsonCredential :: EncodeJson Credential where
  encodeJson (Credential credential)
     = "id" := credential.id
    ~> "username" := credential.username
    ~> "password" := credential.password
    ~> jsonEmptyObject

instance encodeJsonNote :: EncodeJson Note where
  encodeJson (Note note)
     = "title" := note.title
    ~> "content" := note.content
    ~> jsonEmptyObject

instance encodeJsonVault :: EncodeJson Vault where
  encodeJson (Vault vault)
     = "credentials" := vault.credentials
    ~> "notes" := vault.notes
    ~> jsonEmptyObject
