module EditEntryComponent (State, Query(..), ui) where

import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Random (randomInt, RANDOM)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Char (fromCharCode)
import Data.String (fromCharArray)
import Data.Unfoldable (replicateA)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Vault (Credential(Credential), Note(Note), VaultEntry(NoteEntry, CredentialEntry))

type State =
  { entry :: VaultEntry
  }
-- json-server --watch example.json

data Query a
  = HandleInput VaultEntry a
  | SetCredentialId String a
  | SetCredentialUsername String a
  | SetCredentialPassword String a
  | GeneratePassword a
  | SetNoteTitle String a
  | SetNoteContent String a
  | GetVaultEntry (VaultEntry -> a)

renderEditCredentialForm :: Credential -> H.ComponentHTML Query
renderEditCredentialForm (Credential credential) =
  HH.div_ $
    [ HH.div
        [ HP.class_ (HH.ClassName "mdc-text-field mdc-text-field--fullwidth") ]
        [ HH.input
                [ HP.value credential.id
                , HP.type_ InputText
                , HP.placeholder "Id"
                , HE.onValueInput (HE.input SetCredentialId)
                , HP.class_ (HH.ClassName "mdc-text-field__input")
                ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "mdc-text-field mdc-text-field--fullwidth") ]
        [ HH.input
          [ HP.value credential.username
          , HP.type_ InputText
          , HP.placeholder "Username"
          , HE.onValueInput (HE.input SetCredentialUsername)
          , HP.class_ (HH.ClassName "mdc-text-field__input")
          ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "mdc-text-field mdc-text-field--with-trailing-icon mdc-text-field--fullwidth") ]
        [ HH.input
          [ HP.value credential.password
          , HP.type_ InputText
          , HP.placeholder "Password"
          , HE.onValueInput (HE.input SetCredentialPassword)
          , HP.class_ (HH.ClassName "mdc-text-field__input")
          ]
        , HH.i
          [ HP.class_ (HH.ClassName "material-icons mdc-text-field__icon generate-password")
          , HE.onClick (HE.input_ GeneratePassword)
          ] [HH.text "autorenew"]
        ]
    ]

renderEditNoteForm :: Note -> H.ComponentHTML Query
renderEditNoteForm (Note note) =
  HH.div_ $
    [ HH.div
        [ HP.class_ (HH.ClassName "mdc-text-field mdc-text-field--fullwidth") ]
        [ HH.input
                [ HP.value note.title
                , HP.type_ InputText
                , HP.placeholder "Title"
                , HE.onValueInput (HE.input SetNoteTitle)
                , HP.class_ (HH.ClassName "mdc-text-field__input")
                ]
        ]
    , HH.div
        [ HP.class_ (HH.ClassName "mdc-text-field mdc-text-field--fullwidth mdc-text-field--textarea note-content")]
        [ HH.textarea
          [ HP.value note.content
          , HP.id_ "content"
          , HP.placeholder "Content"
          , HE.onValueInput (HE.input SetNoteContent)
          , HP.class_ (HH.ClassName "mdc-text-field__input")
          ]
        ]
    ]

render :: State -> H.ComponentHTML Query
render st =
  case st.entry of
    CredentialEntry c -> renderEditCredentialForm c
    NoteEntry n -> renderEditNoteForm n

initialState :: VaultEntry -> State
initialState entry =
  { entry: entry
  }

updateCredential :: (Credential -> Credential) -> VaultEntry -> VaultEntry
updateCredential f (CredentialEntry entry) = CredentialEntry $ f entry
updateCredential _ vaultEntry = vaultEntry

updateNote :: (Note -> Note) -> VaultEntry -> VaultEntry
updateNote f (NoteEntry entry) = NoteEntry $ f entry
updateNote _ vaultEntry = vaultEntry

eval :: forall eff a. Query a -> H.ComponentDSL State Query Void (Aff (random :: RANDOM | eff)) a
eval query = case query of
  HandleInput vaultEntry next -> do
    H.modify (_ { entry = vaultEntry })
    pure next
  SetCredentialId credId next -> do
    H.modify (\st -> st { entry = updateCredential (\(Credential c) -> Credential c { id = credId } ) st.entry })
    pure next
  SetCredentialUsername username next -> do
    H.modify (\st -> st { entry = updateCredential (\(Credential c) -> Credential c { username = username } ) st.entry })
    pure next
  SetCredentialPassword password next -> do
    H.modify (\st -> st { entry = updateCredential (\(Credential c) -> Credential c { password = password } ) st.entry })
    pure next
  GeneratePassword next -> do
    randomCodes :: Array Int <- H.liftEff $ replicateA 20 (randomInt 33 126)
    H.modify (\st -> st { entry = updateCredential (\(Credential c) -> Credential c { password = fromCharArray $ map fromCharCode randomCodes } ) st.entry })
    pure next
  SetNoteTitle title next -> do
    H.modify (\st -> st { entry = updateNote (\(Note n) -> Note n { title = title } ) st.entry })
    pure next
  SetNoteContent content next -> do
    H.modify (\st -> st { entry = updateNote (\(Note n) -> Note n { content = content } ) st.entry })
    pure next
  GetVaultEntry reply -> do
    entry <- H.gets _.entry
    pure (reply entry)

ui :: forall eff. VaultEntry ->  H.Component HH.HTML Query VaultEntry Void (Aff (random :: RANDOM | eff))
ui vaultEntry =
  H.component
    { initialState: const (initialState vaultEntry)
    , render
    , eval
    , receiver: HE.input HandleInput
    }
