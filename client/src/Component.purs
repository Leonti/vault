module Component (State, Query(..), ui) where

import Prelude

import Control.Monad.Aff (Aff, attempt)
import Control.Monad.Eff.Exception (message)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Argonaut (decodeJson)
import Data.Either (Either(Left, Right), either)
import Data.Maybe (Maybe(..))
import Data.String.Base64 (encode)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
--import Halogen.Query.InputF (InputF(..))
import Network.HTTP.Affjax as AX
import Network.HTTP.RequestHeader (RequestHeader(..))
import Vault (Vault(..), Credential(..), Note(..))

type State =
  { loading :: Boolean
  , password :: String
  , error :: Maybe String
  , vault :: Maybe Vault
  }

data VaultView = Credentials | Notes

data Query a
  = SetPassword String a
  | MakeRequest a

getVault :: forall eff1. String -> Aff (ajax :: AX.AJAX | eff1) (Either String Vault)
getVault password = do
  res <- attempt $ AX.affjax $ AX.defaultRequest
    { url = "http://localhost:3000/db/1"
    , headers = [RequestHeader "Authorization" ("Basic " <> (encode $ "vault:" <> password)) ]
    }
  let decode r = decodeJson r.response :: Either String Vault
  let vault = either (Left <<< message) decode res
  pure vault

renderCredential :: Credential -> H.ComponentHTML Query
renderCredential (Credential credential) =
  HH.div [ HP.class_ (HH.ClassName "mdc-card") ]
    [ HH.div_ [ HH.text credential.id ]
    , HH.div_ [ HH.text credential.username ]
    , HH.div_ [ HH.text credential.password ]
    ]

renderCredentialList :: Array Credential -> H.ComponentHTML Query
renderCredentialList credentials =
  HH.div_ $ map renderCredential credentials

renderNote :: Note -> H.ComponentHTML Query
renderNote (Note note) =
  HH.div [ HP.class_ (HH.ClassName "mdc-card") ]
    [ HH.div_ [ HH.text note.title ]
    , HH.div_ [ HH.text note.content ]
    ]

renderNoteList :: Array Note -> H.ComponentHTML Query
renderNoteList notes =
  HH.div_ $ map renderNote notes

renderVault :: Vault -> H.ComponentHTML Query
renderVault (Vault vault) =
  HH.div_
    [ HH.div_ [ HH.text "Passwords" ]
    , renderCredentialList vault.credentials
    , HH.div_ [ HH.text "Notes" ]
    , renderNoteList vault.notes
    ]

render :: State -> H.ComponentHTML Query
render st =
  HH.div_ $
    [ HH.label_
        [ HH.input
            [ HP.value st.password
            , HP.type_ InputPassword
            , HP.autocomplete false
            , HP.id_ "some_random"
            , HE.onValueInput (HE.input SetPassword)
            , HP.class_ (HH.ClassName "mdc-text-field__input")
            ]
        ]
    , HH.button
        [ HP.disabled st.loading
        , HE.onClick (HE.input_ MakeRequest)
        ]
        [ HH.text "Fetch info" ]
    , HH.p_
        [ HH.text (if st.loading then "Working..." else "") ]
    , HH.div_
        case st.vault of
          Nothing -> []
          Just vault ->
            [ renderVault vault ]
    , HH.div_
        case st.error of
          Nothing -> []
          Just e ->
            [ HH.h2_
                [ HH.text "Error:" ]
            , HH.pre_
                [ HH.code_ [ HH.text e ] ]
            ]
    ]

initialState :: State
initialState = { loading: false, password: "", error: Nothing, vault: Nothing }

eval :: forall eff. Query ~> H.ComponentDSL State Query Void (Aff (ajax :: AX.AJAX | eff))
eval = case _ of
  SetPassword password next -> do
    H.modify (_ { password = password, vault = Nothing })
    pure next
  MakeRequest next -> do
    password <- H.gets _.password
    H.modify (_ { loading = true })
    vaultEither <- H.liftAff $ getVault password
    case vaultEither of
      Right vault -> H.modify (_ { loading = false, vault = Just vault })
      Left e -> H.modify (_ { loading = false, error = Just e })
    pure next

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX | eff))
ui =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
