module Component (State, VaultView, OpenedView, Query(..), ui) where

import Prelude

import Api (getVault, saveVault)
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import DOM.Event.KeyboardEvent as KE
import DOM.Event.Types (KeyboardEvent)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Either (Either(Left, Right))
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Network.HTTP.Affjax as AX
import Vault (Vault, Credential, Note)
import VaultComponent as VaultComponent

data VaultView = Credentials | Notes
derive instance eqVaultView :: Eq VaultView

type OpenedEntry a =
  { entry :: a,
    position :: Maybe Int
  }

data OpenedView = OpenedCredential (OpenedEntry Credential) | OpenedNote (OpenedEntry Note)

type State =
  { loading :: Boolean
  , password :: String
  , error :: Maybe String
  , vault :: Maybe Vault
  }

data Slot = VaultSlot
derive instance eqVaultSlot :: Eq Slot
derive instance ordVaultSlot :: Ord Slot

data Query a
  = HandleVault (VaultComponent.Message) a
  | SetPassword String a
  | MakeRequest a
  | OnKeyboard KeyboardEvent a

renderLogin :: forall eff. String -> Boolean ->  H.ParentHTML Query VaultComponent.Query Slot eff
renderLogin password isLoading =
  HH.div_ $
    [ HH.div
        [ HP.class_ (HH.ClassName "mdc-text-field") ]
        [ HH.input
          [ HP.value password
          , HP.type_ InputPassword
          , HP.autocomplete false
          , HP.placeholder "Master password"
          , HE.onValueInput (HE.input SetPassword)
          , HE.onKeyUp (HE.input OnKeyboard)
          , HP.class_ (HH.ClassName "mdc-text-field__input")
          ]
        ]
    , HH.button
        [ HP.class_ (HH.ClassName "mdc-button mdc-button--outlined")
        , HP.disabled isLoading
        , HE.onClick (HE.input_ MakeRequest)
        ]
        [ HH.text "Login" ]
    ]

renderError :: forall eff. Maybe String -> H.ParentHTML Query VaultComponent.Query Slot eff
renderError maybeError =
  HH.div_
      case maybeError of
        Nothing -> []
        Just e ->
          [ HH.h2_
              [ HH.text "Error:" ]
          , HH.pre_
              [ HH.code_ [ HH.text e ] ]
          ]

render :: forall eff. State -> H.ParentHTML Query VaultComponent.Query Slot (Aff (random :: RANDOM, dom :: DOM | eff))
render st =
  HH.div_ $
    [ case st.vault of
        Nothing -> renderLogin st.password st.loading
        Just vault -> HH.slot VaultSlot (VaultComponent.ui vault) unit (HE.input HandleVault)
    , HH.p_
        [ HH.text (if st.loading then "Working..." else "") ]
    , renderError st.error
    , HH.a [ HP.href "download" ] [HH.text "Download"]
    ]

initialState :: State
initialState =
  { loading: false
  , password: ""
  , error: Nothing
  , vault: Nothing
  }

eval :: forall eff a. Query a -> H.ParentDSL State Query VaultComponent.Query Slot Void (Aff (ajax :: AX.AJAX | eff)) a
eval query = case query of
  HandleVault (VaultComponent.Updated vault) next -> do
    _ <- updateVault vault next
    pure next
  SetPassword password next -> do
    H.modify (_ { password = password, vault = Nothing })
    pure next
  MakeRequest next -> loadVault next
  OnKeyboard e next -> do
    let code = KE.code e
    case code of
      "Enter" -> loadVault next
      _ -> pure next

loadVault :: forall eff a. a -> H.ParentDSL State Query VaultComponent.Query Slot Void (Aff (ajax :: AX.AJAX | eff)) a
loadVault next = do
  password <- H.gets _.password
  H.modify (_ { loading = true })
  vaultEither <- H.liftAff $ getVault password
  case vaultEither of
    Right vault -> H.modify (_ { loading = false, vault = Just vault })
    Left e -> H.modify (_ { loading = false, error = Just e })
  pure next

updateVault :: forall eff a. Vault -> a -> H.ParentDSL State Query VaultComponent.Query Slot Void (Aff (ajax :: AX.AJAX | eff)) a
updateVault vault next = do
  password <- H.gets _.password
  H.modify (_ { loading = true })
  responseEither <- H.liftAff $ saveVault vault password
  case responseEither of
    Right response -> H.modify (_ { loading = false })
    Left e -> H.modify (_ { loading = false, error = Just e })
  pure next

ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX, random :: RANDOM, dom :: DOM | eff))
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
