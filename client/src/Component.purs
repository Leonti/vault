module Component (State, Page(..), LoginState(..), VaultView, Query(..), ui) where

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
import Vault (Vault)
import VaultComponent as VaultComponent

data VaultView = Credentials | Notes
derive instance eqVaultView :: Eq VaultView

type OpenedEntry a =
  { entry :: a,
    position :: Maybe Int
  }

data LoginState = EnteringPassword String | Loading | FailedLogin String

data Page = LoginPage LoginState | VaultPage String Vault VaultComponent.VaultSave

type State = Page

data Slot = VaultSlot
derive instance eqVaultSlot :: Eq Slot
derive instance ordVaultSlot :: Ord Slot

data Query a
  = HandleVault (VaultComponent.Message) a
  | SetPassword String a
  | OnKeyboard KeyboardEvent a

type MainComponentHTML eff = H.ParentHTML Query VaultComponent.Query Slot eff

renderLogin :: forall eff. String -> MainComponentHTML eff
renderLogin password =
  HH.div_ [ HH.div
            [ HP.class_ (HH.ClassName "mdc-text-field mdc-text-field--fullwidth") ]
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
        ]

renderError :: forall eff. String -> MainComponentHTML eff
renderError msg =
  HH.div [ HP.class_ (HH.ClassName "mdc-snackbar mdc-snackbar--active") ]
    [ HH.div [ HP.class_ (HH.ClassName "mdc-snackbar__text") ] [ HH.text msg ] ]

renderLoginPage :: forall eff. LoginState -> MainComponentHTML eff
renderLoginPage loginState =
  HH.div [ HP.class_ (HH.ClassName "mdc-layout-grid login-grid") ]
    [ HH.div [ HP.class_ (HH.ClassName "mdc-layout-grid__inner login-grid-inner") ]
      [ HH.div [ HP.class_ (HH.ClassName "mdc-layout-grid__cell mdc-layout-grid__cell--span-4") ] []
      , HH.div [ HP.class_ (HH.ClassName "mdc-layout-grid__cell mdc-layout-grid__cell--align-middle") ]
        [ case loginState of
            EnteringPassword password -> HH.div_ [ renderLogin password ]
            Loading -> HH.h4 [HP.class_ $ HH.ClassName "login-loading mdc-typography--headline4"] [ HH.text "Loading..." ]
            FailedLogin error -> HH.div_ [
              renderLogin ""
              , renderError error
            ]
        ]
      , HH.div [ HP.class_ (HH.ClassName "mdc-layout-grid__cell mdc-layout-grid__cell--span-12 mdc-layout-grid__cell--align-bottom") ]
        [ HH.a [ HP.class_ (HH.ClassName ""), HP.href "download" ]
            [ HH.i [ HP.class_ (HH.ClassName "material-icons mdc-text-field__icon edit-icon") ]
              [ HH.text "vertical_align_bottom" ]
            ]
        ]
      ]
    ]

render :: forall eff. State -> MainComponentHTML (Aff (random :: RANDOM, dom :: DOM | eff))
render st = case st of
  LoginPage loginState -> renderLoginPage loginState
  VaultPage _ vault vaultSave -> HH.slot VaultSlot (VaultComponent.ui vault) vaultSave (HE.input HandleVault)

initialState :: State
initialState = LoginPage $ EnteringPassword ""

eval :: forall eff a. Query a -> H.ParentDSL State Query VaultComponent.Query Slot Void (Aff (ajax :: AX.AJAX | eff)) a
eval query = case query of
  HandleVault (VaultComponent.Updated vault) next -> do
    _ <- updateVault vault next
    pure next
  SetPassword password next -> do
    H.put $ LoginPage (EnteringPassword password)
    pure next
  OnKeyboard e next -> do
    let code = KE.code e
    case code of
      "Enter" -> loadVault next
      _ -> pure next

loadVault :: forall eff a. a -> H.ParentDSL State Query VaultComponent.Query Slot Void (Aff (ajax :: AX.AJAX | eff)) a
loadVault next = do
  state <- H.get
  case state of
    LoginPage (EnteringPassword password) -> do
      H.put $ LoginPage Loading
      vaultEither <- H.liftAff $ getVault password
      case vaultEither of
        Right vault -> H.put $ VaultPage password vault VaultComponent.Saved
        Left e -> H.put $ LoginPage (FailedLogin e)
      pure next
    _ -> pure next

updateVault :: forall eff a. Vault -> a -> H.ParentDSL State Query VaultComponent.Query Slot Void (Aff (ajax :: AX.AJAX | eff)) a
updateVault vault next = do
  state <- H.get
  case state of
    VaultPage password _ _ -> do
      H.put $ VaultPage password vault VaultComponent.Saving
      responseEither <- H.liftAff $ saveVault vault password
      case responseEither of
        Right response -> H.put $ VaultPage password vault VaultComponent.Saved
        Left e ->  H.put $ VaultPage password vault (VaultComponent.SaveError e)
      pure next
    _ -> pure next


ui :: forall eff. H.Component HH.HTML Query Unit Void (Aff (ajax :: AX.AJAX, random :: RANDOM, dom :: DOM | eff))
ui =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
