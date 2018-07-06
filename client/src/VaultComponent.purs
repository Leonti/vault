module VaultComponent (State, VaultView, Message(..), OpenedEntry, Query(..), ui) where

import Prelude

import CSS (Selector, fromString)
import Clipboard as C
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM)
import DOM (DOM)
import DOM.HTML.Indexed.InputType (InputType(..))
import DOM.Node.Element (getAttribute)
import DOM.Node.Types (Element)
import Data.Array (filter, length, range, zip, updateAt, deleteAt)
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (over)
import Data.String (contains, Pattern(..), toLower)
import Data.Tuple (Tuple(..))
import EditEntryComponent as EditEntryComponent
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Vault (Credential(Credential), Note(Note), Vault(Vault), VaultEntry(..))

data VaultView = Credentials | Notes
derive instance eqVaultView :: Eq VaultView

type OpenedEntry =
  { entry :: VaultEntry,
    position :: Maybe Int
  }

type State =
  { vault :: Vault
  , vaultView :: VaultView
  , maybeOpenedEntry :: Maybe OpenedEntry
  , maybeFilter :: Maybe String
  , maybeDelete :: Maybe Int
  }
-- json-server --watch example.json

data Message = Updated Vault

data ChildSlot = EditEntrySlot
derive instance eqEditEntrySlot :: Eq ChildSlot
derive instance ordEditEntrySlot :: Ord ChildSlot

data Query a
  = Initialize a
  | SwitchTab VaultView a
  | SearchFilter String a
  | AddEntry a
  | SaveEntry a
  | CancelEdit a
  | EditCredential Credential Int a
  | EditNote Note Int a
  | ConfirmedNoteDelete Int a
  | Delete Int a
  | ConfirmedCredentialDelete Int a
  | CancelDelete a

renderCredential :: forall eff. (Tuple Int Credential) -> H.ParentHTML Query EditEntryComponent.Query ChildSlot eff
renderCredential (Tuple i (Credential credential)) =
  HH.div [ HP.class_ (HH.ClassName "mdc-card") ]
    [ HH.div_ [ HH.text credential.id ]
    , HH.div_ [ HH.text credential.username ]
    , HH.div_ [ HH.text credential.password ]
    , HH.button
        [ HP.class_ (HH.ClassName "test-selector")
        , HP.attr (HH.AttrName "data-copy-text") credential.password ]
        [ HH.text "Copy" ]
    , HH.div []
      [ HH.i
        [ HP.class_ (HH.ClassName "material-icons mdc-text-field__icon edit-icon")
        , HE.onClick (HE.input_ (EditCredential (Credential credential) i))
        ] [HH.text "edit"]
      , HH.i
        [ HP.class_ (HH.ClassName "material-icons mdc-text-field__icon edit-icon")
        , HE.onClick (HE.input_ (Delete i))
        ] [HH.text "delete"]
      ]
    ]

renderDeleteCredentialConfirmation ::  forall eff. (Tuple Int Credential) -> VaultComponentHTML eff
renderDeleteCredentialConfirmation (Tuple i (Credential credential)) =
  HH.div [ HP.class_ (HH.ClassName "mdc-card") ]
    [ HH.div_ [ HH.text $ "Are you sure you want to delete credential: " <> credential.id ]
    , HH.div []
      [ HH.button
          [ HP.class_ (HH.ClassName "mdc-button mdc-button--outlined")
          , HE.onClick (HE.input_ (ConfirmedCredentialDelete i)) ]
          [ HH.text "Delete" ]
      , HH.button
          [ HP.class_ (HH.ClassName "mdc-button")
          , HE.onClick (HE.input_ CancelDelete) ]
          [ HH.text "Cancel" ]
      ]
    ]

credentialFilter :: String -> Credential -> Boolean
credentialFilter filter (Credential c) = contains pattern (toLower c.id) || contains pattern (toLower c.username) || contains pattern (toLower c.password)
  where
    pattern = (Pattern $ toLower filter)

noteFilter :: String -> Note -> Boolean
noteFilter filter (Note n) = contains pattern (toLower n.title) || contains pattern (toLower n.content)
  where
    pattern = (Pattern $ toLower filter)

type VaultComponentHTML eff = H.ParentHTML Query EditEntryComponent.Query ChildSlot eff

renderNote :: forall eff. (Tuple Int Note) -> VaultComponentHTML eff
renderNote (Tuple i (Note note)) =
  HH.div [ HP.class_ (HH.ClassName "mdc-card") ]
    [ HH.div_ [ HH.text note.title ]
    , HH.div [ HP.class_ (HH.ClassName "note-content") ] [ HH.text note.content ]
    , HH.div []
      [ HH.i
        [ HP.class_ (HH.ClassName "material-icons mdc-text-field__icon edit-icon")
        , HE.onClick (HE.input_ (EditNote (Note note) i))
        ] [HH.text "edit"]
      , HH.i
        [ HP.class_ (HH.ClassName "material-icons mdc-text-field__icon edit-icon")
        , HE.onClick (HE.input_ (Delete i))
        ] [HH.text "delete"]
      ]
    ]

renderDeleteNoteConfirmation ::  forall eff. (Tuple Int Note) -> VaultComponentHTML eff
renderDeleteNoteConfirmation (Tuple i (Note note)) =
  HH.div [ HP.class_ (HH.ClassName "mdc-card") ]
    [ HH.div_ [ HH.text $ "Are you sure you want to delete note: " <> note.title ]
    , HH.div []
      [ HH.button
          [ HP.class_ (HH.ClassName "mdc-button mdc-button--outlined")
          , HE.onClick (HE.input_ (ConfirmedNoteDelete i)) ]
          [ HH.text "Delete" ]
      , HH.button
          [ HP.class_ (HH.ClassName "mdc-button")
          , HE.onClick (HE.input_ CancelDelete) ]
          [ HH.text "Cancel" ]
      ]
    ]

renderEntries :: forall eff a. (Tuple Int a -> VaultComponentHTML eff) -> (a -> Boolean) -> Array a -> VaultComponentHTML eff
renderEntries toHtml f entries = HH.div_ $ map toHtml $ filter (\(Tuple _ entry) -> f entry) (zip (range 0 (length entries)) entries)

renderVault :: forall eff. State -> H.ParentHTML Query EditEntryComponent.Query ChildSlot (Aff (random :: RANDOM | eff))
renderVault st =
  HH.div_
    [ HH.nav
      [ HP.class_ (HH.ClassName "mdc-tab-bar") ]
      [ HH.a
        [ HP.class_ (HH.ClassName $ tabClass Credentials st.vaultView)
        , HE.onClick (HE.input_ $ SwitchTab Credentials)
        ]
        [ HH.text "Credentials"
        , HH.span [ HP.class_ (HH.ClassName "mdc-tab__indicator") ] []
        ]
      , HH.a
        [ HP.class_ (HH.ClassName $ tabClass Notes st.vaultView)
        , HE.onClick (HE.input_ $ SwitchTab Notes)
        ]
        [ HH.text "Notes"
        , HH.span [ HP.class_ (HH.ClassName "mdc-tab__indicator") ] []
        ]
      ]
    , HH.div
        [ HP.class_ (HH.ClassName "mdc-text-field mdc-text-field--fullwidth") ]
        [ HH.input
                [ HP.type_ InputText
                , HP.placeholder "Search"
                , HE.onValueInput (HE.input SearchFilter)
                , HP.class_ (HH.ClassName "mdc-text-field__input")
                ]
        ]
    , case st.maybeOpenedEntry of
        Nothing -> HH.button
                    [ HP.class_ (HH.ClassName "mdc-button mdc-button--outlined")
                    , HE.onClick (HE.input_ AddEntry) ]
                    [ HH.text "Add" ]
        Just _ -> HH.div_ []
    , case st.maybeOpenedEntry of
        Nothing -> HH.div_ []
        Just openedEntry ->
          if isJust openedEntry.position
            then HH.div_ []
            else HH.slot EditEntrySlot (EditEntryComponent.ui openedEntry.entry ) openedEntry.entry absurd
    , case st.maybeOpenedEntry of
        Just openedEntry -> case openedEntry.position of
          Just _ -> HH.div_ []
          Nothing -> renderEditActions
        Nothing -> HH.div_ []
    , renderVaultList st.vault st.vaultView st.maybeFilter st.maybeDelete st.maybeOpenedEntry
    ]

renderEditActions :: forall eff. VaultComponentHTML eff
renderEditActions = HH.div_
    [ HH.button
        [ HP.class_ (HH.ClassName "mdc-button mdc-button--outlined")
        , HE.onClick (HE.input_ SaveEntry) ]
        [ HH.text "Save" ]
    , HH.button
      [ HP.class_ (HH.ClassName "mdc-button")
      , HE.onClick (HE.input_ CancelEdit) ]
      [ HH.text "Cancel" ]
    ]

renderVaultList :: forall eff. Vault -> VaultView -> Maybe String -> Maybe Int -> Maybe OpenedEntry -> VaultComponentHTML (Aff (random :: RANDOM | eff))
renderVaultList (Vault vault) Credentials maybeFilter maybeDelete maybeOpenedEntry = renderEntries renderEntity (maybe (const true) credentialFilter maybeFilter) vault.credentials
  where
    renderEntity e@(Tuple i _) = case maybeDelete of
      Just position -> if position == i then renderDeleteCredentialConfirmation e else renderCredential e
      Nothing -> case maybeOpenedEntry of
        Just openedEntry -> case openedEntry.position of
          Just p -> if p == i then
              HH.div_
                [HH.slot EditEntrySlot (EditEntryComponent.ui openedEntry.entry ) openedEntry.entry absurd
                , renderEditActions
                ]
            else
              renderCredential e
          Nothing -> renderCredential e
        Nothing -> renderCredential e
renderVaultList (Vault vault) Notes maybeFilter maybeDelete maybeOpenedEntry = renderEntries renderEntity (maybe (const true) noteFilter maybeFilter) vault.notes
  where
    renderEntity e@(Tuple i _) = case maybeDelete of
      Just position -> if position == i then renderDeleteNoteConfirmation e else renderNote e
      Nothing -> renderNote e

tabClass :: VaultView -> VaultView -> String
tabClass tab currentView = "mdc-tab" <> (if tab == currentView then " mdc-tab--active" else "")

addOrUpdateEntry :: VaultEntry -> Maybe Int -> Vault -> Vault
addOrUpdateEntry (CredentialEntry credential) Nothing (Vault vault) = Vault $ vault { credentials = [credential] <> vault.credentials }
addOrUpdateEntry (CredentialEntry credential) (Just position) (Vault vault) = Vault $ vault { credentials = credentials }
  where
    credentials = case updateAt position credential vault.credentials of
      Just creds -> creds
      Nothing -> vault.credentials
addOrUpdateEntry (NoteEntry note) Nothing (Vault vault) = Vault $ vault { notes = [note] <> vault.notes }
addOrUpdateEntry (NoteEntry note) (Just position) (Vault vault) = Vault $ vault { notes = notes }
  where
    notes = case updateAt position note vault.notes of
      Just ns -> ns
      Nothing -> vault.notes

render :: forall eff. State -> H.ParentHTML Query EditEntryComponent.Query ChildSlot (Aff (random :: RANDOM | eff))
render st =
  HH.div_ $
    [ renderVault st
    ]

initialState :: Vault -> State
initialState vault =
  { vault: vault
  , vaultView: Credentials
  , maybeOpenedEntry: Nothing
  , maybeFilter: Nothing
  , maybeDelete: Nothing
  }

setPassword :: String -> Credential -> Credential
setPassword password = over Credential (_ { password = password })

stringFromAttr :: forall eff. String -> Element -> Eff (dom :: DOM | eff) String
stringFromAttr attr el = fromMaybe "" <$> getAttribute attr el

testSelector :: forall eff. Selector -> Eff (dom :: DOM | eff) Unit
testSelector sel = void $ C.fromCSSSelector sel $ stringFromAttr "data-copy-text"

eval :: forall eff a. Query a -> H.ParentDSL State Query EditEntryComponent.Query ChildSlot Message (Aff (dom :: DOM | eff)) a
eval query = case query of
  Initialize next -> do
    H.liftEff $ testSelector $ fromString ".test-selector"
    pure next
  SearchFilter filter next -> do
    H.modify (_ { maybeFilter = if filter /= "" then Just filter else Nothing })
    pure next
  SwitchTab vaultView next -> do
    H.modify (_ { vaultView = vaultView, maybeOpenedEntry = Nothing, maybeDelete = Nothing })
    pure next
  AddEntry next -> do
    vaultView <- H.gets _.vaultView
    case vaultView of
      Credentials -> do
        H.modify (_ { maybeOpenedEntry = Just
          { entry: CredentialEntry $ Credential
            { id: ""
            , username: ""
            , password: ""
            }
          , position: Nothing
          } })
        pure next
      Notes -> do
        H.modify (_ { maybeOpenedEntry = Just
          { entry: NoteEntry $ Note
            { title: ""
            , content: ""
            }
          , position: Nothing
          } })
        pure next
  SaveEntry next -> do
    maybeVaultEntry <- H.query EditEntrySlot $ H.request EditEntryComponent.GetVaultEntry
    case maybeVaultEntry of
      Just vaultEntry -> do
        st <- H.get
        let vault = addOrUpdateEntry vaultEntry (bind st.maybeOpenedEntry (_.position)) st.vault
        H.modify (_ { vault = vault, maybeOpenedEntry = Nothing })
        H.raise $ Updated vault
        pure next
      Nothing ->
        pure next
  CancelEdit next -> do
        H.modify (_ { maybeOpenedEntry = Nothing })
        pure next
  EditCredential credential position next -> do
    H.modify (_ { maybeOpenedEntry = Just
      { entry: CredentialEntry credential
      , position: Just position
      } })
    pure next
  EditNote note position next -> do
    H.modify (_ { maybeOpenedEntry = Just
      { entry: NoteEntry note
      , position: Just position
      } })
    pure next
  Delete position next -> do
    H.modify (_ { maybeDelete = Just position })
    pure next
  ConfirmedCredentialDelete position next -> do
    st <- H.get
    let (Vault vault) = st.vault
    let updatedVault = Vault $ vault { credentials = fromMaybe vault.credentials $ deleteAt position vault.credentials }
    H.modify (_ { vault = updatedVault, maybeDelete = Nothing })
    H.raise $ Updated updatedVault
    pure next
  ConfirmedNoteDelete position next -> do
    st <- H.get
    let (Vault vault) = st.vault
    let updatedVault = Vault $ vault { notes = fromMaybe vault.notes $ deleteAt position vault.notes }
    H.modify (_ { vault = updatedVault, maybeDelete = Nothing })
    H.raise $ Updated updatedVault
    pure next
  CancelDelete next -> do
    H.modify (_ { maybeDelete = Nothing })
    pure next

ui :: forall eff. Vault -> H.Component HH.HTML Query Unit Message (Aff (random :: RANDOM, dom :: DOM | eff))
ui vault =
  H.lifecycleParentComponent
    { initialState: const (initialState vault)
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    , render
    , eval
    , receiver: const Nothing
    }
