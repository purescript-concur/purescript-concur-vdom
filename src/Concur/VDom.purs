module Concur.VDom where

import Concur.Core (Widget, runWidget)
import Concur.Core.DOM as CD
import Concur.Core.LiftWidget (class LiftWidget, liftWidget)
import Concur.Core.Types (display, result)
import Control.Applicative (pure)
import Control.Bind (bind, discard)
import Control.MultiAlternative (class MultiAlternative)
import Control.ShiftMap (class ShiftMap)
import Data.Function (identity, ($))
import Data.Functor (class Functor)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un, wrap)
import Data.Traversable (for_)
import Data.Unit (Unit, unit)
import Data.Void (Void, absurd)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Uncurried (runEffectFn1, runEffectFn2)
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop as P
import Halogen.VDom.Machine (Step)
import Halogen.VDom.Thunk as T
import Halogen.VDom.Types (ElemName(..), VDom(..))
import Safe.Coerce (coerce)
import Web.DOM (Node, Element)
import Web.DOM.Document (Document)
import Web.DOM.Element (toNode)
import Web.DOM.Node (appendChild) as DOM
import Web.DOM.ParentNode (querySelector) as Web
import Web.Event.Event (EventType(..))
import Web.HTML (window) as Web
import Web.HTML.HTMLDocument (toDocument, toParentNode) as Web
import Web.HTML.Window (document) as Web

--------------------------
-- Types

type HTMLProps :: Type -> Type
type HTMLProps a = Array (P.Prop a)

type HTMLBody :: Type -> Type
type HTMLBody a = T.Thunk HTMLF a

type HTMLSpec a = V.VDomSpec (HTMLProps a) (HTMLBody a)
type HTMLVDom a = VDom (HTMLProps a) (HTMLBody a)
newtype HTMLF a = HTMLF (HTMLVDom a)

derive instance Functor HTMLF
derive instance Newtype (HTMLF a) _

type HTML1 = HTMLF (Effect Unit)
type HTML = Array HTML1

--------------------------
-- Runners

mkSpec :: Document -> HTMLSpec (Effect Unit)
mkSpec document = V.VDomSpec
  { buildWidget: T.buildThunk (un HTMLF)
  , buildAttributes: P.buildProp identity
  , document
  }

-- To monoidal append views, we just dump them into a container div
unHTML :: HTML -> HTMLVDom (Effect Unit)
unHTML arr = Elem Nothing (coerce "div") [] (coerce arr)

mkHandler
  :: Ref (Maybe (Step (HTMLVDom (Effect Unit)) Node))
  -> HTMLSpec (Effect Unit)
  -> Element
  -> HTML
  -> Effect Unit
mkHandler machineRef spec body v = do
  mmachine <- Ref.read machineRef
  machine <- case mmachine of
    Just machine -> do
      machine' <- runEffectFn2 V.step machine (unHTML v)
      Ref.write (Just machine') machineRef
      pure machine'
    Nothing -> do
      machine' <- runEffectFn1 (V.buildVDom spec) (unHTML v)
      DOM.appendChild (V.extract machine') (toNode body)
      pure machine'
  Ref.write (Just machine) machineRef

run :: Widget HTML Void -> Effect Unit
run ww = do
  win ← Web.window
  doc ← Web.document win
  bod ← Web.querySelector (wrap "body") (Web.toParentNode doc)
  for_ bod \body → do
    machineRef <- Ref.new Nothing
    let
      spec = mkSpec (Web.toDocument doc)
      handler = mkHandler machineRef spec body
    runWidget ww $ result handler absurd

------------------------------
-- Constructors

el
  :: forall m a
   . ShiftMap (Widget HTML) m
  => MultiAlternative m
  => String
  -> Array (P.Prop a)
  -> Array (m a)
  -> m a
el s = CD.elArr' \a v -> HTMLF (V.Elem Nothing (ElemName s) a (coerce v))

text :: forall m a. LiftWidget HTML m => String -> m a
text s = liftWidget $ display ([ HTMLF (Text s) ] :: HTML)

prop :: forall a. String -> String -> P.Prop a
prop k v = P.Attribute Nothing k v

handle :: String -> P.Prop Unit
handle eventName = P.Handler (EventType eventName) (\_ -> Just unit)

