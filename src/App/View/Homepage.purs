module App.View.Homepage where

import Prelude hiding (div)
import App.Events (Event(..), StateEvent(..), StreamEvent(..), QueueEvent(..), DequeEvent(..))
import App.State (State(..), StreamState(..), disp)
import Control.Bind (discard)
import Data.Array ((..))
import Data.Function (($))
import Data.Show (show)
import Data.Traversable (traverse_)
import Data.Tuple (fst, snd)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML)
import Text.Smolder.HTML (a, br, button, div, h1, input, label, span)
import Text.Smolder.HTML.Attributes (href, className, type', value, id, for, name, checked)
import Text.Smolder.Markup ((!), (#!), text, Markup)

mapper :: StreamState -> (StreamEvent -> StateEvent) -> String -> HTML Event
mapper st evtConstr inputName =
  div do
    button #! onClick (mkEvt Map) $ text "Map"
    br
    input ! type' "radio" ! id "add" ! name inputName #! onClick (mkEvt $ SetAdd true)
    label ! for "add" $ do 
      text "+"
      text <<< show $ st.add
      button #! onClick (mkEvt2 true $ IncAdd true) $ text "↑"
      button #! onClick (mkEvt2 true $ IncAdd false) $ text "↓"
    br
    input ! type' "radio" ! id "mult" ! name inputName #! onClick (mkEvt $ SetAdd false)
    label ! for "mult" $ do
      text "*"
      text <<< show $ st.mult
      button #! onClick (mkEvt2 false $ IncMult true) $ text "↑"
      button #! onClick (mkEvt2 false $ IncMult false) $ text "↓"
  where
    mkEvt = const <<< StEvt <<< evtConstr
    mkEvt2 b = const <<< StEvt <<< Both (evtConstr $ SetAdd b) <<< evtConstr
   

view :: State -> HTML Event
view (State s) =
  div do
    disp s.stream1.stream
    button #! onClick (const <<< StEvt <<< Both Inc <<< Stream1Evt $ ConsEvt s.number) $ text "Cons"
    button #! onClick (const <<< StEvt $ Stream1Evt HeadTail) $ text "Uncons"
    mapper s.stream1 Stream1Evt "op1"
    br
    disp s.stream2.stream
    button #! onClick (const <<< StEvt <<< Both Inc <<< Stream2Evt $ ConsEvt s.number) $ text "Cons"
    button #! onClick (const <<< StEvt $ Stream2Evt HeadTail) $ text "Uncons"
    mapper s.stream2 Stream2Evt "op2"
    br
    button #! onClick (const <<< StEvt $ Append) $ text "++"
    br
    br
    disp s.queue
    button #! onClick (const <<< StEvt <<< Both Inc <<< QueueEvt $ SnocEvt s.number) $ text "Snoc"
    button #! onClick (const <<< StEvt $ QueueEvt QHeadTail) $ text "Uncons"
    br
    br
    disp s.deque
    button #! onClick (const <<< StEvt <<< Both Inc <<< DequeEvt $ DeqConsEvt s.number) $ text "Cons"
    button #! onClick (const <<< StEvt <<< Both Inc <<< DequeEvt $ DeqSnocEvt s.number) $ text "Snoc"
    button #! onClick (const <<< StEvt $ DequeEvt DeqHeadTail) $ text "Uncons"
    button #! onClick (const <<< StEvt $ DequeEvt DeqLastInit) $ text "Unsnoc"
    div $ text s.msg
