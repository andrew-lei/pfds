module App.Events where

import Prelude
import App.Routes (Route)
import App.State (State(..), StreamState, Queue, Deque, cons, head, tail, headTail
                 ,(++), initStreamState, map', snoc, qHeadTail, deqCons, deqSnoc
                 ,deqHeadTail, deqLastInit)
import Data.Function (($))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Network.HTTP.Affjax (AJAX)
import Pux (EffModel, noEffects)

data Event = PageView Route
           | StEvt StateEvent


type AppEffects fx = (ajax :: AJAX | fx)

foldp :: ∀ fx. Event -> State -> EffModel State Event (AppEffects fx)
foldp (PageView route) (State st) = noEffects $ State st { route = route, loaded = true }
foldp (StEvt evt) st = noEffects $ foldState evt st 

data StateEvent = Inc
                | Stream1Evt StreamEvent
                | Stream2Evt StreamEvent
                | Append
                | QueueEvt QueueEvent
                | DequeEvt DequeEvent
                | Both StateEvent StateEvent

foldState :: ∀ fx. StateEvent -> State -> State
foldState Inc (State st) = State st { number = st.number + 1 }
foldState (Stream1Evt evt) (State st) =
  let
    Tuple mbMsg strSt = foldStream evt st.stream1
  in
    case mbMsg of
      Nothing -> State st { stream1 = strSt }
      Just m -> State st { msg = m, stream1 = strSt }
foldState (Stream2Evt evt) (State st) =
  let
    Tuple mbMsg strSt = foldStream evt st.stream2
  in
    case mbMsg of
      Nothing -> State st { stream2 = strSt }
      Just m -> State st { msg = m, stream2 = strSt }
foldState Append (State st) = State st { stream1 = st.stream1 { stream = st.stream1.stream ++ st.stream2.stream }, stream2 = initStreamState } 
foldState (QueueEvt evt) (State st) =
  let
    Tuple mbMsg qSt = foldQueue evt st.queue
  in
    case mbMsg of
      Nothing -> State st { queue = qSt }
      Just m -> State st { msg = m, queue = qSt }
foldState (DequeEvt evt) (State st) =
  let
    Tuple mbMsg qSt = foldDeque evt st.deque
  in
    case mbMsg of
      Nothing -> State st { deque = qSt }
      Just m -> State st { msg = m, deque = qSt }
foldState (Both evt1 evt2) st = foldState evt2 $ foldState evt1 st


data StreamEvent = ConsEvt Int
                 | Head
                 | Tail
                 | HeadTail
                 | IncAdd Boolean
                 | IncMult Boolean
                 | SetAdd Boolean
                 | Map

foldStream :: StreamEvent -> StreamState -> Tuple (Maybe String) StreamState
foldStream (ConsEvt i) st = Tuple Nothing $ st { stream = cons i st.stream }
foldStream Head st = Tuple (Just $ head st.stream) st
foldStream Tail st = Tuple Nothing $ st { stream = tail st.stream }
foldStream HeadTail st =
  let
    Tuple h t = headTail st.stream
  in
    Tuple (Just h) $ st { stream = t }
foldStream (IncAdd true) st = Tuple Nothing $ st { add = st.add + 1 }
foldStream (IncAdd false) st = Tuple Nothing $ st { add = st.add - 1 }
foldStream (IncMult true) st = Tuple Nothing $ st { mult = st.mult + 1 }
foldStream (IncMult false) st = Tuple Nothing $ st { mult = st.mult - 1 }
foldStream (SetAdd true) st = Tuple Nothing $ st { adding = true }
foldStream (SetAdd false) st = Tuple Nothing $ st { adding = false }
foldStream Map st =
  case st.adding of
    true -> Tuple Nothing $ st { stream = map' (Tuple ((+) st.add) $ "(+" <> show st.add <> ")") st.stream }
    false -> Tuple Nothing $ st { stream = map' (Tuple ((*) st.mult) $ "(*" <> show st.mult <> ")") st.stream }

data QueueEvent = SnocEvt Int
                | QHeadTail

foldQueue :: QueueEvent -> Queue Int -> Tuple (Maybe String) (Queue Int)
foldQueue (SnocEvt i) q = Tuple Nothing $ snoc q i
foldQueue QHeadTail q =
  let
    Tuple h t = qHeadTail q
  in
    Tuple (Just h) t

data DequeEvent = DeqConsEvt Int
                | DeqSnocEvt Int
                | DeqHeadTail
                | DeqLastInit

foldDeque :: DequeEvent -> Deque Int -> Tuple (Maybe String) (Deque Int)
foldDeque (DeqConsEvt i) q = Tuple Nothing $ deqCons q i
foldDeque (DeqSnocEvt i) q = Tuple Nothing $ deqSnoc q i
foldDeque DeqHeadTail q =
  let
    Tuple h t = deqHeadTail q
  in
    Tuple (Just h) t
foldDeque DeqLastInit q =
  let
    Tuple l i = deqLastInit q
  in
    Tuple (Just l) i
