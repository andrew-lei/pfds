module App.State where

import Prelude
import App.Config (config)
import App.Routes (Route, match)
import Data.List as List
import Data.List ((:))
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Data.Unit (unit)
import Text.Smolder.HTML (br, div, span)
import Text.Smolder.HTML.Attributes (className)
import Text.Smolder.Markup ((!), (#!), text, Markup)
import Partial.Unsafe (unsafePartial)

class Disp a where
    disp :: forall e. a -> Markup e

data Lazy f a = Lazy (forall e. Markup e) (Unit -> f a)

data StreamCell a = Nil | Cons a (Stream a)

data Stream a = Stream (Lazy StreamCell a)

data Queue a = Queue 
  { f :: Stream a
  , r :: List.List a
  , s :: Stream a
  }

data Deque a = Deque
  { f :: Stream a
  , lenF :: Int
  , sf :: Stream a
  , r :: Stream a
  , lenR :: Int
  , sr :: Stream a
  }

instance dispString :: Disp String where
    disp = text

instance dispLazy :: Disp (Lazy f a) where
    disp (Lazy e _) = e

instance dispStream :: Disp a => Disp (Stream a) where
    disp (Stream l) = disp l

instance dispQueue :: (Disp a, Show a) => Disp (Queue a) where
    disp (Queue q) = div ! className "test" $ do
       disp q.f
       br
       disp q.r
       br
       disp q.s

instance dispDeque :: Disp a => Disp (Deque a) where
    disp (Deque q) = div ! className "test" $ do
       disp q.lenF
       br
       disp q.f
       br
       disp q.sf
       br
       disp q.lenR
       br
       disp q.r
       br
       disp q.sr

-- instance chaining not working :'(
-- desired:
-- else instance dispShow :: Show a => Disp a
instance dispList :: Show a => Disp (List.List a) where
    disp = text <<< show

instance dispInt :: Disp Int where
    disp = text <<< show

class Funct f where
    map' :: forall a b. Tuple (a -> b) String -> f a -> f b

instance functLazy :: Funct f => Funct (Lazy f) where
    map' f@(Tuple _ name) (Lazy e a) =
      let
        e' :: forall e. Markup e
        e' = span ! className "test" $ do
           text $ "map " <> name
           e
        a' = a unit
      in
        Lazy e' $ \_ -> map' f a'

instance functStreamCell :: Funct StreamCell where
    map' _ Nil = Nil
    map' f'@(Tuple f _) (Cons x xs) = Cons (f x) $ map' f' xs

instance functStream :: Funct Stream where
    map' f (Stream l) = Stream $ map' f l

force :: forall f a. Lazy f a -> f a
force (Lazy _ def) = def unit

-- stream funcs

empty :: forall a. Stream a
empty = Stream (Lazy (span ! className "test" $ text "[]") (\_ -> Nil))

cons :: forall a. Show a => a -> Stream a -> Stream a
cons a s@(Stream (Lazy e _)) = 
  let
    e' :: forall e. Markup e
    e' = span ! className "test" $ do
        text $ show a
        e
  in 
    Stream $ Lazy e' $ \_ -> Cons a s

streamAppend :: forall a. Stream a -> Stream a -> Stream a
streamAppend (Stream xs@(Lazy e1 _)) ys@(Stream (Lazy e2 _)) =
  let
    e :: forall e. Markup e
    e = span ! className "test" $ do
       e1
       text "++"
       e2
  in
    Stream $ Lazy e $ \_ -> streamCellAppend (force xs) ys

infixr 5 streamAppend as ++

streamCellAppend :: forall a. StreamCell a -> Stream a -> StreamCell a
streamCellAppend Nil (Stream ys) = force ys
streamCellAppend (Cons x xs) ys = Cons x $ streamAppend xs ys

headTail :: forall a e. Show a => Stream a -> Tuple String (Stream a)
headTail (Stream xs) = case force xs of
    Nil -> Tuple "Error: Empty list" empty
    Cons a r -> Tuple (show a) r

head :: forall a e. Show a => Stream a -> String
head (Stream xs) = case force xs of
    Nil -> "Error: Empty list"
    Cons a _ -> show a

tail :: forall a. Stream a -> Stream a
tail (Stream xs) = case force xs of
    Nil -> empty
    Cons _ r -> r

take :: forall a. Disp a => Show a => Int -> Stream a -> Stream a
take 0 _ = empty
take n (Stream xs) = case force xs of
    Nil -> empty
    Cons a r -> 
      let 
        e :: forall e. Markup e
        e = span ! className "test" $ do
           text $ "take " <> show n
           disp (Stream xs)
      in
        Stream $ Lazy e $ \_ -> Cons a $ take (n-1) r

drop :: forall a. Int -> Stream a -> Stream a
drop 0 xs = xs
drop n (Stream xs) = case force xs of
    Nil -> empty
    Cons _ r -> drop (n-1) r

reverse :: forall a. Show a => Stream a -> Stream a
reverse = reverse' empty
  where
    reverse' acc (Stream xs) = case force xs of
      Nil -> acc
      Cons x xss -> reverse' (cons x acc) xss

-- queue funcs

emptyQ :: forall a. Queue a
emptyQ = Queue { f: empty, r: List.Nil, s: empty }

rotate :: forall a. Disp a => Show a => Stream a -> List.List a -> Stream a -> Stream a
rotate (Stream f) r a = case Tuple (force f) r of
    Tuple _ List.Nil -> Stream f -- Okasaki doesn't cover this case, presumably impossible
    Tuple Nil (List.Cons y _) -> cons y a
    Tuple (Cons x f') (List.Cons y r') -> 
      let
        ya :: Stream a
        ya = cons y a
        e :: forall e. Markup e
        e = span ! className "test" $ do
           text $ show x
           span ! className "test" $ do
              text "rotate "
              disp f'
              disp r'
              disp ya
      in
        Stream $ Lazy e $ \_ -> Cons x (rotate f' r' ya)

queue :: forall a. Disp a => Show a => Stream a -> List.List a -> Stream a -> Queue a
queue f r (Stream s) = case force s of
    Cons x s' -> Queue { f: f, r: r, s: s' }
    Nil -> let f' = rotate f r (Stream s)
           in Queue { f: f', r: List.Nil, s: f' }

snoc :: forall a. Disp a => Show a => Queue a -> a -> Queue a
snoc (Queue q) x = queue q.f (x : q.r) q.s

qHead :: forall a. Disp a => Show a => Queue a -> String
qHead (Queue q) = head q.f

qTail :: forall a. Disp a => Show a => Queue a -> Queue a
qTail (Queue { f: Stream f, r: r, s: s }) = case force f of
    Nil -> emptyQ
    Cons _ f' -> queue f' r s

qHeadTail :: forall a. Disp a => Show a => Queue a -> Tuple String (Queue a)
qHeadTail (Queue { f: Stream f, r: r, s: s }) = case force f of
    Nil -> Tuple "Error: Empty queue" emptyQ
    Cons x f' -> Tuple (show x) $ queue f' r s

-- deque functions

emptyDeq :: forall a. Deque a
emptyDeq = Deque { f: empty, lenF: 0, sf: empty, r: empty, lenR: 0, sr: empty }

exec1 :: forall a. Stream a -> Stream a
exec1 (Stream s) = case force s of
    Nil -> Stream s
    Cons _ s' -> s'

exec2 :: forall a. Stream a -> Stream a
exec2 = exec1 <<< exec1

rotateRev :: forall a. Disp a => Show a => Stream a -> Stream a -> Stream a -> Stream a
rotateRev (Stream xs) f a = case force xs of
    Nil -> reverse f ++ a
    Cons x r -> 
      let
        -- params 2 and 3 are strict
        -- but guaranteed to be small
        param2 = drop 2 f
        param3 = reverse (take 2 f) ++ a
        e :: forall e. Markup e
        e = span ! className "test" $ do
           text $ show x
           span ! className "test" $ do
             text "rotateRev"
             disp r
             disp param2
             disp param3
      in
        Stream $ Lazy e $ \_ -> Cons x (rotateRev r param2 param3)

rotateDrop :: forall a. Disp a => Show a => Stream a -> Int -> Stream a -> Stream a
rotateDrop r@(Stream r'') i f =
  if i < 2
    then
      let
        param2 = drop i f
        Stream (Lazy _ xs) = rotateRev r param2 empty
        e :: forall e. Markup e
        e = span ! className "test" $ do
           text $ "rotateRev"
           disp r
           disp param2
           span ! className "test" $ text "[]"
      in
        Stream $ Lazy e xs
    else 
      let
        Tuple x r' = unsafePartial $ case force r'' of
          Cons x r' -> Tuple x r'
          -- Okasaki doesn't handle the other case
          -- so this is presumably safe
        param3 = drop 2 f
        e :: forall e. Markup e
        e = span ! className "test" $ do
           text $ show x
           span ! className "test" $ do
              text "rotateDrop"
              disp r'
              text $ "(" <> show (i - 2) <> ")"
              disp param3
      in
        Stream $ Lazy e $ \_ -> Cons x (rotateDrop r' (i - 2) param3)

deque :: forall a. Disp a => Show a => Deque a -> Deque a
deque (Deque q) =
  if q.lenF > 2 * q.lenR + 1
    then 
      let
        i = (q.lenF + q.lenR) / 2
        j = q.lenF + q.lenR - i
        f' = take i q.f
        r' = rotateDrop q.r i q.f
      in
        Deque { f: f', lenF: i, sf: f', r: r', lenR: j, sr: r' }
  else if q.lenR > 2 * q.lenF + 1
    then
      let
        j = (q.lenF + q.lenR) / 2
        i = q.lenF + q.lenR - j
        f' = rotateDrop q.f j q.r
        r' = take j q.r
      in
        Deque { f: f', lenF: i, sf: f', r: r', lenR: j, sr: r' }
  else Deque q

deqCons :: forall a. Disp a => Show a => Deque a -> a -> Deque a 
deqCons (Deque q) x = deque $ Deque q { f = cons x q.f, lenF = q.lenF + 1, sf = exec1 q.sf, sr = exec1 q.sr }

deqHeadTail :: forall a. Disp a => Show a => Deque a -> Tuple String (Deque a)
deqHeadTail (Deque q) = let Stream f' = q.f in case force f' of
    Nil -> let Stream r = q.r in case force r of
      Nil -> Tuple "Error: Empty deque" emptyDeq
      Cons x _ -> Tuple (show x) emptyDeq
    Cons x f -> Tuple (show x) <<< deque $ Deque q { f = f, lenF = q.lenF - 1, sf = exec2 q.sf, sr = exec2 q.sr }

deqSnoc :: forall a. Disp a => Show a => Deque a -> a -> Deque a 
deqSnoc (Deque q) x = deque $ Deque q { r = cons x q.r, lenR = q.lenR + 1, sf = exec1 q.sf, sr = exec1 q.sr }

deqLastInit :: forall a. Disp a => Show a => Deque a -> Tuple String (Deque a)
deqLastInit (Deque q) = let Stream r' = q.r in case force r' of
    Nil -> let Stream f = q.f in case force f of
      Nil -> Tuple "Error: Empty deque" emptyDeq
      Cons x _ -> Tuple (show x) emptyDeq
    Cons x r -> Tuple (show x) <<< deque $ Deque q { r = r, lenR = q.lenR - 1, sf = exec2 q.sf, sr = exec2 q.sr }


newtype State = State
  { title :: String
  , route :: Route
  , loaded :: Boolean
  , number :: Int
  , stream1 :: StreamState
  , stream2 :: StreamState
  , queue :: Queue Int
  , deque :: Deque Int
  , msg :: String
  }


type StreamState = { stream :: Stream Int
                   , adding :: Boolean
                   , add :: Int
                   , mult :: Int
                   }

initStreamState :: StreamState
initStreamState = { stream: empty
                  , adding: true
                  , add: 0
                  , mult: 1
                  }

derive instance newtypeState :: Newtype State _

init :: String -> State
init url = State
  { title: config.title
  , route: match url
  , loaded: false
  , number: 0
  , stream1: initStreamState
  , stream2: initStreamState
  , queue: emptyQ
  , deque: emptyDeq
  , msg: ""
  }
