{-# LANGUAGE RankNTypes #-}
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- Credit: https://www.bilibili.com/video/av9741532/


-- LENS: A FIRST IDEA

-- Lets try to play with these record types:

data Person = Person { name :: String
                     , addr :: Address
                     , salary :: Int }

data Address = Address { road :: String
                       , city :: String
                       , postCode :: String }

bob :: Person
bob = Person
  { name = "Bob"
  , addr = Address { road = "Elm Street", city = "Medford", postCode = "90210" }
  , salary = 20000
  }

-- Make nice function to "update" the person's name
setName :: String -> Person -> Person
setName newName p = p { name = newName }
-- This syntax means "a new record like p but with name set to newName"

-- One to look at the person's name
viewName :: Person -> String
viewName p = name p
-- This syntax means "the name field from the record p"

-- Try it out yourself for something more deeply nested:
setPostCode :: String -> Person -> Person
setPostCode newPostCode p = undefined

viewPostCode :: String -> Person -> Person
viewPostCode newPostCode p = undefined

-- You can see how this might get tedious
-- (more so if things a more deeply nested)

-- So here is the idea: for each field of a record
-- create a view and a set function. We call this
-- a Lens which is focused on the field:
data LensRecord s a = LensRecord { viewRecord :: s -> a
                                 , setRecord :: a -> s -> s }

viewr :: LensRecord s a -> (s -> a)
viewr (LensRecord v _) = v

setr :: LensRecord s a -> (a -> s -> s)
setr (LensRecord _ s) = s

nameLensR :: LensRecord Person String
nameLensR = LensRecord { viewRecord = viewName
                       , setRecord = setName
                       }

bob_s_name = viewr nameLensR bob -- This is kinda nice
alice = setr nameLensR "Alice" bob -- This is kinda nice

-- Load this at the repl to get a feel for how setR and viewR work

addrLensR :: LensRecord Person Address
addrLensR = LensRecord
  { viewRecord = (\person -> addr person)
  , setRecord = (\addr ->
                   \person ->
                     Person (name person) addr (salary person))
  }

postCodeFromAddressLensR :: LensRecord Address String
postCodeFromAddressLensR = undefined

-- Now that we have a lens which lets us
--     view and set a postCode in an Address
-- and one which lets us:
--     view and set a Address of a Person
-- It would be nice to be able to compose them to be able to:
--     view and set the PostCode of a Person

composeLensRs :: LensRecord a b -> LensRecord b c -> LensRecord a c
composeLensRs (LensRecord v1 s1) (LensRecord v2 s2) =
  LensRecord
  { viewRecord = (\s -> undefined)
  , setRecord = (\a -> \s -> undefined)
  }

-- If we compose the two below we want a new one as listed:
-- addrLensR :: LensRecord Person Address
-- `composeLensRs`
-- postCodeFromAddressLensR :: LensRecord Address String
-- == LensRecord {
--  vewRecord = (\person -> (viewR postCodeFromAddressLensR)
--                                ((viewR addrLensR) person)),
--  setRecord = (\string -> \person ->
--                 (setR addrLensR)
--                     ((setR postCodeFromAddressLensR)
--                          string ((viewR addrLensR) person))
--                  person)
-- }

-- This ability to compose Len's is important. It lets use build only
-- basic lenses and then compose them to access things deep in our records.

-- INTERMISSION

-- So that is a kinda neat. We can compose lens to look deep into a structure
-- and set something which is deep down there, without manually rebuilding
-- everything each time.

-- This works great (except it's costs us a bit at run time). But you might
-- want to start doing other things for instance what if I want to upperCase
-- the City field? Or what if I want to add one to a person's age.

-- This is the subject of the next section. But for now just take a breath and
-- talk the idea over with the group.

-- More theory lies below. If you'd like to see a barrage of examples:
-- https://www.youtube.com/watch?v=QZy4Yml3LTY

-- MODIFICATION

-- One easy way to do this would be to add a modify field to our Record

-- (I have to start numbering these or they will name clash)
data LensRecord1 s a =
  LensRecord1
  { viewRecord1 :: s -> a
  , setRecord1 :: a -> s -> s
  , modRecord1 :: (a -> a) -> s -> s
  }

-- You might want to think about how a general implementation of
-- modify would go in terms of set and view...

-- Okay so what about if we want to only do a modification when a
-- partial function (one which returns a Maybe) succeeds?:

data LensRecord2 s a =
  LensRecord2
  { viewRecord2 :: s -> a
  , setRecord2 :: a -> s -> s
  , modRecord2 :: (a -> a) -> s -> s
  , modMaybeRecord2 :: (a -> Maybe a) -> s -> Maybe s
  }

-- Or if we want to print out the deeply nested feild

data LensRecord3 s a =
  LensRecord3
  { viewRecord3 :: s -> a
  , setRecord3 :: a -> s -> s
  , modRecord3 :: (a -> a) -> s -> s
  , modMaybeRecord3 :: (a -> Maybe a) -> s -> Maybe s
  , printRecord3 :: (a -> IO a) -> s -> IO s
  }

-- This is not working out as nicely as I'd like. We could
-- just stick to view and set, which would work, but the implementation
-- that creates for modify and these other things would not be
-- as simple as if they where built in as we have above.

-- Do you notice anything repeated in the last two lines above?

-- Maybe a way to generalize?

-- Look at Maybe and IO. They take the same spots in the type signature.

data LensRecord4 s a =
  LensRecord4
  { viewRecord4 :: s -> a
  , setRecord4 :: a -> s -> s
  , modRecord4 :: (a -> a) -> s -> s
  , modFRecord4 :: forall f. Functor f => (a -> f a) -> s -> f s
  }

-- This means that we don't need to add a feild for each of Maybe, IO, ect...
-- Cool.

-- A QUICK DIVERSON
-- class Functor a where
--      fmap :: (a -> b) -> f a -> f b

-- instance Functor Maybe where
--      fmap f Nothing = Nothing
--      fmap f (Just x) = Just (f x)

-- instance Functor [] where
--      fmap f [] = []
--      fmap f (x:xs) = f x : fmap f xs

-- BACK TO IT:

-- Lets go back to the original View and Set in a record:
data LensR s a =
  LensR
  { viewR :: s -> a
  , setR :: a -> s -> s
  }

-- The view and set functions can be combined to make the
-- function which does (a -> Maybe a) -> (s -> Maybe s).
-- All we have to do is
-- 1) view the a
-- 2) run the (a -> Maybe a) function over the a
-- 3) fmap the set function (with the original function) over the (Maybe a)

-- Notice that nothing in the process was specific to Maybe (we used fmap so
-- maybe could be any Functor f).

-- It turns out that we can use something of type (a -> f a) -> (s -> f s) to
-- get the view and set functions back, if f is a Functor.

-- If we can show that the transformation works both ways then we could equivalently
-- use either type. To show this we will write two functions to do the transform:
type Lens' s a = forall f. Functor f => (a -> f a) -> (s -> f s)

--- Build setR from Lens'

set :: Lens' s a -> (a -> s -> s)
set lens a s =
   undefined

-- To do this you'll need an odd functor instance:
newtype Id a = Id  a

runId :: Id a -> a
runId (Id x) = x

-- This is just so that we have something which is a functor:
instance Functor Id where
  fmap f (Id x) = Id (f x)

-- Build viewR from Lens'
view :: Lens' s a -> (s -> a)
view lens s =
  undefined

-- to do this you'll need an odd functor instance
newtype Const v a = Const v

getConst :: Const v a -> v
getConst (Const v) = v

-- all so we are a functor
instance Functor (Const v) where
  fmap f (Const v) = Const v -- note: fmap does nothing

-- REVISTING COMPOSE

-- Okay so these two type are equivalent, but which one should
-- we use?

-- Well lets look at composing lens again:
-- type Lens' s a :: forall f. Functor f => (a -> f a) -> (s -> f s)

-- If:
-- lens1 :: (a -> f a) -> (b -> f b)
-- lens2 :: (b -> f b) -> (c -> f c)
-- Then lens1 composed with lens2 is just: lens1 . lens2
-- Cute...


-- If you still have time take a look at this to get an idea how to
-- use lens (and prisms)
-- https://www.youtube.com/watch?v=QZy4Yml3LTY

-- all the content came from here: https://www.bilibili.com/video/av9741532/
-- take a look for an explanation, answers,
-- and an hint at the theory of prisms (at the end)
