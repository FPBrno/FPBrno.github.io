% Types as Values: Derive correctness from practicality
% Peter Trško \<<peter.trsko@gmail.com>\>
% 30th May, 2015


# Introduction

## Introduction

There are corner cases where Haskell doesn't shine that well, but smart people
found ways around them.

I would like to expand your horizons when it comes to Haskell, as a language,
and also introduce you to few GHC language extensions.

This is a shallow talk, it doesn't go in to details or too deep in to the
problems, its here just to inspire you to do that your self.


# Values, Types, and Kinds

## Haskell Type Hierarchy

```Haskell
value :: type
type :: kind
```

<div class="incremental">
<div>
Values have types:

```Haskell
True :: Bool
map :: (a -> b) -> [a] -> [b]
```
</div>

<div>
Types have kinds:

```Haskell
Int :: *
Maybe :: * -> *
(->) :: * -> * -> *
```
</div>

And all the way up, until you reach co-turtles.
</div>


# Information Loss

## Information Loss

```Haskell
read . show
```

Have you ever tried this? Lets try it together, in GHCi.


## read . show

```Haskell
GHCi> read . show $ True
```


## read . show

```Haskell
GHCi> read . show $ True
*** Exception: Prelude.read: no parse
```

<div class="incremental">
What the <code class="sourceCode haskell"><span
class="fu">&lt;$&gt;</span></code> happened? Let us investigate.

```Haskell
GHCi> :set -Wall
GHCi> read . show $ True

<interactive>:4:1: Warning:
    Defaulting the following constraint(s) to type ‘()’
      (Read a0) arising from a use of ‘it’ at <interactive>:4:1-18
      (Show a0) arising from a use of ‘print’ at <interactive>:4:1-18
    In the first argument of ‘print’, namely ‘it’
    In a stmt of an interactive GHCi command: print it
*** Exception: Prelude.read: no parse
```
</div>


## read . show (again)

```{.haskell}
-- Type signature intentionally omitted.
example = read . show $ True
```

<div class="incremental">
```Haskell
[ someone@something types-as-values ]$ ghci information-loss-example.hs 
GHCi, version 7.10.1: http://www.haskell.org/ghc/  :? for help
[1 of 1] Compiling Main             ( information-loss-example.hs, interpreted )

information-loss-example.hs:2:8:
    No instance for (Read c0) arising from a use of ‘read’
    The type variable ‘c0’ is ambiguous
    Relevant bindings include
      bool :: c0 (bound at information-loss-example.hs:2:1)
    Note: there are several potential instances:
      instance (GHC.Arr.Ix a, Read a, Read b) => Read (GHC.Arr.Array a b)
        -- Defined in ‘GHC.Read’
      instance Read a => Read (Maybe a) -- Defined in ‘GHC.Read’
      instance (Integral a, Read a) => Read (GHC.Real.Ratio a)
        -- Defined in ‘GHC.Read’
      ...plus 25 others
    In the first argument of ‘(.)’, namely ‘read’
    In the expression: read . show
    In the expression: read . show $ True
Failed, modules loaded: none.
```
</div>


## Phantom of The Type (Soap)Opera

We lost the type information. How can we pass it around?

<div class="incremental">
```Haskell
data Proxy a = Proxy
```

Eh, not really.

```Haskell
{-# LANGUAGE PolyKinds #-}

data Proxy (a :: k) = Proxy
    -- Defined in ‘Data.Proxy’
```

Do I need to care about `PolyKinds`? That depends. Do you want to be ready for
dependent types in Haskell?
</div>


## Phantom of The Type (Soap)Opera (cont.)

```Haskell
{-# LANGUAGE TupleSections #-}

import Data.Proxy -- Surprisingly, this is in base. And the Thanks goes to ekmett.


readMe :: Read a => (String, Proxy a) -> a
readMe (str, _) = read str

showMe :: Show a => a -> (String, Proxy a)
showMe = (, Proxy) . show

example = readMe . showMe $ True
```

<div class="incremental">
```Haskell
GHCi> example
True
```

We have just successfully passed around a type variable.
</div>


# The Phantom Menace, err, Delight

## This Tea You Serve is Delightful 

* We can pass around types as first class citizens.
* Phantom types passed using `Proxy` allow us to get rid of these ugly
  <code class="sourceCode haskell"><span class="ot">undefined ::</span> <span
  class="kw">type</span></code> expression.

<div class="incremental">
Wait, that's it?! No, we are just getting started.

```Haskell
Data.Typeable.typeRep :: Typeable a => proxy a -> TypeRep
```

```Haskell
GHCi> typeRep (Proxy :: Proxy (Maybe Int))
Maybe Int
```

```Haskell
GHC.TypeLits.symbolVal :: KnownSymbol n => proxy n -> String
GHC.TypeLits.natVal :: KnownNat n => proxy n -> Integer
```

```Haskell
GHCi> :set -XDataKinds
GHCi> :k "type-level-string"
"type-level-string" :: Symbol
GHCi> symbolVal (Proxy :: Proxy "type-level-string")
"type-level-string"
```
</div>


## Apples And Oranges Do Not Mix Well

```Haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PolyKinds #-}

import Data.Proxy


newtype Weight (t :: k) = Weight {getWeight :: Rational}
  deriving (Num, Show)

data Apple
data Orange

ofApples :: Proxy Apple
ofApples = Proxy

ofOranges :: Proxy Orange
ofOranges = Proxy

sumWeight :: Proxy t -> [Weight t] -> Weight t
sumWeight Proxy = sum

weight :: Proxy t -> Weight t -> Weight t
weight Proxy = id
```


## Apples And Oranges Do Not Mix Well (cont.)

```Haskell
GHCi> sumWeight ofApples [1,2,3] + weight ofApples 6
Weight {getWeight = 12 % 1}
```

<div class="incremental">
```Haskell
GHCi> sumWeight ofApples [1,2,3] + weight ofOranges 6

<interactive>:19:40:
    Couldn't match type ‘Orange’ with ‘Apple’
    Expected type: Proxy Apple
      Actual type: Proxy Orange
    In the first argument of ‘sumWeight’, namely ‘ofOranges’
    In the second argument of ‘(+)’, namely
      ‘sumWeight ofOranges [1, 2, 3]’
```
</div>


# Real-World Phantom Types

## Exception Handling

```Haskell
import Control.Exception
import Data.Proxy


someException :: Proxy SomeException
someException = Proxy

ignoring :: Exception e => IO () -> Proxy e -> IO ()
ignoring m proxy = m `catch` \e -> handler (e `asProxyTypeOf` proxy)
  where
    handler _ = return ()

main :: IO ()
main = do
    error "Hear, hear, we have an ERROR in our land!"
        `ignoring` someException
    putStrLn "Nothing ever happens in this town."
```

<div class="incremental">
```
GHCi> :main
Nothing ever happens in this town.
```
</div>


## Same Food, Multiple Flavors

Get down and dirty with phantom types on a first date. Don't forget to be safe,
and use [tagged](https://hackage.haskell.org/package/tagged).

<div class="incremental">
```Haskell
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
import Data.Aeson as Aeson
import Data.Tagged
import Data.ByteString.Lazy.Char8 as Lazy
import System.Process

data AsObject
data AsArray

instance (ToJSON a, ToJSON b) => ToJSON (Tagged AsObject (a, b)) where
    toJSON (Tagged (a, b)) = Aeson.object
        [ "first"  .= a
        , "second" .= b
        ]

instance (ToJSON a, ToJSON b) => ToJSON (Tagged AsArray (a, b)) where
    toJSON (Tagged (a, b)) = toJSON [toJSON a, toJSON b]
```
</div>


## Same Food, Multiple Flavors (cont.)

```Haskell
asObject :: a -> Tagged AsObject a
asObject = Tagged

asArray :: a -> Tagged AsArray a
asArray = Tagged

printPrettyJson :: Lazy.ByteString -> IO ()
printPrettyJson json =
    readProcess "jq" [".", "-C"] (unpack json) >>= Prelude.putStr
```

<div class="incremental">
```Haskell
GHCi> printPrettyJson . encode $ asObject (1 :: Int, "foo")
{
  "second": "foo",
  "first": 1
}
GHCi> printPrettyJson . encode $ asArray (1 :: Int, "foo")
[
  1,
  "foo"
]
```
</div>


## Beyond The Infinite

Last example is little bit longer, and Web related.

<div class="incremental">
```Haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
import Data.Char (toLower)
import Data.Data (Data(toConstr), Typeable, showConstr)
import Data.Monoid ((<>))
import Data.String (IsString(fromString))
import Data.Proxy (Proxy(Proxy))

import Data.Text (Text)

import Data.CaseInsensitive as CI (mk)
import Web.PathPieces (PathPiece(fromPathPiece, toPathPiece))
import Web.Spock.Safe
```
</div>


## Beyond The Infinite (cont.)

```Haskell
data StarTrekTerm = Scotty | Spock | Warp
  deriving (Bounded, Data, Enum, Eq, Ord, Typeable)

instance PathPiece StarTrekTerm where
    fromPathPiece txt = CI.mk txt `lookup` [(str v, v) | v <- terms]
      where
        str = fromString . showConstr . toConstr
        terms = [minBound..maxBound :: StarTrekTerm]

    toPathPiece = fromString . map toLower . showConstr . toConstr

toUrl :: StarTrekTerm -> Text
toUrl = (packageUrl <>) . \case
    Scotty -> "scotty"
    Spock -> "Spock"
    Warp -> "warp"
  where
    packageUrl = "https://hackage.haskell.org/package/"
```


## Beyond The Infinite (cont.)

```Haskell
starTrekTerm :: Proxy StarTrekTerm
starTrekTerm = Proxy

varOf :: (Typeable a, PathPiece a) => Proxy a -> Path (a ': '[])
varOf _ = var

main :: IO ()
main = runSpock 3000 . spockT id
    . get ("haskell-package/by-star-trek-term" <//> varOf starTrekTerm)
        $ \term -> text $ toUrl term <> "\n"
```

<div class="incremental">
```Haskell
GHCi> :t get ("haskell-package/by-star-trek-term" <//> varOf starTrekTerm)
get ("haskell-package/by-star-trek-term" <//> varOf starTrekTerm)
    :: Control.Monad.IO.Class.MonadIO m
    => Data.HVect.HVectElim '[StarTrekTerm] (ActionCtxT ctx m ())
    -> SpockCtxT ctx m ()
```

```Haskell
type family HVectElim (ts :: [*]) (a :: *) :: * where
    HVectElim '[] a = a
    HVectElim (t ': ts) a = t -> HVectElim ts a
```

```
[ someone@somewhere types-as-values ]$ curl localhost:3000/haskell-package/by-star-trek-term/spock
https://hackage.haskell.org/package/Spock
[ someone@somewhere types-as-values ]$ curl localhost:3000/haskell-package/by-star-trek-term/foo; echo
<html><head><title>404 - File not found</title></head><body><h1>404 - File not found</h1></body></html>
```
</div>


# Outro

## Outro

(Imagine sad music playing in the background.)

**Thank you for your attention.**


## Thanks And Credits

My many thanks goes to Matej Kollár for making FPB happen and for kicking me in
to actually doing this.

Some of the libraries and tools used in this talk or while creating it:
[aeson](https://hackage.haskell.org/package/aeson),
[base](https://hackage.haskell.org/package/base),
[bytestring](https://hackage.haskell.org/package/bytestring),
[case-insensitive](https://hackage.haskell.org/package/case-insensitive),
[GHC](https://www.haskell.org/ghc/),
[hvect](https://hackage.haskell.org/package/hvect),
[jq](https://stedolan.github.io/jq/),
[pandoc](https://hackage.haskell.org/package/pandoc),
[path-pieces](https://hackage.haskell.org/package/path-pieces),
[process](https://hackage.haskell.org/package/process),
[Slidy2](http://www.w3.org/Talks/Tools/Slidy2/),
[Spock](https://hackage.haskell.org/package/Spock),
[tagged](https://hackage.haskell.org/package/tagged),
[text](https://hackage.haskell.org/package/text), and many others.


## Co-Beginning

If you have any questions then you can

* ask me personally,
* or on [FPB mailing](https://groups.google.com/d/forum/fpbrno) list,
* or send me an email to Peter Trško \<<peter.trsko@gmail.com>\>.

Social: [G+](https://www.google.com/+PeterTr%C5%A1ko) and
[GitHub](https://github.com/trskop).


<!--
vim: filetype=markdown spell spelllang=en
-->
