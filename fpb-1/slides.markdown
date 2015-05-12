% Apples and Oranges \
(How Not to Mix Things)
% Matej Kollár \
<mkollar+fpb1@redhat.com>
% May 2015

We are FPB
----------

<div class="vbox"></div>
<div class="hbox" style="height: 80%">
<img src="http://fpbrno.github.io/images/FPB.svg">
</div>

So You Were Told...
-------------------

...that you should not mix apples and oranges...

<div class="vbox"></div>
<div class="hbox">
<img src="mixed.svg">
</div>

Everything in a Single Type
---------------------------

~~~ { .haskell file=Fruit1.hs }
module Fruit1 where

data Fruit
    = Apples  Integer
    | Oranges Integer
    deriving Show

mix :: Fruit -> Fruit -> Fruit
mix (Apples m)  (Apples n)  = Apples (m + n)
mix (Oranges m) (Oranges n) = Oranges (m + n)
mix _ _ = error "Not gonna happen!"

main :: IO ()
main = do
    print $ Apples 4  `mix` Apples 5
    print $ Oranges 4 `mix` Oranges 5
    print $ Apples 4  `mix` Oranges 5 -- :-(
~~~

<div class="handout">

* Programmer have to be careful.
* Errors occur at runtime (too late).

</div>

Multiple Types
--------------

~~~ { .haskell file=Fruit2.hs }
module Fruit2 where

newtype Apples  = Apples  Integer deriving Show
newtype Oranges = Oranges Integer deriving Show

mixApples :: Apples -> Apples -> Apples
Apples m `mixApples` Apples n = Apples (m + n)

mixOranges :: Oranges -> Oranges -> Oranges
Oranges m `mixOranges` Oranges n = Oranges (m + n)

main :: IO ()
main = do
    print $ Apples 4  `mixApples`  Apples 5
    print $ Oranges 4 `mixOranges` Oranges 5
    -- print $ Apples 4  `mixApples`  Oranges 5
~~~

<div class="handout">

* Violates DRY.
* Function with same abstract semantics have different names.

</div>

Template Haskell
----------------

~~~ { .haskell file=Fruit2THHelper.hs }
module Fruit2THHelper (genFruit) where

import Language.Haskell.TH

genFruit :: String -> Q [Dec]
genFruit fruit_name' = do
    let fun_name  = mkName ("mix" ++ fruit_name')
        fruit_name = mkName fruit_name'
    m <- newName "m"
    n <- newName "n"
    return
        [ NewtypeD [] fruit_name []
            (NormalC fruit_name [(NotStrict, ConT (mkName "Integer"))])
            [mkName "Show"]
        , SigD fun_name (ArrowT `AppT` ConT fruit_name
            `AppT` (ArrowT `AppT` ConT fruit_name `AppT` ConT fruit_name))
        , FunD fun_name
            [ Clause
                [ConP fruit_name [VarP m],ConP fruit_name [VarP n]]
                (NormalB (ConE fruit_name
                    `AppE` UInfixE (VarE m) (VarE (mkName "+")) (VarE n)))
                []
            ]
        ]
~~~

---

~~~ { .haskell file=Fruit2TH.hs }
{-# LANGUAGE TemplateHaskell #-}
module Fruit2TH where

import Fruit2THHelper

genFruit "Apples"
genFruit "Oranges"
~~~

<div class="handout">

* Helps with DRY.
* Meta magic (hides definitions).

</div>

Type Classes
------------

~~~ { .haskell file=Fruit3.hs }
module Fruit3 where

newtype Apples  = Apples  Integer
newtype Oranges = Oranges Integer

class Fruit a where
    mix :: a -> a -> a

instance Fruit Apples where
    Apples m `mix` Apples n = Apples (m + n)

instance Fruit Oranges where
    Oranges m `mix` Oranges n = Oranges (m + n)
~~~

<div class="handout">

* Fixes issue with semantically same things having different names.

</div>

Template Haskell (Again)
------------------------

~~~ { .haskell file=Fruit3THHelper.hs }
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fruit3THHelper where

import Data.String
import Language.Haskell.TH

class Fruit a where
    mix :: a -> a -> a

instance IsString Name where
    fromString = mkName

genFruit :: String -> Q [Dec]
genFruit fruit_name' = do
    let fruit_name = mkName fruit_name'
    m <- newName "m"
    n <- newName "n"
    return
        [ NewtypeD [] fruit_name []
            (NormalC fruit_name [(NotStrict, ConT "Integer")])
            ["Show"]
        , InstanceD [] (ConT "Fruit" `AppT` ConT fruit_name)
            [ FunD "mix"
                [ Clause
                    [ConP fruit_name [VarP m],ConP fruit_name [VarP n]]
                    (NormalB (ConE fruit_name
                        `AppE` UInfixE (VarE m) (VarE "+") (VarE n)))
                    []
                ]
            ]
        ]
~~~

---

~~~ { .haskell file=Fruit3TH.hs }
{-# LANGUAGE TemplateHaskell #-}
module Fruit3TH where

import Fruit3THHelper

genFruit "Apples"
genFruit "Oranges"
~~~

<div class="handout">

* Again magic...

</div>

Phantom Types
-------------

~~~ { .haskell file=FruitPhantom.hs }
module FruitPhantom where

data Count a = Count Integer deriving Show

mix :: Count a -> Count a -> Count a
Count m `mix` Count n = Count (m + n)

data Apples
data Oranges
~~~

<div class="handout">

* Type annotation of `mix` is important. Otherwise it would be `Count a -> Count b -> Count c`.
* One have to explicitly annotate values (not really a issue in code, only when playing).
* Note that you do not have to make fruit type instances of `Show`.
    * Exercise: Why?
    * Hint: What is the type of `[]` and why are you able to `show` it?

</div>

Phantom Types in the Wild
-------------------------

* [Data.GPS](https://hackage.haskell.org/package/gps-0.3.0)
  used phantom types (in older versions) to differentiate latitude and longitude.
* [Data.Tagged](https://hackage.haskell.org/package/tagged)
  use phantom types to avoid unsafely passing dummy arguments.

GADTs
-----

~~~ { .haskell file=FruitGADTs.hs }
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
module FruitGADTs where

data Apples
data Oranges

data Fruit a where
    Apples  :: Integer -> Fruit Apples
    Oranges :: Integer -> Fruit Oranges

mix :: Fruit a -> Fruit a -> Fruit a
Apples m  `mix` Apples n  = Apples (m + n)
Oranges m `mix` Oranges n = Oranges (m + n)
~~~

(Generalized Abstract Data Types)

<div class="handout">

* Problematic checking of pattern-match exhaustiveness.
* You explicitly use appropriate constructors (more obvious code, easier to play with).

</div>

Template Haskell (Again Again)
------------------------------

* Exercise for active reader: create TemplateHaskell generator `genFruit :: [String] -> Q [Dec]`.

<div class=incremental>
* Not really as there is no support in TH for GADTs syntax.
* Or I just do not understand resolution in
  [comment #7 of related issue](https://ghc.haskell.org/trac/ghc/ticket/3497#comment:7).
</div>

Notes
-----

* Should you be (despite this presentation) compelled to play with
  Template Haskell (trying provided examples or suggested exercise),
  very useful switches to GHC(i) are `-ddump-splices` (dumps pieces
  of code generated by Template Haskell) and `-v0` (silences usual
  (non-interesting) stuff).
* Documentation for data structures to create syntax
  trees for latest Template Haskell can be found at
  <http://hackage.haskell.org/package/template-haskell/docs/src/Language-Haskell-TH-Syntax.html>.

Questions?
----------

* Thank you for your attention.
* Slides will be available at <https://fpbrno.github.io/>.

Attributions
------------

* Images [Simple fruit](https://openclipart.org/search/?query=simple+fruit):
    * <https://openclipart.org/detail/8538/simple-fruit-apple>
    * <https://openclipart.org/detail/8537/simple-fruit-orange>
    * <https://openclipart.org/detail/8535/simple-fruit-pear>
    * <https://openclipart.org/detail/8534/simple-fruit-strawberry>
    * <https://openclipart.org/detail/8536/simple-fruit-lemon>
* Slides created using [Pandoc](http://pandoc.org/) and [Slidy](http://www.w3.org/Talks/Tools/Slidy2).
