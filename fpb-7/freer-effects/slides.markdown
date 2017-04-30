---
title: Extensible Effects
subtitle: Effectful Computations Composable
author: Matej Kollár <br/>[github.com/xkollar](https://github.com/xkollar)
date: 2017-02-22
---

Programs that *do* something
============================

~~~ { .hs }
main :: IO ()
main = do
    putStrLn "Hello, what is your name?"
    name <- readLine
    putStrLn ("Hello " <> name <> "!")
~~~

<div class="handout">
* Sequence
* Bind "variable"
</div>

Monads
======

* `IO`{ .hs }, `State`{ .hs }, `Reader`{ .hs }, `Writer`{ .hs },
  `Except`{ .hs }, `[]`{ .hs }, `Maybe`{ .hs }, …

* You are free to create your own as long as you obey *monad laws*.

Monad Transformers
==================

* Monads do not compose well

* `State s a`{ .hs } → `Monad m => StateT s m a`{ .hs }

<div class="incremental">


~~~ { .hs }
newtype ProtocolT m a = ProtocolT (ReaderT Request (WriterT Response (MaybeT m)) a)
  deriving (Applicative, Functor, Monad)
~~~

`:-(`

</div>

<div class="handout">
* All operations on the protocol depend on whole stack
    * send only needs writer, yet from its (specialized) type
      signature it is impossible to deduce so...
* Low flexibility
</div>

Extensible Effects
==================

* One monad to rule them all, into effects bind them.

~~~ { .hs }
type Protocol = '[Reader Request, Writer Response, Exc ()]
action :: Members Protocol effs => Eff effs a
--  action :: Monad m => ProtocolT m a
~~~

<div class="handout">
* Also called *Algebraic Effects*
* Quite recent idea (2001)
* Approach to computational effects based on a premise that
  impure behaviour arises from a set of operations

* One monad instead of monad ZOO.
* No more need for deriving instances (everything needed is in `Eff`)
* Order of effects is insignificant (until we start handling them)
* Given `action`{ .hs } uses only those effects it requires, but
  stack can contain any, even our custom ones.
</div>

Console
=======

~~~ { .hs }
data Console s where
    PutStrLn :: String -> Console ()
    GetLine :: Console String

putStrLn' :: Member Console r => String -> Eff r ()
putStrLn' = send . PutStrLn

getLine' :: Member Console r => Eff r String
getLine' = send GetLine
~~~

~~~ { .hs }
hello :: Member Console effs => Eff effs ()
hello = do
    putStrLn' "Hello, what is your name?"
    name <- readLine'
    putStrLn' ("Hello " <> name <> "!")
~~~

Handlers
========

~~~ { .hs }
runConsoleIO :: Member IO r => Eff (Console ': r) w -> Eff r w
~~~

~~~ { .hs }
main :: IO ()
main =     runM (runConsoleIO hello)
--         |     |            |
--  IO () -'     |            |
-- Eff '[IO] () -'            |
--     Eff '[Console, IO] () -'
~~~

<div class="incremental">

~~~ { .hs }
runConsolePure :: [String] -> Eff (Console ': r) a
    -> Eff r (a, ([String], [String]))
    -- ^ (value, (unconsumed input, produced output))
~~~

</div>

<div class="handout">
* Pure interpreter: how to test things without need for dark wizardry.
</div>

Use in Other Languages
======================

* PureScript
* OCaml
* Idris
* Eff

Conclusion
==========

* I hope you got curious
* <https://github.com/IxpertaSolutions/freer-effects>
* Pull requests are welcome `:-)`

Questions?
==========

* Thank you for your attention…

References
==========

Haskell
-------

* [Extensible Effects, An Alternative to Monad Transformers](http://okmij.org/ftp/Haskell/extensible/exteff.pdf)
* [Freer Monads, More Extensible Effects](http://okmij.org/ftp/Haskell/extensible/more.pdf)

Other Languages
---------------

* [Programming with Algebraic Effects and Handlers](http://math.andrej.com/wp-content/uploads/2012/03/eff.pdf) (Eff)
* [Programming and Reasoning with Algebraic Effects and Dependent Types](http://web.archive.org/web/20150102181601/http://eb.host.cs.st-andrews.ac.uk/drafts/effects.pdf) (Idris)

Original paper
--------------

* [Adequacy for Algebraic Effects](http://homepages.inf.ed.ac.uk/gdp/publications/Op_Sem_Comp_Lam.pdf)

Haskell
=======

* Purely functional
* Statically typed
* Lazy

Problems With Monad Transformers
================================

* Order in which transformers apply matters
* Complications when only a subset of the transformers
  stack is needed. (`lift`, ...)
* It lures you into putting everything into one huge
  state where you can access everything from everywhere...
* (Yet we grew love them and use them every day)

Examples
========

~~~ { .hs }
exampleEcho :: Member Console r => Eff r ()
exampleEcho = forever $ do
    putStrLn' "Send me something to echo..."
    s <- getLine'
    putStrLn' ("Thanks for sending " ++ show s)

mainEcho :: IO ()
mainEcho = runM (runConsoleM exampleEcho)
--         |     |            |
--  IO () -'     |            |
-- Eff '[IO] () -'            |
--    Eff '[Console, IO] () -'
~~~

---

Capitalization as a Service
---------------------------

~~~ { .hs }
data Capitalize v where
    Capitalize :: String -> Capitalize String

capitalize :: Member Capitalize r => String -> Eff r String
capitalize = send . Capitalize

runCapitalizeM :: Eff (Capitalize ': r) w -> Eff r w
~~~

---

Composing
---------

~~~ { .hs }
exampleCapitalize :: Members '[Console, Capitalize] effs => Eff effs ()
exampleCapitalize = forever $ do
    putStrLn' "Send me something to capitalize..."
    l <- getLine'
    when (null l) exitSuccess'
    capitalize l >>= putStrLn'
~~~

~~~ { .hs }
mainCapitalize1 :: IO ()
mainCapitalize1 = runM (runConsoleM (runCapitalizeM exampleCapitalize))
--                |     |             |              |
--         IO () -'     |             |              |
--        Eff '[IO] () -'             |              |
--             Eff '[Console, IO] () -'              |
--                Eff '[Capitalize, Console, IO] () -'
~~~

~~~ { .hs }
mainCapitalize2 :: IO ()
mainCapitalize2 = runM (runCapitalizeM (runConsoleM exampleCapitalize))
--                |     |             |              |
--         IO () -'     |             |              |
--        Eff '[IO] () -'             |              |
--          Eff '[Capitalize, IO] () -'              |
--                Eff '[Console, Capitalize, IO] () -'
~~~

---

Extra notes
===========

* Extending stack locally (exceptions)
* Testability
    * Pure vs Effectful interpreters
* Combining with existing transformer stack
* Disadvantages


Instances for Eff
=================

* `Functor`, `Applicative`, `Monad`

Effects Readily available
=========================

* `Reader`
* `Writer`
* `State`
* `Exception`
* `NonDet`
* And couple of half-baked ones
    * `Coroutine`
    * `Fresh`
    * `Trace`
    * `Cut`
