<p align="center">
<img src="./logo.png">
</p>

# effet

[![Hackage](https://img.shields.io/hackage/v/effet.svg?logo=haskell&label=effet)](https://hackage.haskell.org/package/effet)

[plucking constraints]: https://www.parsonsmatt.org/2020/01/03/plucking_constraints.html

[Overview]: https://github.com/typedbyte/effet#overview
[Quick-Start Guide]: https://github.com/typedbyte/effet#quick-start-guide
[Defining Effects]: https://github.com/typedbyte/effet#defining-effects
[Using Effects]: https://github.com/typedbyte/effet#using-effects
[Defining Effect Handlers]: https://github.com/typedbyte/effet#defining-effect-handlers
[Using Effect Handlers]: https://github.com/typedbyte/effet#using-effect-handlers
[Tagging, Retagging, Untagging]: https://github.com/typedbyte/effet#tagging-retagging-untagging
[Limitations and Remarks]: https://github.com/typedbyte/effet#limitations-and-remarks

* [Overview][]
* [Quick-Start Guide][]
  * [Defining Effects][]
  * [Using Effects][]
  * [Defining Effect Handlers][]
  * [Using Effect Handlers][]
  * [Tagging, Retagging, Untagging][]
* [Limitations and Remarks][]

## Overview

`effet` is an effect system based on type classes, written in Haskell. A central idea of an effect system is to track effects (like performing a file system access) on the type level in order to describe the behavior of functions more precisely. Another central idea of an effect system is the well-known design principle of separating an interface (the *effect*) from its actual implementation (the *effect handler* or *effect interpreter*). Hence, `effet` allows developers to write programs by composing functions which describe their effects on the type level, and to provide different implementation strategies for those effects when running the programs.

`effet` is by far not the first library which pursues these ideas. It borrows various ideas from existing libraries, just to name a few:

* It is based on type classes, like `mtl` and `fused-effects`.
* It is based on [plucking constraints][], like `mtl`, but without the "n² instances problem".
* It supports `TemplateHaskell`-based generation of the effect infrastructure, like `polysemy`.
* It supports functional dependencies and tagged effects for effect disambiguation, like `ether`.

`effet` has a rather down-to-earth implementation without much magic. It is like a thin wrapper around the `transformers` library and its lifting friends `transformers-base` and `monad-control`, thus minimizing the reinvention of the wheel. The library and its documentation can be found on [Hackage](https://hackage.haskell.org/package/effet).

## Quick-Start Guide

### Defining Effects

When defining effects or effect handlers, the module *Control.Effect.Machinery* provides everything we need:

```haskell
import Control.Effect.Machinery
```

Effects are ordinary type classes, like the following:

```haskell
class Monad m => FileSystem' tag m where
  readFile'  :: FilePath -> m String
  writeFile' :: FilePath -> String -> m ()
```

Note the `tag` type parameter. Such a parameter is used to disambiguate effects of the same type, i.e. we can use multiple `FileSystem` effects in our program simultaneously. In `effet`, the naming convention is to use an apostrophe as name suffix whenever we use tags. However, `effet` is not limited to that, we can easily define our effect without a `tag` parameter:

```haskell
class Monad m => FileSystem m where
  readFile  :: FilePath -> m String
  writeFile :: FilePath -> String -> m ()
```

Let's go on with the tagged version for now and pretend that the untagged type class does not exist. The next step is to generate the effect handling, lifting and tagging infrastructure for our new effect using the `TemplateHaskell` language extension:

```haskell
makeTaggedEffect ''FileSystem'
```

This line generates the necessary infrastructure for combining our new effect with other effects. We also get the untagged version of the effect for free, i.e. the corresponding untagged definitions (`FileSystem`, `readFile`, `writeFile`, note the missing apostrophes) are generated for us. This line also generates functions which help us to tag (`tagFileSystem'`), retag
(`retagFileSystem'`) and untag (`untagFileSystem'`) our effect. More on that later.

`effet` also provides the function `makeTaggedEffectWith` to define our own naming convention if we don't like the apostrophes. For untagged effects, we would have simply used the function `makeEffect` instead.

### Using Effects

We can now use our effect to write programs that access the file system. We will define a simple program which appends something to a file. In order to make it more interesting, we will combine it with the pre-defined `Writer` effect to report the file size before and after appending, just to demonstrate the interplay with other effects:

```haskell
import Control.Effect.Writer
```

```haskell
program :: (FileSystem m, Writer [Int] m) => FilePath -> String -> m ()
program path txt = do
  content <- readFile path
  let size = length content
  tell [size]
  seq size $ writeFile path (content ++ txt)
  tell [size + length txt]
```

In order to run this program, we need a handler (or "interpreter") for our effect. There are several ways to interpret our effect. We could, for example, really access our local file system or just provide a virtual, in-memory file system. Hence, we will define two effect handlers for demonstration purposes.

### Defining Effect Handlers

First of all, we could &ndash; in theory &ndash; interpret effects without using `effet` at all, which is something that is not possible in many other effect systems. Since effects are ordinary type classes, we would just instantiate the monad type `m` with a type that provides instances for all (!) the effect type classes that constrain `m`. This is where the so-called "n² instances problem" of `mtl` comes from, since every effect handler that wants to handle a single effect (i.e., that wants to provide a type class instance for the effect it wants to handle) must also provide type class instances for all other effects &ndash; some of which might not even be known at compile-time &ndash; in order to delegate them to their corresponding effect handlers.

The advantage of `effet` is the ability to interpret effects separately, one by one, by [plucking constraints][] and not caring about all effects at the same time. In `effet`, we write an effect handler by defining a type and a corresponding type class instance for the effect we are interested in and let the infrastructure of `effet` handle the delegation of all other effects to their corresponding effect handlers.

An effect handler is a monad transformer which provides an instance for our effect type class. First, let's define the handler type for the local file system handler. We can derive all the necessary instances for proper effect lifting, some of them by using the `DerivingVia` language extension:

```haskell
newtype LocalFS m a =
  LocalFS { runLocalFS :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl) via Default
    deriving (MonadBase b, MonadBaseControl b)
```

Next, we provide the instance for our `FileSystem'` effect. We need another import for that:

```haskell
import qualified System.IO as IO
```

```haskell
instance MonadIO m => FileSystem' tag (LocalFS m) where
  readFile' path = liftIO $ IO.readFile path
  writeFile' path txt = liftIO $ IO.writeFile path txt
```

Note that we do not need a separate instance for the untagged version of our effect which was generated before.

In order to make the usage of our instance more comfortable, we additionally provide two functions which instruct the type system to use our particular instance for handling the `FileSystem` effect when running a particular program. We write one tagged and one untagged version:

```haskell
runLocalFileSystem' :: (FileSystem' tag `Via` LocalFS) m a -> m a
runLocalFileSystem' = coerce

runLocalFileSystem :: (FileSystem `Via` LocalFS) m a -> m a
runLocalFileSystem = runLocalFileSystem' @G
```

Done! Now let's provide similar definitions for our virtual file system. Instead of `IO`, we will use the `Map` effect that is shipped with `effet` in order to map file paths to their corresponding contents. For simplification, we will assume that the contents of non-existing files are empty strings:

```haskell
newtype VirtualFS m a =
  VirtualFS { runVirtualFS :: m a }
    deriving (Applicative, Functor, Monad, MonadIO)
    deriving (MonadTrans, MonadTransControl) via Default
    deriving (MonadBase b, MonadBaseControl b)

instance Map' tag FilePath String m => FileSystem' tag (VirtualFS m) where
  readFile' path = VirtualFS $ fromMaybe "" <$> lookup' @tag path
  writeFile' path txt = VirtualFS $ insert' @tag path txt

runVirtualFileSystem' :: (FileSystem' tag `Via` VirtualFS) m a -> m a
runVirtualFileSystem' = coerce

runVirtualFileSystem :: (FileSystem `Via` VirtualFS) m a -> m a
runVirtualFileSystem = runVirtualFileSystem' @G
```

As we can see above, there is some boilerplate involved when defining effect handlers, like the two `run`-functions which look almost identically. There is certainly potential to generate more code to mitigate this in the future.

### Using Effect Handlers

Now we have all our puzzle pieces together in order to run our program with different effect handlers. Let's recap the type of our program:

```haskell
program :: (FileSystem m, Writer [Int] m) => FilePath -> String -> m ()
```

Let's see what happens if we use our `runVirtualFileSystem` function on that program after we feed it some file path and the content that should be appended to the file:

```haskell
test :: (Map FilePath String m, Writer [Int] m) => m ()
test = runVirtualFileSystem $ program "/tmp/test.txt" "hello"
```

What happened? We handled the `FileSystem` effect using our virtual file system handler (i.e., we "plucked the constraint"), which gives us a new program as result that still needs its `Map` and `Writer` effects handled. We essentially reinterpreted one effect (`FileSystem`) in terms of another (`Map`) without touching all the other effects (`Writer`). In order to run the remaining two effects, we just need to import some of the pre-defined effect handlers for the `Map` and `Writer` effects:

```haskell
import Control.Effect.Map.Lazy
import Control.Effect.Writer.Lazy
```

And here is the complete code for running our program either locally or virtually:

```haskell
runLocalProgram :: MonadIO m => m ([Int], ())
runLocalProgram
  = runWriter
  . runLocalFileSystem
  $ program "/tmp/test.txt" "hello"

runVirtualProgram :: Monad m => m ([Int], ())
runVirtualProgram
  = runWriter
  . runMap
  . runVirtualFileSystem
  $ program "/tmp/test.txt" "hello"
```

As we can see by the types, no `IO` is involved in the virtual program, since we do not touch the actual file system.

We decided to handle our `Map` and `Writer` effects using their lazy implementations. We could, for example, easily switch the handlers to their strict counterparts, or even use some other Map-like implementation for our `Map` effect, like Redis.

### Tagging, Retagging, Untagging

If we write a program with multiple `FileSystem'` effects, we can disambiguate them using the tags ...

```haskell
fsProgram :: (FileSystem' "fs1" m, FileSystem' "fs2" m) => m ()
fsProgram = do
  writeFile' @"fs1" "/tmp/test.txt" "first content"
  writeFile' @"fs2" "/tmp/test.txt" "second content"
```

... and interpret them differently, one using the local and one using the virtual file system, for example:

```haskell
runDifferently :: MonadIO m => m ()
runDifferently
  = runMap' @"fs1"
  . runVirtualFileSystem' @"fs1"
  . runLocalFileSystem' @"fs2"
  $ fsProgram
```

We could also use one tagged (`FileSystem'`) and one untagged (`FileSystem`) effect to disambiguate them, of course.

We can also change the tags of our effects before interpretation using the generated `tagFileSystem'`, `retagFileSystem'` and `untagFileSystem'` functions. We could, for example, merge the two effects into a single tag and interpret them uniformly:

```haskell
runUniformly :: Monad m => m ()
runUniformly
  = runMap' @"fs1"
  . runVirtualFileSystem' @"fs1"
  . retagFileSystem' @"fs2" @"fs1"
  $ fsProgram
```

In the example above, we merge tag `fs2` into tag `fs1` and interpret them together using the virtual file system. We can achieve the same result by untagging both effects and then using the untagged versions of the interpretation functions:

```haskell
runUntagged :: Monad m => m ()
runUntagged
  = runMap
  . runVirtualFileSystem
  . untagFileSystem' @"fs2"
  . untagFileSystem' @"fs1"
  $ fsProgram
```

Tagging, retagging and untagging are useful if we want to compose two functions which have the same effects, but we want to interpret them differently after composition. Note that there is a chance that two authors write two different libraries using the same effects, but do not know from each other ...

```haskell
-- somewhere in library A
functionA :: FileSystem m => m ()
functionA = ...
```

```haskell
-- somewhere in library B
functionB :: FileSystem m => m ()
functionB = ...
```

... and we want to compose these functions, but interpret the effects differently after composition:

```haskell
-- oops, the effects were merged!
ourProgram :: FileSystem m => m ()
ourProgram = functionA >> functionB
```

We then need to introduce a tag for at least one of the two functions ...

```haskell
-- now the effects are separated
ourProgram :: (FileSystem m, FileSystem' "b" m) => m ()
ourProgram = functionA >> tagFileSystem' @"b" functionB
```

... which again allows us to interpret the effects independently as described above.

## Limitations and Remarks

* `TemplateHaskell`-based code generation can yield code that does not compile if you go crazy with `m`-based parameters in higher-order effect methods (where `m` is the monad type parameter of the effect type class). In such cases, one has to write the necessary type class instances by hand. They are explained in the documentation of the module `Control.Effect.Machinery.TH`.
* Effect type classes that are based on other effect type classes (like `RWS`) are possible, but cannot be used with the provided code generation infrastructure yet (not to be confused with writing an effect *handler* based on other effects, which is possible).
* The performance should be `mtl`-like, but this has not been verified yet.
* The library needs some tests.