<div align="center">

# monad-effects

![haskell](https://img.shields.io/static/v1?label=&message=9.4&logo=haskell&logoColor=655889&labelColor=2f353e&color=655889)
[![MIT](https://img.shields.io/github/license/tbidne/monad-effects?color=blue)](https://opensource.org/licenses/MIT)

[![nix](http://img.shields.io/github/actions/workflow/status/tbidne/monad-effects/nix.yaml?branch=main&label=nix&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/monad-effects/actions/workflows/nix.yaml)
[![cabal](http://img.shields.io/github/actions/workflow/status/tbidne/monad-effects/cabal.yaml?branch=main&label=cabal&labelColor=2f353c)](https://github.com/tbidne/monad-effects/actions/workflows/cabal.yaml)
[![style](http://img.shields.io/github/actions/workflow/status/tbidne/monad-effects/style.yaml?branch=main&label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/monad-effects/actions/workflows/style.yaml)

</div>

---

### Table of Contents
- [Introduction](#introduction)
- [Effects](#effects)
  - [effects-async](#effects-async)
  - [effects-env](#effects-env)
  - [effects-exceptions](#effects-exceptions)
  - [effects-fs](#effects-fs)
  - [effects-ioref](#effects-ioref)
  - [effects-logger-ns](#effects-logger-ns)
  - [effects-optparse](#effects-optparse)
  - [effects-stm](#effects-stm)
  - [effects-terminal](#effects-terminal)
  - [effects-thread](#effects-thread)
  - [effects-time](#effects-time)
  - [effects-typed-process](#effects-typed-process)

# Introduction

This repository contains a number of monadic "effects" for writing code that implements an abstract interface. For instance, instead of writing:

```haskell
usesFile :: MonadIO m => m ()
usesFile = do
  liftIO $ readBinaryFile path
  liftIO $ runProcess_ config
  ...
```

We have:

```haskell
usesFile :: (MonadFileReader m, MonadProcess m) => m ()
usesFile = do
  readBinaryFile path
  runProcess_ config
  ...
```

This facilitates:

* More principled function signatures i.e avoiding `IO` / `MonadIO` where anything can happen.
* Multiple implementations (e.g. mocking)

Most effects are thin wrappers over common functionality e.g. a `base` module or popular library API.

NOTE: Although this is "MTL-style", the only transformer we provide instances for is `ReaderT`.

# Effects

The following lists the supported effects, along with the libraries/modules they represent.

## effects-async

### Library: `async`

### Modules
* `Effects.Concurrent.Async (Control.Concurrent.Async)`

### Description

Effect for the `async` library. The implementation is nearly identical to `async`'s API, with the additions of `unliftio`'s "pooled concurrency" functions.

## effects-env

### Library: `base`

### Modules
* `Effects.System.Environment (System.Environment)`

### Description

Effect for `System.Environment`.

## effects-exceptions

### Library: `exceptions`, `safe-exceptions`

### Modules
* `Effects.Exception`

### Description

Provides:

* Functions for throwing/catching exceptions with callstacks.
* `MonadThrow/MonadCatch/MonadMask` typeclasses (`exceptions`).
* General throw/catch, disallowing catching async exceptions (`safe-exceptions`).
* Brackets _without_ `uninterruptibleMask` (`exceptions`).

The `CallStack` machinery may be removed once GHC natively supports adding `CallStack` to exceptions (GHC 9.8?). See
https://github.com/ghc-proposals/ghc-proposals/pull/330.

## effects-fs

### Library: `base`, `bytestring`, `directory`

### Modules
* `Effects.FileSystem.FileReader (Data.ByteString)`
* `Effects.FileSystem.FileWriter (Data.ByteString)`
* `Effects.FileSystem.PathReader (System.Directory)`
* `Effects.FileSystem.PathWriter (System.Directory)`
* `Effects.FileSystem.HandleReader (System.IO)`
* `Effects.FileSystem.HandleWriter (System.IO)`

### Description

Filesystem effects. In particular:

* The `File*` modules are for reading/writing to files, and include helper functions for (de/en)coding UTF-8.
* The `Path*` modules implement the `directory` interface.
* The `Handle*` modules implement `System.IO` handle operations.

These are written to be compatible with the upcoming `FilePath -> OsPath` change, i.e. if `filepath` and `directory` libraries are new enough (>= `1.4.100` and `1.3.8`, respectively), then the APIs require `OsPath`, not `FilePath`.

## effects-ioref

### Library: `base`

### Modules
* `Effects.IORef (Data.IORef)`

### Description

`IORef` effects.

## effects-logger-ns

### Library: `effects-logger`

### Modules
* `Effects.LoggerNS`

### Description

Builds on top of the `monad-logger` library to add the concept of "namespacing" to logs. Includes helper functions for formatting.

## effects-optparse

### Library: `optparse-applicative`

### Modules
* `Effects.MonadOptParse (Options.Applicative)`

### Description

Most of `optparse-applicative`'s API is pure, so there is not much here, just functions for running the parser.

## effects-stm

### Library: `stm`

### Modules
* `Effects.Concurrent.STM (Control.Concurrent.STM)`

### Description

Provides a single function `atomically :: MonadSTM m => STM a -> m a` and helper combinators for other `STM` concepts (e.g. `readTVarM :: MonadSTM m => TVar a -> m a`).

## effects-terminal

### Library: `base`

### Modules
* `Effects.System.Terminal (System.IO)`

### Description

Implements typical terminal functions e.g. `putStrLn`.

## effects-thread

### Library: `base`

### Modules
* `Effects.Concurrent.Thread (Control.Concurrent)`

### Description

Implements functions from `Control.Concurrent` along with `Control.Concurrent.QSem` and `Control.Concurrent.QSemN`.

## effects-time

### Library: `time`

### Modules
* `Effects.Time`

### Description

Provides functions for retrieving the current system time and monotonic time.

## effects-typed-process

### Library: `typed-process`

### Modules
* `Effects.System.Process (System.Process.Typed)`

### Description

Effect for the `typed-process` library.