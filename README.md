<div align="center">

# monad-effects

[![ci](http://img.shields.io/github/actions/workflow/status/tbidne/monad-effects/ci.yaml?branch=main)](https://github.com/tbidne/monad-effects/actions/workflows/ci.yaml)
[![MIT](https://img.shields.io/github/license/tbidne/monad-effects?color=blue)](https://opensource.org/licenses/MIT)

</div>

---

### Table of Contents
- [Introduction](#introduction)
- [Effects](#effects)
  - [effects-async](#effects-async)
  - [effects-env](#effects-env)
  - [effects-fs](#effects-fs)
  - [effects-haskeline](#effects-haskeline)
  - [effects-ioref](#effects-ioref)
  - [effects-logger](#effects-logger)
  - [effects-optparse](#effects-optparse)
  - [effects-stm](#effects-stm)
  - [effects-terminal](#effects-terminal)
  - [effects-thread](#effects-thread)
  - [effects-time](#effects-time)
  - [effects-typed-process](#effects-typed-process)
  - [effects-unix](#effects-unix)
  - [effects-unix-compat](#effects-unix-compat)

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
usesFile :: (MonadFileReader m, MonadTypedProcess m) => m ()
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

## effects-haskeline

### Library: `haskeline`

### Modules
* `Effects.Haskeline`

### Description

Effects for the `haskeline` library.

## effects-ioref

### Library: `base`

### Modules
* `Effects.IORef (Data.IORef)`

### Description

`IORef` effects.

## effects-logger

### Library: `effects-logger`

### Modules
* `Effects.Logger`
* `Effects.Logger.Namespace`

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

Provides a single function `atomically :: MonadSTM m => STM a -> m a` and helper combinators for other `STM` concepts (e.g. `readTVarA :: MonadSTM m => TVar a -> m a`).

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
* `Effects.Process.Typed (System.Process.Typed)`

### Description

Effect for the `typed-process` library.

## effects-unix

### Library: `unix`

### Modules

* `Effects.System.Posix (System.Posix)`

### Description

Effect for the `unix` library.

## effects-unix-compat

### Library: `unix-compat`

### Modules
* `Effects.System.PosixCompat (System.PosixCompat)`

### Description

Effect for the `unix-compat` library.
