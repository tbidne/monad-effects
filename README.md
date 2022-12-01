<div align="center">

# monad-effects

[![GitHub release (latest SemVer)](https://img.shields.io/github/v/release/tbidne/monad-effects?include_prereleases&sort=semver)](https://github.com/tbidne/monad-effects/releases/)
![haskell](https://img.shields.io/static/v1?label=&message=9.4&logo=haskell&logoColor=655889&labelColor=2f353e&color=655889)
[![MIT](https://img.shields.io/github/license/tbidne/monad-effects?color=blue)](https://opensource.org/licenses/MIT)

[![nix](https://img.shields.io/github/workflow/status/tbidne/monad-effects/nix/main?label=nix&&logo=nixos&logoColor=85c5e7&labelColor=2f353c)](https://github.com/tbidne/monad-effects/actions/workflows/nix.yaml)
[![cabal](https://img.shields.io/github/workflow/status/tbidne/monad-effects/cabal/main?label=cabal&labelColor=2f353c)](https://github.com/tbidne/monad-effects/actions/workflows/cabal.yaml)
[![stack](https://img.shields.io/github/workflow/status/tbidne/monad-effects/stack/main?label=stack&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/monad-effects/actions/workflows/stack.yaml)
[![style](https://img.shields.io/github/workflow/status/tbidne/monad-effects/style/main?label=style&logoColor=white&labelColor=2f353c)](https://github.com/tbidne/monad-effects/actions/workflows/style.yaml)

</div>

---

### Table of Contents
* [Introduction](#introduction)
* [Effects](#effects)
  * [MonadCallStack](#monadcallstack)
  * [MonadFsReader](#monadfsreader)
  * [MonadLoggerNamespace](#monadloggernamespace)
  * [MonadThread](#monadthread)
  * [MonadTerminal](#monadterminal)
  * [MonadTime](#monadtime)

# Introduction

This repository provides typeclasses representing common "effects". The intention is to enable code-reuse and abstract away various effects into corresponding typeclasses.

# Effects

## [MonadCallStack](./monad-callstack/)

The `MonadCallStack` effect integrates `Exception` with `CallStack`.

## [MonadFsReader](./monad-fs-reader/)

The `MonadFsReader` effect emulates a file-system reader.

## [MonadLoggerNamespace](./monad-logger-namespace/)

The `MonadLoggerNamespace` effect builds on top of the popular `MonadLogger` effect by providing namespace functionality. Additionally, helpful formatters are provided.

## [MonadThread](./monad-thread/)

The `MonadThread` effect provides basic thread functionality.

## [MonadTerminal](./monad-terminal/)

The `MonadTerminal` effect provides terminal-like functions e.g. printing to a terminal, receiving user input, and detecting terminal dimensions.

## [MonadTime](./monad-time)

The `MonadTime` effect retrieves the local system and time and provides utilities for timing actions.
