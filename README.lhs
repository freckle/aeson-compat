# aeson-compat

[![Hackage](https://img.shields.io/hackage/v/aeson-compat.svg?style=flat)](https://hackage.haskell.org/package/aeson-compat)
[![Stackage Nightly](http://stackage.org/package/aeson-compat/badge/nightly)](http://stackage.org/nightly/package/aeson-compat)
[![Stackage LTS](http://stackage.org/package/aeson-compat/badge/lts)](http://stackage.org/lts/package/aeson-compat)
[![CI](https://github.com/freckle/aeson-compat/actions/workflows/ci.yml/badge.svg)](https://github.com/freckle/aeson-compat/actions/workflows/ci.yml)

Compatibility package between aeson major versions.

## Organization

This library contains modules named `Data.Aeson.VN.M`, which

1. Present the same interface as `Data.Aeson.M` as of aeson `vN`, and
2. Compile with all versions of aeson

## Example

<!--
```haskell
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Main (main) where
import Prelude
```
-->

```haskell
import Data.Aeson
import qualified Data.Aeson.V2.Key as Key
import qualified Data.Aeson.V2.KeyMap as KeyMap

someExample :: IO ()
someExample = putStrLn
  $ unwords
  $ map Key.toString
  $ KeyMap.keys
  $ (\(Object km) -> km)
  $ object
    [ "hello" .= True
    , "world" .= True
    ]
```

<!--
```haskell
main :: IO ()
main = someExample
```
-->

## Gotchas

There are cases where it's not practical to match the aeson functionality
exactly when we're making compatibility functions against older (or newer)
versions. The following lists concessions we've made.

- Our version of `alterF` (`unordered-containers < 0.2.10`) may compute the hash
  key multiple times, since we don't have access to the internals necessary to
  pre-compute the key once and use it for any lookup, insert, and delete
  actions.
- We may use strict implementations for should-be lazy functions (or vice
  versa). For example, in `unordered-containers < 0.2.11`, `foldl = foldl'` and
  `foldr' = foldr`.
- Only one of `coercionToHashMap` or `coercionToMap` will be `Just`, depending
  on your actual aeson version.
- Either conversions dealing with `HashMap` or those dealing with `Map` will be
  fast (runtime-erased), depending on your actual aeson version.

## Development & Tests

```console
stack build --fast --pedantic --test --file-watch
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
