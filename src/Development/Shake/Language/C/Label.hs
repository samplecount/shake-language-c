-- Copyright 2012-2013 Samplecount S.L.
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE TypeOperators #-}

{-|
Description: Lens utilities

This module provides some lens utilities and also re-exports "Data.Label",
which can avoid a package dependency on @fclabels@ in some cases.
-}
module Development.Shake.Language.C.Label (
    module Data.Label
  , append
  , prepend
) where

import Data.Label
import Data.Monoid (Monoid, mappend)

-- | Append stuff to the 'Monoid' in record field specified by lens.
append :: Monoid a =>
     (f :-> a) -- ^ lens
  -> a -- ^ stuff to append
  -> f -- ^ original record
  -> f -- ^ modified record
append l n = modify l (`mappend` n)

-- | Prepend stuff to the 'Monoid' in record field specified by lens.
prepend :: Monoid a =>
     (f :-> a) -- ^ lens
  -> a -- ^ stuff to prepend
  -> f -- ^ original record
  -> f -- ^ modified record
prepend l n = modify l (n `mappend`)
