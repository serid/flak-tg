module Flak.AesonUtils (trimLabel) where

import Flak.Util (trimStart)

import qualified Data.Aeson as A

-- Aeson deserialization options that trim beginning of the field name until '
-- user'id -> id
trimLabel = A.defaultOptions { A.fieldLabelModifier = f }
    where f label = trimStart '\'' label

