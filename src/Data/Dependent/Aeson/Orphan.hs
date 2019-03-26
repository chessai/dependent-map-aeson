{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

{-# options_ghc -fno-warn-orphans #-}

module Data.Dependent.Aeson.Orphan () where

import Data.Aeson
import Data.Exists
import Data.Dependent.Map
import Data.Text
import Data.HashMap.Strict
import Data.Foldable (foldlM)
import qualified Data.Aeson as AE
import qualified Data.Dependent.Map as DM
import qualified Data.HashMap.Strict as HM
import qualified Data.Aeson.Types as AET

instance (DM.GCompare k, FromJSONKeyExists k, FromJSONForeach v, ToSing k) => FromJSON (DMap k v) where
  parseJSON obj = case fromJSONKeyExists of
    AE.FromJSONKeyCoerce _ -> error "FromJSON instance for DMap: FromJSONKeyCoerce"
    AE.FromJSONKeyText fromText -> AE.withObject "DMap"
      (HM.foldrWithKey (f1 fromText) (pure DM.empty))
      obj
    AE.FromJSONKeyTextParser fromText -> AE.withObject "DMap"
      (HM.foldrWithKey (f2 fromText) (pure DM.empty))
      obj
    AE.FromJSONKeyValue fromValue -> AE.withArray "DMap"
      (foldlM (f3 fromValue) DM.empty)
      obj
    where
    f1 :: (Text -> Exists k) -> Text -> AE.Value -> AET.Parser (DMap k v) -> AET.Parser (DMap k v)
    f1 fromText keyText valRaw m = case fromText keyText of
      Exists key -> do
        let keySing = toSing key
        val <- parseJSONForeach keySing valRaw
        dm <- m
        pure (DM.insert key val dm)
    f2 :: (Text -> AET.Parser (Exists k)) -> Text -> AE.Value -> AET.Parser (DMap k v) -> AET.Parser (DMap k v)
    f2 fromText keyText valRaw m = do
      Exists key <- fromText keyText
      let keySing = toSing key
      val <- parseJSONForeach keySing valRaw
      dm <- m
      pure (DM.insert key val dm)
    f3 :: (AE.Value -> AET.Parser (Exists k)) -> DMap k v -> AE.Value -> AET.Parser (DMap k v)
    f3 fromValue dm pairRaw = do
      (keyRaw :: AE.Value,valRaw :: AE.Value) <- parseJSON pairRaw
      Exists key <- fromValue keyRaw
      let keySing = toSing key
      val <- parseJSONForeach keySing valRaw
      pure (DM.insert key val dm)

instance (ToJSONKeyForall k, ToJSONForeach v, ToSing k) => ToJSON (DMap k v) where
  toJSON dm = case toJSONKeyForall of
    ToJSONKeyTextForall toText _ -> AE.Object (DM.foldlWithKey (f toText) HM.empty dm)
    ToJSONKeyValueForall toValue _ -> toJSON (DM.foldrWithKey (g toValue) [] dm)
    where
    f :: forall a. (k a -> Text) -> HashMap Text AE.Value -> k a -> v a -> HashMap Text AE.Value
    f toText hm k v = HM.insert (toText k) (toJSONForeach (toSing k) v) hm
    g :: forall a. (k a -> AE.Value) -> k a -> v a -> [(AE.Value,AE.Value)] -> [(AE.Value,AE.Value)]
    g toValue k v xs = (toValue k, toJSONForeach (toSing k) v) : xs
