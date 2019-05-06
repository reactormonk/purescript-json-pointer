module JsonPointer where

import Prelude

import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.String (Pattern(..), Replacement(..), replaceAll, split)
import Foreign as F

newtype JsonPointer = JsonPointer (Array String)

derive newtype instance jsonPointerSemigroup :: Semigroup JsonPointer
derive newtype instance jsonPointerMonoid :: Monoid JsonPointer
derive newtype instance jsonPointerEq :: Eq JsonPointer
derive instance jsonPointerGeneric :: Generic JsonPointer _
instance jsonPointerShow :: Show JsonPointer where
  show = genericShow

renderJsonPointer :: JsonPointer -> String
renderJsonPointer (JsonPointer nea) =
  intercalate "/" $ map escaping nea
  where
    escaping =
      replaceAll (Pattern "/") (Replacement "~1") <<<
      replaceAll (Pattern "~") (Replacement "~0")

parseJsonPointer :: String -> JsonPointer
parseJsonPointer input =
  JsonPointer $ map unescaping $ split (Pattern "/") input
  where
    unescaping =
      replaceAll (Pattern "~0") (Replacement "~") <<<
      replaceAll (Pattern "~1") (Replacement "/")

foreignToPtr :: F.ForeignError -> { pointer :: JsonPointer, error :: ForeignError }
foreignToPtr (F.ForeignError str) = { pointer: mempty, error: ForeignError str }
foreignToPtr (F.TypeMismatch a b) = { pointer: mempty, error: TypeMismatch a b }
foreignToPtr (F.ErrorAtIndex i err) =
  let rec = (foreignToPtr err) in
  rec { pointer = JsonPointer [show i] <> rec.pointer }
foreignToPtr (F.ErrorAtProperty p err) =
  let rec = (foreignToPtr err) in
  rec { pointer = JsonPointer [p] <> rec.pointer }

data ForeignError =
  ForeignError String
  | TypeMismatch String String

derive instance foreignErrorGeneric :: Generic ForeignError _
instance foreignErrorShow :: Show ForeignError where
  show = genericShow
instance foreignErrorEq :: Eq ForeignError where
  eq = genericEq
