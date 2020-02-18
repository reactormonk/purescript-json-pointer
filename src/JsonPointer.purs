module JsonPointer where

import Prelude

import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.List.Types (NonEmptyList(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.String (Pattern(..), Replacement(..), joinWith, replaceAll, split)
import Foreign (F)
import Foreign as F

newtype JsonPointer = JsonPointer (Array JsonPointerElement)
derive newtype instance jsonPointerSemigroup :: Semigroup JsonPointer
derive newtype instance jsonPointerMonoid :: Monoid JsonPointer
derive newtype instance jsonPointerEq :: Eq JsonPointer
derive instance jsonPointerNewtype :: Newtype JsonPointer _
derive instance jsonPointerGeneric :: Generic JsonPointer _
instance jsonPointerShow :: Show JsonPointer where
  show = genericShow

data JsonPointerElement =
    ArrayPointer Int
  | ObjectPointer String

derive instance jsonPointerElementEq :: Eq JsonPointerElement
derive instance jsonPointerElementGeneric :: Generic JsonPointerElement _
instance jsonPointerElementShow :: Show JsonPointerElement where
  show = genericShow


type E a = Either F.MultipleErrors a

jqF :: forall a. Show a => F a -> Either (NonEmptyList String) a
jqF parseResult = jqE $ runExcept parseResult

jqE :: forall a. Show a => E a -> Either (NonEmptyList String) a
jqE (Left err) =
  let
    parsed = foreignToPtr <$> err
    asJqPath (ArrayPointer i)  = "[" <> show i <> "]"
    asJqPath (ObjectPointer o) = "." <> o
  in Left $ parsed <#> (\p ->
    "jq '" <> (joinWith "" $ asJqPath <$> (unwrap p.pointer)) <> "' # to display the json at " <> show p.error
  )
jqE (Right res) = Right $ res

renderJsonPointer :: JsonPointer -> String
renderJsonPointer (JsonPointer nea) =
  intercalate "/" $ map (escaping <<< renderElement) nea
  where
    renderElement (ArrayPointer i)  = show i
    renderElement (ObjectPointer s) = s
    escaping =
      replaceAll (Pattern "/") (Replacement "~1") <<<
      replaceAll (Pattern "~") (Replacement "~0")

parseJsonPointer :: String -> JsonPointer
parseJsonPointer input =
  JsonPointer $ map (parseElement <<< unescaping) $ split (Pattern "/") input
  where
    parseElement s = (fromMaybe (ObjectPointer s) (ArrayPointer <$> fromString s))
    unescaping =
      replaceAll (Pattern "~0") (Replacement "~") <<<
      replaceAll (Pattern "~1") (Replacement "/")

foreignToPtr :: F.ForeignError -> { pointer :: JsonPointer, error :: ForeignError }
foreignToPtr (F.ForeignError str) = { pointer: mempty, error: ForeignError str }
foreignToPtr (F.TypeMismatch a b) = { pointer: mempty, error: TypeMismatch a b }
foreignToPtr (F.ErrorAtIndex i err) =
  let rec = (foreignToPtr err) in
  rec { pointer = JsonPointer [ArrayPointer i] <> rec.pointer }
foreignToPtr (F.ErrorAtProperty p err) =
  let rec = (foreignToPtr err) in
  rec { pointer = JsonPointer [ObjectPointer p] <> rec.pointer }

data ForeignError =
  ForeignError String
  | TypeMismatch String String

derive instance foreignErrorGeneric :: Generic ForeignError _
instance foreignErrorShow :: Show ForeignError where
  show = genericShow
instance foreignErrorEq :: Eq ForeignError where
  eq = genericEq
