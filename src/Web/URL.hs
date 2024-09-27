module Web.URL (URLFrag (..), ToURL (..)) where

import Data.Maybe (fromMaybe)
import Data.String (IsString (..))
import Data.Text (Text, pack, stripPrefix, stripSuffix)

newtype URLFrag = URLFrag Text
    deriving (Eq, Show)

instance IsString URLFrag where
    fromString = URLFrag . pack

instance Semigroup URLFrag where
    (<>) (URLFrag a) (URLFrag b) =
        URLFrag $
            fromMaybe a (stripSuffix "/" a) <> "/" <> fromMaybe b (stripPrefix "/" b)

instance Monoid URLFrag where
    mempty = URLFrag ""

class ToURL a where
    toURL :: a -> Text

instance ToURL URLFrag where
    toURL (URLFrag a) = a