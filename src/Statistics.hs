{-# LANGUAGE DeriveGeneric, FlexibleInstances, StandaloneDeriving, TemplateHaskell #-}

module Statistics where
{-
 - This module handles the file interaction with the highscores file
 - also the actual score is described and how we show it
 - If you want to keep track of additional things, add it here and don't forget to show it in the Show instance
 -}
import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Default
import Data.Functor.Identity
import System.Directory
import Control.Lens
import GHC.Generics

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL


jload :: FromJSON a => FilePath -> IO (Maybe a)
jload fp = do
    exists <- doesFileExist fp

    if exists then
        decode . BL.fromStrict <$> BS.readFile fp
    else
        return Nothing



jdump :: ToJSON a => a -> FilePath -> IO ()
jdump obj fp = BL.writeFile fp (encode obj)


data Stats' f = Stats' {_survived :: f Float, _attempt :: f Int, _score:: Int} deriving Generic
makeLenses ''Stats'

blankStats :: Stats' Identity
blankStats = Stats' 0.0 0 0

instance FromJSON (Stats' Maybe)
instance ToJSON (Stats' Identity)

instance Show (Stats' Identity) where
    show s = "Attempt: "    ++ show (runIdentity $ _attempt s) ++
             ", survived: " ++ takeWhile (/= '.') (show (runIdentity $ _survived s)) ++ " seconds" ++  
             ", score : "   ++ show (_score s) ++ "points"

deriving instance Eq (Stats' Identity)

instance Ord (Stats' Identity) where
    x < y = x ^. score < y ^. score
    x <= y = x == y || x < y


instance Default Stats' where
    constrDef _ = blankStats

type Stats = Stats' Identity
