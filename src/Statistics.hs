{-# LANGUAGE DeriveGeneric, FlexibleInstances, StandaloneDeriving, TemplateHaskell #-}

module Statistics where

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
             ", survived: " ++ show (runIdentity $ _survived s)


instance Default Stats' where
    constrDef _ = blankStats

type Stats = Stats' Identity
