{-# LANGUAGE DeriveGeneric, FlexibleInstances, StandaloneDeriving, TemplateHaskell #-}

module Statistics where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.Default
import Data.Functor.Identity
import System.Directory
import Control.Lens
import GHC.Generics
import qualified Data.ByteString.Lazy as LB

import Enemies

--data Statistics = Statistics {survived :: Float} deriving Show

--emptyStats :: Statistics
--emptyStats = Statistics 0.0


jload :: FromJSON a => FilePath -> IO (Maybe a)
jload fp = do
    exists <- doesFileExist fp

    if exists then
        decode <$> LB.readFile fp
    else
        return Nothing

    

jdump :: ToJSON a => a -> FilePath -> IO ()
jdump obj fp = LB.writeFile fp (encode obj)


newtype Stats' f = Stats' {_survived :: f Float} deriving Generic
makeLenses ''Stats'

instance FromJSON (Stats' Maybe)
instance ToJSON (Stats' Identity)
deriving instance Show (Stats' Identity)


instance Default Stats' where
    constrDef _ = Stats' 0.0
