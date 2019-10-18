{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Zureg.Hackathon.Bristol2020.Model
    (
      Project (..)
    , RegisterInfo (..)
    , csvHeader
    ) where

import qualified Data.Aeson.TH.Extended as A
import           Data.Csv               as Csv
import qualified Data.HashMap.Strict    as HM
import qualified Data.Text              as T
import           Zureg.Model.Csv        ()

data Project = Project
    { pName             :: !(Maybe T.Text)
    , pWebsite          :: !(Maybe T.Text)
    , pShortDescription :: !(Maybe T.Text)
    } deriving (Eq, Show)

data RegisterInfo = RegisterInfo
    { riProject       :: !Project
    } deriving (Eq, Show)

$(A.deriveJSON A.options ''Project)
$(A.deriveJSON A.options ''RegisterInfo)

instance Csv.ToNamedRecord Project where
    toNamedRecord Project {..}
        = HM.unions [ namedRecord [ "Project Name"              .= pName
                                  , "Project Website"           .= pWebsite
                                  , "Project Short Description" .= pShortDescription
                                  ]
                    ]

instance Csv.ToNamedRecord RegisterInfo where
  toNamedRecord RegisterInfo {..}
          = HM.unions [
                      toNamedRecord riProject
                     ]

csvHeader :: Csv.Header
csvHeader = Csv.header
    [ "UUID"
    , "State"
    , "Scanned"
    , "Name"
    , "Name on Badge"
    , "Email"
    , "Affiliation"
    , "AskMeAbout"
    , "Project Name"
    , "Project Website"
    , "Project Short Description"
    , "Registered At"
    ]
