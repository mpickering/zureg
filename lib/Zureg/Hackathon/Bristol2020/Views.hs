{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Zureg.Hackathon.Bristol2020.Views
    ( ticketView
    , scanView
    ) where

import           Data.List                         (intercalate)
import qualified Text.Blaze.Html5                  as H

import           Zureg.Hackathon.Bristol2020.Model as B20

ticketView :: B20.RegisterInfo -> H.Html
ticketView RegisterInfo {..} = do
    mempty

scanView :: B20.RegisterInfo -> H.Html
scanView RegisterInfo {..} = mempty
