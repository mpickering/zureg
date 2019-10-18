{-# LANGUAGE OverloadedStrings #-}
module Zureg.Hackathon.Bristol2020
    ( bristol2020
    ) where

import qualified Data.Text                         as T
import           System.Environment                (getEnv)
import qualified Zureg.Database                    as Database
import           Zureg.Hackathon.Interface         (Hackathon)
import qualified Zureg.Hackathon.Interface         as Hackathon
import           Zureg.Hackathon.Bristol2020.Form  as B20
import           Zureg.Hackathon.Bristol2020.Model as B20
import           Zureg.Hackathon.Bristol2020.Views as B20
import qualified Zureg.ReCaptcha                   as ReCaptcha
import qualified Zureg.SendEmail                   as SendEmail

bristol2020 :: IO (Hackathon RegisterInfo)
bristol2020 = do
    scannerSecret   <- T.pack <$> getEnv "ZUREG_SCANNER_SECRET"
    reCaptchaSecret <- T.pack <$> getEnv "ZUREG_RECAPTCHA_SECRET"

    return Hackathon.Hackathon
        { Hackathon.name = "Bristol Haskell 2019"
        , Hackathon.baseUrl = "https://mpickering.github.io"
        , Hackathon.contactUrl = "https://mpickering.github.io"
        , Hackathon.slackUrl = "https://"
        , Hackathon.waitlist = True

        , Hackathon.registerForm = B20.additionalInfoForm
        , Hackathon.registerView = B20.additionalInfoView
        , Hackathon.ticketView = B20.ticketView
        , Hackathon.scanView = B20.scanView
        , Hackathon.csvHeader = B20.csvHeader

        , Hackathon.databaseConfig = Database.defaultConfig
        , Hackathon.sendEmailConfig = SendEmail.Config
            { SendEmail.cFrom = "Bristol Registration Bot <noreply@bristol.ac.uk>"
            }
        , Hackathon.reCaptchaConfig = ReCaptcha.Config
            { ReCaptcha.cEnabled = True
            , ReCaptcha.cSiteKey = "6LfAV74UAAAAABMxQvT3yAv4MXaF7S4bZn6CTFCe"
            , ReCaptcha.cSecretKey = reCaptchaSecret
            }
        , Hackathon.scannerSecret = scannerSecret
        }
