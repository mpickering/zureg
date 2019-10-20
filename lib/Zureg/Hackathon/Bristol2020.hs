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
        { Hackathon.name = "Bristol Haskell 2020"
        , Hackathon.baseUrl = "https://gds2eyz1uh.execute-api.us-east-1.amazonaws.com/beta"
        , Hackathon.contactUrl = "https://mpickering.github.io/bristol2020"
        , Hackathon.slackUrl = "https://"
        , Hackathon.waitlist = False

        , Hackathon.registerForm = B20.additionalInfoForm
        , Hackathon.registerView = B20.additionalInfoView
        , Hackathon.ticketView = B20.ticketView
        , Hackathon.scanView = B20.scanView
        , Hackathon.csvHeader = B20.csvHeader

        , Hackathon.databaseConfig = Database.defaultConfig
        , Hackathon.sendEmailConfig = SendEmail.Config
            { SendEmail.cFrom = "Bristol Registration Bot <bristol.haskell.2020@gmail.com>"
            }
        , Hackathon.reCaptchaConfig = ReCaptcha.Config
            { ReCaptcha.cEnabled = True
            , ReCaptcha.cSiteKey = "6Lf7d74UAAAAAPcMs1ZDnzqwrObnyIPM_puYCfpJ"
            , ReCaptcha.cSecretKey = reCaptchaSecret
            }
        , Hackathon.scannerSecret = scannerSecret
        }
