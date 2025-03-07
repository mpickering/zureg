module Zureg.Hackathon.Interface
    ( Hackathon (..)
    ) where

import qualified Data.Csv         as Csv
import qualified Data.Text        as T
import qualified Text.Blaze.Html5 as H
import qualified Text.Digestive   as D
import qualified Zureg.Database   as Database
import qualified Zureg.ReCaptcha  as ReCaptcha
import qualified Zureg.SendEmail  as SendEmail

data Hackathon a = Hackathon
    {
    -- | Name of the Hackathon, e.g. "ZuriHac 2020"
      name            :: T.Text
    -- | Base URL, e.g. "https://zureg.zfoh.ch"
    , baseUrl         :: T.Text
    -- | URL of the contact homepage, e.g. "https://zfoh.ch/zurihac2019/#contact"
    , contactUrl      :: T.Text
    -- | Slack URL, e.g. "https://slack.zurihac.info/"
    , slackUrl        :: T.Text
    -- | When 'True', new registrants are added to the waitlist
    , waitlist        :: Bool

    -- | Registration form
    , registerForm    :: D.Form H.Html IO a
    -- | Registration view
    , registerView    :: D.View H.Html -> H.Html
    -- | Ticket view
    , ticketView      :: a -> H.Html
    -- | Scan view
    , scanView        :: a -> H.Html
    -- | CSV header
    , csvHeader       :: Csv.Header

    -- | Database configuration
    , databaseConfig  :: Database.Config
    -- | Email sending configuration
    , sendEmailConfig :: SendEmail.Config
    -- | ReCaptcha configuration
    , reCaptchaConfig :: ReCaptcha.Config
    -- | Secret for the scanner
    , scannerSecret   :: T.Text
    }
