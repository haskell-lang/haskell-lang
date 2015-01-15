{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Community page view.

module HL.View.Donate where

import HL.View
import HL.View.Template

import Lucid.Base

-- | Donation view.
donateV :: FromLucid App
donateV =
  templateWithBodyEnder
    []
    "Donate to Haskell.org"
    (\_ ->
       do container_
            (row_
               (span12_
                  (do h1_ "Make a donation to Haskell.org"
                      p_
                        "Using the form below, you can make a tax-deductible \
                        \donation to Haskell.org."
                      p_ (do "You can donate with any credit card, or using "
                             a_ [href_ "https://bitcoin.org/en/"] "Bitcoin"
                             ". You'll also need a valid email for a donation, but "
                             "only for a receipt (if you want to stay anonymous, "
                             "try a temporary random email from "
                             a_ [href_ "http://maildrop.cc"] "maildrop.cc"
                             ").")
                      p_ (do "Your donations are processed by "
                             a_ [href_ "https://stripe.com"] "Stripe"
                             ", and granted to Haskell.org on behalf of "
                             a_ [href_ "http://www.spi-inc.org/"] "Software in the Public Interest"
                             ", a non-profit §501(c)3 registered in the state of New York.")
                      p_ (do "Alternatively, you can donate to Haskell.org/SPI via "
                             a_ [href_ "https://co.clickandpledge.com/advanced/default.aspx?wid=69561"] "Click & Pledge"
                             ", a free online fundraising platform.")
                      p_ donateForm
                      p_ statusWindow))))
    (\_ url -> do script_ [src_ "https://checkout.stripe.com/checkout.js"] ""
                  script_ [src_ "https://donate.haskell.org/pubkey.js"] ""
                  scripts url [js_donate_js])

donateForm :: Html ()
donateForm =
  form_ [class_ "form-inline"] (do
    input_ [id_ "monies", type_ "number", class_ "input-large", placeholder_ "Amount in USD"]
    " "
    button_ [id_ "paybtn", class_ "btn btn-info", type_ "button"] "Donate")

statusWindow :: Html ()
statusWindow =
  div_ [ id_ "payment_status", class_ "alert alert-block fade", role_ "alert" ] (do
    button_ [ id_ "pay_status_close_btn", href_ "#", type_ "button"
            , class_ "close" ] "x"
    div_ [] (do
      span_ [ id_ "status_glyph", class_ "glyphicon glyphicon-exclamation-sign"
            , aria_hidden_ "true" ] ""
      span_ [ id_ "status_title", class_ "sr-only" ] "Error:"
      span_ [ id_ "status_message" ] "  Enter an amount above."))

role_ :: Text -> Attribute
role_ = makeAttribute "role"

aria_hidden_ :: Text -> Attribute
aria_hidden_ = makeAttribute "role"
