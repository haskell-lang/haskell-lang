{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-missing-signatures #-}

-- | CSS theme.

module HL.View.Theme
  (theme)
  where

import Language.CSS

theme =
  do main
     containers
     breadcrumb
     navbar
     footer
     codes

main =
  do rule "html"
          (do position "relative"
              minHeight "100%")
     rule "body"
           (do background "#ffffff"
               padding "0"
               margin "0 0 4em 0"
               fontFamily "Open Sans"
               fontSize "13px")
     rule ".wrap"
          (do background "#ffffff"
              paddingBottom "2em")
     rule "p,ul,li"
          (do fontSize "15px")
     rule "h1"
          (do marginTop "0.1em"
              marginLeft "0"
              textIndent "-0.05em")
     rule "h2"
          (do color "#6e618d")

containers =
  rule ".container > .row"
       (do marginLeft "0"
           marginRight "0"
           maxWidth "60em")

navbar =
  do rule ".navbar"
          (do backgroundColor "#352f44"
              borderRadius "0"
              border "0"
              marginBottom "0.5em")
     rule ".navbar-header .navbar-brand"
          (do color "#fff"
              fontSize "inherit"
              fontWeight "bold"
              rule ".logo"
                   (do marginRight "0.5em"
                       fontFamily "haskell"
                       fontWeight "normal"))
     rule ".navbar-header .navbar-brand:hover"
          (color "#fff")
     rule ".navbar-default .navbar-nav > .active > a"
          (do theme
              backgroundColor "#312b3f"
              borderBottom "0.3em solid #465787")
     rule ".navbar-default .navbar-nav > .active > a:hover"
          (do theme
              backgroundColor "#312b3f")
     rule ".navbar-default .navbar-nav > li > a"
          theme
  where theme =
          do color "#ffffff !important"
             backgroundColor "inherit"

breadcrumb =
  rule ".breadcrumb"
       (do marginLeft "0"
           paddingLeft "0"
           backgroundColor "inherit"
           marginBottom "0")

footer =
  rule ".footer"
       (do backgroundColor "#323232"
           color "#999999"
           position "absolute"
           bottom "0"
           width "100%"
           height "4em"
           lineHeight "2em"
           rule "p"
                (do marginTop "1em"
                    fontSize "13px"))

codes =
  rule "code"
       (do backgroundColor "#f5f5f5"
           color "#366354")
