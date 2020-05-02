{-# LANGUAGE OverloadedStrings #-}

module Kiwi.Locales where

import           Data.Time

import Kiwi.Types


locales :: UI_Lang -> Locales
locales lg = case lg of
  UI_French -> loc_french
  _          -> loc_english

loc_english, loc_french :: Locales

loc_english = Locales
  { loc_Home = "Home"
  , loc_Tags = "Tags"
  , loc_TagsList = "tags"
  , loc_PagesList = "Pages"
  , loc_Search = "Search"
  , loc_SearchResults = "Results"
  , loc_SearchNothing = "No page match"
  , loc_Agenda = "Agenda"
  , loc_Today = "Today"
  , loc_ToCome = "To come"
  , loc_NotFound = "This page does not exists"
  , loc_Forbidden = "Forbidden access"
  , loc_TimeLocale = enTimeLocale
  }

loc_french = Locales
  { loc_Home = "Home"
  , loc_Tags = "Tags"
  , loc_TagsList = "tags"
  , loc_PagesList = "pages"
  , loc_Search = "Recherche"
  , loc_SearchResults = "Résultats"
  , loc_SearchNothing = "Aucun résultat"
  , loc_Agenda = "Agenda"
  , loc_Today = "Aujourd'hui"
  , loc_ToCome = "À venir"
  , loc_NotFound = "Cette page n'existe pas"
  , loc_Forbidden = "Accès interdit"
  , loc_TimeLocale = frTimeLocale
  }


  

enTimeLocale :: TimeLocale
enTimeLocale =
    TimeLocale
        { wDays =
              [ ("Sunday", "Sun")
              , ("Monday", "Mon")
              , ("Tuesday", "Tue")
              , ("Wednesday", "Wed")
              , ("Thursday", "Thu")
              , ("Friday", "Fri")
              , ("Saturday", "Sat")
              ]
        , months =
              [ ("January", "Jan")
              , ("February", "Feb")
              , ("March", "Mar")
              , ("April", "Apr")
              , ("May", "May")
              , ("June", "Jun")
              , ("July", "Jul")
              , ("August", "Aug")
              , ("September", "Sep")
              , ("October", "Oct")
              , ("November", "Nov")
              , ("December", "Dec")
              ]
        , amPm = ("AM", "PM")
        , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
        , dateFmt = "%m/%d/%y"
        , timeFmt = "%H:%M:%S"
        , time12Fmt = "%I:%M:%S %p"
        , knownTimeZones =
              [ TimeZone 0 False "UT"
              , TimeZone 0 False "GMT"
              , TimeZone (-5 * 60) False "EST"
              , TimeZone (-4 * 60) True "EDT"
              , TimeZone (-6 * 60) False "CST"
              , TimeZone (-5 * 60) True "CDT"
              , TimeZone (-7 * 60) False "MST"
              , TimeZone (-6 * 60) True "MDT"
              , TimeZone (-8 * 60) False "PST"
              , TimeZone (-7 * 60) True "PDT"
              ]
        }

frTimeLocale :: TimeLocale
frTimeLocale =
    TimeLocale
        { wDays =
              [ ("Dimanche", "Dim")
              , ("Lundi", "Lun")
              , ("Mardi", "Mar")
              , ("Mercredi", "Mer")
              , ("Jeudi", "Jeu")
              , ("Vendredi", "Ven")
              , ("Samedi", "Sam")
              ]
        , months =
              [ ("Janvier", "Jan")
              , ("Février", "Fév")
              , ("Mars", "Mar")
              , ("Avril", "Avr")
              , ("Mai", "Mai")
              , ("Juin", "Juin")
              , ("Juillet", "Juil")
              , ("Août", "Août")
              , ("Septembre", "Sep")
              , ("Octobre", "Oct")
              , ("Novembre", "Nov")
              , ("Décembre", "Déc")
              ]
        , amPm = ("AM", "PM")
        , dateTimeFmt = "%A %e %B %H:%M:%S %Z %Y"
        , dateFmt = "%d-%m-%Y"
        , timeFmt = "%H:%M:%S"
        , time12Fmt = "%I:%M:%S %p"
        , knownTimeZones =
              [ TimeZone 0 False "UT"
              , TimeZone 0 False "GMT"
              , TimeZone (-5 * 60) False "EST"
              , TimeZone (-4 * 60) True "EDT"
              , TimeZone (-6 * 60) False "CST"
              , TimeZone (-5 * 60) True "CDT"
              , TimeZone (-7 * 60) False "MST"
              , TimeZone (-6 * 60) True "MDT"
              , TimeZone (-8 * 60) False "PST"
              , TimeZone (-7 * 60) True "PDT"
              ]
        }
