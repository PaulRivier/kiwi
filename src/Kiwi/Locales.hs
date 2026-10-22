module Kiwi.Locales (locales) where

-- import           Data.Time

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
  , loc_RecentPages = "Recently modified pages"
  , loc_Search = "Search"
  , loc_SearchResults = "Results"
  , loc_SearchNothing = "No page match"
  , loc_Agenda = "Agenda"
  , loc_Today = "Today"
  , loc_ToCome = "To come"
  , loc_NotFound = "This page does not exists"
  , loc_BadRequest = "Invalid request"
  , loc_Forbidden = "Forbidden access"
  , loc_RemoveFilter = "Remove this filter"
  , loc_LoginFailed = "Wrong username or password"
  , loc_LangCode = "en"
  , loc_Contents = "Contents"
  , loc_Login = "Log in"
  , loc_Logout = "Log out"
  , loc_Username = "Username"
  , loc_Password = "Password"
  , loc_EditPage = "Edit this page"
  , loc_CopyLink = "Copy a link to this page"
  , loc_Reload = "Reload pages"
  -- , loc_TimeLocale = enTimeLocale
  }

loc_french = Locales
  { loc_Home = "Home"
  , loc_Tags = "Tags"
  , loc_TagsList = "tags"
  , loc_PagesList = "pages"
  , loc_RecentPages = "Pages modifiées dernièrement"
  , loc_Search = "Recherche"
  , loc_SearchResults = "Résultats"
  , loc_SearchNothing = "Aucun résultat"
  , loc_Agenda = "Agenda"
  , loc_Today = "Aujourd'hui"
  , loc_ToCome = "À venir"
  , loc_NotFound = "Cette page n'existe pas"
  , loc_BadRequest = "Requête invalide"
  , loc_Forbidden = "Accès interdit"
  , loc_RemoveFilter = "Retirer ce filtre"
  , loc_LoginFailed = "Identifiant ou mot de passe incorrect"
  , loc_LangCode = "fr"
  , loc_Contents = "Sommaire"
  , loc_Login = "Connexion"
  , loc_Logout = "Déconnexion"
  , loc_Username = "Identifiant"
  , loc_Password = "Mot de passe"
  , loc_EditPage = "Modifier cette page"
  , loc_CopyLink = "Copier un lien vers cette page"
  , loc_Reload = "Recharger les pages"
  -- , loc_TimeLocale = frTimeLocale
  }




-- enTimeLocale :: TimeLocale
-- enTimeLocale =
--     TimeLocale
--         { wDays =
--               [ ("Sunday", "Sun")
--               , ("Monday", "Mon")
--               , ("Tuesday", "Tue")
--               , ("Wednesday", "Wed")
--               , ("Thursday", "Thu")
--               , ("Friday", "Fri")
--               , ("Saturday", "Sat")
--               ]
--         , months =
--               [ ("January", "Jan")
--               , ("February", "Feb")
--               , ("March", "Mar")
--               , ("April", "Apr")
--               , ("May", "May")
--               , ("June", "Jun")
--               , ("July", "Jul")
--               , ("August", "Aug")
--               , ("September", "Sep")
--               , ("October", "Oct")
--               , ("November", "Nov")
--               , ("December", "Dec")
--               ]
--         , amPm = ("AM", "PM")
--         , dateTimeFmt = "%a %b %e %H:%M:%S %Z %Y"
--         , dateFmt = "%m/%d/%y"
--         , timeFmt = "%H:%M:%S"
--         , time12Fmt = "%I:%M:%S %p"
--         , knownTimeZones =
--               [ TimeZone 0 False "UT"
--               , TimeZone 0 False "GMT"
--               , TimeZone (-5 * 60) False "EST"
--               , TimeZone (-4 * 60) True "EDT"
--               , TimeZone (-6 * 60) False "CST"
--               , TimeZone (-5 * 60) True "CDT"
--               , TimeZone (-7 * 60) False "MST"
--               , TimeZone (-6 * 60) True "MDT"
--               , TimeZone (-8 * 60) False "PST"
--               , TimeZone (-7 * 60) True "PDT"
--               ]
--         }

-- frTimeLocale :: TimeLocale
-- frTimeLocale =
--     TimeLocale
--         { wDays =
--               [ ("Dimanche", "Dim")
--               , ("Lundi", "Lun")
--               , ("Mardi", "Mar")
--               , ("Mercredi", "Mer")
--               , ("Jeudi", "Jeu")
--               , ("Vendredi", "Ven")
--               , ("Samedi", "Sam")
--               ]
--         , months =
--               [ ("Janvier", "Jan")
--               , ("Février", "Fév")
--               , ("Mars", "Mar")
--               , ("Avril", "Avr")
--               , ("Mai", "Mai")
--               , ("Juin", "Juin")
--               , ("Juillet", "Juil")
--               , ("Août", "Août")
--               , ("Septembre", "Sep")
--               , ("Octobre", "Oct")
--               , ("Novembre", "Nov")
--               , ("Décembre", "Déc")
--               ]
--         , amPm = ("AM", "PM")
--         , dateTimeFmt = "%A %e %B %H:%M:%S %Z %Y"
--         , dateFmt = "%d-%m-%Y"
--         , timeFmt = "%H:%M:%S"
--         , time12Fmt = "%I:%M:%S %p"
--         , knownTimeZones =
--               [ TimeZone 0 False "UT"
--               , TimeZone 0 False "GMT"
--               , TimeZone (-5 * 60) False "EST"
--               , TimeZone (-4 * 60) True "EDT"
--               , TimeZone (-6 * 60) False "CST"
--               , TimeZone (-5 * 60) True "CDT"
--               , TimeZone (-7 * 60) False "MST"
--               , TimeZone (-6 * 60) True "MDT"
--               , TimeZone (-8 * 60) False "PST"
--               , TimeZone (-7 * 60) True "PDT"
--               ]
--         }
