module Kiwi.Locales (locales) where

-- import           Data.Time

import Kiwi.Types


locales :: UI_Lang -> Locales
locales lg = case lg of
  UI_French -> loc_french
  _          -> loc_english

loc_english, loc_french :: Locales

loc_english = Locales
  { loc_Tags = "Tags"
  , loc_TagsList = "tags"
  , loc_FiltersList = "Filters"
  , loc_PagesList = "Pages"
  , loc_RecentPages = "Recently modified pages"
  , loc_Search = "Search"
  , loc_SearchResults = "results"
  , loc_SearchNothing = "No page match"
  , loc_SearchQuery = "Query"
  , loc_SearchMore = "Other pages match too: refine the search to find them."
  , loc_Colon = ":"
  , loc_NotFound = "This page does not exists"
  , loc_BadRequest = "Invalid request"
  , loc_Forbidden = "Forbidden access"
  , loc_ServerError = "Server error"
  , loc_RemoveFilter = "Remove this filter"
  , loc_PagesWithField = "The pages with this metadata"
  , loc_LoginFailed = "Wrong username or password"
  , loc_LangCode = "en"
  , loc_Contents = "Contents"
  , loc_Login = "Log in"
  , loc_Logout = "Log out"
  , loc_Username = "Username"
  , loc_Password = "Password"
  , loc_EditPage = "Edit this page"
  , loc_EditLocally = "Open this page in the editor"
  , loc_EditBlock = "Edit from here"
  , loc_Editing = "Editing"
  , loc_Save = "Save"
  , loc_Cancel = "Cancel"
  , loc_SaveConflict = "The page has changed since it was opened: keep a copy of your text, and open the editor again."
  , loc_SaveInvalid = "The page can not be saved:"
  , loc_SaveIgnored = "You can not change these metadata, they are left as they were:"
  , loc_EditorButtons =
      [ ("bold", "Bold"), ("italic", "Italic"), ("strikethrough", "Strikethrough")
      , ("heading", "Heading"), ("quote", "Quote"), ("code", "Code")
      , ("unordered-list", "List"), ("ordered-list", "Numbered list")
      , ("link", "Link"), ("image", "Image"), ("table", "Table")
      , ("horizontal-rule", "Horizontal line"), ("preview", "Preview")
      , ("side-by-side", "Side by side"), ("fullscreen", "Full screen")
      , ("undo", "Undo"), ("redo", "Redo"), ("guide", "CommonMark syntax") ]
  , loc_CopyLink = "Copy a link to this page"
  , loc_Reload = "Reload pages"
  , loc_ActionSession = "Your session has expired: log in again."
  , loc_ActionForbidden = "You are not allowed to do this."
  , loc_ActionFailed = "The action failed."
  , loc_Metadata = "Metadata"
  , loc_Lang = "Language"
  , loc_Access = "Access"
  , loc_Copy = "Copy"
  , loc_Archives = "Archives"
  , loc_Doctor = "Doctor"
  , loc_BrokenLinks = "Broken links"
  , loc_NoBrokenLink = "No broken link"
  , loc_LoadErrors = "Load errors"
  , loc_Backlinks = "Backlinks"
  -- , loc_TimeLocale = enTimeLocale
  }

loc_french = Locales
  { loc_Tags = "Tags"
  , loc_TagsList = "tags"
  , loc_FiltersList = "Filtres"
  , loc_PagesList = "pages"
  , loc_RecentPages = "Pages modifiées dernièrement"
  , loc_Search = "Recherche"
  , loc_SearchResults = "résultats"
  , loc_SearchNothing = "Aucun résultat"
  , loc_SearchQuery = "Requête"
  , loc_SearchMore = "D'autres pages correspondent aussi\x00A0: précisez la recherche pour les trouver."
  , loc_Colon = "\x00A0:"
  , loc_NotFound = "Cette page n'existe pas"
  , loc_BadRequest = "Requête invalide"
  , loc_Forbidden = "Accès interdit"
  , loc_ServerError = "Erreur du serveur"
  , loc_RemoveFilter = "Retirer ce filtre"
  , loc_PagesWithField = "Les pages avec cette métadonnée"
  , loc_LoginFailed = "Identifiant ou mot de passe incorrect"
  , loc_LangCode = "fr"
  , loc_Contents = "Sommaire"
  , loc_Login = "Connexion"
  , loc_Logout = "Déconnexion"
  , loc_Username = "Identifiant"
  , loc_Password = "Mot de passe"
  , loc_EditPage = "Modifier cette page"
  , loc_EditLocally = "Ouvrir cette page dans l'éditeur"
  , loc_EditBlock = "Modifier à partir d'ici"
  , loc_Editing = "Modification"
  , loc_Save = "Enregistrer"
  , loc_Cancel = "Annuler"
  , loc_SaveConflict = "La page a changé depuis son ouverture\x00A0: gardez une copie de votre texte, et ouvrez l'éditeur à nouveau."
  , loc_SaveInvalid = "La page ne peut pas être enregistrée\x00A0:"
  , loc_SaveIgnored = "Vous ne pouvez pas changer ces métadonnées, elles restent inchangées\x00A0:"
  , loc_EditorButtons =
      [ ("bold", "Gras"), ("italic", "Italique"), ("strikethrough", "Barré")
      , ("heading", "Titre"), ("quote", "Citation"), ("code", "Code")
      , ("unordered-list", "Liste"), ("ordered-list", "Liste numérotée")
      , ("link", "Lien"), ("image", "Image"), ("table", "Tableau")
      , ("horizontal-rule", "Ligne horizontale"), ("preview", "Aperçu")
      , ("side-by-side", "Côte à côte"), ("fullscreen", "Plein écran")
      , ("undo", "Annuler"), ("redo", "Rétablir"), ("guide", "Syntaxe CommonMark") ]
  , loc_CopyLink = "Copier un lien vers cette page"
  , loc_Reload = "Recharger les pages"
  , loc_ActionSession = "Votre session a expiré\x00A0: reconnectez-vous."
  , loc_ActionForbidden = "Vous n'avez pas le droit de faire cette action."
  , loc_ActionFailed = "L'action a échoué."
  , loc_Metadata = "Métadonnées"
  , loc_Lang = "Langue"
  , loc_Access = "Accès"
  , loc_Copy = "Copier"
  , loc_Archives = "Archives"
  , loc_Doctor = "Doctor"
  , loc_BrokenLinks = "Liens cassés"
  , loc_NoBrokenLink = "Aucun lien cassé"
  , loc_LoadErrors = "Erreurs de chargement"
  , loc_Backlinks = "Rétroliens"
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
