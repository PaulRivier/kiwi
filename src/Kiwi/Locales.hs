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
  , loc_UidPage = "Page"
  , loc_UidFile = "File"
  , loc_UidLinking = "Pages linking this uid"
  , loc_UidNothing = "No page for this uid"
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
  , loc_CommitMessage = "Message of the change (optional)"
  , loc_NewPage = "New page"
  , loc_NewPageIn = "New page in"
  , loc_NewPageSource = "Where to create the page?"
  , loc_PagePath = "Path of the file in the source, without extension (optional: else named after the title)"
  , loc_DeletePage = "Delete the page…"
  , loc_Deleting = "Deleting"
  , loc_DeleteHelp = "The file of the page will be deleted. It stays in the history of its git repository, from where it can be restored, with its uid (see the README)."
  , loc_DeleteBacklinks = "These pages link to it, their links will be broken:"
  , loc_DeleteFiles = "Only this page links these files: if they are kept, they will no longer be served."
  , loc_DeleteFilesToo = "Delete these files too"
  , loc_Delete = "Delete"
  , loc_DeleteImpossible = "The page can not be deleted:"
  , loc_FilesNotDeletable = "These files can not be deleted:"
  , loc_NotCommitted = "the pages are not committed (git-commit: no in kiwi.ini)"
  , loc_NotInGit = "not in a git repository:"
  , loc_IgnoredByGit = "ignored by git:"
  , loc_GitFailed = "git failed:"
  , loc_DeleteConflict = "The page or its files have changed since this page was opened: check again what will be deleted."
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
  , loc_UnlinkedFiles = "Files linked by no page"
  , loc_CopyFileLink = "Copy a link to this file"
  , loc_Backlink = "This page links here"
  , loc_RelatedPages = "Related pages"
  , loc_Keywords = "Keywords"
  , loc_OpenMetadataEditor = "Open the metadata editor"
  , loc_MetadataEditor = "Metadata editor"
  , loc_MetaScriptHelp = "Changes of the headers of many pages: all of them or none, in one commit. Check shows the pages that would change, without writing anything. The syntax is below."
  , loc_Check = "Check"
  , loc_Apply = "Apply"
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
  , loc_UidPage = "Page"
  , loc_UidFile = "Fichier"
  , loc_UidLinking = "Pages qui lient cet uid"
  , loc_UidNothing = "Aucune page pour cet uid"
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
  , loc_CommitMessage = "Message de la modification (facultatif)"
  , loc_NewPage = "Nouvelle page"
  , loc_NewPageIn = "Nouvelle page dans"
  , loc_NewPageSource = "Où créer la page\x00A0?"
  , loc_PagePath = "Chemin du fichier dans la source, sans extension (facultatif\x00A0: sinon d'après le titre)"
  , loc_DeletePage = "Supprimer la page…"
  , loc_Deleting = "Suppression"
  , loc_DeleteHelp = "Le fichier de la page sera supprimé. Il reste dans l'historique de son dépôt git, d'où il peut être restauré, avec son uid (voir le README)."
  , loc_DeleteBacklinks = "Ces pages la citent, leurs liens seront cassés\x00A0:"
  , loc_DeleteFiles = "Seule cette page cite ces fichiers\x00A0: s'ils sont gardés, ils ne seront plus servis."
  , loc_DeleteFilesToo = "Supprimer aussi ces fichiers"
  , loc_Delete = "Supprimer"
  , loc_DeleteImpossible = "La page ne peut pas être supprimée\x00A0:"
  , loc_FilesNotDeletable = "Ces fichiers ne peuvent pas être supprimés\x00A0:"
  , loc_NotCommitted = "les pages ne sont pas commitées (git-commit: no dans kiwi.ini)"
  , loc_NotInGit = "hors d'un dépôt git\x00A0:"
  , loc_IgnoredByGit = "ignoré par git\x00A0:"
  , loc_GitFailed = "échec de git\x00A0:"
  , loc_DeleteConflict = "La page ou ses fichiers ont changé depuis l'ouverture de cette page\x00A0: vérifiez à nouveau ce qui sera supprimé."
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
  , loc_UnlinkedFiles = "Fichiers liés par aucune page"
  , loc_CopyFileLink = "Copier un lien vers ce fichier"
  , loc_Backlink = "Cette page renvoie ici"
  , loc_RelatedPages = "Pages connexes"
  , loc_Keywords = "Mots-clés"
  , loc_OpenMetadataEditor = "Ouvrir l'éditeur de métadonnées"
  , loc_MetadataEditor = "Éditeur de métadonnées"
  , loc_MetaScriptHelp = "Modifications des en-têtes de nombreuses pages : toutes ou aucune, en un seul commit. « Tester » montre les pages qui changeraient, sans rien écrire. La syntaxe (en anglais) est en dessous."
  , loc_Check = "Tester"
  , loc_Apply = "Appliquer"
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
