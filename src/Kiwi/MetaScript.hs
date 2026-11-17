-- | The language of the changes of metadata of the API (POST
-- /api/metadata/SOURCE): one command by line, on all the pages of a source
-- or on one page.
--
-- > # comment
-- > meta tags move "cuisine > matériel" "matériel > cuisine"
-- > meta tags delete obsolete
-- > meta ingrédients move bananes banane
-- > meta ingrédients delete sel
-- > meta auteur rename-to author
-- > page 0123456789 meta summary set "First aid: wounds, burns"
-- > page 0123456789 meta tags add santé
-- > page 0123456789 meta tags remove todo
-- > page 0123456789 meta lang unset
--
-- An argument with spaces is between double quotes (\" and \\ inside). A
-- value is written as in a header ("a > b, c" for tags). The commands apply
-- to the header of the files, in their order (see 'applyCommand').
module Kiwi.MetaScript
  ( Scope (..)
  , Op (..)
  , Command (..)
  , parseScript
  , applyCommand
  , commandText
  ) where

import           Data.Char (isSpace)
import           Data.List (nubBy)
import           Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T
import           Data.Void (Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char (char, hspace, hspace1, string)

import           Kiwi.Meta (clearMeta, emptyMeta, invalidMetaValues, keepOnlyMeta, kiwiKeys,
                            mergeMeta, parseHeader, writeHeader)
import           Kiwi.Tags (segmentsToTag, splitMeta, splitTags, tagToSegments)
import           Kiwi.Types
import           Kiwi.Utils (readCustomValue)


-- | The pages of a command: every page of the source, or one page by its uid
data Scope = AllPages | OnePage T.Text
  deriving (Eq, Show)

-- | What a command does to a field
data Op = Move T.Text T.Text  -- ^ a value becomes another one (all the pages)
        | Delete T.Text       -- ^ a value is removed (all the pages)
        | RenameTo T.Text     -- ^ the key becomes another one (all the pages)
        | Set T.Text          -- ^ the field gets this value (one page)
        | Add T.Text          -- ^ values added to a list (one page)
        | Remove T.Text       -- ^ values removed from a list (one page)
        | Unset               -- ^ the field is removed (one page)
  deriving (Eq, Show)

-- | A command, with its line in the script
data Command = Command { commandLine :: Int
                       , commandScope :: Scope
                       , commandField :: T.Text
                       , commandOp :: Op }
  deriving (Eq, Show)


type Parser = Parsec Void T.Text

-- | The commands of a script, or the errors of its lines (with their line
-- and column, and what was expected)
parseScript :: T.Text -> Either String [Command]
parseScript script =
  case partitionEithers' [ parseLine n l | (n, l) <- zip [1 ..] (T.lines script)
                                         , let s = T.strip l
                                         , not (T.null s), not ("#" `T.isPrefixOf` s) ] of
    ([], cmds) -> Right cmds
    (errs, _)  -> Left $ concat errs
  where
    partitionEithers' es = ([ e | Left e <- es ], [ c | Right c <- es ])

parseLine :: Int -> T.Text -> Either String Command
parseLine n l = case runParser (hspace *> commandP <* eof) ("line " ++ show n) l of
  Left e  -> Left (errorBundlePretty e)
  Right c -> Right c
  where
    commandP = do
      scope <- option AllPages (OnePage <$> (keyword "page" *> argument "uid"))
      _ <- keyword "meta"
      field <- argument "field"
      op <- case scope of
        AllPages -> choice
          [ Move <$> (keyword "move" *> argument "old value") <*> argument "new value"
          , Delete <$> (keyword "delete" *> argument "value")
          , RenameTo <$> (keyword "rename-to" *> argument "new field") ]
        OnePage _ -> choice
          [ Set <$> (keyword "set" *> argument "value")
          , Add <$> (keyword "add" *> argument "value")
          , Remove <$> (keyword "remove" *> argument "value")
          , Unset <$ keyword "unset" ]
      return $ Command n scope field op

-- | A word of the language, before a space or the end of the line
keyword :: T.Text -> Parser T.Text
keyword w = try (string w <* notFollowedBy (satisfy (not . isSpace))) <* hspace

-- | An argument: a word, or a text between double quotes
argument :: String -> Parser T.Text
argument what = (quoted <|> word <?> what) <* (hspace1 <|> eof)
  where
    word = T.pack <$> some (satisfy (\c -> not (isSpace c) && c /= '"'))
    quoted = do
      _ <- char '"'
      cs <- many (escaped <|> satisfy (`notElem` ['"', '\\']))
      _ <- char '"' <?> "closing quote"
      return (T.pack cs)
    escaped = char '\\' *> (char '"' <|> char '\\')

-- | A command as it would be written, for the messages
commandText :: Command -> T.Text
commandText c = T.unwords $ scope ++ ["meta", quote (commandField c)] ++ op
  where
    scope = case commandScope c of
      AllPages  -> []
      OnePage u -> ["page", u]
    op = case commandOp c of
      Move a b   -> ["move", quote a, quote b]
      Delete a   -> ["delete", quote a]
      RenameTo a -> ["rename-to", quote a]
      Set a      -> ["set", quote a]
      Add a      -> ["add", quote a]
      Remove a   -> ["remove", quote a]
      Unset      -> ["unset"]
    quote t | T.any (\ch -> isSpace ch || ch == '"') t || T.null t =
                "\"" <> T.concatMap esc t <> "\""
            | otherwise = t
    esc ch | ch `elem` ['"', '\\'] = T.pack ['\\', ch]
           | otherwise = T.singleton ch


-- | Kind of a field, for the commands
data FieldKind = TagsField'   -- ^ tags: a list of tags, compared by segments
               | Single       -- ^ one value (title, summary, lang, archive)
               | List (T.Text -> T.Text -> Bool)  -- ^ a list of values, with their equality

fieldKind :: [CustomMetaConfig] -> T.Text -> FieldKind
fieldKind cmc k
  | k == "tags" = TagsField'
  | k `elem` ["title", "summary", "lang", "archive"] = Single
  | Just c <- lookupConfig = List $ \a b -> case (readCustomValue (cmcType c) a,
                                                  readCustomValue (cmcType c) b) of
      (Just x, Just y) -> x == y
      _ -> a == b
  | otherwise = List (==)
  where lookupConfig = case filter ((== k) . cmcName) cmc of
          c : _ -> Just c
          []    -> Nothing

-- | Applies a command to the header of a page (when it is in the scope of
-- the command): the new header, Nothing when the command does not change
-- it, or why the command can not apply (an invalid value, a field that can
-- not be changed so). "uid" and "previous-uids" can never be changed. A
-- value is read as in a header (see 'parseHeader'), and refused when it can
-- not be read there.
applyCommand :: [CustomMetaConfig] -> Command -> MetaData -> Either String (Maybe MetaData)
applyCommand cmc c header
  | field `elem` kiwiKeys = Left $ field' ++ " can not be changed"
  | otherwise = fmap changed $ case (fieldKind cmc field, commandOp c) of
      (_, Unset) -> Right $ clearMeta [field] header
      (_, Set v) -> setField field v header
      (Single, Add _) -> Left $ field' ++ " has one value: use set"
      (Single, Remove _) -> Left $ field' ++ " has one value: use set or unset"
      (kind, Add v) -> do
        new <- items kind <$> setField field v emptyMeta
        setItems kind (nubBy (equal kind) (current kind ++ new))
      (kind, Remove v) -> do
        old <- items kind <$> setField field v emptyMeta
        setItems kind (filter (\x -> not (any (equal kind x) old)) (current kind))
      (Single, _) -> Left $ field' ++ " can only be changed page by page"
      (TagsField', Move a b) ->
        let (sa, sb) = (tagToSegments a, tagToSegments b)
            moved t = let s = tagToSegments t
                      in if take (length sa) s == sa then segmentsToTag (sb ++ drop (length sa) s) else t
        in setItems TagsField' (nubBy (equal TagsField') $ map moved (current TagsField'))
      (TagsField', Delete a) ->
        let sa = tagToSegments a
        in setItems TagsField' $ filter (\t -> take (length sa) (tagToSegments t) /= sa)
                                        (current TagsField')
      (TagsField', RenameTo _) -> Left "tags can not be renamed"
      (kind@(List eq), Move a b) ->
        setItems kind (nubBy eq [ if eq x a then b else x | x <- current kind ])
      (kind@(List eq), Delete a) -> setItems kind (filter (not . eq a) (current kind))
      (List _, RenameTo new)
        | new `elem` kiwiKeys || new `elem` ["title", "summary", "lang", "archive", "tags", "access"] ->
            Left $ "a field can not be renamed to " ++ T.unpack new
        | otherwise -> case fieldText field header of
            Nothing -> Right header
            Just v | isJust (fieldText new header) ->
                       Left $ "the page already has the field " ++ T.unpack new
                   | otherwise -> setField new v (clearMeta [field] header)
  where
    field = commandField c
    field' = "the field " ++ T.unpack field
    -- compared as written: the order of a list may change, not its header
    changed new = if writeHeader cmc new == writeHeader cmc header then Nothing else Just new
    current kind = maybe [] (itemsOfText kind) (fieldText field header)
    items kind m = maybe [] (itemsOfText kind) (fieldText field m)
    itemsOfText TagsField' = splitTags
    itemsOfText _ = splitMeta
    equal TagsField' a b = tagToSegments a == tagToSegments b
    equal (List eq) a b = eq a b
    equal Single a b = a == b
    -- the field with these values, removed without any
    setItems kind xs
      | null xs = Right $ clearMeta [field] header
      | otherwise = setField field (T.intercalate ", " $ map (escapeItem kind) xs) header
    escapeItem TagsField' t = escape ",{}" (T.intercalate " > " $ map (escape ">") $ tagToSegments t)
    escapeItem _ t = escape "," t

    -- the field with the value, as if it were written in the header
    setField k v m = do
      parsed <- parseHeader cmc (k <> ": " <> v)
      case [ bad | (k', bad) <- invalidMetaValues cmc parsed, k' == k ] of
        bad : _ -> Left $ T.unpack $ T.concat ["invalid value for ", k, ": ", bad]
        [] -> Right $ mergeMeta (clearMeta [k] m) parsed
    -- the value of a field as written in a header, if the header has it
    fieldText k m = case T.lines (writeHeader cmc (keepOnlyMeta [k] m)) of
      [] -> Nothing
      ls -> Just $ T.intercalate ", " [ T.strip $ fromMaybe l $ T.stripPrefix ":" (T.drop (T.length k) l)
                                      | l <- ls ]

-- | Text with these characters escaped by a backslash
escape :: String -> T.Text -> T.Text
escape cs = T.concatMap (\ch -> if ch `elem` cs then T.pack ['\\', ch] else T.singleton ch)
