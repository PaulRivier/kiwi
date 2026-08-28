{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Kiwi.HtmlWriter (writeHtml5) where

import qualified Data.Text as T
import Text.Pandoc.Definition as PD


-- Step 2: modify the AST

-- Text.Pandoc.Walk (part of pandoc-types, not pandoc) lets you transform
-- every occurrence of a given node type anywhere in the tree.
-- upcaseStrs :: Pandoc -> Pandoc
-- upcaseStrs = walk go
--   where
--     go :: Inline -> Inline
--     go (Str s) = Str (T.toUpper s)
--     go x = x



writeHtml5 :: PD.Pandoc -> T.Text
writeHtml5 (PD.Pandoc _meta blocks) = blocksToHtml blocks

blocksToHtml :: [PD.Block] -> T.Text
blocksToHtml = T.intercalate "\n" . map blockToHtml

blockToHtml :: Block -> T.Text
blockToHtml blk = case blk of
  PD.Plain ils -> inlinesToHtml ils
  PD.Para (img@(PD.Image _ _ _):[]) -> "<figure>" <> inlineToHtml img <> "</figure>"
  PD.Para ils -> "<p>" <> inlinesToHtml ils <> "</p>"
  PD.LineBlock lns ->
    "<div class=\"line-block\">"
      <> T.intercalate "<br/>\n" (map inlinesToHtml lns)
      <> "</div>"
  PD.CodeBlock attr code ->
    "<pre><code" <> codeBlockClass attr <> ">" <> escapeHtml False code <> "</code></pre>"
  PD.RawBlock (PD.Format fmt) raw
    | fmt `elem` ["html", "html5"] -> raw
    | otherwise -> ""
  PD.BlockQuote bs -> "<blockquote>\n" <> blocksToHtml bs <> "\n</blockquote>"
  PD.OrderedList (start, _, _) items ->
    "<ol"
      <> (if start /= 1 then " start=\"" <> T.pack (show start) <> "\"" else "")
      <> ">\n"
      <> T.concat (map itemToHtml items)
      <> "</ol>"
  PD.BulletList items -> "<ul>\n" <> T.concat (map itemToHtml items) <> "</ul>"
  PD.DefinitionList _ -> "<!-- definition lists not supported by this minimal writer -->"
  PD.Header lvl attr ils ->
    let tag = "h" <> T.pack (show lvl)
     in "<" <> tag <> attrsToHtml attr <> ">" <> inlinesToHtml ils <> "</" <> tag <> ">"
  PD.HorizontalRule -> "<hr/>"
  PD.Table {} -> "<!-- tables not supported by this minimal writer -->"
  PD.Figure {} -> "<!-- figures not supported by this minimal writer -->"
  PD.Div attr bs -> "<div" <> attrsToHtml attr <> ">\n" <> blocksToHtml bs <> "\n</div>"
  -- PD.Null -> ""
  where
    itemToHtml bs = "<li>" <> blocksToHtml bs <> "</li>\n"
    codeBlockClass (_, classes, _) = case classes of
      (lang : _) -> " class=\"language-" <> lang <> "\""
      _ -> ""

inlinesToHtml :: [PD.Inline] -> T.Text
inlinesToHtml = T.concat . map inlineToHtml

inlineToHtml :: PD.Inline -> T.Text
inlineToHtml il = case il of
  PD.Str s -> escapeHtml False s
  PD.Emph ils -> "<em>" <> inlinesToHtml ils <> "</em>"
  PD.Strong ils -> "<strong>" <> inlinesToHtml ils <> "</strong>"
  PD.Underline ils -> "<u>" <> inlinesToHtml ils <> "</u>"
  PD.Strikeout ils -> "<del>" <> inlinesToHtml ils <> "</del>"
  PD.Superscript ils -> "<sup>" <> inlinesToHtml ils <> "</sup>"
  PD.Subscript ils -> "<sub>" <> inlinesToHtml ils <> "</sub>"
  PD.SmallCaps ils -> "<span style=\"font-variant:small-caps;\">" <> inlinesToHtml ils <> "</span>"
  PD.Quoted PD.SingleQuote ils -> "&lsquo;" <> inlinesToHtml ils <> "&rsquo;"
  PD.Quoted PD.DoubleQuote ils -> "&ldquo;" <> inlinesToHtml ils <> "&rdquo;"
  PD.Cite _ ils -> inlinesToHtml ils
  PD.Code attr code -> "<code" <> attrsToHtml attr <> ">" <> escapeHtml False code <> "</code>"
  PD.Space -> " "
  PD.SoftBreak -> "\n"
  PD.LineBreak -> "<br/>\n"
  PD.Math _ str -> escapeHtml False str -- plug in texmath yourself if you need real math rendering
  PD.RawInline (PD.Format fmt) raw
    | fmt `elem` ["html", "html5"] -> raw
    | otherwise -> ""
  PD.Link attr ils (url, title) ->
    "<a href=\""
      <> escapeHtml True url
      <> "\""
      <> (if T.null title then "" else " title=\"" <> escapeHtml True title <> "\"")
      <> attrsToHtml attr
      <> ">"
      <> inlinesToHtml ils
      <> "</a>"
  PD.Image attr ils (url, title) ->
    "<img src=\""
      <> escapeHtml True url
      <> "\" alt=\""
      <> escapeHtml True (inlinesToPlain ils)
      <> "\""
      <> (if T.null title then "" else " title=\"" <> escapeHtml True title <> "\"")
      <> attrsToHtml attr
      <> " />"
  PD.Note _ -> "" -- footnotes need collecting + numbering; add if you need them
  PD.Span attr ils -> "<span" <> attrsToHtml attr <> ">" <> inlinesToHtml ils <> "</span>"

inlinesToPlain :: [PD.Inline] -> T.Text
inlinesToPlain = T.concat . map go
  where
    go (PD.Str s) = s
    go PD.Space = " "
    go PD.SoftBreak = " "
    go (PD.Emph ils) = inlinesToPlain ils
    go (PD.Strong ils) = inlinesToPlain ils
    go _ = ""

attrsToHtml :: PD.Attr -> T.Text
attrsToHtml (ident, classes, kvs) =
  T.concat $
    ["" | True]
      ++ [" id=\"" <> escapeHtml True ident <> "\"" | not (T.null ident)]
      ++ [" class=\"" <> escapeHtml True (T.unwords classes) <> "\"" | not (null classes)]
      ++ [" " <> k <> "=\"" <> escapeHtml True v <> "\"" | (k, v) <- kvs]

escapeHtml :: Bool -> T.Text -> T.Text
escapeHtml escapeQuotes = T.concatMap esc
  where
    esc '&' = "&amp;"
    esc '<' = "&lt;"
    esc '>' = "&gt;"
    esc '"' | escapeQuotes = "&quot;"
    esc c = T.singleton c

