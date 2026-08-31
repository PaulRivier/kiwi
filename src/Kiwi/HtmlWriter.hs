{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Kiwi.HtmlWriter (writeHtml5) where

import qualified Data.List as List
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import qualified Text.Pandoc.Definition as PD

import Text.Blaze.Html ((!))
import qualified Text.Blaze.Html.Renderer.Text as Renderer
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A


writeHtml5 :: PD.Pandoc -> T.Text
writeHtml5 = TL.toStrict . Renderer.renderHtml . writeHtml5Blaze


writeHtml5Blaze :: PD.Pandoc -> H.Html
writeHtml5Blaze (PD.Pandoc _meta blocks) = blocksToHtml blocks

-- Apply a Pandoc 'PD.Attr' (id, classes, arbitrary key/value pairs) to an
-- existing Html value. 'H.customAttribute' + 'H.textTag' let us turn
-- a runtime Text key into a blaze attribute name, which is how arbitrary
-- kv-attrs (e.g. from the commonmark 'attributes' extension) get through.
applyAttrs :: PD.Attr -> H.Html -> H.Html
applyAttrs (ident, classes, kvs) html0 = List.foldl' (!) html0 attrs
  where
    attrs =
      [A.id (H.toValue ident) | not (T.null ident)]
        ++ [A.class_ (H.toValue (T.unwords classes)) | not (null classes)]
        ++ [H.customAttribute (H.textTag k) (H.toValue v) | (k, v) <- kvs]

blocksToHtml :: [PD.Block] -> H.Html
blocksToHtml = mconcat . map blockToHtml

blockToHtml :: PD.Block -> H.Html
blockToHtml blk = case blk of
  PD.Plain ils -> inlinesToHtml ils
  PD.Para (img@(PD.Image _ desc _):[]) ->
    H.figure (inlineToHtml img <> H.figcaption (inlinesToHtml desc))
  PD.Para ils -> H.p $ inlinesToHtml ils
  PD.LineBlock lns ->
    H.div ! A.class_ "line-block" $
      mconcat (List.intersperse H.br (map inlinesToHtml lns))
  PD.CodeBlock attr code -> H.pre (applyAttrs attr (H.code (H.toHtml code)))
  PD.RawBlock (PD.Format fmt) raw
    | T.toLower fmt `elem` ["html", "html5"] -> H.preEscapedToHtml raw
    | otherwise -> mempty
  PD.BlockQuote bs -> H.blockquote (blocksToHtml bs)
  PD.OrderedList (start, _, _) items ->
    let ol0 = H.ol (mconcat (map (H.li . blocksToHtml) items))
     in if start /= 1
          then ol0 ! A.start (H.toValue (T.pack (show start)))
          else ol0
  PD.BulletList items -> H.ul (mconcat (map (H.li . blocksToHtml) items))
  PD.DefinitionList items -> H.dl (mconcat (map defItemToHtml items))
  PD.Header lvl attr ils -> applyAttrs attr (headerTag lvl (inlinesToHtml ils))
  PD.HorizontalRule -> H.hr
  PD.Table attr caption colspecs thead tbodies tfoot ->
    applyAttrs attr $
      H.table $
        captionToHtml H.caption caption
          <> colgroupToHtml colspecs
          <> tableHeadToHtml thead
          <> mconcat (map tableBodyToHtml tbodies)
          <> tableFootToHtml tfoot
  PD.Figure attr caption blocks ->
    applyAttrs attr (H.figure (blocksToHtml blocks <> captionToHtml H.figcaption caption))
  PD.Div attr bs -> applyAttrs attr (H.div (blocksToHtml bs))



headerTag :: Int -> H.Html -> H.Html
headerTag 1 = H.h1
headerTag 2 = H.h2
headerTag 3 = H.h3
headerTag 4 = H.h4
headerTag 5 = H.h5
headerTag _ = H.h6

--------------------------------------------------------------------------------
-- Tables

-- Whether a cell is a column header (<th scope="col">, from TableHead or a
-- TableBody's intermediate head rows), a row header (<th scope="row">, the
-- leading RowHeadColumns cells of a TableBody's regular rows), or plain data.
-- (Our own type, so it needs no qualification.)
data CellKind = ColHeader | RowHeader | DataCell

unsupported :: T.Text -> H.Html
unsupported feature =  H.div $ H.toHtml ("UNSUPPORTED FEATURE: " <> feature)

captionToHtml :: (H.Html -> H.Html) -> PD.Caption -> H.Html
captionToHtml h (PD.Caption _short bs)
  | null bs = mempty
  | otherwise = h (blocksToHtml bs)

-- Each DefinitionList item is (term, [definition]) where a definition is
-- itself [Block] (a <dd> can hold multiple blocks); a term can have several
-- definitions, hence several <dd>s per <dt>.
defItemToHtml :: ([PD.Inline], [[PD.Block]]) -> H.Html
defItemToHtml (term, defs) =
  H.dt (inlinesToHtml term) <> mconcat (map (H.dd . blocksToHtml) defs)

colgroupToHtml :: [PD.ColSpec] -> H.Html
colgroupToHtml colspecs
  | all ((== PD.ColWidthDefault) . snd) colspecs = mempty
  | otherwise = H.colgroup (mconcat (map colToHtml colspecs))
  where
    colToHtml (_, PD.ColWidthDefault) = H.col
    colToHtml (_, PD.ColWidth w) =
      H.col ! A.style (H.toValue ("width: " <> T.pack (show (round (w * 100) :: Int)) <> "%;"))

tableHeadToHtml :: PD.TableHead -> H.Html
tableHeadToHtml (PD.TableHead attr rows)
  | null rows = mempty
  | otherwise = applyAttrs attr (H.thead (mconcat (map (rowToHtml (const ColHeader)) rows)))

tableBodyToHtml :: PD.TableBody -> H.Html
tableBodyToHtml (PD.TableBody attr (PD.RowHeadColumns n) headRows bodyRows) =
  applyAttrs attr $
    H.tbody $
      mconcat (map (rowToHtml (const ColHeader)) headRows)
        <> mconcat (map (rowToHtml (\i -> if i < n then RowHeader else DataCell)) bodyRows)

tableFootToHtml :: PD.TableFoot -> H.Html
tableFootToHtml (PD.TableFoot attr rows)
  | null rows = mempty
  | otherwise = applyAttrs attr (H.tfoot (mconcat (map (rowToHtml (const DataCell)) rows)))

-- 'kindAt i' classifies the cell at column index i (only varies for
-- TableBody rows, where the first RowHeadColumns cells are row headers).
rowToHtml :: (Int -> CellKind) -> PD.Row -> H.Html
rowToHtml kindAt (PD.Row attr cells) =
  applyAttrs attr (H.tr (mconcat (zipWith (\i c -> cellToHtml (kindAt i) c) [0 ..] cells)))

cellToHtml :: CellKind -> PD.Cell -> H.Html
cellToHtml kind (PD.Cell attr align (PD.RowSpan rs) (PD.ColSpan cs) bs) =
  applyAttrs attr (withSpans (withScope (withAlign (tagFor kind (blocksToHtml bs)))))
  where
    tagFor DataCell = H.td
    tagFor _ = H.th
    withScope h = case kind of
      ColHeader -> h ! A.scope "col"
      RowHeader -> h ! A.scope "row"
      DataCell -> h
    withAlign h
      | align == PD.AlignDefault = h
      | otherwise = h ! A.style (H.toValue (alignStyle align))
    alignStyle :: PD.Alignment -> T.Text
    alignStyle PD.AlignLeft = "text-align: left;"
    alignStyle PD.AlignRight = "text-align: right;"
    alignStyle PD.AlignCenter = "text-align: center;"
    alignStyle PD.AlignDefault = ""
    withSpans h0 =
      let h1 = if rs /= 1 then h0 ! A.rowspan (H.toValue (T.pack (show rs))) else h0
       in if cs /= 1 then h1 ! A.colspan (H.toValue (T.pack (show cs))) else h1

--------------------------------------------------------------------------------
-- Inlines

inlinesToHtml :: [PD.Inline] -> H.Html
inlinesToHtml = mconcat . map inlineToHtml

inlineToHtml :: PD.Inline -> H.Html
inlineToHtml il = case il of
  PD.Str s -> H.toHtml s
  PD.Emph ils -> H.em (inlinesToHtml ils)
  PD.Strong ils -> H.strong (inlinesToHtml ils)
  PD.Strikeout ils -> H.del (inlinesToHtml ils)
  PD.Underline ils -> H.u $ inlinesToHtml ils
  PD.Superscript ils -> H.sup (inlinesToHtml ils)
  PD.Subscript ils -> H.sub (inlinesToHtml ils)
  PD.SmallCaps ils -> H.span ! A.style "font-variant: small-caps;" $ inlinesToHtml ils
  PD.Quoted PD.SingleQuote ils ->
    H.preEscapedToHtml ("&lsquo;" :: T.Text) <> inlinesToHtml ils <> H.preEscapedToHtml ("&rsquo;" :: T.Text)
  PD.Quoted PD.DoubleQuote ils ->
    H.preEscapedToHtml ("&ldquo;" :: T.Text) <> inlinesToHtml ils <> H.preEscapedToHtml ("&rdquo;" :: T.Text)
  PD.Cite _ ils -> inlinesToHtml ils
  PD.Code attr code -> applyAttrs attr (H.code (H.toHtml code))
  PD.Space -> H.toHtml (" " :: T.Text)
  PD.SoftBreak -> H.toHtml ("\n" :: T.Text)
  PD.LineBreak -> H.br
  PD.Math _ str -> H.toHtml str -- plug in texmath yourself if you need real math rendering
  PD.RawInline (PD.Format fmt) raw
    | T.toLower fmt `elem` ["html", "html5"] -> H.preEscapedToHtml raw
    | otherwise -> mempty
  PD.Link attr ils (url, title) ->
    applyAttrs attr (addTitle title (H.a ! A.href (H.toValue url) $ inlinesToHtml ils))
  PD.Image attr ils (url, title) ->
    applyAttrs attr (addTitle title (H.img ! A.src (H.toValue url) ! A.alt (H.toValue (inlinesToPlain ils))))
  PD.Note _ -> unsupported "--- No support for Notes ---"
  PD.Span attr ils -> applyAttrs attr (H.span (inlinesToHtml ils))
  where
    addTitle t h = if T.null t then h else h ! A.title (H.toValue t)

inlinesToPlain :: [PD.Inline] -> T.Text
inlinesToPlain = T.concat . map go
  where
    go (PD.Str s) = s
    go PD.Space = " "
    go PD.SoftBreak = " "
    go (PD.Emph ils) = inlinesToPlain ils
    go (PD.Strong ils) = inlinesToPlain ils
    go _ = ""


