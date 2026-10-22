-- | Renders a Pandoc document as HTML5, with Lucid.
--
-- The rendering runs in a 'State' that collects the footnotes: pandoc keeps
-- the content of each footnote inline, where it is referenced, so the writer
-- numbers each 'PD.Note' it meets and renders its content at the end of the
-- document.
module Kiwi.HtmlWriter (writeHtml5) where

import           Control.Monad (forM_, unless)
import           Control.Monad.State.Strict (State, evalState, gets, modify)
import qualified Data.List as List
import qualified Data.Map.Strict as M
import qualified Data.Text as T

import           Lucid
import           Lucid.Base (commuteHtmlT2, makeAttributes)
import qualified Text.Pandoc.Definition as PD
import qualified Text.Pandoc.Walk as PW


-- | Footnotes met so far, by number (from 1)
type Notes = M.Map Int [PD.Block]
type W = HtmlT (State Notes)



writeHtml5 :: PD.Pandoc -> Html ()
writeHtml5 (PD.Pandoc _meta blocks) =
  evalState (commuteHtmlT2 render) M.empty
  where render = blocksToHtml blocks >> footnotesToHtml


-- Turns a Pandoc 'PD.Attr' (id, classes, arbitrary key/value pairs) into Lucid
-- attributes. 'makeAttributes' takes a runtime key, which is how arbitrary
-- kv-attrs (e.g. from the commonmark 'attributes' extension) get through.
attrs :: PD.Attr -> [Attributes]
attrs (ident, classes, kvs) =
  [id_ ident | not (T.null ident)]
    ++ [class_ (T.unwords classes) | not (null classes)]
    ++ [makeAttributes k v | (k, v) <- kvs]

showT :: Show a => a -> T.Text
showT = T.pack . show

-- unsupported :: T.Text -> W ()
-- unsupported feature = div_ $ toHtml ("UNSUPPORTED FEATURE: " <> feature)


--------------------------------------------------------------------------------
-- Blocks

blocksToHtml :: [PD.Block] -> W ()
blocksToHtml = mapM_ blockToHtml

blockToHtml :: PD.Block -> W ()
blockToHtml blk = case blk of
  PD.Plain ils -> inlinesToHtml ils
  -- an image alone in its paragraph is a figure, captioned by its description
  -- (the image then gets an empty alt: the caption already describes it)
  PD.Para [PD.Image attr desc target] -> figure_ $ do
    imageToHtml attr "" target
    unless (null desc) $ figcaption_ (inlinesToHtml desc)
  PD.Para ils -> p_ (inlinesToHtml ils)
  PD.LineBlock lns ->
    div_ [class_ "line-block"] $
      sequence_ (List.intersperse (br_ []) (map inlinesToHtml lns))
  PD.CodeBlock attr code -> pre_ (code_ (attrs attr) (toHtml code))
  PD.RawBlock fmt raw -> rawToHtml fmt raw
  PD.BlockQuote bs -> blockquote_ (blocksToHtml bs)
  PD.OrderedList (start, _, _) items ->
    ol_ [start_ (showT start) | start /= 1] (mapM_ (li_ . blocksToHtml) items)
  PD.BulletList items -> ul_ (mapM_ (li_ . blocksToHtml) items)
  PD.DefinitionList items -> dl_ (mapM_ defItemToHtml items)
  PD.Header lvl attr ils -> headerTag lvl (attrs attr) (inlinesToHtml ils)
  PD.HorizontalRule -> hr_ []
  PD.Table attr caption colspecs thead tbodies tfoot ->
    table_ (attrs attr) $ do
      captionToHtml caption_ caption
      colgroupToHtml colspecs
      tableHeadToHtml aligns thead
      mapM_ (tableBodyToHtml aligns) tbodies
      tableFootToHtml aligns tfoot
    where aligns = map fst colspecs
  PD.Figure attr caption bs ->
    figure_ (attrs attr) (blocksToHtml bs >> captionToHtml figcaption_ caption)
  PD.Div attr bs -> div_ (attrs attr) (blocksToHtml bs)

headerTag :: Int -> [Attributes] -> W () -> W ()
headerTag 1 = h1_
headerTag 2 = h2_
headerTag 3 = h3_
headerTag 4 = h4_
headerTag 5 = h5_
headerTag _ = h6_

rawToHtml :: PD.Format -> T.Text -> W ()
rawToHtml (PD.Format fmt) raw
  | T.toLower fmt `elem` ["html", "html5"] = toHtmlRaw raw
  | otherwise = mempty

captionToHtml :: (W () -> W ()) -> PD.Caption -> W ()
captionToHtml h (PD.Caption _short bs)
  | null bs = mempty
  | otherwise = h (blocksToHtml bs)

-- Each DefinitionList item is (term, [definition]) where a definition is
-- itself [Block] (a <dd> can hold multiple blocks); a term can have several
-- definitions, hence several <dd>s per <dt>.
defItemToHtml :: ([PD.Inline], [[PD.Block]]) -> W ()
defItemToHtml (dterm, defs) =
  dt_ (inlinesToHtml dterm) >> mapM_ (dd_ . blocksToHtml) defs


--------------------------------------------------------------------------------
-- Tables

-- Whether a cell is a column header (<th scope="col">, from TableHead or a
-- TableBody's intermediate head rows), a row header (<th scope="row">, the
-- leading RowHeadColumns cells of a TableBody's regular rows), or plain data.
data CellKind = ColHeader | RowHeader | DataCell

colgroupToHtml :: [PD.ColSpec] -> W ()
colgroupToHtml colspecs
  | all ((== PD.ColWidthDefault) . snd) colspecs = mempty
  | otherwise = colgroup_ (mapM_ (col_ . colAttrs . snd) colspecs)
  where
    colAttrs PD.ColWidthDefault = []
    colAttrs (PD.ColWidth w) =
      [style_ ("width: " <> showT (round (w * 100) :: Int) <> "%;")]

-- The 'PD.Alignment' lists below are the alignments of the columns.

tableHeadToHtml :: [PD.Alignment] -> PD.TableHead -> W ()
tableHeadToHtml aligns (PD.TableHead attr rows)
  | null rows = mempty
  | otherwise = thead_ (attrs attr) (mapM_ (rowToHtml aligns (const ColHeader)) rows)

tableBodyToHtml :: [PD.Alignment] -> PD.TableBody -> W ()
tableBodyToHtml aligns (PD.TableBody attr (PD.RowHeadColumns n) headRows bodyRows) =
  tbody_ (attrs attr) $ do
    mapM_ (rowToHtml aligns (const ColHeader)) headRows
    mapM_ (rowToHtml aligns (\i -> if i < n then RowHeader else DataCell)) bodyRows

tableFootToHtml :: [PD.Alignment] -> PD.TableFoot -> W ()
tableFootToHtml aligns (PD.TableFoot attr rows)
  | null rows = mempty
  | otherwise = tfoot_ (attrs attr) (mapM_ (rowToHtml aligns (const DataCell)) rows)

-- 'kindAt i' classifies the cell at column index i (only varies for
-- TableBody rows, where the first RowHeadColumns cells are row headers).
-- Cells are matched with columns by position, regardless of spans.
rowToHtml :: [PD.Alignment] -> (Int -> CellKind) -> PD.Row -> W ()
rowToHtml aligns kindAt (PD.Row attr cells) =
  tr_ (attrs attr) $ sequence_ $
    zipWith3 (cellToHtml . kindAt) [0 ..] (aligns ++ repeat PD.AlignDefault) cells

-- A cell has the alignment of its column, unless it has its own (commonmark
-- only sets the columns').
cellToHtml :: CellKind -> PD.Alignment -> PD.Cell -> W ()
cellToHtml kind colAlign (PD.Cell (ident, classes, kvs) cellAlign (PD.RowSpan rs) (PD.ColSpan cs) bs) =
  tag (scope ++ spans ++ attrs (ident, alignClass ++ classes, kvs)) (blocksToHtml bs)
  where
    align = if cellAlign == PD.AlignDefault then colAlign else cellAlign
    tag = case kind of
      DataCell -> td_
      _ -> th_
    scope = case kind of
      ColHeader -> [scope_ "col"]
      RowHeader -> [scope_ "row"]
      DataCell -> []
    -- a class rather than a style, so that it merges with the cell's own
    -- classes (the theme defines .align-*)
    alignClass = case align of
      PD.AlignLeft -> ["align-left"]
      PD.AlignRight -> ["align-right"]
      PD.AlignCenter -> ["align-center"]
      PD.AlignDefault -> []
    spans = [rowspan_ (showT rs) | rs /= 1] ++ [colspan_ (showT cs) | cs /= 1]


--------------------------------------------------------------------------------
-- Inlines

inlinesToHtml :: [PD.Inline] -> W ()
inlinesToHtml = mapM_ inlineToHtml

inlineToHtml :: PD.Inline -> W ()
inlineToHtml il = case il of
  PD.Str s -> toHtml s
  PD.Emph ils -> em_ (inlinesToHtml ils)
  PD.Strong ils -> strong_ (inlinesToHtml ils)
  PD.Strikeout ils -> del_ (inlinesToHtml ils)
  PD.Underline ils -> term "u" (inlinesToHtml ils) -- lucid has no u_
  PD.Superscript ils -> sup_ (inlinesToHtml ils)
  PD.Subscript ils -> sub_ (inlinesToHtml ils)
  PD.SmallCaps ils -> span_ [style_ "font-variant: small-caps;"] (inlinesToHtml ils)
  PD.Quoted PD.SingleQuote ils -> "‘" >> inlinesToHtml ils >> "’"
  PD.Quoted PD.DoubleQuote ils -> "“" >> inlinesToHtml ils >> "”"
  PD.Cite _ ils -> inlinesToHtml ils
  PD.Code attr code -> code_ (attrs attr) (toHtml code)
  PD.Space -> " "
  PD.SoftBreak -> "\n"
  PD.LineBreak -> br_ []
  PD.Math _ str -> toHtml str -- plug in texmath yourself if you need real math rendering
  PD.RawInline fmt raw -> rawToHtml fmt raw
  PD.Link attr ils (url, title) ->
    a_ ([href_ url] ++ titleAttr title ++ attrs attr) (inlinesToHtml ils)
  PD.Image attr ils target -> imageToHtml attr (inlinesToPlain ils) target
  PD.Note bs -> noteToHtml bs
  PD.Span attr ils -> span_ (attrs attr) (inlinesToHtml ils)

imageToHtml :: PD.Attr -> T.Text -> PD.Target -> W ()
imageToHtml attr alt (url, title) =
  img_ ([src_ url, alt_ alt] ++ titleAttr title ++ attrs attr)

titleAttr :: T.Text -> [Attributes]
titleAttr t = [title_ t | not (T.null t)]

-- The text of inlines, at any depth, without formatting (for alt attributes)
inlinesToPlain :: [PD.Inline] -> T.Text
inlinesToPlain = T.concat . PW.query go
  where
    go (PD.Str s) = [s]
    go (PD.Code _ s) = [s]
    go (PD.Math _ s) = [s]
    go PD.Space = [" "]
    go PD.SoftBreak = [" "]
    go PD.LineBreak = [" "]
    go _ = []


--------------------------------------------------------------------------------
-- Footnotes

noteId, noteRefId :: Int -> T.Text
noteId n = "fn" <> showT n
noteRefId n = "fnref" <> showT n

-- Stores the content of the note under the next number, and renders the
-- reference: <a href="#fn1" id="fnref1" class="footnote-ref">1</a>
noteToHtml :: [PD.Block] -> W ()
noteToHtml bs = do
  n <- gets ((+ 1) . M.size)
  modify (M.insert n bs)
  a_ [href_ ("#" <> noteId n), id_ (noteRefId n), class_ "footnote-ref"]
    (toHtml (showT n))

-- <section class="footnotes"><ol><li id="fn1">…</li>…</ol></section>
--
-- The notes are looked up one by one: a note inside another note (possible in
-- a Pandoc AST, though commonmark never produces one) is only stored while
-- the latter is rendered.
footnotesToHtml :: W ()
footnotesToHtml = do
  noNotes <- gets M.null
  unless noNotes $ section_ [class_ "footnotes"] $ ol_ (note 1)
  where
    note :: Int -> W ()
    note n = do
      bs <- gets (M.lookup n)
      forM_ bs $ \nbs -> do
        li_ [id_ (noteId n)] (blocksToHtml (addBackLink n nbs))
        note (n + 1)

-- The back link goes at the end of the last paragraph, if there is one.
addBackLink :: Int -> [PD.Block] -> [PD.Block]
addBackLink n bs = case List.unsnoc bs of
  Just (bs', PD.Para ils) -> bs' ++ [PD.Para (ils ++ [PD.Space, back])]
  Just (bs', PD.Plain ils) -> bs' ++ [PD.Plain (ils ++ [PD.Space, back])]
  _ -> bs ++ [PD.Plain [back]]
  where
    back = PD.Link ("", ["footnote-back"], []) [PD.Str "↑"]
                   ("#" <> noteRefId n, "")
