data DateRef = DateExact Time.Day
             | DateTodo  Time.Day
             | DateRepeat (Maybe Integer, Maybe Int, Int) -- ^ Year? Month? Day
  deriving (Show)


data CollectedDate = CollectedDate DateRef T.Text T.Text  -- ^ Date Label Indentifier
  deriving (Show)

data CollectedLinkToPage = CollectedLinkToPage PageId T.Text T.Text -- ^ PageId Label Identifier
  deriving (Show, Eq, Ord)




-- TODO : really ugly, I should use a proper parser
parseColDates :: Lang -> [CollectedFromDoc] -> [CollectedDate]
parseColDates lg = catMaybes . map parseColDate
  where
    parseColDate :: CollectedFromDoc -> Maybe CollectedDate
    parseColDate (CollectedPageLink _ _ ) = Nothing
    parseColDate (CollectedDateLink t l i) =
      case parseDateRef t of
        Just dr -> Just $ CollectedDate dr l i
        _       -> Nothing

    parseDateRef t =
      let (stamp', kw') = T.breakOn " " t
          (stamp, kw) = (T.strip stamp', T.strip kw')
          todo = T.toLower kw == "todo"
          parts = T.splitOn "-" stamp
      in case parts of
        (y':m':d':[]) -> Just $
          let (y,m,d) = case lg of
                French -> (parseD d', parseD m', parseD y')
                _      -> (parseD y', parseD m', parseD d') in
            if y == Nothing || m == Nothing
            then DateRepeat (y,m,fromMaybe 1 d)
            else
              let day = C.fromGregorian (fromMaybe 0 y) (fromMaybe 1 m) (fromMaybe 1 d) in
                if todo then DateTodo day else DateExact day
                
        _  -> Nothing

    parseD n = case TR.decimal n of
      Right (r, "") -> Just r
      _             -> Nothing



serveAgenda :: ActM ()
serveAgenda = do
  db <- getRestrictedDB
  today <- liftAndCatchIO (Time.utctDay <$> Time.getCurrentTime)
  let idx = K.pagesIndex db
      -- (_,tdM,futureS) = M.splitLookup today (K.ltpSetByCollectedDates db)
      tdQ = DI.query idx $ DI.qOnly K.FieldColDate today
      futureQ = DI.query idx $ DI.qRange K.FieldColDate (today+1) (today+365)
      td = map (\tdS -> mapDateWithLTPS (today, tdS)) tdQ
      futures = concatMap mapDateWithLTPS futureQ
  r <- V.renderAgenda td futures
  -- r <- V.renderAgenda [] []
  html r
  where
    mapDateWithLTPS (d,ltps) = map (\ltp -> (d, ltp)) ltps


-- Pandoc.hs
          DateLink l -> do
            let anchorId = PS.inlineListToIdentifier P.pandocExtensions [link]
            tell [CollectedDateLink (T.strip l) (PS.stringify txt) anchorId ]
            return $ P.Span (setId anchorId $ addClass "kiwi-date" attr) 
                            (txt ++ [P.Space, P.Emph [P.Str $ T.concat ["[",l,"]"]]])


-- view

type DateLTP = (Time.Day, K.CollectedLinkToPage)

renderAgenda :: [DateLTP] -> [DateLTP] -> ActM TL.Text
renderAgenda todayLTP futureLTP = do
  loc <- getLocales
  locJ <- getLocalesJSON
  tpl <- getTemplate
  let c = object [ "today-ltp" .= (toJSONListHack $ map (ltpJ loc) todayLTP)
                 , "future-ltp" .= (toJSONListHack $ map (ltpJ loc) futureLTP)
                 , "locales" .= locJ
                 ]
      lc = X.renderMustache (K.agendaTemplate tpl) c
  renderLayout (K.loc_Agenda loc) lc Nothing
  
  where
    ltpJ loc (d, (K.CollectedLinkToPage pId label ident)) =
      object [ "target" .= T.concat ["/page/", pId, "#", ident]
             , "label" .= label
             , "date" .= Time.formatTime (K.loc_TimeLocale loc) "%A %e %B %Y" d
             ]




    -- makeLTPSetByColDate pages today =
    --   M.fromListWith S.union (concatMap pairDates pages)
    --   where
    --     pairDates p = map (datePagePair p) $ pageCollectedDates p
    --     datePagePair p (CollectedDate r label anchorId) = 
    --       let d = case r of
    --             DateExact x -> x
    --             DateTodo x -> x
    --             DateRepeat (yM, mM, dom) ->
    --               let (yT, mT, dT) = Cal.toGregorian today in
    --                 case (yM, mM) of
    --                   (Nothing, Just m) -> if (mT, dT) <= (m, dom) then
    --                                          Cal.fromGregorian yT m dom
    --                                        else Cal.fromGregorian (yT+1) m dom
    --                   (Nothing, Nothing) ->
    --                     let dc = Cal.fromGregorian yT mT dom in
    --                       if dT <= dom then dc else Cal.addGregorianMonthsClip 1 dc
    --                   (Just y, Nothing) -> if mT == 12 then 
    --                                          Cal.fromGregorian y 12 dom
    --                                        else if dT <= dom then 
    --                                               Cal.fromGregorian y mT dom
    --                                             else Cal.fromGregorian y (mT+1) dom
    --                   (Just y, Just m) -> Cal.fromGregorian y m dom
    --       in (d, S.singleton $ CollectedLinkToPage (pageId p) label anchorId)


