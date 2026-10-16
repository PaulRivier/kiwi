-- | Snowball stemmers (<https://snowballstem.org/>), through the bundled
-- libstemmer_c sources (UTF-8 only).
--
-- The algorithms expect lower case words with composed accents (Unicode NFC):
-- folding and normalization are left to the caller.
module Text.Stemmer.Snowball (
    Algorithm(..)
  , algorithmName
  , stem
  , stems
  ) where

import           Control.Exception (bracket, throwIO)
import           Control.Monad (when)
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Foreign as TF
import           Data.Word (Word8)
import           Foreign.C.String (CString, withCString)
import           Foreign.C.Types (CInt(..))
import           Foreign.Ptr (Ptr, castPtr, nullPtr)
import           System.IO.Unsafe (unsafePerformIO)


-- | The stemming algorithms available in UTF-8, by their canonical names.
data Algorithm
    = Arabic
    | Armenian
    | Basque
    | Catalan
    | Czech
    | Danish
    | Dutch
    | DutchPorter
    | English
    | Esperanto
    | Estonian
    | Finnish
    | French
    | German
    | Greek
    | Hindi
    | Hungarian
    | Indonesian
    | Irish
    | Italian
    | Lithuanian
    | Nepali
    | Norwegian
    | Persian
    | Polish
    | Porter      -- ^ the original Porter stemmer for English
    | Portuguese
    | Romanian
    | Russian
    | Serbian
    | Sesotho
    | Spanish
    | Swedish
    | Tamil
    | Turkish
    | Yiddish
    deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- | The libstemmer name of an algorithm.
algorithmName :: Algorithm -> String
algorithmName a = case a of
  Arabic      -> "arabic"
  Armenian    -> "armenian"
  Basque      -> "basque"
  Catalan     -> "catalan"
  Czech       -> "czech"
  Danish      -> "danish"
  Dutch       -> "dutch"
  DutchPorter -> "dutch_porter"
  English     -> "english"
  Esperanto   -> "esperanto"
  Estonian    -> "estonian"
  Finnish     -> "finnish"
  French      -> "french"
  German      -> "german"
  Greek       -> "greek"
  Hindi       -> "hindi"
  Hungarian   -> "hungarian"
  Indonesian  -> "indonesian"
  Irish       -> "irish"
  Italian     -> "italian"
  Lithuanian  -> "lithuanian"
  Nepali      -> "nepali"
  Norwegian   -> "norwegian"
  Persian     -> "persian"
  Polish      -> "polish"
  Porter      -> "porter"
  Portuguese  -> "portuguese"
  Romanian    -> "romanian"
  Russian     -> "russian"
  Serbian     -> "serbian"
  Sesotho     -> "sesotho"
  Spanish     -> "spanish"
  Swedish     -> "swedish"
  Tamil       -> "tamil"
  Turkish     -> "turkish"
  Yiddish     -> "yiddish"


-- | Stems one word. To stem many words, 'stems' is cheaper: it creates the
-- stemmer only once.
stem :: Algorithm -> T.Text -> T.Text
stem a w = case stems a [w] of
  [r] -> r
  _   -> w

-- | Stems a list of words with a single stemmer.
--
-- Each call creates its own stemmer, so concurrent calls are safe. A word the
-- stemmer fails on (out of memory) is returned unchanged.
stems :: Algorithm -> [T.Text] -> [T.Text]
stems _ [] = []
stems a ws = unsafePerformIO $ withStemmer a $ \s -> mapM (stemWord s) ws
{-# NOINLINE stems #-}


data SbStemmer

foreign import ccall unsafe "libstemmer.h sb_stemmer_new"
  sb_stemmer_new :: CString -> CString -> IO (Ptr SbStemmer)

foreign import ccall unsafe "libstemmer.h sb_stemmer_delete"
  sb_stemmer_delete :: Ptr SbStemmer -> IO ()

foreign import ccall unsafe "libstemmer.h sb_stemmer_stem"
  sb_stemmer_stem :: Ptr SbStemmer -> Ptr Word8 -> CInt -> IO (Ptr Word8)

foreign import ccall unsafe "libstemmer.h sb_stemmer_length"
  sb_stemmer_length :: Ptr SbStemmer -> IO CInt


withStemmer :: Algorithm -> (Ptr SbStemmer -> IO b) -> IO b
withStemmer a = bracket new sb_stemmer_delete
  where
    new = withCString (algorithmName a) $ \name ->
          withCString "UTF_8" $ \enc -> do
            s <- sb_stemmer_new name enc
            when (s == nullPtr) $
              throwIO $ userError $ "snowball-c: cannot create the stemmer "
                                    ++ algorithmName a
            return s

-- The size given to the stemmer is the length of the word in bytes, not in
-- characters.
stemWord :: Ptr SbStemmer -> T.Text -> IO T.Text
stemWord s w = TF.withCStringLen w $ \(p, len) -> do
  r <- sb_stemmer_stem s (castPtr p) (fromIntegral len)
  if r == nullPtr
    then return w
    else do
      l <- sb_stemmer_length s
      TE.decodeUtf8Lenient <$> BS.packCStringLen (castPtr r, fromIntegral l)
