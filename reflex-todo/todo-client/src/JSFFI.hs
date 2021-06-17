{-# LANGUAGE CPP #-}
{-# LANGUAGE MonoLocalBinds #-}

module JSFFI where

import Reflex.Dom

#ifdef ghcjs_HOST_OS

import Control.Monad.IO.Class

foreign import javascript unsafe
  "(function() {\
    flatpickr($1, {\
      enableTime: false,\
      dateFormat: \"Y-m-d\"\
    }); \
  })()"
  addDatePicker_js :: RawInputElement GhcjsDomSpace -> IO ()

addDatePicker :: MonadWidget t m => InputElement er GhcjsDomSpace t -> m ()
addDatePicker = liftIO . addDatePicker_js . _inputElement_raw

#else

addDatePicker :: MonadWidget t m => InputElement er GhcjsDomSpace t -> m ()
addDatePicker _ = pure ()

#endif
