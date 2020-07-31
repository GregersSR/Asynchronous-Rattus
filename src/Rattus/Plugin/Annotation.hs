{-# LANGUAGE DeriveDataTypeable #-}
module Rattus.Plugin.Annotation (Rattus(..)) where

import Data.Data

-- | Use this type to mark a Haskell function definition as a Rattus
-- function:
--
-- > {-# ANN myFunction Rattus #-}
-- 
-- Or mark a whole module as consisting of Rattus functions only:
--
-- > {-# ANN module Rattus #-}
--
-- If you use the latter option, you can mark exceptions
-- (i.e. functions that should be treated as ordinary Haskell function
-- definitions) as follows:
--
-- > {-# ANN myFunction NotRattus #-}
--
-- By default all Rattus functions are checked for use of lazy data
-- types, since these may cause memory leaks. If any lazy data types
-- are used, a warning is issued. These warnings can be disabled by
-- annotating the module or the function with 'AllowLazyData'
--
-- > {-# ANN myFunction AllowLazyData #-}
-- >
-- > {-# ANN module AllowLazyData #-}

data Rattus = Rattus | NotRattus | AllowLazyData deriving (Typeable, Data, Show, Eq)
