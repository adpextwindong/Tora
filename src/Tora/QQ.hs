module Tora.QQ where

import Language.Haskell.TH
import Language.Haskell.TH.Quote

tigerSrc = QuasiQuoter { quoteExp = stringE
                       , quotePat = error "qqPat not supported"
                       , quoteType = error "qqType not supported"
                       , quoteDec = error "qqDec not supported" }

--https://wiki.haskell.org/Poor_man%27s_here_document#Quasiquoting
