module Data.DOM.Simple.NodeList where

import DOM
import Control.Monad.Eff
import Data.Maybe
import Data.Array(map, range, catMaybes)
import Data.Traversable(sequence)

import Data.DOM.Simple.Unsafe.NodeList
import Data.DOM.Simple.Types
import Data.DOM.Simple.Unsafe.Utils(ensure)

class NodeListInst b where
  length  :: forall eff. b -> (Eff (dom :: DOM | eff) Number)
  item    :: forall eff. Number -> b -> (Eff (dom :: DOM | eff) (Maybe HTMLElement))

instance nodeList :: NodeListInst NodeList where
  length = unsafeNodeListLength
  item idx el = (unsafeNodeListItem idx el) >>= (ensure >>> return)

nodeListToArray :: forall eff. NodeList -> (Eff (dom :: DOM | eff) [HTMLElement])
nodeListToArray nl = do
  len <- length nl
  xs <- sequence (map (\i -> item i nl) $ range 0 len)
  return $ catMaybes xs

nodeListToArray' :: forall eff. NodeList -> (Eff (dom :: DOM | eff) [HTMLElement])
nodeListToArray' = unsafeNodeListToArray
