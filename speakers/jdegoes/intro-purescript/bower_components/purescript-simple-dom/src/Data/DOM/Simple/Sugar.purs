module Data.DOM.Simple.Sugar where

-- I'm not sure this is a good idea, just an experiment..
{-
  - set the contents of the .container div inside body to hello world
  document <-? "body > .container" %<- "Hello World"

  - get an attribute on an element and add 'woo' to it
  someDiv <- document <-? "#someDiv"
  title <- someDiv <-# "my-attribute"
  someDiv #<- (Tuple "my-attribute" $ title ++ "woo")
-}


import Control.Monad.Eff

import Data.Tuple
import Data.Maybe
import Data.Foldable
import Data.DOM.Simple.Types
import Data.DOM.Simple.Element
import Data.DOM.Simple.Document
import Data.DOM.Simple.Unsafe.Sugar
import DOM

class DOMArrows b where
  (#<-) :: forall eff. b -> (Tuple String String) -> (Eff (dom :: DOM | eff) Unit)
  (<-#) :: forall eff. b -> String -> (Eff (dom :: DOM | eff) String)
  (<-?) :: forall eff. b -> String -> (Eff (dom :: DOM | eff) (Maybe HTMLElement))
  (%<-) :: forall eff. b -> String -> (Eff (dom :: DOM | eff) Unit)
  (@<-) :: forall eff. b -> String -> (Eff (dom :: DOM | eff) Unit)

  -- (<<-) :: forall eff a c. b -> [(a -> (Eff (dom :: DOM | eff) c))] -> (Eff (dom :: DOM | eff) Unit)

instance arrowsHTMLElement :: (Element a) => DOMArrows a where
  (#<-) el val = setAttribute (fst val) (snd val) el
  (<-#) el key = getAttribute key el
  (<-?) el sel = querySelector sel el
  (%<-) el txt = setInnerHTML txt el
  (@<-) el txt = setTextContent txt el

  -- (<<-) el act = for_ (\x -> x el) act

instance arrowsEffHTMLElement :: (Element a) => DOMArrows (Eff eff a) where
  (#<-) el val = (dirtyKindDomRecast el) >>= (\x -> x #<- val)
  (<-#) el key = (dirtyKindDomRecast el) >>= (\x -> x <-# key)
  (<-?) el sel = (dirtyKindDomRecast el) >>= (\x -> x <-? sel)
  (%<-) el txt = (dirtyKindDomRecast el) >>= (\x -> x %<- txt)
  (@<-) el txt = (dirtyKindDomRecast el) >>= (\x -> x @<- txt)

  -- (<<-) el act = (dirtyKindDomRecast el) >>= (\el' -> for_ (\x -> x el) act)


instance arrowsMaybeHTMLElement :: (Element a) => DOMArrows (Maybe a) where
  (#<-) (Just el) val = Just $ el #<- val
  (<-#) (Just el) key = Just $ el <-# key
  (<-?) (Just el) sel = Just $ el <-? sel
  (%<-) (Just el) txt = Just $ el %<- txt
  (@<-) (Just el) txt = Just $ el @<- txt

  -- (<<-) (Just el) act = Just $ for_ (\x -> x el) act
