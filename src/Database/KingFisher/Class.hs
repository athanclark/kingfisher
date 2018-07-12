{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , ConstraintKinds
  , FlexibleContexts
  #-}

module Database.KingFisher.Class where


import Data.Proxy (Proxy)



-- * Generics for different data views

class GetStorable pred view symbol m
  | view -> pred symbol where
  getStorable :: pred -> m (Maybe view)

class GetAllStorable pred view symbol m
  | view -> pred symbol where
  getAllStorable :: pred -> m [view]

class NewStorable key view symbol m
  | view -> key symbol where
  newStorable :: view -> m (Maybe key)

class SetStorable pred err view symbol m
  | view -> pred err symbol where
  setStorable :: pred -> view -> m (Maybe err)

class DeleteStorable pred err symbol m where
  deleteStorable :: Proxy symbol -> pred -> m (Maybe err)


-- | A single data view 'a' that supports CRUD
type CRUDStorable getPred getAllPred key setPred setErr deletePred deleteErr view symbol m =
  ( GetStorable    getPred         view symbol m
  , GetAllStorable getAllPred      view symbol m
  , NewStorable    key             view symbol m
  , SetStorable    setPred setErr  view symbol m
  , DeleteStorable deletePred deleteErr symbol m
  )

-- | A single data view 'a' that supports CRUD, all sharing the same key as the
-- predicate.
type BasicCRUDStorable key setErr deleteErr view symbol m =
  CRUDStorable key () key key setErr key deleteErr view symbol m
