{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , ConstraintKinds
  , FlexibleContexts
  #-}

module Database.KingFisher.Class where


-- * Generics for different data views

class GetStorable pred a m | a -> pred where
  getStorable :: pred -> m (Maybe a)

class GetAllStorable pred a m | a -> pred where
  getAllStorable :: pred -> m [a]

class NewStorable key a m | a -> key where
  newStorable :: a -> m (Maybe key)

class SetStorable pred err a m | a -> pred err where
  setStorable :: pred -> a -> m (Maybe err)

class DeleteStorable pred err m | pred -> err where
  deleteStorable :: pred -> m (Maybe err)


-- | A single data view 'a' that supports CRUD
type CRUDStorable getPred getAllPred key setPred setErr deletePred deleteErr a m =
  ( GetStorable getPred a m
  , GetAllStorable getAllPred a m
  , NewStorable key a m
  , SetStorable setPred setErr a m
  , DeleteStorable deletePred deleteErr m
  )

-- | A single data view 'a' that supports CRUD, all sharing the same key as the
-- predicate.
type BasicCRUDStorable key setErr deleteErr a m =
  CRUDStorable key () key key setErr key deleteErr a m
