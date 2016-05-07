{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
module Yesod.JobQueue.GenericConstr where

import GHC.Generics
import Data.Proxy
import Data.Typeable

-- *Demo> genericConstructors  (Proxy :: Proxy (Either Int Bool))
-- [["Left","Int"],["Right","Bool"]]
-- *Demo> genericConstructors  (Proxy :: Proxy [Bool])
-- [["[]"],[":","Bool","[Bool]"]]

class Constructors (f :: * -> *) where
  constructors :: Proxy f -> [[String]]

pmap :: (f a -> g b) -> proxy g -> Proxy f
pmap _ _ = Proxy

genericConstructors :: forall a proxy.
  (Generic a, Constructors (Rep a)) => proxy a -> [[String]]
genericConstructors _ = constructors (Proxy :: Proxy (Rep a))

instance Constructors f => Constructors (D1 d f) where
  constructors = constructors . pmap M1

instance (Constructors f, Constructors g) => Constructors (f :+: g) where
  constructors p = constructors (pmap L1 p) ++ constructors (pmap R1 p)

instance (Fields f, Constructor c) => Constructors (C1 c f) where
  constructors p =
      [conName (M1 Proxy :: C1 c Proxy ()) : fields (pmap M1 p) ]

class Fields (f :: * -> *) where
  fields :: proxy f -> [String]

instance Fields f => Fields (M1 i c f) where
  fields = fields . pmap M1

instance Fields U1 where
  fields _ = []

instance (Fields f, Fields g) => Fields (f :*: g) where
  fields _ = fields (Proxy :: Proxy f)
            ++ fields (Proxy :: Proxy g)

instance Typeable a => Fields (K1 i a) where
  fields _ = [show (typeRep (Proxy :: Proxy a))]
  
