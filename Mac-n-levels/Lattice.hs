{-# LANGUAGE Safe #-}

-- | Encodes a security lattice.
module Mac.Lattice (
    Less (),
    H (),
    L (),
)
where

-- | Label for public data
data L = MkL

-- | Label for secrets
data H = MkH

-- Pablo's trick to avoid instances by defining a superclass

-- | Type class used to avoid arbitrary instances by attackers (Pablo's trick)
class CanFlowTo l l'

-- | Type class encoding security lattices
class (CanFlowTo l l') => Less l l'

instance CanFlowTo L L
instance CanFlowTo L H
instance CanFlowTo H H

instance Less L L
instance Less L H
instance Less H H
