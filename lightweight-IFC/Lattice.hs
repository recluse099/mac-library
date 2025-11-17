module Lattice where
data L = L
data H = H
class Less sl sh where
  less :: sl -> sh -> ()
instance Less L L where
  less _ _ = ()
instance Less L H where
  less _ _ = ()
instance Less H H where
  less _ _ = ()
