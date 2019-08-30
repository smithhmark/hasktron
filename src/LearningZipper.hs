module LearningZipper
  ( NetZip(..)
  , LLayer(..)
  , nets
  , acts
  , endp
  , beginp
  , emptyp
  , start
  , end
  , fromList
  , goDeeper
  , goShallower
  , procLayerD
  , cursor
  , scursor
  , dcursor
  ) where

import qualified NNTypes as NNT

data LLayer = LLayer
  { weights :: [NNT.Weights]
  , biases  :: NNT.Biases
  , outs    :: [(Double, NNT.Activation)]
  } deriving (Show)

nets :: LLayer -> [Double]
nets = map fst . outs

acts :: LLayer -> [NNT.Activation]
acts = map snd . outs

fromLayer :: NNT.Layer -> LLayer
fromLayer l = LLayer ws bs []
  where
    ws = NNT.weights l
    bs = NNT.biases l

data NetZip = NetZip
  { shallower :: [LLayer]
  , deeper    :: [LLayer]
  } deriving (Show)

endp :: NetZip -> Bool
endp NetZip {deeper = []} = True
endp _                    = False

beginp :: NetZip -> Bool
beginp NetZip {shallower = []} = True
beginp _                       = False

emptyp :: NetZip -> Bool
emptyp NetZip {shallower = [], deeper = []} = True
emptyp _                                    = False

start, end :: NetZip -> NetZip
start NetZip {shallower = ss, deeper = ds} =
  NetZip {shallower = [], deeper = (reverse ss ++ ds)}

end NetZip {shallower = ss, deeper = ds} =
  NetZip {shallower = (reverse ds ++ ss), deeper = []}

goDeeper :: NetZip -> NetZip
goDeeper net@NetZip {shallower = s, deeper = l:ls} =
  net {shallower = l : s, deeper = ls}
goDeeper net = net

goShallower :: NetZip -> NetZip
goShallower net@NetZip {shallower = l:ls, deeper = d} =
  net {shallower = ls, deeper = l : d}
goShallower net = net

fromList :: [NNT.Layer] -> NetZip
fromList ls = NetZip {shallower=[], deeper = map fromLayer ls}

popD :: NetZip -> NetZip
popD nz@NetZip {deeper = l:ls} = nz {deeper = ls}
popD nz@NetZip {deeper = []}   = nz

pushD :: NetZip -> LLayer -> NetZip
pushD nz@NetZip {deeper = ls} l = nz {deeper = l : ls}

popS :: NetZip -> NetZip
popS nz@NetZip {shallower = l:ls} = nz {shallower = ls}
popS nz@NetZip {shallower = []}   = nz

pushS :: NetZip -> LLayer -> NetZip
pushS nz@NetZip {shallower = ls} l = nz {shallower = l : ls}

procLayerD :: NetZip -> (LLayer -> LLayer) -> NetZip
procLayerD nz@NetZip {deeper = l:ls, shallower = ss} f =
  nz {shallower = f l : ss, deeper = ls}

cursor :: NetZip -> LLayer
cursor NetZip{deeper = l:ls} = l

scursor :: NetZip -> LLayer
scursor NetZip{shallower = l:ls} = l

dcursor :: NetZip -> LLayer
dcursor NetZip{deeper = l1:l2:ls} = l2
