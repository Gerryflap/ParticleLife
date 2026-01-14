{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
module PlDefinitions (
    Particle(..),
    distance,
    ParticleState,
    LookupTable(..),
    ParticleListLt(..),
    generateRandomForceMatrix,
    SimulationParameters(..),
    ForceMatrix
) where

import System.Random
import Data.Array
import GHC.Generics (Generic)
import Control.DeepSeq (NFData)

data Particle = P {
    position :: (Double, Double, Double),
    velocity :: (Double, Double, Double),
    colourIdx :: Int
} deriving (Show, Eq)

deriving instance Generic Particle
instance NFData Particle

distance :: Particle -> Particle -> Double
distance pa1 pa2 = sqrt ((x1 - x2) ** 2.0 + (y1 - y2) ** 2.0 + (z1 - z2) ** 2.0)
                    where
                        (x1, y1, z1) = position pa1
                        (x2, y2, z2) = position pa2

type ParticleState = [Particle]

class LookupTable lt where {
    -- Query the lookuptable with a particle and a radius around that particle, which should result in all particles that are within that range
    findAllInRange :: lt -> Particle -> Double -> [Particle]
}

newtype ParticleListLt = PListLt [Particle]

instance LookupTable ParticleListLt where 
    findAllInRange (PListLt ps) p range = filter (\op -> range > distance p op) ps

type ForceMatrix = Array (Int, Int) Double

data SimulationParameters = PLifeSP {
    -- Number of unique colour indices
    colours :: Int,
    width :: Int,
    height :: Int,
    depth :: Double,
    -- Multiplier of the wall force
    wforcemult :: Double,
    -- Multiplier of the particle force
    pforcemult :: Double,
    forceMatrix :: ForceMatrix
} deriving (Show)

generateRandomForceMatrix :: SplitGen g => Int -> g -> (ForceMatrix, g)
generateRandomForceMatrix ncolours gen = (listArray ((1,1), (ncolours,ncolours)) (uniformRs (-1.0, 1.0) cgen), newgen)
                                where
                                    (cgen, newgen) = splitGen gen