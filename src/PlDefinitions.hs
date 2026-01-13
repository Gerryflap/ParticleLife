module PlDefinitions (
    Particle(..),
    distance,
    ParticleState,
    LookupTable(..),
    ParticleListLt(..),
) where

data Particle = P {
    position :: (Double, Double),
    velocity :: (Double, Double),
    colourIdx :: Int
} deriving (Show, Eq)

distance :: Particle -> Particle -> Double
distance pa1 pa2 = sqrt ((x1 - x2) ** 2.0 + (y1 - y2) ** 2.0)
                    where
                        (x1, y1) = position pa1
                        (x2, y2) = position pa2

type ParticleState = [Particle]

class LookupTable lt where {
    -- Query the lookuptable with a particle and a radius around that particle, which should result in all particles that are within that range
    findAllInRange :: lt -> Particle -> Double -> [Particle]
}

newtype ParticleListLt = PListLt [Particle]

instance LookupTable ParticleListLt where 
    findAllInRange (PListLt ps) p range = filter (\op -> range > distance p op) ps
