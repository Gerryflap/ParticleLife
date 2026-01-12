
module ParticleLife (
    generateInitialState,
    simulateStep,
    ParticleState,
    Particle,
    position,
    velocity,
    colourIdx
) where

import System.Random

data Particle = P {
    position :: (Double, Double),
    velocity :: (Double, Double),
    colourIdx :: Int
} deriving (Show)

distance :: Particle -> Particle -> Double
distance pa1 pa2 = sqrt ((x1 - x2) ** 2.0 + (y1 - y2) ** 2.0)
                    where
                        (x1, y1) = position pa1
                        (x2, y2) = position pa2

type ParticleState = [Particle]

class LookupTable lt where {
    -- Query the lookuptable with a particle and a radius around that particle, which should result in all particles that are within that range
    finalAllInRange :: lt -> Particle -> Double -> [Particle]
}

newtype ParticleListLt = PListLt [Particle]

instance LookupTable ParticleListLt where 
    finalAllInRange (PListLt ps) p range = filter (\op -> range > distance p op) ps

simulateStep :: Double -> ParticleState -> ParticleState
simulateStep dt pstate = map (step lt dt) pstate
                        where
                            lt = PListLt pstate

step :: LookupTable lt => lt -> Double -> Particle -> Particle
step lt dt p = P {position = newPos, velocity = newVel, colourIdx = colourIdx p}
                where
                    newVel = velocity p
                    (vx, vy) = velocity p
                    (x, y) = position p
                    newPos = (x + vx * dt, y + vy * dt)


generateRandomParticle :: RandomGen g => g -> (Particle, g)
generateRandomParticle g = (P {position = (x, y), velocity = (vx, vy), colourIdx = cidx}, g3)
                            where
                                (x, g1) = uniformR (0.0, 512.0) g
                                (y, g2) = uniformR (0.0, 512.0) g1
                                (vx, g21) = uniformR (-10.0, 10.0) g2
                                (vy, g22) = uniformR (-10.0, 10.0) g21

                                -- Inclusive
                                (cidx, g3) = uniformR (1, 3) g22


generateInitialState :: RandomGen g => Int -> g -> (ParticleState, g)
generateInitialState 0 gen = ([], gen)
generateInitialState particlecount gen = (p : nparticles, nngen)
                                        where
                                            (nparticles, ngen) = generateInitialState (particlecount-1) gen
                                            (p, nngen) = generateRandomParticle ngen