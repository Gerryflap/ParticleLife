
module ParticleLife (
    generateInitialState,
    simulateStep,
    ParticleState,
    Particle,
    position,
    velocity,
    colourIdx,
    computeForce
) where

import System.Random

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
    finalAllInRange :: lt -> Particle -> Double -> [Particle]
}

newtype ParticleListLt = PListLt [Particle]

instance LookupTable ParticleListLt where 
    finalAllInRange (PListLt ps) p range = filter (\op -> range > distance p op) ps

-- Compute new velocities based on forces, and then move the particles according to those velocities
simulateStep :: Double -> ParticleState -> ParticleState
simulateStep dt pstate = pstate''
                        where
                            lt = PListLt pstate
                            -- Update velocities
                            pstate' = map (computeVelocity lt dt) pstate
                            -- Move particles
                            pstate'' = map (moveParticle dt) pstate'

tcomb :: (Double -> Double -> Double) -> (Double, Double) -> (Double, Double) -> (Double, Double)
tcomb f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

tadd = tcomb (+)

dampening :: Double
dampening = 0.995

-- Compute new velocity of a particle without moving
computeVelocity :: LookupTable lt => lt -> Double -> Particle -> Particle
computeVelocity lt dt p = p {velocity = (vx', vy')}
                        where
                            (vx, vy) = velocity p
                            -- Find all particles in dMed range that are not the current particle
                            inRangeParticles = filter (p /=) $ finalAllInRange lt p dMed
                            -- Particle force, sum of all in range particle forces
                            (fpx, fpy) = foldl tadd (0.0, 0.0) $ map (computeParticleForce 1.0 p) inRangeParticles
                            -- TODO: In the future we can compute a boundary force here if needed
                            --Dampening and velocity
                            vx' = vx * (dampening ** dt) + fpx * dt
                            vy' = vy * (dampening ** dt) + fpy * dt


-- move particles with the set velocity
moveParticle :: Double -> Particle -> Particle
moveParticle dt p = p {position = newPos, colourIdx = colourIdx p}
                where
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

-- End of the "short" distance range (starts at 0 distance)
dShort :: Double
dShort = 10.0

-- End of the medium range (starts at d_short), after this distance the particles will ignore eachother
dMed :: Double
dMed = 50.0

-- Computes the repulsion or attraction force, based on the input force multiplier
-- Particles will always repell eachother on close range
-- On medium range they will either repell or attract based on the force multiplier (fmul), getting stronger towards the middle of the range and getting weaker when closer to dShort or dMed
-- On long range the particles will ignore eachother
computeForce :: Double -> Double -> Double
computeForce fmul dist  | dist <= dShort = -1 * (1.0 - dist/dShort)
                        | dist <= dMed = fmul *  (1.0 - (2.0 * abs (0.5 - (ndist / mrange))))
                        | otherwise = 0.0
                        where
                            -- Distance past the d_short threshold
                            ndist = dist - dShort
                            -- Length of the medium range (d_med - d_short)
                            mrange = dMed - dShort

-- Computes the directional force on p1, caused by p2.
-- Using fmul as defined in computeForce above
computeParticleForce :: Double -> Particle -> Particle -> (Double, Double)
computeParticleForce fmul p1 p2 = (cosv * f, sinv * f)
                        where
                            (x1, y1) = position p1
                            (x2, y2) = position p2
                            d = distance p1 p2
                            sinv = (y2 - y1) / d
                            cosv = (x2 - x1) / d
                            f = computeForce fmul d
