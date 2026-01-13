{-# LANGUAGE InstanceSigs #-}

module ParticleLife (
    generateInitialState,
    simulateStep,
    ParticleState,
    Particle(..),
    position,
    velocity,
    colourIdx,
    computeForce,
    generateRandomForceMatrix,
    SimulationParameters(..),
    ForceMatrix
) where

import System.Random
import Data.Array
import PlDefinitions
import KdTrees


type ForceMatrix = Array (Int, Int) Double

data SimulationParameters = PLifeSP {
    width :: Double,
    height :: Double,
    -- Multiplier of the wall force
    wforcemult :: Double,
    -- Multiplier of the particle force
    pforcemult :: Double,
    forceMatrix :: ForceMatrix
} deriving (Show)

generateRandomForceMatrix :: SplitGen g => g -> (ForceMatrix, g)
generateRandomForceMatrix gen = (listArray ((1,1), (3,3)) (uniformRs (-1.0, 1.0) cgen), newgen)
                                where
                                    (cgen, newgen) = splitGen gen

-- Compute new velocities based on forces, and then move the particles according to those velocities
simulateStep :: SimulationParameters -> Double -> ParticleState -> ParticleState
simulateStep sp dt pstate = pstate''
                        where
                            lt = fromList pstate
                            -- Update velocities
                            pstate' = map (computeVelocity sp lt dt) pstate
                            -- Move particles
                            pstate'' = map (moveParticle dt) pstate'

tcomb :: (Double -> Double -> Double) -> (Double, Double) -> (Double, Double) -> (Double, Double)
tcomb f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

tadd = tcomb (+)

dampening :: Double
dampening = 0.95

computeWallForce :: SimulationParameters -> Particle -> (Double, Double)
computeWallForce sp p = (fx, fy)
                        where
                            w = width sp
                            h = height sp
                            (x, y) = position p
                            fx  | x < 0.0 = -x
                                | x > w = w - x
                                | otherwise = 0.0

                            fy  | y < 0.0 = -y
                                | y > h = h - y
                                | otherwise = 0.0


-- Compute new velocity of a particle without moving
computeVelocity :: LookupTable lt => SimulationParameters -> lt -> Double -> Particle -> Particle
computeVelocity sp lt dt p = p {velocity = (vx', vy')}
                        where
                            (vx, vy) = velocity p
                            -- Find all particles in dMed range that are not the current particle
                            inRangeParticles = filter (p /=) $ findAllInRange lt p dMed
                            -- Particle force, sum of all in range particle forces
                            (fpx, fpy) = foldl tadd (0.0, 0.0) $ map (computeParticleForce sp p) inRangeParticles
                            -- Wall force, pushes back on the particles when they exceed the bounds
                            (fwx, fwy) = computeWallForce sp p
                            pfmult = pforcemult sp
                            wfmult = wforcemult sp
                            (fx, fy) = (fpx * pfmult + fwx * wfmult, fpy * pfmult + fwy * wfmult)
                            --Dampening and velocity
                            vx' = vx * (dampening ** dt) + fx * dt
                            vy' = vy * (dampening ** dt) + fy * dt


-- move particles with the set velocity
moveParticle :: Double -> Particle -> Particle
moveParticle dt p = seq newPos $ p {position = newPos}
                where
                    (vx, vy) = velocity p
                    (x, y) = position p
                    newPos = (x + vx * dt, y + vy * dt)


generateRandomParticle :: RandomGen g => g -> (Particle, g)
generateRandomParticle g = (P {position = (x, y), velocity = (vx, vy), colourIdx = cidx}, g3)
                            where
                                (x, g1) = uniformR (0.0, 1600.0) g
                                (y, g2) = uniformR (0.0, 1000.0) g1
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
computeParticleForce :: SimulationParameters -> Particle -> Particle -> (Double, Double)
computeParticleForce sp p1 p2 = (cosv * f, sinv * f)
                        where
                            fmat = forceMatrix sp
                            fmul = fmat ! (colourIdx p1, colourIdx p2)
                            (x1, y1) = position p1
                            (x2, y2) = position p2
                            d = distance p1 p2
                            sinv = (y2 - y1) / d
                            cosv = (x2 - x1) / d
                            f = computeForce fmul d

                            
