{-# LANGUAGE InstanceSigs #-}

module ParticleLife (
    generateInitialState,
    simulateStep,
    ParticleState,
    Particle(..),
    computeForce,
    generateRandomForceMatrix,
    SimulationParameters(..),
    ForceMatrix
) where

import System.Random
import Data.Array
import PlDefinitions
import KdTrees
import Control.Monad.State
import qualified Data.Vector as V

type ForceMatrix = Array (Int, Int) Double

data SimulationParameters = PLifeSP {
    -- Number of unique colour indices
    colours :: Int,
    width :: Int,
    height :: Int,
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

-- Compute new velocities based on forces, and then move the particles according to those velocities
simulateStep :: SimulationParameters -> Double -> ParticleState -> ParticleState
simulateStep sp dt pstate = pstate''
                        where
                            lt = fromList $ V.toList pstate
                            -- Update velocities
                            pstate' = V.map (computeVelocity sp lt dt) pstate
                            -- Move particles
                            pstate'' = V.map (moveParticle dt) pstate'

-- Combine 2 tuples using the same function f for the 2 left and the 2 right values
tcomb :: (Double -> Double -> Double) -> (Double, Double) -> (Double, Double) -> (Double, Double)
tcomb f (x1, y1) (x2, y2) = (f x1 x2, f y1 y2)

-- Combine 2 tuples by adding their values together
tadd :: (Double, Double) -> (Double, Double) -> (Double, Double)
tadd = tcomb (+)

dampening :: Double
dampening = 0.7

computeWallForce :: SimulationParameters -> Particle -> (Double, Double)
computeWallForce sp p = (fx, fy)
                        where
                            w = fromIntegral $ width sp
                            h = fromIntegral $ height sp
                            (x, y) = position p
                            fx  | x < 0.0 = -x
                                | x > w = w - x
                                | otherwise = 0.0

                            fy  | y < 0.0 = -y
                                | y > h = h - y
                                | otherwise = 0.0

blockWallVelocity ::  SimulationParameters -> Particle -> Particle
blockWallVelocity sp p = p {velocity = (vx', vy')}
                        where
                            w = fromIntegral $ width sp
                            h = fromIntegral $ height sp                            
                            (x, y) = position p
                            (vx, vy) = velocity p

                            vx' | x < 0.0 && vx < 0.0 = -vx
                                | x > w && vx > 0.0 = -vx
                                | otherwise = vx

                            vy' | y < 0.0 && vy < 0.0 = -vy
                                | y > h && vy > 0.0 = -vy
                                | otherwise = vy


-- Compute new velocity of a particle without moving
computeVelocity :: LookupTable lt => SimulationParameters -> lt -> Double -> Particle -> Particle
computeVelocity sp lt dt p = blockWallVelocity sp (p {velocity = (vx', vy')})
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


generateRandomParticle :: RandomGen g => SimulationParameters -> State g Particle
generateRandomParticle sp = do 
    x <- state (uniformR (0.0, fromIntegral $ width sp))
    y <- state (uniformR (0.0, fromIntegral $ height sp))
    vx <- state (uniformR (-10.0, 10.0))
    vy <- state (uniformR (-10.0, 10.0))
    cidx <- state (uniformR (1, colours sp))
    pure (P {position = (x, y), velocity = (vx, vy), colourIdx = cidx})

generateInitialState :: RandomGen g => SimulationParameters -> Int -> g -> (ParticleState, g)
generateInitialState sp particlecount gen = runState (V.replicateM particlecount (generateRandomParticle sp)) gen

-- End of the "short" distance range (starts at 0 distance)
dShort :: Double
dShort = 20.0

-- End of the medium range (starts at d_short), after this distance the particles will ignore eachother
dMed :: Double
dMed = 60.0

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

                            
