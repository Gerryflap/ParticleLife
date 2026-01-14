# ParticleLife
Inspired by a Youtube video called ["Simulating Particle Life"](https://www.youtube.com/watch?v=4vk7YvBYpOs&list=FLaIhJl1cp1_pu733JbdFX4Q) by DigitalGenius.

This toy repo contains a hacked together implementation of the concept in Haskell, using OpenGL and GLFW to render the simulation.

## Installation
If you have Haskell Stack installed, you should just be able to build the project using `stack build` and run using `stack run`.

## Changing parameters
For now, the parameters are still a bit spread out across the project, though a goal is to make them all adjustable via the command line or a settings file later.

### Settings list:

#### In PlGraphics.hs, display function

- `ncolours`, the amount of unique colours in use. Every colour adds new interactions with every other colour. More colours is generally more chaos, and fewer colours can be too stable. I'd recommend something like 5 for a nice experience

- `pcount`, amount of particles in the simulation. Generally speaking more particles will tank the performance, though this is also very dependent on the chaos in the simulation. More clustered particles will greatly increase the simulation cost. What value to pick depends a lot on your system.

- `width` and `height` in pixels, initial screen (and simulation) width and height. Window is resizable, so only really relevant at the start, when you want to have a specific resolution, or if you don't constantly want to resize.

- `wforcemult`, controls the "wall force" strength. Used to be more relevant, but since walls deflect velocity now, it is now only really relevant when the window size is changed. Pushes particles inward when out of the screen. Recommended to keep at 1

- `pforcemult`, controls the particle force strength globally. This has a big effect on the simulation. A mutliplier of 10 seems to cause the desired behavior, so I'd recommend something like that.

#### In ParticleLife.hs
- `dShort`, The upper bound of the "short" distance. Below this bound, all particles will repell eachother more and more as the distance approches 0. This is linear, so at distance = dShort the force will be 0, at distance = dShort/2 it'll be 0.5, and at distance = 0 it'll be 1 (assuming it doesn't crash the sim :3 ). Recommended value is 20.

- `dMed`, upper bound of the medium distance. Between `dShort` and `dMed` distance is where the magic happens. From `dShort` to the middle of the range the force between 2 particles linearly grows to the force dictated by the force matrix (based on the colours). And from the middle of the range to `dMed` the force linearly shrinks to 0 again. Recommended value is 60, but this setting has a huge effect on the sim, so by all means mess around with it.

