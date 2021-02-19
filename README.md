# Voronoi Art

CPSC 312 project

Given an input image, and the number of voronoi partitions to create, we will implement a flood fill algorithm starting from a randomly generated set of center coordinates in the image to create the specified number of partitions in the image, resulting in interesting artistic results where every pixel in a given voronoi partition have the same color.

## How to run

- `git clone https://github.com/shlyyzy/voronoiArt.git`
- `stack setup`
- `stack build`
- `stack ghci`
  - `:set args <input image path> <output image path> <number of voronoi partitions>`
  - `main`
