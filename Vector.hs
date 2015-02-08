-- Vector.hs
--
-- vector arithmetic
--
-- Copyright (c) 2010 Colin Smith.

module Vector (
  Coordinate, Vec,
  x, y, z, w,
  (|.), (|*), (|+), (|-), (|><), (|*|),
  norm, dist, midpoint, unit, as_list,
  vec2, vec2t, vec3, vec3t, vec4, vec_index,
  origin, x_unit, y_unit, z_unit, mx_unit, my_unit, mz_unit,
  line_intersection,
  perpendicular_bisector,
  frame_matrix, scale_matrix,
  scaled_frame_matrix,
  uniform_scale_matrix,
  identity_matrix,
  extend_to_frontier,
  perp
) where


import qualified Data.Vector.Unboxed as V
import Data.List(foldl')

-- warning: OpenGL performance can suffer if this is Double.
type Coordinate = Float
newtype Vec = V (V.Vector Coordinate) deriving(Show)

vec2 x y      = V $ V.fromListN 2 [x, y]
vec2t (x,y)   = V $ V.fromListN 2 [x, y]
vec3 x y z    = V $ V.fromListN 3 [x, y, z]
vec3t (x,y,z) = V $ V.fromListN 3 [x, y, z]
vec4 x y z w  = V $ V.fromListN 4 [x, y, z, w]

origin = vec3 0 0 0
x_unit = vec3 1 0 0
y_unit = vec3 0 1 0
z_unit = vec3 0 0 1
mx_unit = vec3 (-1) 0 0
my_unit = vec3 0 (-1) 0
mz_unit = vec3 0 0 (-1)

x (V v) = v V.! 0
y (V v) = v V.! 1
z (V v) = v V.! 2
w (V v) = v V.! 3

as_list (V a) = V.toList a

(V u) |. (V v) = V.sum $ V.zipWith (*) u v

r |* (V u) = V $ V.map (r*) u
(V u) |+ (V v) = V $ V.zipWith (+) u v
(V u) |- (V v) = V $ V.zipWith (-) u v
norm u = sqrt(u |. u)
unit u = (1 / norm u) |* u
dist u v = norm (u |- v)
midpoint u v = 0.5 |* (u |+ v)
u |>< v =
  let i =   det (y u) (z u) (y v) (z v)
      j = - det (x u) (z u) (x v) (z v)
      k =   det (x u) (y u) (x v) (y v)
  in vec3 i j k
vec_index (V v) i = v V.! i

det x1 y1 x2 y2 = x1*y2 - x2*y1

infixl 7 |*, |., |><
infixl 6 |+, |-

-- Two-dimensional geometry.  XXX document that these functions only
-- consider the xy-plane even if they are handed vectors with more
-- coordinates.

line_intersection p q r s =
  let dpq = det (x p) (y p) (x q) (y q)
      drs = det (x r) (y r) (x s) (y s)
      a = det dpq ((x p)-(x q)) drs ((x r)-(x s))
      b = det dpq ((y p)-(y q)) drs ((y r)-(y s))
      c = det ((x p)-(x q)) ((y p)-(y q)) ((x r)-(x s)) ((y r)-(y s))
  in vec3 (a/c) (b/c) 0

perpendicular_bisector p q =
  let m = p `midpoint` q
  in [xf m p, xf m q]
  where xf m p = vec3 ((y m) - (y p) + (x m)) ((x p) - (x m) + (y m)) 0

-- return a line coincident with (p q) with endpoints outside or on
-- the circle of radius r centered at the origin. (Quick & dirty.)
extend_to_frontier r p q =
  let m = p `midpoint` q
      l = m `dist` q
      d = q |- m
      s = 2 * r / l
  in (m |+ s |* d, m |+ (-s) |* d)

-- rotate a plane vector 90 degrees counterclockwise, resulting in a vector
-- perpendicular to that given
perp u = vec3 (-(y u)) (x u) 0

-- These matrices are in ROW MAJOR ORDER

frame_matrix pos up forward =
  let (uu, uf) = (unit up, unit forward)
      un = uu |>< uf
  -- in the OpenGL coordinate system, the initial camera position is at the
  -- origin looking along the -z axis.  If this function is called with
  -- pos = <0,0,0>, forward = <0,0,1> (i.e., "toward the camera") and
  -- up = <0,1,0> (canonical for OpenGL) then it will return the identity
  -- matrix.  This motivates the sense of the cross product above.
  in V $ V.fromListN 16 [x un, x uu, x uf, x pos,
                         y un, y uu, y uf, y pos,
                         z un, z uu, z uf, z pos,
                            0,    0,    0,     1]

scale_matrix xs ys zs =
  V $ V.fromListN 16 [xs,  0,  0,  0,
                       0, ys,  0,  0,
                       0,  0, zs,  0,
                       0,  0,  0,  1]

uniform_scale_matrix s = scale_matrix s s s
identity_matrix = uniform_scale_matrix 1

scaled_frame_matrix scale pos up forward =
  let m = frame_matrix pos up forward
  in m |*| uniform_scale_matrix scale

-- a horrible implementation of matrix multiplication, suited only for
-- our limited purposes.  We work with 4x4 matrices stored as ordinary
-- 16-element vectors in row-major order.

(V a) |*| (V b) =
    V $ V.fromListN 16 [product i j | i <- [0..3], j <- [0..3]]
  where
    elt a i j = a V.! (i*4+j)
    product i j = foldl' (\p k -> p + elt a i k * elt b k j) 0 [0..3]


