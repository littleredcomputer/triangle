-- triangle.hs
-- geometric facts about triangles

module Triangle (
  TriangleFacts(..),
  Circle(..),
  triangle_facts,
) where

import Vector
import SceneItem

data Circle =
    Circle {
      center :: Vec,
      radius :: Coordinate
    }

data TriangleFacts =
    TriangleFacts {
      vertices :: [Vec],
      midpoints :: [Vec],
      in_circle :: Circle,
      circum_circle :: Circle,
      centroid :: Vec,
      orthocenter :: Vec,
      excircles :: [Circle],
      nine_pt_circle :: Circle,
      excircle_tangents :: [Vec],
      inc_tangent :: Vec,
      altitudes :: [Vec],
      angle_bisectors :: [Vec],
      exangle_bisectors :: [Vec],
      excenters :: [Vec]
    }

triangle_facts [p, q, r] =
  let a = dist q r                  -- length of side opposite p
      b = dist p r                  -- length of side opposite q
      c = dist p q                  -- length of side opposite r
      pm = a + b + c                -- perimeter
      s = pm / 2                    -- semiperimeter
      in_radius = sqrt((s-a)*(s-b)*(s-c)/s)   -- radius of incircle
      in_center = ((a / pm) |* p)
                   |+ ((b / pm) |* q)
                   |+ ((c / pm) |* r)
      circum_radius = (a * b * c) / (4*sqrt(s*(a+b-s)*(a+c-s)*(b+c-s)))
      centroid = line_intersection p (q `midpoint` r) q (p `midpoint` r)
      orthocenter = ortho_c p q r
      circumcenter = circum_c p q r
      nine_pt_center = orthocenter `midpoint` circumcenter
      nine_pt_radius = nine_pt_center `dist` (p `midpoint` q)
      nine_pt_circle = Triangle.Circle nine_pt_center nine_pt_radius
      altitudes = [point_on_line_nearest_to p q r,
                   point_on_line_nearest_to q p r,
                   point_on_line_nearest_to r p q]
      exangle_bisectors = [xbp, xbq, xbr]
      angle_bisectors = [angle_bisector q p r,
                         angle_bisector p q r,
                         angle_bisector p r q]

      -- exterior angle bisectors
      xbp = angle_bisector (2 |* p |- r) p q
      xbq = angle_bisector (2 |* q |- p) q r
      xbr = angle_bisector (2 |* r |- q) r p

      -- excenters
      xp = line_intersection q xbq r xbr
      xq = line_intersection r xbr p xbp
      xr = line_intersection p xbp q xbq
      excenters = [xp, xq, xr]
      excircles = [excircle xp p q, excircle xq q r, excircle xr r p]

      excircle_tangents = map (circle_tangent nine_pt_center) excircles
      incircle = Triangle.Circle in_center in_radius
      inc_tangent = circle_tangent in_center nine_pt_circle
  in TriangleFacts [p, q, r]
                   [p `midpoint` q, q `midpoint` r, r `midpoint` p]
                   incircle
                   (Triangle.Circle circumcenter circum_radius)
                   centroid
                   orthocenter
                   excircles
                   nine_pt_circle
                   excircle_tangents
                   inc_tangent
                   altitudes
                   angle_bisectors
                   exangle_bisectors
                   excenters
  where
    circum_c p q r =
      let [a1,a2] = perpendicular_bisector p q
          [b1,b2] = perpendicular_bisector q r
      in line_intersection a1 a2 b1 b2
    ortho_c p q r =
      let a1 = point_on_line_nearest_to p q r
          a2 = point_on_line_nearest_to q r p
      in line_intersection p a1 q a2
    excircle c p q =
      let rad = c `dist` point_on_line_nearest_to c p q
      in Triangle.Circle c rad
    circle_tangent c (Triangle.Circle center radius) =
      let u = unit $ c |- center
      in center |+ radius |* u

-- find the point on the line spanned by (vw) nearest to p
point_on_line_nearest_to p v w =
  let u = w |- v
      t = ((p |- v) |. u) / (u |. u)
  in v |+ (t |* u)

-- considering (pqr) as an angle with vertex q, return a point lying
-- on the angle bisector
angle_bisector p q r =
  let u = q |+ (unit (p |- q) |+ unit (r |- q))
  in line_intersection q u p r




