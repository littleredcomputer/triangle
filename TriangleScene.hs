module TriangleScene (
  triangle_control
) where

import Data.List
import Vector
import Triangle
import SceneItem
import Text.Printf

-- We typically name the vertices of the triangle p, q, r.  Side1
-- [resp. 2, 3] is the side which does not contain point p [resp. q,
-- r], so that the sides listed in order are (qr), (pr), (pq).  Angle1
-- [resp. 2, 3] is the angle with vertex p [resp. q, r].  The angle
-- bisectors and altiudes are numbered with the angles they concern.

triangle_control = SceneControl
  ([], [])
  triangle_scene_list
  triangle_scene
  initial_triangle_state
  triangle_projection

data TriangleSceneState =
    TSState {
      wall_time::Coordinate,
      v_time::Coordinate,
      triangle :: [Vec],
      tri_facts :: TriangleFacts,
      draw::TriangleSceneState -> [SceneItem]
    }

initial_triangle = [vec3 0 0 0, vec3 1 0 0, vec3 0 1 0]
initial_triangle_state = TSState
  0.0                                -- wall_time
  0.0                                -- v_time
  initial_triangle
  (triangle_facts initial_triangle)
  draw_nothing                       --  function

triangle_projection = Orthogonal 400
draw_lines = Lines 2.5                 -- global line width

red       = RGBA 1 0 0 1
blue      = RGBA 0 0 1 1
blue2     = RGBA 0.39 0.58 0.93 1
green     = RGBA 0 1 0 1
cyan      = RGBA 0 1 1 1
yellow    = RGBA 1 1 0 1
gold      = RGBA 1 0.73 0 1
orange    = RGBA 1 0.54 0 1
magenta   = RGBA 1 0 1 1
white     = RGBA 1 1 1 1
black     = RGBA 0 0 0 1
gray      = RGBA 1 1 1 0.6

time_advance scene_time scene_duration total_time state =
  let tri1 = map ($ total_time) triangle_vertices
  in state{wall_time = total_time,
           v_time = scene_time / scene_duration,
           triangle = tri1,
           tri_facts = triangle_facts tri1}

make_cue state str = Cue (wall_time state) str
cue str scene_state =
  if v_time scene_state == 0 then [make_cue scene_state str] else []

-- f then g, as scene generating functions. %% just joins the two operations.
-- %= supplies the steady state function; %+ fades up, and %- fades down, their
-- respective right hand sides.
f %% g = (\s -> f s ++ g s)
f %= g = (\s -> f s ++ steady g s)
f %+ g = (\s -> f s ++ fade_up g s)
f %- g = (\s -> f s ++ fade_down g s)
-- XXX : the above looks a lot like a monad, as far as we know Haskell today.
-- Do the research to find out if we should be instancing that with these
-- computations. We might get "do" notation out of it, and that might be a
-- plus (?).

triangle_scene state = Scene Nothing Nothing (Group $ (draw state) state)

triangle_scene_list =
  let scg dur f = SceneGenerator dur (\s -> s{draw=f}) time_advance
  in [
    --
    -- introduction: bring up vertices and triangle sides
    --
      scg 5 $ fade_up draw_vertices
    , scg 5 $ steady draw_vertices
    , scg 8 $ fade_down draw_vertices %+ draw_sides
    , scg 1 $ steady draw_sides
    ]
    ++ present_center scg (steady draw_sides) draw_perpb
                      draw_outcenter (Just draw_outcircle)
    ++ present_center scg (steady draw_sides) draw_angb
                      draw_incenter (Just draw_incircle)

    -- we've made some progress with the complaints below.
    -- by adding a scene_setup function, we can plug the functions
    -- into the generator for a given scene once and for all and
    -- in a very nice way.  Then the state evolution is doing
    -- only the job that it needs to do. And we were able to
    -- banish the previous scene business forever.

    -- Would it have been better to handle the whole thing just by
    -- manipulating the color of the various elements, and then
    -- "drawing everything that isn't black or Nothing?" That might
    -- actually have been more sensible with our state model. Another
    -- approach might have been to just go ahead and make a big
    -- variant record of everything we could potentially draw, with
    -- its drawing level, and work with that.  Since "struct-update"
    -- notation is so horrible, we could possibly use a map or other
    -- structure. And for that matter, we could have present_center
    -- amend a basic map as its task, sparing us the effort of passing
    -- in the background draw function. Or make present_center return
    -- a function that could be composed with something else. Or
    -- something.

    -- and yet there's something to be said for the composed-function
    -- approach. It allows us to add new things without generating
    -- some huge union.

    -- something is wrong with the architecture here, though, that bears
    -- thinking about.

    -- or maybe not. in each scene, actors recite their lines. what's
    -- wrong with that?

    -- what I don't like in some sense is that the drawing is not
    -- incremental.

    -- start with a map, and modify it. The map can generate the drawing
    -- function.

    ++ [
      scg 4 $ (steady draw_sides %+ draw_sidexes),
      scg 1 $ (steady draw_sides %= draw_sidexes)
    ]
    ++ present_center scg (steady draw_sides %= draw_sidexes) draw_exangb
                      draw_excenters (Just draw_excircles)
    ++ [
      scg 2 $ steady draw_sides %- draw_sidexes,
      scg 1 $ steady draw_sides
    ]
    ++ present_center scg (steady draw_sides) draw_median
                      draw_centroid Nothing
    ++ present_center scg (steady draw_sides) (draw_together draw_alt draw_sidex)
                      draw_orthocenter Nothing
    ++ [
    --
    -- introduce the three components of the Euler line
    --
      scg 3 $  cue "begin-section"
            %= draw_sides
    , scg 1 $  cue "flash-centroid"
            %= draw_sides
            %+ draw_medians
    , scg 2.5 $  steady draw_sides
              %- draw_medians
              %+ draw_centroid
    , scg 3 $  steady draw_sides
            %= draw_centroid
    , scg 1 $  cue "flash-orthocenter"
           %= draw_sides
           %+ draw_alts
           %+ draw_sidexes
           %= draw_centroid
    , scg 2.5 $  steady draw_sides
              %- draw_alts
              %- draw_sidexes
              %+ draw_orthocenter
              %= draw_centroid
    , scg 3 $  steady draw_sides
            %= draw_orthocenter
            %= draw_centroid
    , scg 1 $  cue "flash-circumcenter"
            %= draw_sides
            %= draw_centroid
            %= draw_orthocenter
            %+ draw_perpbs
    , scg 2.5 $  steady draw_sides
              %= draw_centroid
              %= draw_orthocenter
              %- draw_perpbs
              %+ draw_outcenter
    , scg 3 $  steady draw_sides
            %= draw_centroid
            %= draw_orthocenter
            %= draw_outcenter
    , scg 2 $  cue "eulerline"
            %= draw_sides
            %+ draw_eulerline
            %= draw_centroid
            %= draw_orthocenter
            %= draw_outcenter
    , scg 5 $  steady draw_sides
            %= draw_eulerline
            %= draw_centroid
            %= draw_orthocenter
            %= draw_outcenter
    , scg 5 $  steady draw_sides
            %- draw_eulerline
            %- draw_centroid
            %- draw_orthocenter
            %- draw_outcenter
    --
    -- introduce he components of the 9-point circle
    --
    , scg 1 $  cue "begin-section"
            %= draw_sides
    ] ++ present_points scg draw_nothing draw_midpoint ++ [
      scg 5 $  steady draw_sides
            %= draw_midpoints
            %+ draw_side_bisectors
    ] ++ present_points scg
                        (steady draw_midpoints %= draw_side_bisectors)
                        draw_altfoot ++
    [ scg 5 $  steady draw_sides
            %= draw_side_bisectors
            %+ draw_sidexes
            %+ draw_alts
            %= draw_altfeet
            %= draw_midpoints
    ] ++ present_points scg
                        (steady draw_side_bisectors %= draw_sidexes %=
                         draw_alts %= draw_altfeet %= draw_midpoints)
                        (draw_together draw_orthomidpt draw_orthomidpt_tic) ++
    [ scg 5 $  steady draw_sides
            %- draw_side_bisectors
            %= draw_sidexes
            %= draw_alts
            %= draw_altfeet
            %= draw_midpoints
            %= draw_orthomidpt_tics
            %= draw_orthomidpts
    , scg 5 $  steady draw_sides
            %- draw_alts
            %- draw_sidexes
            %= draw_altfeet
            %= draw_midpoints
            %- draw_orthomidpt_tics
            %= draw_orthomidpts
    , scg 2 $  steady draw_sides
            %= draw_altfeet
            %= draw_midpoints
            %= draw_orthomidpts
    , scg 5 $  cue "9pt-circle"
            %= draw_sides
            %+ draw_9ptcircle
            %= draw_altfeet
            %= draw_midpoints
            %= draw_orthomidpts
    , scg 8 $  steady draw_sides
            %= draw_9ptcircle
            %= draw_altfeet
            %= draw_orthomidpts
            %= draw_midpoints
    , scg 8 $  steady draw_sides
            %= draw_9ptcircle
            %- draw_altfeet
            %- draw_orthomidpts
            %- draw_midpoints
    , scg 8 $  cue "begin-section"
            %= draw_sides
            %= draw_9ptcircle
            %+ draw_incircle
    , scg 5 $  steady draw_sides
            %= draw_9ptcircle
            %= draw_incircle
    , scg 8 $  steady draw_sides
            %=  draw_9ptcircle
            %= draw_incircle
            %+ draw_sidexes
    , scg 8 $  steady draw_sides
            %= draw_9ptcircle
            %= draw_incircle
            %= draw_sidexes
            %+ draw_exangbs
            %+ draw_excenters
            %+ draw_excircles
    , scg 8 $  steady draw_sides
            %= draw_9ptcircle
            %= draw_incircle
            %= draw_excircles
            %- draw_sidexes
            %- draw_exangbs
            %- draw_excenters
            %+ draw_exc_tangents
            %+ draw_inc_tangent
    , scg 5 $  steady draw_sides
            %= draw_9ptcircle
            %= draw_incircle
            %= draw_excircles
            %= draw_exc_tangents
            %= draw_inc_tangent
    , scg 2 $  cue "begin-section"
            %= draw_sides
            %= draw_9ptcircle
            %- draw_incircle
            %- draw_excircles
            %- draw_exc_tangents
            %- draw_inc_tangent
            %+ draw_9ptcenter
    , scg 3 $  cue "flash-centroid"
            %= draw_sides
            %+ draw_medians
            %= draw_9ptcircle
            %= draw_9ptcenter
    , scg 3 $  cue "flash-orthocenter"
            %= draw_sides
            %- draw_medians
            %+ draw_centroid
            %+ draw_alts
            %+ draw_sidexes
            %= draw_9ptcircle
            %= draw_9ptcenter
    , scg 3 $  cue "flash-circumcenter"
            %= draw_sides
            %= draw_centroid
            %+ draw_orthocenter
            %- draw_alts
            %- draw_sidexes
            %+ draw_perpbs
            %= draw_9ptcircle
            %= draw_9ptcenter
    , scg 3 $  steady draw_sides
            %= draw_centroid
            %- draw_perpbs
            %= draw_orthocenter
            %= draw_centroid
            %+ draw_outcenter
            %= draw_9ptcircle
            %= draw_9ptcenter
    , scg 2 $  cue "eulerline"
            %= draw_sides
            %= draw_9ptcircle
            %+ draw_eulerline
            %= draw_outcenter
            %= draw_centroid
            %= draw_orthocenter
            %= draw_9ptcenter
    , scg 8 $  steady draw_sides
            %= draw_9ptcircle
            %= draw_eulerline
            %= draw_centroid
            %= draw_orthocenter
            %= draw_outcenter
            %= draw_9ptcenter
    , scg 8 $  steady draw_sides
            %= draw_9ptcircle
            %= draw_eulerline
            %- draw_centroid
            %- draw_orthocenter
            %- draw_outcenter
            %= draw_9ptcenter
    , scg 12 $  cue "begin-section"
             %= draw_sides
             %= draw_eulerline
             %+ draw_alts
             %+ draw_medians
             %+ draw_angbs
             %+ draw_exangbs
             %+ draw_incenter
             %+ draw_sidexes
             %+ draw_perpbs
             %+ draw_outcenter
             %+ draw_outcircle
             %+ draw_orthomidpt_tics
             %+ draw_exc_tangents
             %+ draw_incircle
             %+ draw_excircles
             %= draw_9ptcircle
             %+ draw_orthomidpts
             %+ draw_inc_tangent
             %+ draw_excenters
             %= draw_9ptcenter
    , scg 12 $ cue "begin-section"
             %= draw_sides
             %- draw_eulerline
             %- draw_alts
             %- draw_medians
             %- draw_angbs
             %- draw_exangbs
             %- draw_incenter
             %- draw_sidexes
             %- draw_perpbs
             %- draw_outcenter
             %- draw_outcircle
             %- draw_orthomidpts
             %- draw_orthomidpt_tics
             %- draw_exc_tangents
             %- draw_excenters
             %- draw_incircle
             %- draw_excircles
             %- draw_inc_tangent
             %- draw_9ptcircle
             %- draw_9ptcenter
    , scg 5 $ fade_down draw_sides %+ draw_vertices
    , scg 5 $ fade_down draw_vertices
    , scg 0.1 $ cue "end" %% draw_nothing
    ]
  where
    present_center scg bg line center maybe_circle = [
        -- fade up lines that fix the center
          scg 12 $ bg %% cue "begin-section"
        , scg  1 $ bg %% cue "center-line 0" %+ line 0
        , scg  3.2 $ bg %= line 0
        , scg  1 $ bg %% cue "center-line 1" %= line 0 %+ line 1
        , scg  3.2 $ bg %= line 0 %= line 1
        , scg  1 $ bg %% cue "center-line 2" %= line 0 %= line 1 %+ line 2
        , scg  4.2 $ bg %= line 0 %= line 1 %= line 2
        -- now fade up the center itself
        , scg  1 $ bg %% cue "center-point" %= line 0 %= line 1 %= line 2 %+ center
        -- if there's a corresponding circle, fade it up; in any case,
        -- fade the construction lines down.
        , scg  3.2 $ bg %= line 0 %= line 1 %= line 2 %= center
        , scg  6 $ bg %- line 0 %- line 1 %- line 2 %= center
                       %% maybe (steady draw_sides) (\c ->
                            cue "center-circle" %= draw_sides %+ c) maybe_circle
        -- fade down center and circle
        , scg 5 $ bg %- center
                     %% maybe (steady draw_sides) (\c ->
                          steady draw_sides %- c) maybe_circle]
    present_points scg background point = [
        scg 0.8 $ cue "point 0" %= draw_sides %% background %+ point 0
      , scg 0.8 $ cue "point 1" %= draw_sides %% background %= point 0 %+ point 1
      , scg 0.8 $ cue "point 2" %= draw_sides %% background %= point 0 %= point 1 %+ point 2]

circular_motion center radius freq t =
  let θ = 2 * pi * freq * t
  in center |+ radius |* vec3 (cos θ) (sin θ) 0

triangle_vertices = [circular_motion (vec3 0.8 0.3 0.0) 0.5 0.035,
                     circular_motion (vec3 (-0.5) 0.7 0.0) 0.3 0.025,
                     circular_motion (vec3 (-0.8) (-0.3) 0.0) 0.4 0.055]

faraway = extend_to_frontier 4
fade_color fade (RGBA r g b a) = Just $ Color (RGBA r g b (a*fade))
circle color fade (Triangle.Circle c r) =
  [SceneItem.Circle c r $ fade_color color fade]

point_radius = 0.025
point fade color p =
  [Disk p point_radius $ fade_color fade black,
   SceneItem.Circle p point_radius $ fade_color fade color]
points fade color = concatMap (point fade color)

draw_all_three f fade = f 0 fade %% f 1 fade %% f 2 fade
draw_together f g n fade state = f n fade state ++ g n fade state
perpendicular center p notch_point fade color =
   [draw_lines [faraway center notch_point] $ fade_color fade color,
   perp_notch 0.05 p notch_point $ fade_color fade color]
perp_bisector n center p q notch_point fade color =
  perpendicular center p notch_point fade color ++
    [bisector_tics (n+1) 0.05 p q $ fade_color fade color]

draw_nothing _         = []
draw_angbs             = draw_all_three draw_angb
draw_exangbs           = draw_all_three draw_exangb
draw_perpbs            = draw_all_three draw_perpb
draw_medians           = draw_all_three draw_median
draw_alts              = draw_all_three draw_alt
draw_sidexes           = draw_all_three draw_sidex
draw_side_bisectors    = draw_all_three $ draw_side_bisector red
draw_altfeet           = draw_all_three draw_altfoot
draw_orthomidpts       = draw_all_three draw_orthomidpt
draw_orthomidpt_tics   = draw_all_three draw_orthomidpt_tic
draw_midpoints         = draw_all_three draw_midpoint
draw_vertices     fade = points fade white . triangle
draw_incenter     fade = point fade green . center . in_circle . tri_facts
draw_incircle     fade = circle fade green . in_circle . tri_facts
draw_9ptcircle    fade = circle fade gold . nine_pt_circle . tri_facts
draw_9ptcenter    fade = point fade gold . center . nine_pt_circle . tri_facts
draw_outcircle    fade = circle fade cyan . circum_circle . tri_facts
draw_centroid     fade = point fade magenta . centroid . tri_facts
draw_orthocenter  fade = point fade yellow . orthocenter . tri_facts
draw_exc_tangents fade = points fade orange . excircle_tangents . tri_facts
draw_inc_tangent  fade = point fade blue . inc_tangent . tri_facts
draw_outcenter    fade = point fade cyan . center . circum_circle . tri_facts
draw_excircles    fade = concatMap (circle fade red) . excircles . tri_facts
draw_excenters    fade = points fade red . excenters . tri_facts
draw_altfoot    n fade = point fade yellow . (!!n) . altitudes . tri_facts
draw_midpoint   n fade = point fade red . (!!n) . midpoints . tri_facts
draw_orthomidpt n fade scene_state =
  let f = tri_facts scene_state
  in point fade orange $ (vertices f !! n) `midpoint` (orthocenter f)
draw_perpb n fade scene_state =
  let t = vertices . tri_facts $ scene_state
      m = midpoints . tri_facts $ scene_state
      outc = center . circum_circle . tri_facts $ scene_state
  in perp_bisector n outc (t!!n) (t!! ((n+1) `mod` 3)) (m!!n) fade cyan
draw_orthomidpt_tic n fade scene_state =
  let t@[p,q,r] = vertices . tri_facts $ scene_state
      orc = orthocenter . tri_facts $ scene_state
  in [bisector_tics (n+1) 0.05 (t!!n) orc $ fade_color fade orange]
draw_exangb = draw_angle_bisectors red exangle_bisectors
draw_angb = draw_angle_bisectors green angle_bisectors
draw_angle_bisectors color f n fade scene_state =
  let t = triangle scene_state
      b = f (tri_facts scene_state)
  in [draw_lines [faraway (t!!n) (b!!n)] $ fade_color fade color]
draw_median n fade scene_state =
  let t = vertices . tri_facts $ scene_state
      m = midpoints . tri_facts $ scene_state
  in [draw_lines [faraway (t!! ((n+2) `mod` 3)) (m!!n)] $ fade_color fade magenta]
     ++ draw_side_bisector magenta n fade scene_state
draw_alt n fade scene_state =
  let t = vertices . tri_facts $ scene_state
      a = altitudes . tri_facts $ scene_state
  in perpendicular (t!!n) (t!!n) (a!!n) fade yellow
draw_sidex n fade scene_state =
  let t = vertices . tri_facts $ scene_state
  in [draw_lines [faraway (t!! ((n+1) `mod` 3)) (t!! ((n+2) `mod` 3))]
            $ fade_color fade gray]
draw_eulerline fade scene_state =
  let orc = orthocenter . tri_facts $ scene_state
      outc = center . circum_circle . tri_facts $ scene_state
  in [draw_lines [faraway orc outc] $ fade_color fade blue2]
draw_side_bisector color n fade scene_state =
  let t = vertices . tri_facts $ scene_state
  in [bisector_tics (n+1) 0.05 (t!!n) (t!! ((n+1) `mod` 3))
      $ fade_color fade color]
draw_sides fade scene_state =
  let [p,q,r] = triangle scene_state
      area = 0.5 * norm ((q |- p) |>< (r |- p))
  in [make_cue scene_state (printf "triangle-area %f" area),
      draw_lines [(p, q), (q, r), (r, p)] $ fade_color fade white]

fade_up draw state = draw (v_time state) state
fade_down draw state = draw (1.0 - (v_time state)) state
steady draw = draw 1.0

-- return data for a "perpendicularity notch" of side length l for the
-- line (pq) at the point q.
perp_notch l p q =
  let u = l |* unit (q |- p)
      p0 = q |- u
      p1 = p0 |+ perp u
      p2 = p1 |+ u
  in draw_lines [(p0, p1), (p1, p2)]

-- return data for n "bisector tics" of length l for the line pq
bisector_tics n l p q =
  let u = (l/2) |* unit (q |- p)
      tic_sep = 0.6
      tics = [(2 * tic_sep * realToFrac t
               - (tic_sep * (realToFrac n-1))) | t <- [0..n-1]]
  in draw_lines $ (make_tics 0.25 u tics) ++ (make_tics 0.75 u tics)
  where cc = convex_combination p q
        make_tics ratio u tics = map (\tic ->
          let p1 = cc ratio |+ tic |* u
              p2 = cc ratio |- tic |* u
          in (p1 |- perp u, p1 |+ perp u)) tics
        convex_combination p q ratio = p |+ ratio |* (q |- p)
