module SceneItem (
  SceneGenerator(..),
  SceneItem(..),
  GeometryReference(..),
  SceneControl(..),
  Scene(..),
  Light(..),
  Camera(..),
  Texture(..),
  RGBA(..),
  TriangleN1T2D(..),
  VertexNT2D(..),
  StateEvolver,
  Projection(..),
  actor_from_frame,
  scaled_actor_from_frame,
  frame_matrix,
  generate_frames
) where

import Vector
import Data.Word

-- XXX doesn't this look a little bit like a monad?  How should we wire it
-- into the framework as such?
type StateEvolver s = Coordinate -> Coordinate -> Coordinate
                      -> s -> s

data SceneGenerator s =
    SceneGenerator {
      duration::Coordinate,
      state_setup::s -> s,
      state_evolve::StateEvolver s
    }

data Camera =
    Camera {
      c_position::Vec,
      target::Vec,
      up::Vec,
      perspective_angle::Float   -- XXX: this is shared with Projection
    }
    deriving Show

data Scene =
    Scene {
      lights::Maybe [Light],
      camera::Maybe Camera,
      action::SceneItem
    }
    deriving Show

data RGBA =
    RGBA {
      r::Coordinate,
      g::Coordinate,
      b::Coordinate,
      a::Coordinate
    }
    deriving Show

data Light =
    Light {
      l_position::Vec,
      color::RGBA
    }
    deriving Show

data Texture =
    Color RGBA
  | TextureID Int
  deriving Show

data SceneControl s =
    SceneControl {
      static_objects::([SceneItem], [String]),
      scene_list::[SceneGenerator s],
      scene_builder::s -> Scene,
      initial_state::s,
      projection::Projection
    }

{- possible roadmap here: a triangulated surface unifies both
   kinds of TS we have today. We have a list of flat triangles,
   and an indexed list of vertices.

   We don't store normals with the flat triangles. We compute
   them when we build the VBOs on the OpenGL side, and just
   ignore them on the POVRay side.  We can also (maybe) do
   index compression on the OpenGL side ...

   Or maybe, better yet, we just convert to the indexed format
   in both cases, always pre-computing and then supplying normals
   (wastefully, perhaps, in the POV case, but who cares).
-}

-- a triangle represented as 3 vertices. Each vertex is given 2D
-- texture coordinates (T2D) and the three vertices share one normal
-- vector (N1).

data TriangleN1T2D =
    TriangleN1T2D {
      nor0:: Vec, tex0:: Vec, v0:: Vec,
                  tex1:: Vec, v1:: Vec,
                  tex2:: Vec, v2:: Vec
    }
  deriving Show

data VertexNT2D =
    VertexNT2D {
      nor::Vec,
      tex::Vec,
      v::Vec
    }
  deriving Show

data GeometryReference =
    Action (IO ())
  | Reference String

data SceneItem =
    Lines Coordinate [(Vec, Vec)] (Maybe Texture)
  | Circle Vec Coordinate (Maybe Texture)
  | Disk Vec Coordinate (Maybe Texture)
  | Points Float [Vec] (Maybe Texture)
  | String [Char] (Maybe Texture)
  | TriangulatedSurface {
      triangles::[TriangleN1T2D]
    }
  | TriangulatedSurface2 {
      surface2::[VertexNT2D],
      -- for efficient filling of vertex buffer objects in OpenGL, we
      -- need indices to be [Word32] and not [Int]. If it were [Int],
      -- then on 64-bit platforms we would have to copy the entire
      -- array into a [Word32] array before creating the VBO.  This is
      -- a little bit of backend dependency, but worthwhile in terms
      -- of performance; since indices are always nonnegative and 4
      -- billion vertices is more than we are likely to render, this
      -- choice doesn't have a realistic downside at this writing
      indices::[Word32]
    }
  | CompiledGeometry GeometryReference

  | CompiledObject Int (Maybe Texture)
  | Sphere {  -- m, n: small names to expose from a module. reconsider XXX
      radius::Double,
      m::Int,
      n::Int
    }
  | Actor {
      matrix::Vec,
      item::SceneItem
    }
  | Group {
      items::[SceneItem]
    }
  | Cue Coordinate String
  deriving Show

instance Show GeometryReference where
  show x = case x of
    Action a -> "{-compiled drawing action-}"
    Reference s -> s

data Projection =
    Orthogonal {pixels_per_unit::Double}
  | Perspective {
      view_angle::Double,
      near_dist::Double,
      far_dist::Double
    }

actor_from_frame position up forward =
  Actor $ frame_matrix position up forward

scaled_actor_from_frame scale position up forward =
  Actor $ scaled_frame_matrix scale position up forward

generate_frame_times dt =
  let ft x = x : (ft $ x + dt)
  in ft 0.0

generate_frames frame_interval scene_control scene_cycler =
  let step [] _ _ = []
      step (generator:generators) times state =
        let (times', state', frames) = generate_scene_frames
                                         generator
                                         scene_control
                                         times
                                         state
        in frames ++ step generators times' state'

  in step (scene_cycler $ scene_list scene_control)
          (generate_frame_times frame_interval)
          (initial_state scene_control)

generate_scene_frames scene scene_control frame_times scene_state =
  let
      dur = duration scene
      t0 = head frame_times
      next t state = (state_evolve scene) (t-t0) dur t state
      frame state = ((scene_builder scene_control) state)
      step (time:times) scene_state frames =
        let state' = next time scene_state
        in if (time - t0) >= dur
           then (time:times, state', frames)
           else step times state' ((time, frame state') : frames)
      (times', state', frames) = step frame_times
                                      (state_setup scene $ scene_state)
                                      []
  in (times', state', reverse frames)

