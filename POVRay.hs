{-# LANGUAGE FlexibleInstances #-}

module POVRay (
  povray_main
) where

import Vector

import SceneItem

import Data.Array
import Data.Time
import System.IO
import System.Environment(getArgs)
import System.Console.GetOpt
import System.Exit
import Text.Printf
import Text.PrettyPrint.HughesPJ

data Resource =
    Resource

comp_geo = "compiled_geometry.inc"

options = [
  Option ['f'] ["fps"] (ReqArg (\rate f -> f{frame_rate=read rate}) "FLOAT")
         "frames per second"
  ]

data Flags a = Flags {frame_rate :: Float}

flag_defaults = Flags 29.98

povray_main scene_control = do
    args <- getArgs

    flags <- case getOpt RequireOrder options args of
                  (funcs,n,[]) -> return (foldl (\flags func -> func flags)
                                                flag_defaults funcs)
                  (_,_,errs) -> ioError (userError (concat errs))
    t0 <- getCurrentTime

    let (objects_to_compile, texture_files) = static_objects scene_control
    let (Group o1, o2, _) = compile_object (Group objects_to_compile) ([], [0..])
    let texs = (flip map) texture_files (\tf ->
          block pigment . block image_map . (tga <+>) . doubleQuotes . text $ tf)

    withFile comp_geo WriteMode (\file -> do
      emit_objects file obj_id_stem . map to_doc . reverse $ o2
      emit_objects file c_obj_id_stem . map to_doc $ o1
      emit_objects file texture_stem $ texs)

    let frame_interval = recip (frame_rate flags)
    let frames = zip [(0::Int)..] $
                 generate_frames frame_interval scene_control id
    mapM_ (display t0) frames

  where
    emit_objects file stem = hPutStrLn file . show . vcat . declare_objects stem

display t0 (ix, (time, frame)) = do
  t1 <- getCurrentTime
  let elapsed_time = realToFrac (diffUTCTime t1 t0)
  let fps = (fromIntegral ix + 0::Double) / elapsed_time
  printf "f%06d %.3g %.2g fps\n" ix time fps
  withFile (printf "f%06d.pov" ix) WriteMode (\file -> do
    hPutStrLn file $ "#include \"" ++ comp_geo ++ "\""
    hPutStrLn file $ "camera { location 3*z look_at 0 }"  -- XXX ! XXX !
    hPutStrLn file . show . to_doc $ frame)

-- ------------------------------------------------------------------------
--
-- POV-Ray translation code.
--
-- We implement translation to the POV-Ray scene description language
-- as a form of Pretty Printing using Haskell's standard library for
-- this purpose.  Each of our scene types is made an instance of
-- PrettyPrintable.

class PrettyPrintable t where
  to_doc :: t -> Doc

-- Some POV-Ray output grammar
angle_brackets p = char '<' <> p <> char '>'
angle            = text "angle"
camera           = text "camera"
color            = text "color"
comment p        = text "/*" <+> p <+> text "*/"
cylinder         = text "cylinder"
declare          = text "#declare"
face_indices     = text "face_indices"
image_map        = text "image_map"
light_source     = text "light_source"
location         = text "location"
look_at          = text "look_at"
matrix           = text "matrix"
mesh             = text "mesh"
mesh2            = text "mesh2"
normal_vectors   = text "normal_vectors"
object           = text "object"
perspective      = text "perspective"
pigment          = text "pigment"
rgbf             = text "rgbf"
rotate v         = text "rotate" <+> to_doc v
sphere r tex x   = block (text "sphere") (
                     to_doc x <> comma <+> to_doc r $$ to_doc tex)
texture          = text "texture"
tga              = text "tga"
torus            = text "torus"
translate v      = text "translate" <+> to_doc v
triangle         = text "triangle"
union            = text "union"
up               = text "up"
uv_vectors       = text "uv_vectors"
vertex_vectors   = text "vertex_vectors"

obj_id_stem      = text "o"
c_obj_id_stem    = text "co"
texture_stem     = text "tex"

instance PrettyPrintable Double where
  to_doc = double

instance PrettyPrintable Float where
  to_doc = float

instance PrettyPrintable Vec where
  to_doc = angle_brackets . hcat . punctuate comma . map to_doc . as_list

instance PrettyPrintable (Maybe Texture) where
  to_doc (Just t) = POVRay.texture <+> (braces $
    case t of
      Color c -> pigment <+> braces (POVRay.color <+> to_doc c)
                 <+> text "finish { ambient rgb 0.3 }" -- XXX
      TextureID i -> pigment <+> braces (texture_stem <> int i)
                     <+> text "finish { ambient rgb 0.3 }") -- XXX
  to_doc Nothing = POVRay.texture <+> (braces $ text "pigment{rgb 1}")

instance PrettyPrintable RGBA where
  to_doc (RGBA r g b a) =
    rgbf <+> angle_brackets (hcat . punctuate comma . map to_doc $ [r,g,b,1-a])

instance PrettyPrintable Scene where
  to_doc (Scene lights camera action) =
    to_doc lights $$ to_doc camera $$ to_doc action

instance PrettyPrintable (Maybe Camera) where
  to_doc (Just c) = to_doc c
  to_doc Nothing = empty

instance PrettyPrintable (Maybe [Light]) where
  to_doc (Just l) = to_doc l
  to_doc Nothing = comment $ text "default lighting goes here" -- XXX establish

instance PrettyPrintable t => PrettyPrintable [t] where
  to_doc = vcat . map to_doc

instance PrettyPrintable Camera where
  to_doc (Camera c_position target up a) = block POVRay.camera (
    perspective <+> angle <+> to_doc a $$
    location <+> to_doc c_position $$
    look_at <+> to_doc target     $$
    POVRay.up <+> to_doc up)

instance PrettyPrintable Light where
  to_doc (Light pos col) =
    light_source <+> lbrace <+> to_doc pos <> comma <+>
    POVRay.color <+> to_doc col <+> rbrace

-- this pretty printer for the general SceneItem also "compiles out"
-- nested matrix transformations, leaving the composite transformation
-- available for association with each object.

instance PrettyPrintable SceneItem where
  to_doc = to_doc_withmatrix identity_matrix

to_doc_withmatrix m s =
  case s of
    Group l -> block union $ vcat . map (to_doc_withmatrix m) $ l
    TriangulatedSurface s -> block mesh $ vcat (map triangle_face s)
    TriangulatedSurface2 s i -> block mesh2 $
      block vertex_vectors (vector_list $ map v s) $$
      block normal_vectors (vector_list $ map nor s) $$
      block uv_vectors     (vector_list $ map tex s) $$
      block face_indices ((int $ length i `div` 3) <> comma $$ face_triples i)
    Actor xform i -> to_doc_withmatrix (m |*| xform) i
    CompiledGeometry (Reference o) -> block object $ text o $$ povray_matrix m
    CompiledObject i tex -> block object (
      c_obj_id_stem <> int i $$
      to_doc tex $$
      povray_matrix m)
    Lines width ls tex -> block union $
      -- XXX honor width here
      vcat . map (\(p,q) ->
        block cylinder (
          to_doc p <> comma <+> to_doc q <+> comma <+> text "0.01" $$
          to_doc tex $$
          povray_matrix m)) $ ls
    Points size ps tex -> block union $
      vcat . map (sphere (size / 1000.0) tex) $ ps
      -- XXX add povray_matrix here
    Circle c r tex -> block torus $
      to_doc r <> comma <+> text "0.01" $$
      rotate (vec3 90 0 0) <+> translate c $$
      to_doc tex $$
      povray_matrix m
    Cue time msg -> comment $ text "CUE" <+> to_doc time <+> text msg

    otherwise -> comment . text . show $ s
  where
    face_triples [] = empty
    face_triples (a:b:c:ds) =
      (angle_brackets . hcat . punctuate comma . map (int . fromIntegral) $
        [a,b,c]) <> comma $$
      face_triples ds
    vector_list vs =
        (int . length $ vs) <> comma $$ vcat (map ((<> comma) . to_doc) vs)
    triangle_face t =
      triangle <+> lbrace <+> to_doc (v0 t) <> comma
                          <+> to_doc (v1 t) <> comma
                          <+> to_doc (v2 t) <+> rbrace

block block_name contents =
  block_name <+> lbrace $$ (nest 2 contents) $$ rbrace

declare_objects name =
  map (declare_object name) . zip [0..]
declare_object name (id, obj) =
  declare <+> name <> int id <+> equals $$ nest 2 obj

compile_object obj (compiled_objs, ix:ixs) =
  case obj of
    TriangulatedSurface _ ->
      let co = obj_id_stem <> int ix
      in (CompiledGeometry . Reference . show $ co, obj:compiled_objs, ixs)
    TriangulatedSurface2 _ _ ->
      let co = obj_id_stem <> int ix
      in (CompiledGeometry . Reference . show $ co, obj:compiled_objs, ixs)
    Actor m o ->
      let (co, xos, new_ixs) = compile_object o (compiled_objs, ix:ixs)
      in (Actor m co, xos, new_ixs)
    Group g ->
      let (comp_g, final_xos, final_ixs) = foldr
            (\item (gl, xos, ix:ixs) ->
              let (co, new_xos, new_ixs) = compile_object item (xos, ix:ixs)
              in (co:gl, new_xos, new_ixs))
            ([], compiled_objs, ix:ixs) g
      in (Group comp_g, final_xos, final_ixs)
    otherwise -> (obj, compiled_objs, (ix:ixs))

-- The POVRay matrix acts on vertices by post-multiplication, i.e. y = xM,
-- so we need the transpose of the transformation matrix we work with (which
-- uses the model y = Mx).  The transposed matrix's 4th column is assumed
-- to be <0,0,0,1>, so a POVRay matrix only has 12 entries.  This function
-- selects the correct 12 entries and arranges them in transpose order.
povray_matrix m =
  let v = map (to_doc . vec_index m) [0,4,8, 1,5,9, 2,6,10, 3,7,11]
  in POVRay.matrix <+> angle_brackets (hcat . punctuate comma $ v)

