{-# LANGUAGE TypeSynonymInstances #-}

module OpenGL (
  opengl_main
) where

-- References to [OSB] in this code are to:
--   Richard S. Wright et al. "OpenGL SuperBible" Fourth Ed. 2007
--

import Graphics.UI.GLUT as GLUT

import Vector
import Targa

import SceneItem

import Data.List
import Data.Time
import Data.Array
import Data.Array.Storable
import Control.Monad(forM_, (>=>))

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Internal(toForeignPtr)
import Foreign.Ptr(plusPtr,nullPtr)
import Foreign.ForeignPtr(withForeignPtr)
import Foreign.Storable(sizeOf)

import System.IO
import System.Console.GetOpt
import System.Exit
import Text.Printf

options = [
  Option ['w'] ["write-frames"] (NoArg (\f -> f{write_frames=True}))
         "write frames to disk",
  Option ['c'] ["cycle"] (NoArg (\f -> f{cycle_scenes=cycle}))
         "cycle the scene list endlessly",
  Option ['q'] ["cue-file"] (ReqArg (\file f -> f{cue_filename=Just file}) "FILE")
         "cue file",
  Option ['f'] ["fps"] (ReqArg (\rate f -> f{frame_rate=read rate}) "FLOAT")
         "frames per second",
  Option ['n'] ["no-delay"] (NoArg (\f -> f{no_delay=True}))
         "render without interframe delay"
  ]

data Flags a =
    Flags {
      write_frames :: Bool,
      cycle_scenes :: a -> a,
      frame_rate :: Float,
      no_delay :: Bool,
      cue_filename :: Maybe String,
      cue_file :: Handle
    }

flag_defaults = Flags False id 29.98 False Nothing stdout

sizeof_coordinate = sizeOf (0.0::Coordinate)

-- work ideas:

-- - experiment with fog

-- - x,y coordinate grid (very faint)
-- - experiment with "hired" performers, z-falling segments that
-- - grow dimmer.  should display lists evolve themselves?

data Resource =
    Resource {
      textures::Array Int TextureObject,
      bitmaps::Array Int (Int, Int, BL.ByteString),
      compiled_objects::Array Int SceneItem
    }

opengl_main scene_control =
  let dvd43 = Size 720 480
      hd720p = Size 1280 720
  in do
    (_, args) <- getArgsAndInitialize

    flags0 <- case getOpt RequireOrder options args of
                   (funcs,n,[]) -> return (foldl (\flags func -> func flags)
                                                 flag_defaults funcs)
                   (_,_,errs) -> ioError (userError (concat errs))

    cue_handle <- case (cue_filename flags0) of
      Nothing -> return stdout
      Just filename -> openFile filename WriteMode

    let flags = flags0{cue_file=cue_handle}

    initialWindowSize $= hd720p
    initialDisplayMode $= [RGBAMode, WithAlphaComponent, DoubleBuffered,
                           WithDepthBuffer]

    window <- createWindow "Hello World"
    -- depthBufferDepth $= 16
    -- (get glExtensions) >>= mapM_ putStrLn


    pointSmooth     $= Enabled
    lineSmooth      $= Enabled
    polygonSmooth   $= Enabled
    lineWidth       $= 2
    hint PolygonSmooth $= Nicest
    cullFace        $= Just Back
    frontFace       $= CCW

    blend           $= Enabled
    --depthFunc       $= Just Less
    depthFunc       $= Just Lequal
    blendFunc       $= (SrcAlpha, OneMinusSrcAlpha)

    shadeModel      $= Smooth
    lighting        $= Enabled

    colorMaterial $= Just (Front, AmbientAndDiffuse)
    normalize $= Enabled

    -- vbo
    clientState VertexArray $= Enabled
    clientState NormalArray $= Enabled
    clientState TextureCoordArray $= Enabled

    -- version information
    forM_ [gluVersion, glVersion, shadingLanguageVersion] $ get >=> putStrLn

    let (objects_to_compile, texture_files) = static_objects scene_control

    compiled_objects <- mapM compile_object objects_to_compile

    -- *******************
    -- load texture assets
    --
    texture Texture2D $= Enabled
    textureFunction $= Modulate

    let n_tex = length texture_files
    tex_ids <- genObjectNames n_tex :: IO [TextureObject]
    forM_ (zip texture_files tex_ids) (\(file, tex_id) -> do
      (w, h, img_data) <- load_image file
      let (img_ptr, offset, len) = toForeignPtr . BL.toStrict $ img_data
      withForeignPtr img_ptr (\p -> do
        -- the following texture parameters are recommended by [OSB p. 337]
        textureBinding Texture2D $= Just tex_id
        build2DMipmaps Texture2D RGB8 w h
                       (PixelData BGR UnsignedByte (p `plusPtr` offset))
        textureFilter Texture2D $= ((Linear', Just Linear'), Linear')
        textureWrapMode Texture2D S $= (Repeated, Repeat)))
    textureBinding Texture2D $= Nothing

    let bitmaps = ["texture/earth.tga"]
    let n_bits = length bitmaps
    bitmaps <- mapM load_image bitmaps

    let resources = Resource (listArray (0,n_tex-1) tex_ids)
                             (listArray (0,n_bits-1) bitmaps)
                             (listArray (0,length compiled_objects- 1 ) $
                             compiled_objects)

    let interval = recip (frame_rate flags)
    let frames = generate_frames interval scene_control (cycle_scenes flags)

    -- maybe keep the frame times zipped with the frames, and then
    -- have the delay loop below delay until the "goal" time is reached.
    -- that would actually be cool.

    displayCallback $= return ()
    t0 <- getCurrentTime

    addTimerCallback 0 (timer (0::Int) flags t0
                        (display t0 flags resources)
                        window
                        frames)

    reshapeCallback $= (Just $ reshape (projection scene_control))
    mainLoop

timer _ flags _ _ _ [] = hClose (cue_file flags) >> exitWith ExitSuccess

timer n flags t0 display window (frame@(frame_time, f) : frames) = do
    postRedisplay (Just window)
    displayCallback $= display n frame
    delay <- if (no_delay flags) then return 0 else next_frame_time t0 frames
    addTimerCallback delay (timer (1+n) flags t0 display window frames)
  where
    next_frame_time _ [] = return 0
    next_frame_time t0 ((frame_time, f):_) = do
      t1 <- getCurrentTime
      let elapsed_time = realToFrac (diffUTCTime t1 t0)
      return $ max (round $ 1000 * (frame_time - elapsed_time)) 0

load_image filename = do
  img <- load_targa_image filename
  if depth img /= 24 || img_type img /= 2
    then printf "dep=%d,t=%d " (depth img) (img_type img) >> error "bad image"
            -- NB: [OSB p. 264] recommends the following map, which
            -- we should implement:
            --   depth 3 ==> format = BGR,  components = RGB8
            --         4              BGRA               RGBA8
            --         1              Luminance          Luminance8
    else return (fromIntegral $ width img,
                 fromIntegral $ height img,
                 img_data img)

compile_object object = case object of
    TriangulatedSurface s -> do
      (vbo_id, vbo_len) <- expand_list_to_buffer_object $
        triangle_n1t2d_to_list s
      return (CompiledGeometry . Action $
                render_compiled_triangles vbo_id vbo_len)
    TriangulatedSurface2 v i -> do

      (vbo_id, ibo_id, ibo_len) <- expand_indexed_list_to_vbo
        (vertex_nt2d_to_list v) i
      return (CompiledGeometry . Action $
               render_compiled_indexed_triangles vbo_id ibo_id ibo_len)
    Group g -> mapM compile_object g >>= return . Group
    Actor m i -> compile_object i >>= return . Actor m
    otherwise -> return object
  where
    expand_list_to_buffer_object l = do
      [vbo_id] <- genObjectNames 1
      let l_len = length l
      fill_buffer ArrayBuffer vbo_id l l_len
      return (vbo_id, l_len `div` 8)

    triangle_n1t2d_to_list triangles =
        foldr accumulate_interleaved_data [] triangles
      where
        accumulate_interleaved_data (TriangleN1T2D n t0 v0 t1 v1 t2 v2) l =
          x n : y n : z n : x t0 : y t0 : x v0 : y v0 : z v0 :
          x n : y n : z n : x t1 : y t1 : x v1 : y v1 : z v1 :
          x n : y n : z n : x t2 : y t2 : x v2 : y v2 : z v2 : l

    expand_indexed_list_to_vbo vertices indices = do
      [vbo_id, ibo_id] <- genObjectNames 2
      let v_len = length vertices
      fill_buffer ArrayBuffer vbo_id vertices v_len
      let i_len = length indices
      fill_buffer ElementArrayBuffer ibo_id indices i_len
      return (vbo_id, ibo_id, i_len)

    fill_buffer buffer_type buffer_obj_id buffer_data data_count =
      let data_size = sizeOf $ head buffer_data
      in do
        bindBuffer buffer_type $= Just buffer_obj_id
        a <- newListArray (0,data_count-1) buffer_data
        withStorableArray a (\ptr -> bufferData buffer_type $=
          (fromIntegral $ data_count * data_size, ptr, StaticDraw))
        bindBuffer buffer_type $= Nothing

    vertex_nt2d_to_list vertices =
        foldr accumulate_interleaved_data [] vertices
      where
        accumulate_interleaved_data (VertexNT2D n t v) l =
          x n : y n : z n : x t : y t : x v : y v : z v : l

render_compiled_triangles vbo_id vbo_len = do
  bindBuffer ArrayBuffer $= (Just vbo_id)
  load_vertex_format
  drawArrays GLUT.Triangles 0 (fromIntegral vbo_len)
  bindBuffer ArrayBuffer $= Nothing

render_compiled_indexed_triangles vbo_id ibo_id ibo_len = do
  bindBuffer ArrayBuffer $= (Just vbo_id)
  load_vertex_format
  bindBuffer ElementArrayBuffer $= (Just ibo_id)
  drawElements GLUT.Triangles (fromIntegral ibo_len) UnsignedInt nullPtr
  bindBuffer ArrayBuffer $= Nothing
  bindBuffer ElementArrayBuffer $= Nothing

load_vertex_format =
  let stride = (fromIntegral sizeof_coordinate) * 8
      va_desc n off =
        VertexArrayDescriptor n
          (case sizeof_coordinate of
            4 -> Float
            8 -> Double
            _ -> error "data format / va_desc")
          stride
          (nullPtr `plusPtr` (off*sizeof_coordinate))
      normals = va_desc 3 0
      texcoords = va_desc 2 3
      vertices = va_desc 3 5
  in do
    arrayPointer NormalArray $= normals
    arrayPointer TextureCoordArray $= texcoords
    arrayPointer VertexArray $= vertices

--
-- CALLBACKS
--

reshape projection s@(Size w h) = do
  let [wf,hf] = map fromIntegral [w,h]
  viewport $= (Position 0 0, s)
  matrixMode $= Projection
  loadIdentity
  case projection of
    Orthogonal ppu ->
        ortho ((-wf)/ppu') (wf/ppu') ((-hf)/ppu') (hf/ppu') (-1) 1
      where
        ppu' = realToFrac ppu
    Perspective fov near far ->
        perspective fov' (wf/hf) near' far'
      where
        [fov', near', far'] = map realToFrac [fov, near, far]

  matrixMode $= Modelview 0

display t0 flags resources n (frame_time, frame) = do
    let frame_name = printf "f%06d" n :: String
    t1 <- getCurrentTime
    let elapsed_time = realToFrac (diffUTCTime t1 t0)
    let fps = (fromIntegral n + 0::Double) / elapsed_time
    printf "%s %.3g %.3g %.2g fps\n" frame_name frame_time elapsed_time fps

    clear [ColorBuffer, DepthBuffer]

    matrixMode $= Modelview 0
    loadIdentity
    setup_camera (camera frame)
    setup_lights (lights frame)
    opengl_render flags resources (action frame)
    swapBuffers

    if (write_frames flags) then write_frame frame_name else return ()

    errs <- get errors
    if (length errs > 0) then putStrLn (show errs) else return ()
  where
    setup_camera (Just (Camera pos target up angle)) =
      lookAt (to_double Vertex3 pos)
             (to_double Vertex3 target)
             (to_double Vector3 up)
      -- XXX set up perspective here?
    setup_camera Nothing = return ()
    setup_lights (Just lights) =
        mapM_ setup_light (zip [0..] lights)
      where
        setup_light (i, SceneItem.Light loc color) = do
          light (GLUT.Light i) $= Enabled
          -- these next 2 lines are dubious
          ambient (GLUT.Light i) $= Color4 0.2 0.2 0.2 1.0
          diffuse (GLUT.Light i) $= Color4 0.7 0.8 0.6 1.0
          -- XXX end dubiety
          position (GLUT.Light i) $=
            to_double4 Vertex4 (vec4 (x loc) (y loc) (z loc) 1)
    setup_lights Nothing = lightModelAmbient $= Color4 1 1 1 1

    to_double f p = f (realToFrac $ x p) (realToFrac $ y p) (realToFrac $ z p)
    to_double4 f p = f (realToFrac $ x p) (realToFrac $ y p) (realToFrac $ z p)
                       (realToFrac $ w p)

opengl_render flags resources item =
  case item of
    SceneItem.Lines width l tex -> lineWidth $= (realToFrac width)
                                   >> render resources GLUT.Lines
                                        (foldr (\(r,s) ps -> r:s:ps) [] l) tex
    SceneItem.Points sz ps tex  -> pointSize $= (realToFrac sz)
                                >> render resources GLUT.Points ps tex
    SceneItem.Circle c r tex    -> render_circle resources c r tex
    SceneItem.Disk c r tex      -> render_disk resources c r tex
    SceneItem.Sphere r m n      -> sphere (realToFrac r) m n
    Actor matrix i              -> preservingMatrix $ opengl_matrix matrix
                                >>= multMatrix >> opengl_render flags resources i
    Group items                 -> mapM_ (opengl_render flags resources) items
    String str tex              -> preservingMatrix $ do
      scale 0.005 0.005 (0.005::GLfloat)
      with_texture resources tex $ renderString Roman str
    CompiledGeometry (Action a) -> a
    CompiledObject i tex        -> with_texture resources tex $
      opengl_render flags resources $ (compiled_objects resources)!i
    Cue time msg                -> hPrintf (cue_file flags) "CUE %g %s\n"
                                     time msg
    otherwise                   -> putStrLn $ show item >> error "render"
  where
    to_Color4 (SceneItem.RGBA r g b a) = Color4 (realToFrac r) (realToFrac g)
                                                (realToFrac b) (realToFrac a)
    sphere r m n =
      let [mi, ni] = map fromIntegral [m,n]
      in renderObject Solid (Sphere' r mi ni)

    opengl_matrix :: Vec -> IO (GLmatrix GLdouble)
    opengl_matrix m = newMatrix RowMajor $ map realToFrac $ as_list m
    establish_texture resource (Just tex) = case tex of
      SceneItem.Color c -> currentColor $= to_Color4 c
      TextureID i -> do
        -- first set color to white: this makes sense, unless we want
        -- to force users of textures to also specify the color to
        -- modulate with. Haven't seen a need for that at present
        currentColor $= Color4 1 1 1 1
        textureBinding Texture2D $= (Just $ (textures resource)!i)
    establish_texture resource Nothing =
      textureBinding Texture2D $= Nothing
    with_texture res t a = establish_texture res t >> a >>
                           establish_texture res Nothing
    render res d_type l tex =
        with_texture res tex $
          renderPrimitive d_type . mapM_ (vertex . to_Vertex3) $ l
      where
        -- XXX this is only  used at present for lines? so we can afford to
        -- force double here
        to_Vertex3 :: Vec -> Vertex3 GLdouble
        to_Vertex3 p = Vertex3 (realToFrac (x p))
                               (realToFrac (y p))
                               (realToFrac (z p))
    render_circle res c r = render res GLUT.LineLoop $ init $ circle_pts c r
    render_disk res c r = render res GLUT.TriangleFan $ c:(circle_pts c r)
    circle_pts c r =
      let npoints = 100
          θs = [2 * pi * k / npoints | k <- [0..npoints]]
      in [c |+ vec3 (r * cos θ) (r * sin θ) 0 | θ <- θs]

{- recipe for drawing a texture bitmap
  let (w,h,b) = (bitmaps res)!0
  let (imgptr, offset, len) = toForeignPtr b
  withForeignPtr imgptr (\p ->
    drawPixels (Size (fromIntegral w) (fromIntegral h))
               (PixelData BGR UnsignedByte (p `plusPtr` offset)))
-}

write_frame filename = do
  wsize@(Size w h) <- get windowSize
  let nbytes = fromIntegral $ w * h * 3
  let pixel_buf = BS.replicate nbytes 0
  let (fptr, off, len) = toForeignPtr pixel_buf
  rowAlignment Pack $= 1
  hFrameFile <- openBinaryFile (filename ++ ".p6") WriteMode
  hPutStrLn hFrameFile ("P6 " ++ show w ++ " " ++ show h ++ " 255")
  withForeignPtr fptr $ \ptr -> do
    readPixels (Position 0 0) wsize (PixelData RGB UnsignedByte
                                     (ptr `plusPtr` off))
    hPutBuf hFrameFile ptr nbytes
  hClose hFrameFile
