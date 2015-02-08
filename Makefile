PROGRAMS = triangle_gl 

LIBRARY = Vector.hs Triangle.hs SceneItem.hs

default: $(PROGRAMS)

GHC_FLAGS = -O2

triangle_gl: triangle_gl.hs $(LIBRARY) TriangleScene.hs OpenGL.hs
	ghc $(GHC_FLAGS) --make $^

clean:
	rm -f *.o *.hi *.pov $(PROGRAMS)
