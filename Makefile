.PHONY: all
all:
	
#	gcc main.c -Ic:/Libraries/SDL2-2.0.5/i686-w64-mingw32/include/SDL2 -Lc:/Libraries/SDL2-2.0.5/i686-w64-mingw32/lib -lmingw32 -lSDL2main -lSDL2
	ghc sdl_wrapper.c gl_wrapper.c main.hs -Ic:/Libraries/SDL2-2.0.5/x86_64-w64-mingw32/include/SDL2 -Lc:/Libraries/SDL2-2.0.5/x86_64-w64-mingw32/lib -Lc:/Libraries/OpenGl/x64 -lSDL2main -lSDL2 -lglew32s -lopengl32
