#include <stdio.h>
#include <stdlib.h>

/* To suppress warnings in clang. */
#undef __SSE__
#undef __SSE2__

#include "SDL.h"

int
main(int argc, char* argv[])
{
  const Uint32 width = 1280, height = 960;
  const Uint32 flags = SDL_WINDOW_SHOWN;
  SDL_Window *window;

  if (SDL_Init(SDL_INIT_VIDEO) < 0) {
    fprintf(stderr, "SDL init failed: %s", SDL_GetError());
    exit(EXIT_FAILURE);
  }

  window = SDL_CreateWindow("test", SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED, width, height, flags);

  for (;;) {
    SDL_Event event;

    while (SDL_PollEvent(&event)) {
      if (event.type == SDL_KEYDOWN || event.type == SDL_QUIT) {
        goto done;
      }
    }
  }

 done:

  SDL_DestroyWindow(window);
  SDL_Quit();
  exit(EXIT_SUCCESS);
}
