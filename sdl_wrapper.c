#include "SDL.h"

/* Data sizes. */

int event_size() 
{
  return sizeof (SDL_Event);
}

/* Subsystems. */

Uint32 video_subsystem()
{
  return SDL_INIT_VIDEO;
}

/* Event types. */

Uint32 event_type_keydown()
{
  return SDL_KEYDOWN;
}

Uint32 event_type_quit()
{
  return SDL_QUIT;
}

/* Window flags. */

Uint32 window_center_pos()
{
  return SDL_WINDOWPOS_CENTERED;
}

Uint32 window_opengl()
{
  return SDL_WINDOW_OPENGL;
}

Uint32 window_shown()
{
  return SDL_WINDOW_SHOWN;
}

/* Data inspectors. */

Uint32 event_type(const SDL_Event *e)
{
  return e->type;
}
