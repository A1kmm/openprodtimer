#include <stdlib.h>
#ifdef WIN32
#include <windows.h>
void asynchronous_playback(const char* filename)
{
  PlaySound(filename, NULL, SND_FILENAME | SND_ASYNC | SND_NOWAIT);
}
#else
#include <canberra.h>
#include <glib.h>

static ca_context* c = NULL;

static void asynchronous_setup()
{
  if (c == NULL)
  {
    g_thread_init(NULL);
    ca_context_create(&c);
  }
}

void asynchronous_playback(const char* filename)
{
  asynchronous_setup();
  ca_context_play(c, 0, CA_PROP_MEDIA_FILENAME, filename, CA_PROP_MEDIA_ROLE, "timemanagement", NULL);
  
}
#endif
