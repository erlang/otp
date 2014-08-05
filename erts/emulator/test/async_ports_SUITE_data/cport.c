#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#ifdef __WIN32__
#  include "windows.h"
#  include "winbase.h"
#else
#  include <unistd.h>
#endif

typedef unsigned char byte;

int read_cmd(byte *buf)
{
  int len;
  if (read_exact(buf, 4) != 4)
      return(-1);

  len = (buf[0] << 24) | (buf[1] << 16) | (buf[2] << 8) | buf[3];
  return read_exact(buf, len);
}

int write_cmd(byte *buf, int len)
{
  byte li[4];
  li[0] = (len >> 24) & 0xff;
  li[1] = (len >> 16) & 0xff;
  li[2] = (len >> 8)  & 0xff;
  li[3] = len  & 0xff;
  write_exact(&li, 4);

  return write_exact(buf, len);
}

int read_exact(byte *buf, int len)
{
  int i, got=0;
  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      {
	return(i);
      }
    got += i;
  } while (got<len);
  return len;
}

int write_exact(byte *buf, int len)
{
  int i, wrote = 0;
  do {
    if ((i = write(1, buf+wrote, len-wrote)) < 0)
      return (i);
    wrote += i;
  } while (wrote<len);
  return len;
}

byte static_buf[31457280]; // 30 mb

int main(int argc, char **argv) {
  int sleep_time = atoi(argv[1]);
  int fn, arg, res;
  byte *buf = &static_buf[0];
  int len = 0;
  if (sleep_time <= 0)
    sleep_time = 0;
#ifdef __WIN32__
  else
    sleep_time = ((sleep_time - 1) / 1000) + 1; /* Milli seconds */
#endif
  while ((len = read_cmd(buf)) > 0) {
#ifdef __WIN32__
    Sleep((DWORD) sleep_time);
#else
    usleep(sleep_time);
#endif
    write_cmd(buf, len);
  }
}
