/* erl_comm.c */
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <arpa/inet.h>

#include "erl_comm.h"

uint32_t read_exact(byte *buf, uint32_t len);
uint32_t write_exact(byte *buf, uint32_t len);

uint32_t
read_cmd(byte *buf)
{
  uint32_t len;

  if (read_exact(buf, sizeof(uint32_t)) != sizeof(uint32_t)) {
    /* fprintf(stderr, "read_cmd() failed!\n"); */
    return(0);
  }
  memcpy(&len, buf, sizeof(uint32_t));
  len = ntohl(len);
  return read_exact(buf, len);
}

uint32_t
write_cmd(byte *buf, uint32_t len)
{
  /* 1st, write the length of buf */
  uint32_t len_n = htonl(len);	/* len in network order */
  write_exact((byte *)&len_n, sizeof(uint32_t));
  /* 2nd, write the buf */
  return write_exact(buf, len);
}

uint32_t
read_exact(byte *buf, uint32_t len)
{
  int i, got=0;

  do {
    if ((i = read(0, buf+got, len-got)) <= 0)
      return(i);
    got += i;
  } while (got<len);
  /* fprintf(stderr, "read_exact: read %d bytes.\n", len); */
  return(len);
}

uint32_t
write_exact(byte *buf, uint32_t len)
{
  int i, wrote = 0;

  do {
    if ((i = write(1, buf+wrote, len-wrote)) <= 0)
      return (i);
    wrote += i;
  } while (wrote<len);

  return (len);
}
