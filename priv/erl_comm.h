#ifndef _ERL_COMM_H
#define _ERL_COMM_H
#include <arpa/inet.h>
typedef unsigned char byte;

uint32_t read_cmd(byte *buf);
uint32_t write_cmd(byte *buf, uint32_t len);

#endif 
