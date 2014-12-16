/*
 * Compile with `gcc -c -O2 gameport_io_c.c',
 */

#include <stdio.h>
#include <unistd.h>
#include <sys/io.h>


void c_outb(int value, int port) {
  outb(value,port);
}

int c_inb(int port) {
  return inb(port);
}

