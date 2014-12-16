/*
 * example.c: very simple example of port I/O
 *
 * This code does nothing useful, just a port write, a pause,
 * and a port read. Compile with `gcc -O2 -o joystick joystick.c',
 * and run as root with `./example'.
 */

#include <stdio.h>
#include <unistd.h>
#include <asm/io.h>

//#define BASEPORT 0x378 /* lp1 */
#define BASEPORT 0x200 /* joystick */

int main() {
  /* Get access to the ports */
  if (ioperm(BASEPORT, 8, 1)) {perror("ioperm"); exit(1);}

  /* Set the data signals (D0-7) of the port to all low (0) */
  outb(0, BASEPORT + 1);

  /* Sleep for a while (100 ms) */
  usleep(100000);

  /* Read from the status port (BASE+1) and display the result */

  for (;;) {
    printf("status: %d\n",inb(BASEPORT + 1));
    usleep(200000);
  }
  /* We don't need the ports anymore */
  if (ioperm(BASEPORT, 8, 0)) {perror("ioperm"); exit(1);}

  exit(0);
}

/* end of example.c */
