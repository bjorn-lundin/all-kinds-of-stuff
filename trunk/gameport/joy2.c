#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>

#include <unistd.h>



int open_port(void) {
  int fd;
  off_t seek_result;

  char *bufptr;
  int nbytes;


  fd = open("/dev/js0",O_RDONLY);
//  fd = open("/home/bnl/ada.xml",O_RDWR);
  perror("after open!");

  seek_result = lseek(fd,(off_t)0x201, SEEK_SET);
//  perror("after seek!");
  nbytes = read(fd, bufptr, 8);
  perror("after read!");

  close(fd);
  perror("after close!");

  return 0;
}

int main() {
  return open_port();
}
