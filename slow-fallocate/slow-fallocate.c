#define _GNU_SOURCE
#include <fcntl.h>
#include <dlfcn.h>
#include <stdio.h>
#include <unistd.h>

typedef int (*fallocate_type)(int fd, int mode, off_t offset, off_t len);

int fallocate(int fd, int mode, off_t offset, off_t len) {
  fallocate_type orig_fallocate;
  orig_fallocate = (fallocate_type)dlsym(RTLD_NEXT, "fallocate");
  sleep(30);
  int ret = orig_fallocate(fd, mode, offset, len);
  printf("fallocate(%d, %d, %ld, %ld) == %d\n", fd, mode, offset, len, ret);
  return ret;
}
