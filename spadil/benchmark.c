#include <stdint.h>
#include <sys/time.h>

static struct timeval tv_start, tv_end;

void time_start() {
  (void)gettimeofday(&tv_start, NULL);
}

void time_stop() {
  (void)gettimeofday(&tv_end, NULL);
}

uint64_t time_diff() {
  struct timeval tv_res;

  timersub(&tv_end, &tv_start, &tv_res);

  return tv_res.tv_sec * 1000000 + tv_res.tv_usec;
}
