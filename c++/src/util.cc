#include "util.h"

int length_c(const char* str) {
  int length = 0;
  while (*str != '\0') {
    length++;
    str++;
  }
  return length;
}