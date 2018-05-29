#include <stdio.h>

#include "rcl/subscription.h"
#include "rcl/wait.h"

void rclada_dark_side(void) {
}

bool rclada_wait_set_subscription_check (rcl_wait_set_t *set, int i) {
  if (i >= set->size_of_subscriptions) {
    printf("rclada_wait_set_subscription_check: index out of range %d >= %zd\n",
	   i, set->size_of_subscriptions);
    return false;
  }
  else
    return set->subscriptions[i];
}
