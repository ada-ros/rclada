#include <stdio.h>

#include "rcl/timer.h"
#include "rcl/subscription.h"
#include "rcl/wait.h"

/*
 * rclada_wait_set_subscription_check
 *
 * The set type is not properly generated by -fada-dump-spec, so this is needed
 */
bool rclada_wait_set_subscription_check (rcl_wait_set_t *set, int i) {
  if (i >= set->size_of_subscriptions) {
    printf("rclada_wait_set_subscription_check: index out of range %d >= %zd\n",
	   i, set->size_of_subscriptions);
    return false;
  }
  else
    return set->subscriptions[i];
}

/*
 * rclada_wait_set_timer_check
 */
bool rclada_wait_set_timer_check (rcl_wait_set_t *set, int i) {
  if (i >= set->size_of_timers) {
    printf("rclada_wait_set_timer_check: index out of range %d >= %zd\n",
	   i, set->size_of_timers);
    return false;
  }
  else
    return set->timers[i];
}
