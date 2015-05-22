#include <stdio.h>
#include <stdlib.h>
#include <mach/mach_time.h>
#include <pthread.h>

void* do_nothing_thread_main(void* ignored) {
    return NULL;
}

int main(int argc, char ** argv) {
    pthread_t tid = 0;

    uint64_t start = mach_absolute_time();

    pthread_create(&tid, NULL, do_nothing_thread_main, NULL);
    pthread_join(tid, NULL);

    uint64_t elapsed = mach_absolute_time() - start;

    mach_timebase_info_data_t info;
    if (mach_timebase_info (&info) != KERN_SUCCESS) {
        printf ("mach_timebase_info failed\n");
    }
    uint64_t nanosecs = elapsed * info.numer / info.denom;

    printf("deltaTime: %d\n", (int) nanosecs);

    return 0;
}
