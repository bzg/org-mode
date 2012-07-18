#include <X11/extensions/scrnsaver.h>
#include <stdio.h>

/* Based on code from
 * http://coderrr.wordpress.com/2008/04/20/getting-idle-time-in-unix/
 *
 * compile with 'gcc -l Xss x11idle.c -o x11idle' and copy x11idle into your
 * path
 */
main() {
    Status querry = 0;
    XScreenSaverInfo *info = XScreenSaverAllocInfo();
    //open the display specified by the DISPLAY environment variable
    Display *display = XOpenDisplay(0);

    //display could be null if there is no X server running
    if (info == NULL || display == NULL) {
    return -1;
    }

    //X11 is running, retrieve and print idle time
	querry = XScreenSaverQueryInfo(display, DefaultRootWindow(display), info);

    if (querry == 0) {
    return -1;
    }

    //idle time was retrieved successfully, print it
    printf("%u\n", info->idle);
    return 0;
}

