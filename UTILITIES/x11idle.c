#include <X11/extensions/scrnsaver.h>
#include <stdio.h>

/* Based on code from
 * http://coderrr.wordpress.com/2008/04/20/getting-idle-time-in-unix/
 *
 * compile with 'gcc -l Xss x11idle.c -o x11idle' and copy x11idle into your
 * path
 */
main() {
    XScreenSaverInfo *info = XScreenSaverAllocInfo();
    Display *display = XOpenDisplay(0);

    //check that X11 is running or else you get a segafult/coredump
    if (display != NULL) {
	XScreenSaverQueryInfo(display, DefaultRootWindow(display), info);
    }
    XScreenSaverQueryInfo(display, DefaultRootWindow(display), info);
    printf("%u\n", info->idle);
    return 0;
}
