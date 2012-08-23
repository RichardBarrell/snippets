#ifndef REGPRINTING_ILLHANDLER_H
#define REGPRINTING_ILLHANLDER_H

/* Call this somewhere in main(), then thereafter:
 * asm(".byte 6") will dump registers every time it's hit, while
 * asm(".byte 7") will dump registers the first time it's hit. */
int regprinting_illhandler_install();

#endif
