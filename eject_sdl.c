/* Ejects all of the CD-ROM drives that can be located using SDL.
 * gcc -o eject_sdl "$(pkg-config --cflags --libs sdl)"
 * I wrote this in the hope that someone might some day try running it on a
 * machine with several dozen CD-ROM drives attached and be amused. */

#include "SDL.h"
#include <stdio.h>
#include <stdlib.h>

int main (int argc, char **argv) {
	SDL_Init(SDL_INIT_CDROM);
	atexit(SDL_Quit);

	int ndrives = SDL_CDNumDrives();
	if (ndrives == 0) {
		printf("Alas, no CD drives.\n");
	}
	int drive_no;
	for (drive_no = 0; drive_no < ndrives; drive_no++) {
		SDL_CD *drive = SDL_CDOpen(drive_no);
		if (drive == NULL) {
			printf("Couldn't get drive %d.\n", drive_no);
			continue;
		}
		if (SDL_CDEject(drive)) {
			printf("Couldn't eject drive %d.\n", drive_no);
		} else {
			printf("Ping! (%d of %d)\n", drive_no + 1, ndrives);
		}
		SDL_CDClose(drive);
	}
	printf("Ejected everything.\n");
	return 0;
}
