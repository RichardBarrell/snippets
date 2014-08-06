#include <stdio.h>
int main()
{
	FILE *f = fopen("/sys/class/leds/smc::kbd_backlight/brightness", "wb");
	if (f == NULL) {
		printf("Can't open brightness.\n");
		return 1;
	}
	if (fwrite("0\n", 1, 2, f) != 2) {
		printf("Can't write brightness.\n");
		return 1;
	}
	fclose(f);
	return 0;
}
