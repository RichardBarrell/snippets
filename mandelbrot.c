/* gcc mandelbrot.c -o mandelbrot -lm -Ofast -fopenmp */
/* Draws a Mandelbrot into out.ppm. Mostly I'm just playing with OpenMP. */
#include <complex.h>
#include <stdio.h>
#include <stdlib.h>
#include <omp.h>

int main(int argc, char **argv) {
    size_t width = 1920, height = 1080;

    unsigned char *block = malloc(width*height*3);
    if (block == NULL) { perror("Couldn't malloc block.\n"); abort(); }

    FILE *out = fopen("out.ppm", "w");
    if (out == NULL) { perror("Couldn't open out.ppm.\n"); abort(); }

    float dx = 2.5f / width, dy = 2.0f / height;
    size_t yi;

#pragma omp parallel for private(yi) shared(block)
    for (yi=0; yi<height; yi++) {
        float y = -1 + (dy * (float)yi);
        char *p = block + (3 * width * yi);
        size_t xi;
        for (xi=0; xi<width; xi++) {
            char *po = p + (3 * xi);
            float x = -2 + (dx * (float)xi);

            complex float z = 0.0f;
            complex float c = x + y * I;
            unsigned char n = 0;

            float zx=0.0f, zy=0.0f;

            while ((zx*zx + zy*zy) < 4.0f) {
                z = z*z + c;
                if (++n == 255) { break; }
                zx = creal(z);
                zy = cimag(z);
            }

            po[0] = 255 - n;
            po[1] = 255 - n;
            po[2] = 255 - n;
        }
    }
#pragma omp end parallel


    fprintf(out, "P6 %zu %zu 255\n", width, height);
    fwrite(block, width*height, 3, out);
    puts("done");
    return 0;
}
