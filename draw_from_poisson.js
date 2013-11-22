// Might-be-correct function for drawing numbers from a poisson distribution.
// The "draw" argument should be a number chosed uniformly from [0..1).
// The "poisson_mean" argument gives the mean of the distribution to draw from.
function draw_from_poisson(draw, poisson_mean) {
    if (draw === 1) { return Infinity; }

    var weighted_cdf = 0, i = 0, power_over_factorial = 1;
    var weight = Math.exp(1 * poisson_mean);

    var weighted_draw = draw * weight;
    if (weighted_draw === Infinity) {
        throw("Overflow");
    }

    while (weighted_cdf < weighted_draw) {
        weighted_cdf += power_over_factorial;
        i += 1;
        power_over_factorial *= poisson_mean / i;
    }

    return i;
}
function poisson(poisson_mean) {
    return draw_from_poisson(Math.random(), poisson_mean);
}
