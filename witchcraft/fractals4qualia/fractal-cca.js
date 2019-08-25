function FractalCCA() {
    function getStateCount(inputs) {
        var nrStates = parseInt(inputs.nrStates.value, 10);
        if (!((nrStates >= 3) && (nrStates <= 50))) {
            nrStates = 3;
        }
        return nrStates;
    }
    function createGridTextures(gl, width, height, grid) {
        console.log("createGridTextures", width, height);
        let texWidth = nextPOT(width),
            texHeight = nextPOT(height),
            oldT = gl.createTexture(),
            newT = gl.createTexture();
        gl.bindTexture(gl.TEXTURE_2D, oldT);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texImage2D(
            gl.TEXTURE_2D, 0, gl.RGBA,
            texWidth, texHeight,
            0, gl.RGBA, gl.UNSIGNED_BYTE, grid);
        gl.bindTexture(gl.TEXTURE_2D, newT);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.NEAREST);
        gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.NEAREST);
        gl.texImage2D(
            gl.TEXTURE_2D, 0, gl.RGBA,
            texWidth, texHeight,
            0, gl.RGBA, gl.UNSIGNED_BYTE, null);
        gl.bindTexture(gl.TEXTURE_2D, null);
        return {
            oldT: oldT,
            newT: newT,
            texWidth: texWidth,
            texHeight: texHeight
        };
    }
    function deleteGridTextures(gl, gridTextures) {
        console.log("deleteGridTextures");
        for (var i=0; i<gridTextures.length; i++) {
            gl.deleteTexture(gridTextures[i]);
            gridTextures[i] = null;
        }
    }
    this.controls = [
        {
            name: 'nrStates',
            label: "Number of states",
            html: "<input type=number min=3 max=50 step=1 value=12>"
        },
        {
            name: 'tiltDiagonal',
            label: "Diagonal tilt",
            html: "<input type=checkbox>"
        }
    ];
    this.glRequirements = {
        antialias: false,
        alpha: false,
        depth: false,
        stencil: false
    };
    this.makeCache = function(gl, state, width, height, inputs) {
        let vertShader = compileShader(gl, gl.VERTEX_SHADER, `
attribute vec2 coord2d;
uniform vec2 texSize;
uniform vec2 vptSize;
varying vec2 st;
varying vec2 dsdt;
varying vec2 min_pixel;
varying vec2 max_pixel;

void main(void) {
 min_pixel = 0.5 / texSize;
 max_pixel = (vptSize - vec2(0.5)) / texSize;
 dsdt = 1.0 / texSize;
 st = coord2d.xy * vptSize / texSize;
 vec2 uv = (2.0 * coord2d.xy) - 1.0;
 gl_Position = vec4(uv, 0.0, 1.0);
}`);

        let nextState_frag = compileShader(gl, gl.FRAGMENT_SHADER, `
precision mediump float;
varying vec2 st;
varying vec2 dsdt;
varying vec2 min_pixel;
varying vec2 max_pixel;
uniform sampler2D grid;
uniform float states;
uniform vec2 tilt;

float quantize(float x) {
  return floor(x * (states + 1.0));
}
float neighbour(float xDir, float yDir) {
  vec2 coord = vec2(st.x + dsdt.x*xDir, st.y + dsdt.y*yDir);
  if (coord.x < min_pixel.x) { coord.x = max_pixel.x; }
  if (coord.y < min_pixel.y) { coord.y = max_pixel.y; }
  if (coord.x > max_pixel.x) { coord.x = min_pixel.x; }
  if (coord.y > max_pixel.y) { coord.y = min_pixel.y; }
  return quantize(texture2D(grid, coord).r);
}
void main(void) {
 int increment = 0;
 float me = neighbour(0.0, 0.0);
 float next = me + 1.0;
 if (next >= states) { next = 0.0; }
 vec2 c0 = tilt;
 vec2 c1 = vec2(c0.y, -c0.x);
 vec2 c2 = vec2(c1.y, -c1.x);
 vec2 c3 = vec2(c2.y, -c2.x);
 if (neighbour(c0.x, c0.y) == next) { increment = 1; }
 if (neighbour(c1.x, c1.y) == next) { increment = 1; }
 if (neighbour(c2.x, c2.y) == next) { increment = 1; }
 if (neighbour(c3.x, c3.y) == next) { increment = 1; }
 if (increment == 1) { me = next; }
 gl_FragColor[0] = me / states;
 gl_FragColor[1] = 0.0;
 gl_FragColor[2] = 0.0;
 gl_FragColor[3] = 1.0;
}`);

        let draw_frag = compileShader(gl, gl.FRAGMENT_SHADER, `
precision mediump float;
varying vec2 st;
uniform sampler2D grid;

void main(void) {
 vec4 look = texture2D(grid, st);
 float h = look.r;
 gl_FragColor[0] = 0.5 * (1.0 + sin(radians(h*360.0)));
 gl_FragColor[1] = 0.5 * (1.0 + sin(radians(h*360.0 + 120.0)));
 gl_FragColor[2] = 0.5 * (1.0 + sin(radians(h*360.0 + 240.0)));
 gl_FragColor[3] = 1.0;
}`);

        let nextState_prog = linkShaders(gl, vertShader, nextState_frag);
        let draw_prog = linkShaders(gl, vertShader, draw_frag);

        let fill_quad = new Float32Array([
            0, 0,
            0, 1,
            1, 1,
            0, 0,
            1, 1,
            1, 0
        ]);

        var vbo = gl.createBuffer();
        gl.bindBuffer(gl.ARRAY_BUFFER, vbo);
        gl.bufferData(gl.ARRAY_BUFFER, fill_quad, gl.STATIC_DRAW);

        var { oldT, newT, texWidth, texHeight } = createGridTextures(
            gl, width, height, state.grid);

        return {
            nextState_prog: nextState_prog,
            draw_prog: draw_prog,
            nextState_locs: extractLocations(
                gl, nextState_prog,
                ["coord2d"],
                ("grid texSize vptSize states tilt").split(" ")
            ),
            draw_locs: extractLocations(
                gl, draw_prog,
                ["coord2d"],
                ("grid texSize vptSize").split(" ")
            ),
            width: width,
            height: height,
            gridTextures: [ oldT, newT ],
            texWidth: texWidth,
            texHeight: texHeight,
            gridFBO: gl.createFramebuffer()
        };
    };
    this.makeState = function(width, height, inputs) {
        var nrStates = getStateCount(inputs);
        console.log("makeState", width, height, nrStates);
        let texWidth = nextPOT(width),
            texHeight = nextPOT(height),
            grid = new Uint8Array(texWidth * texHeight * 4);
        for (var i=0; i<grid.length; i += 4) {
            grid[i+0] = Math.floor(Math.random() * 256.0);
            grid[i+1] = Math.floor(Math.random() * 256.0);
            grid[i+2] = Math.floor(Math.random() * 256.0);
            grid[i+3] = 255;
        }
        return {
            width: width,
            height: height,
            nrStates: nrStates,
            grid: grid
        };
    };
    this.nextState = function(state, cache, gl, width, height, inputs) {
        var nrStates = getStateCount(inputs);
        console.log("nextState", width, height, nrStates);
        if ((width !== state.width) ||
            (height !== state.height) ||
            (nrStates !== state.nrStates)) {
            state = this.makeState(width, height, inputs);
        }
        var { gridFBO, nextState_prog, texWidth, texHeight } = cache;
        var locs = cache.nextState_locs;
        var oldT = cache.gridTextures[0],
            newT = cache.gridTextures[1];
        var tiltY = inputs.tiltDiagonal.checked ? 1.0 : 0.0;

        // Render the nextState program from oldT into newT.
        gl.viewport(0, 0, width, height);
        gl.bindFramebuffer(gl.FRAMEBUFFER, gridFBO);
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, newT, 0);
        gl.clearColor(0.0, 0.0, 0.0, 1.0);
        gl.clear(gl.COLOR_BUFFER_BIT);
        gl.useProgram(nextState_prog);
        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, oldT);
        gl.uniform1i(locs.grid, 0);
        gl.uniform2f(locs.texSize, texWidth, texHeight);
        gl.uniform2f(locs.vptSize, gl.canvas.width, gl.canvas.height);
        gl.uniform1f(locs.states, nrStates);
        gl.uniform2f(locs.tilt, 1.0, tiltY);
        gl.enableVertexAttribArray(locs.coord2d);
        gl.vertexAttribPointer(
            locs.coord2d,
            2,
            gl.FLOAT,
            gl.FALSE,
            0,
            0);
        gl.drawArrays(gl.TRIANGLES, 0, 6);
        // Save newT's contents in the new state var.
        // Possible optimisation: only do this once every $n frames,
        // store a count of how many frames to skip on restore. :)
        var newGrid = state.grid;
        if (0) {
            newGrid = new Uint8Array(texWidth * texHeight * 4);
            gl.readPixels(0, 0, width, height, gl.RGBA, gl.UNSIGNED_BYTE, newGrid);
        }
        // Detach stuff.
        gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.COLOR_ATTACHMENT0, gl.TEXTURE_2D, null, 0);
        gl.bindFramebuffer(gl.FRAMEBUFFER, null);
        gl.disableVertexAttribArray(locs.coord2d);
        gl.bindTexture(gl.TEXTURE_2D, null);
        // Swap newT and oldT around.
        cache.gridTextures[0] = newT;
        cache.gridTextures[1] = oldT;

        return {
            width: width,
            height: height,
            nrStates: nrStates,
            grid: newGrid
        };
    };
    this.draw = function(state, cache, gl, width, height, inputs) {
        if ((width !== cache.width) || (height !== cache.height)) {
            deleteGridTextures(gl, cache.gridTextures);
            let { oldT, newT, texWidth, texHeight } = createGridTextures(
                gl, width, height, state.grid);
            cache.gridTextures[0] = oldT;
            cache.gridTextures[1] = newT;
            cache.width = width;
            cache.height = height;
            cache.texWidth = texWidth;
            cache.texHeight = texHeight;
        }
        let { gridTextures, gridFBO, draw_prog, texWidth, texHeight } = cache;
        let oldT = gridTextures[0];
        let locs = cache.draw_locs;

        // Render from oldT to the screen.
        gl.viewport(0, 0, width, height);
        gl.clearColor(0.5, 1.0, 1.0, 1.0);
        gl.clear(gl.COLOR_BUFFER_BIT);
        gl.useProgram(draw_prog);
        gl.activeTexture(gl.TEXTURE0);
        gl.bindTexture(gl.TEXTURE_2D, oldT);
        gl.uniform1i(locs.grid, 0);
        gl.uniform2f(locs.texSize, texWidth, texHeight);
        gl.uniform2f(locs.vptSize, width, height);
        gl.enableVertexAttribArray(locs.coord2d);
        gl.vertexAttribPointer(
            locs.coord2d,
            2,
            gl.FLOAT,
            gl.FALSE,
            0,
            0);
        gl.drawArrays(gl.TRIANGLES, 0, 6);
        // Detach stuff
        gl.bindTexture(gl.TEXTURE_2D, null);
        gl.disableVertexAttribArray(locs.coord2d);
    };
}
FRACTALS['cca'] = new FractalCCA();
setFractal();
