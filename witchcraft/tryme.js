var messages = document.getElementById("messages");
var t0 = Date.now();
function page_log(message) {
    var t = Date.now();
    var li = document.createElement("li");
    message = String(message);
    message = String(t - t0) + "ms " + message;
    li.appendChild(document.createTextNode(String(message)));
    messages.appendChild(li);
    t0 = t;
}

console.log("messages");
var canvas = document.getElementById("gl");
canvas.width = 640;
canvas.height = 480;
console.log("canvas");

var gl = canvas.getContext("webgl", {
    failIfMajorPerformanceCaveat: true,
    antialias: false,
    alpha: false,
    depth: false,
    stencil: false});
console.log("gl");

page_log("click anywhere to resize fractal");
page_log(canvas);
page_log(gl);

function fpack(i1, i2) {
    return i1 + (i2 / 32.0);
}

var verts = (function make_verts() {
    // 3 nrs
    // 3 verts/tri
    // 2 tris/square
    // 16 squares x
    // 16 squares y
    var a = new Float32Array( 3 * 3 * 2 * 17 * 17 );
    var stride = 3 * 3 * 2;
    for (var square_y = 0; square_y < 17; square_y++) {
        for (var square_x = 0; square_x < 17; square_x++) {
            var left = square_x / 17;
            var top = square_y / 17;
            var i = stride * (square_x + (square_y * 17));
            var packed_thing = fpack(square_x, square_y);
            a[i++] = left;
            a[i++] = top;
            a[i++] = packed_thing;
            a[i++] = left + (1/17);
            a[i++] = top;
            a[i++] = packed_thing;
            a[i++] = left;
            a[i++] = top + (1/17);
            a[i++] = packed_thing;
            a[i++] = left;
            a[i++] = top + (1/17);
            a[i++] = packed_thing;
            a[i++] = left + (1/17);
            a[i++] = top;
            a[i++] = packed_thing;
            a[i++] = left + (1/17);
            a[i++] = top + (1/17);
            a[i++] = packed_thing;
        }
    }
    return a;
})();

var v_shader_text = `
attribute vec3 coord2d_and_packed_thing;
varying float packed_thing;
void main(void) {
 vec2 xy = (2.0 * coord2d_and_packed_thing.xy) - 1.0;
 packed_thing = coord2d_and_packed_thing.z;
 gl_Position = vec4(xy, 0.0, 1.0);
}`;

var v_s = gl.createShader(gl.VERTEX_SHADER);
gl.shaderSource(v_s, v_shader_text);
gl.compileShader(v_s)
page_log("v: " + gl.getShaderInfoLog(v_s));

var f_shader_text = `
precision mediump float;
varying float packed_thing;

ivec2 unpack(float f) {
 float f2 = mod(f, 1.0);
 float f1 = f - f2;
 return ivec2(int(f1), int(f2 * 32.0));
}

void main(void) {
 ivec2 ii = unpack(packed_thing);
 float m1 = float(ii.x) / 17.0;
 float m2 = float(ii.y) / 17.0;
 gl_FragColor[0] = m1;
 gl_FragColor[1] = (m1 + m2) * 0.5;
 gl_FragColor[2] = m2;
 gl_FragColor[3] = 1.0;
}`;

var f_s = gl.createShader(gl.FRAGMENT_SHADER);
gl.shaderSource(f_s, f_shader_text);
gl.compileShader(f_s)
page_log("f: " + gl.getShaderInfoLog(f_s));

var prog = gl.createProgram();
gl.attachShader(prog, v_s);
gl.attachShader(prog, f_s);
gl.linkProgram(prog);

page_log("p: " + gl.getProgramInfoLog(prog));

page_log("compiled");

var coord2d_loc = gl.getAttribLocation(prog, "coord2d_and_packed_thing");

function redisplay() {
 t0 = Date.now();
 gl.viewport(0, 0, gl.canvas.width, gl.canvas.height);
 gl.clearColor(0.5, 1.0, 1.0, 1.0);
 gl.clear(gl.COLOR_BUFFER_BIT);
 gl.useProgram(prog);
 gl.enableVertexAttribArray(coord2d_loc);
 var vbo = gl.createBuffer();
 gl.bindBuffer(gl.ARRAY_BUFFER, vbo);
 gl.bufferData(gl.ARRAY_BUFFER, verts, gl.STATIC_DRAW);
 gl.vertexAttribPointer(
     coord2d_loc,
     3,
     gl.FLOAT,
     gl.FALSE,
     0,
     0);
 gl.drawArrays(gl.TRIANGLES, 0, 17*17*2*3);
 gl.disableVertexAttribArray(coord2d_loc);
 gl.deleteBuffer(vbo);
 page_log("displayed " + String(canvas.width) + "x" + String(canvas.height));
}

redisplay();

function resizeCanvas(event) {
 canvas.width = event.pageX;
 canvas.height = event.pageY;
 redisplay();
}
document.addEventListener("click", resizeCanvas, true);
