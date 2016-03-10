var messages = document.getElementById("messages");
function page_log(message) {
    var li = document.createElement("li");
    li.appendChild(document.createTextNode(String(message)));
    messages.appendChild(li);
}

console.log("messages");
var canvas = document.getElementById("gl");
canvas.width = 320;
canvas.height = 240;
console.log("canvas");

var gl = canvas.getContext("webgl", {
    failIfMajorPerformanceCaveat: true,
    antialias: false,
    alpha: true,
    depth: true,
    stencil: true});
console.log("gl");

page_log(canvas);
page_log(gl);

var triangle_verts = new Float32Array([
        +0.0, +0.8, +0.2,
        -0.8, -0.8, +0.5,
        +0.8, -0.8, +0.0])

// the "2" in the name "coord2d" is kind of a lie
// I stuck a 3rd coordinate on the end which I'm using for greenness
var v_shader_text = [
    "attribute vec3 coord2d;",
    "varying float greenness;",
    "void main(void) {",
    " gl_Position = vec4(coord2d[0], coord2d[1], 0.0, 1.0);",
    " greenness = coord2d[2];",
    "}"].join("\n");

var v_s = gl.createShader(gl.VERTEX_SHADER);
gl.shaderSource(v_s, v_shader_text);
gl.compileShader(v_s)
page_log(gl.getShaderInfoLog(v_s));

var f_shader_text = [
    "precision mediump float;",
    "varying float greenness;",
    "void main(void) {",
    " gl_FragColor[0] = gl_FragCoord.x / 320.0;",
    " gl_FragColor[1] = greenness;",
    " gl_FragColor[2] = gl_FragCoord.y / 240.0;",
    " gl_FragColor[3] = 1.0;",
    "}"].join("\n");

var f_s = gl.createShader(gl.FRAGMENT_SHADER);
gl.shaderSource(f_s, f_shader_text);
gl.compileShader(f_s)
page_log(gl.getShaderInfoLog(f_s));

var prog = gl.createProgram();
gl.attachShader(prog, v_s);
gl.attachShader(prog, f_s);
gl.linkProgram(prog);

page_log(gl.getProgramInfoLog(prog));

page_log("compiled");

var coord2d_loc = gl.getAttribLocation(prog, "coord2d");

gl.clearColor(0.5, 1.0, 1.0, 0.7);
gl.clear(gl.COLOR_BUFFER_BIT);
gl.useProgram(prog);
gl.enableVertexAttribArray(coord2d_loc);
var vbo = gl.createBuffer();
gl.bindBuffer(gl.ARRAY_BUFFER, vbo);
gl.bufferData(gl.ARRAY_BUFFER, triangle_verts, gl.STATIC_DRAW);
gl.vertexAttribPointer(
    coord2d_loc,
    3,
    gl.FLOAT,
    gl.FALSE,
    0,
    0);
gl.drawArrays(gl.TRIANGLES, 0, 3);
gl.disableVertexAttribArray(coord2d_loc);

page_log("displayed");
