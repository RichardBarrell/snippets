 function nextPOT(n) {
     // http://graphics.stanford.edu/~seander/bithacks.html#RoundUpPowerOf2
     n = (n|0) - 1;
     n |= n >> 1;
     n |= n >> 2;
     n |= n >> 4;
     n |= n >> 8;
     n |= n >> 16;
     return (n+1);
 }

 // Cope with context loss et al:
 // * when fractal is picked, initialise state
 // * on GL (re)init, wipe+initialise cache
 // gl = canvas.getContext("webgl", reqs)
 // cache = init_gl(gl)
 // state = init_state()
 // every simulation tick, state = statefn(state, gl_cache)
 // every draw tick, drawfn(state, gl_cache)

var outstandingLoads = {};
var specificControls = document.getElementById("specific-controls");

function squelchEvent(event) { event.preventDefault(); }
specificControls.addEventListener("submit", squelchEvent, false);
document.getElementById("standard-controls").addEventListener("submit", squelchEvent, false);

var cache = undefined,
    state = undefined,
    canvas = undefined,
    gl = undefined,
    // all loaded fractals
    FRACTALS = {},
    // currently selected fractal
    fractal = undefined,
    fractalInputs = undefined;

var anim = {
    running: false,
    animationFrame: null,
    timeout: null,
    t0: Date.now()
};

function setFractal() {
    var desiredFractal = whichFractalInput.value;
    if (FRACTALS.hasOwnProperty(desiredFractal)) {
        var oldFractal = fractal;
        fractal = FRACTALS[desiredFractal];
        if (fractal === oldFractal) { return; }
        if (canvas) {
            canvas.parentElement.removeChild(canvas);
        }
        canvas = document.createElement("canvas");
        canvas.id = "draw-on-me";
        setCanvasSize();
        document.body.appendChild(canvas);
        var glRequirements = {
            failIfMajorPerformanceCaveat: true
        };
        for (prop in fractal.glRequirements) {
            if (Object.hasOwnProperty.call(fractal.glRequirements, prop)) {
                glRequirements[prop] = fractal.glRequirements[prop];
            }
        }
        gl = canvas.getContext("webgl", glRequirements);
        // gl = WebGLDebugUtils.makeDebugContext(gl);
        specificControls.innerHTML = "";
        Array.prototype.forEach.call(fractal.controls, function(control) {
            var div = document.createElement("div");
            div.className = "control";
            div.innerHTML = control.html;
            var label = document.createElement("label");
            label.appendChild(document.createTextNode(control.label));
            div.insertBefore(label, div.firstChild);
            var input = div.querySelector("input");
            input.id = label.for = 'control-' + control.name;
            input.name = control.name;
            specificControls.appendChild(div);
        });
        fractalInputs = extractInputElements();
        state = fractal.makeState(canvas.width, canvas.height, fractalInputs);
        cache = fractal.makeCache(gl, state, canvas.width, canvas.height, fractalInputs);
        anim.t0 = Date.now();
        doDisplay();
    } else {
        fractal = undefined;
        doDisplay();
    }
    if (!outstandingLoads.hasOwnProperty(desiredFractal)) {
        var script = document.createElement("script");
        script.src = "fractal-" + desiredFractal + ".js";
        script.async = true;
        outstandingLoads[desiredFractal] = script;
        document.body.appendChild(script);
    }
}

var speedControl = document.getElementById("speed-control");
function animate() {
    if (anim.timeout !== null) {
        clearTimeout(anim.timeout);
        anim.timeout = null;
    }
    if (anim.animationFrame !== null) {
        cancelAnimationFrame(anim.animationFrame);
        anim.animationFrame = null;
    }
    var desiredSpeed = speedControl.value;
    console.log("animate", desiredSpeed);
    if ((desiredSpeed === "pause") || (fractal === undefined)) {
        anim.running = false;
        return;
    }
    if (anim.running === false) {
        anim.running = true;
    }
    if (desiredSpeed[0] === "t") {
        var delay = parseInt(desiredSpeed.slice(1), 10);
        anim.timeout = window.setTimeout(doDisplay, delay);
    }
    if (desiredSpeed[0] === "f") {
        anim.animationFrame = window.requestAnimationFrame(doDisplay);
    }
}
speedControl.addEventListener("change", () => { animate(); }, false);

var resetStateButton = document.getElementById("reset-state");
resetStateButton.addEventListener("click", function(event) {
    event.preventDefault();
    if (fractal) {
        state = fractal.makeState(canvas.width, canvas.height, fractalInputs);
        doDisplay();
    }
});

function setCanvasSize() {
    canvas.width = window.innerWidth;
    canvas.height = window.innerHeight;
}
window.addEventListener("resize", () => { setCanvasSize(); doDisplay(); }, false);

var whichFractalInput = document.getElementById("which-fractal");
whichFractalInput.addEventListener("change", setFractal, false);
setFractal();

var moving_avg = 0.0;
var moving_dev = 0.0;
var avg_span = document.getElementById("frame-avg");
var dev_span = document.getElementById("frame-dev");
var instant_span = document.getElementById("frame-instant");

function updateFTC(draw_time) {
    moving_avg = moving_avg * 0.98 + draw_time * 0.02;
    moving_dev = moving_dev * 0.98 + Math.abs(moving_avg - draw_time) * 0.02;
    avg_span.textContent = moving_avg.toFixed(2);
    dev_span.textContent = moving_dev.toFixed(2);
    instant_span.textContent = draw_time.toFixed(2);
}

function doDisplay() {
    console.log("--- doDisplay ---");
    if (fractal) {
        var state_start = performance.now();
        state = fractal.nextState(state, cache, gl, canvas.width, canvas.height, fractalInputs);
        var draw_start = performance.now();
        fractal.draw(state, cache, gl, canvas.width, canvas.height, fractalInputs);
        var draw_end = performance.now();
        updateFTC(draw_end - state_start);
    }
    animate();
}

function compileShader(gl, shaderType, source) {
    var shader = gl.createShader(shaderType);
    gl.shaderSource(shader, source);
    gl.compileShader(shader);
    var err = gl.getShaderInfoLog(shader);
    if (err) { throw new Error("shader compile err: " + err); }
    return shader;
}

function linkShaders(gl, vertexShader, fragmentShader) {
    var program = gl.createProgram();
    gl.attachShader(program, vertexShader);
    gl.attachShader(program, fragmentShader);
    gl.linkProgram(program);
    var err = gl.getProgramInfoLog(program);
    if (err) { throw new Error("shader link err: " + err); }
    return program;
}

function extractLocations(gl, program, attribs, uniforms) {
    var ret = {}, name;
    for (var i=0; i<attribs.length; i++) {
        name = attribs[i];
        ret[name] = gl.getAttribLocation(program, name);
    }
    for (var i=0; i<uniforms.length; i++) {
        name = uniforms[i];
        ret[name] = gl.getUniformLocation(program, name);
    }
    return ret;
}

function extractInputElements() {
    var ret = {};
    Array.prototype.forEach.call(
        specificControls.querySelectorAll("input,select,textbox"),
        function (element) {
            ret[element.name] = element;
        }
    );
    return ret;
}
