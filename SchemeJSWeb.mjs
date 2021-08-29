//
// SchemeJSWeb: Scheme in JavaScript
//
// Copyright 2021 Stan Switzer -- (sjswitzer [at] gmail [dot] com)
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//
// SchemeJS primitives and bindings for Web Applications.
// The Web primitices are designed as a DSL for writing web applications.
// The main idea here is that where most procedural APIs use
// conventional functions where arguments seek their parameters, either
// by position or by name, in this DSL two contextual parameters,
// "gfx-context" and "html-document", seek out their _arguments_ by name.
// That is, instead of providing a gfx-context parameter to the move-to
// primitive, the move-to primitive, with no loss of generality
// or performance, seeks a parameter named "gfx-context". Graphics primitives
// typically do something of this sort with an implicit "transform" parameter
// whose nesting context has to be managed manually with save() and restore.
// This API/DSL subsumes that as well by encapsulating it in the "gfx-save"
// structured transform context primitive.
//
// If you want to encapsulate a series of operations to draw on any canvas,
// just define a function that takes a parameter named gfx-context and you're
// set. As a general rule, drawing functions should, by convention, take no placement
// or scale operations and instead draw inside of a 1x1 box with an origin
// at (0, 0). You can use any coordinate system you want in your drawing function
// by writing
//     (gfx-save (scale (/ 100) (/ 200)) draw-stuff here)
// to draw in a 100x200 box for instance. And if a client wants to place
// it at (15, 20) and have it be 30 x 40 in size, they can write
//     (gfx-save (translate 15 20) (scale 30 40) (your-function))
//
// To faclitatee this convention, the defaults of all of the drawing primitives have been
// set so that, assuming the current point is (0,0) the resulting figure will be bounded
// by (0,0,1,1). This occasionally leads to strange defaults, for instance the defaults
// for line-to are (1,1).
//

import * as SchemeJS from './SchemeJS.mjs';

export const VERSION = SchemeJS.VERSION;
export const LogicError = SchemeJS.LogicError;
const isArray = Array.isArray;

// So that optional parameters show up pretty when printed
const optional = undefined;

//
// Creates a SchemeJSWeb instance.
//
export function createInstance(schemeOpts = {}) {
  let globalScope = SchemeJS.createInstance(schemeOpts);
  const defineSchemeBindings = schemeOpts.defineSchemeBindings ?? true;
  if (!defineSchemeBindings)
    return globalScope;

  const string = globalScope.string ?? required();
  const exportAPI = globalScope.exportAPI ?? required();
  const augmentFunctionInfo = globalScope.augmentFunctionInfo ?? required();
  const Atom = globalScope.Atom ?? required();
  function required() { throw "required" }

  const gfxContextAtom = Atom("gfx-context");
  const htmlDocumentAtom = Atom("html-document");
  const pi = Math.PI;
  const eval_string = str => globalScope.eval_string(str);

  // Defines the graphics function and a macro to call it.
  // This is how you write Scheme with no parser or bindings :)
  function gfxFunction(name, jsFunctionName, functionBody, opts = {}) {
    let schemeGfxContextFnName = `gfx-context-${name}`, schemeGfxContextFnNameAtom = Atom(schemeGfxContextFnName);
    let code =  `let pi = Math.PI, optional = undefined; return ${functionBody}`;
    let fn = (new Function(code))();

    // Decorate it for "help" purposes and make available to Scheme
    exportAPI(schemeGfxContextFnNameAtom, fn, { dontInline: true, group: "web-gfx-context", gfxApi: jsFunctionName, ...opts });

    if (!opts.omitGfxMacro) {
      // Define a macro that adds the graphics context and calls it
      eval_string(`
          (defmacro [${name} params] 
            (cons ${schemeGfxContextFnName} (cons 'gfx-context ...params))) `);

      // Decorate it for the help system
      let nameAtom = Atom(name);
      augmentFunctionInfo(nameAtom,  { group: "web-gfx", gfxApi: jsFunctionName,
        implStr: `(${schemeGfxContextFnName} gfx-context ...params)`, ...opts });
    }
  }

  function gfxProp(schemePropName, jsPropName, opts = {}) {
    let jsGfxContextPropFnName = `gfx_context_property_${schemePropName.replace(/\./g, '_').replace(/-/g, '_')}`;
    let schemeGfxContextPropFnName = `gfx-context-property-${schemePropName}`;
    let gfxContextPropName = `gfx-context-${schemePropName}`, gfxContextFnAtom = Atom(gfxContextPropName);
    let code = `let optional = undefined; ` +
      `return function ${jsGfxContextPropFnName}(gfx_context, value = optional) { ` +
        `let oldValue = gfx_context.${jsPropName}; if (value !== optional) gfx_context.${jsPropName} = value; return oldValue }`;
    const fn = (new Function(code))();

    let schemeGfxContextPropFnNameAtom = Atom(schemeGfxContextPropFnName);
    exportAPI(schemeGfxContextPropFnNameAtom, fn, { group: "web-gfx-context", gfxApi: jsPropName, ...opts });

    // Define a macro that adds the graphics context and calls it
    eval_string(`
        (defmacro [${schemePropName} opt-value]
          (cons ${jsGfxContextPropFnName} (cons 'gfx-contsxt ...opt-value))) `);

    // Decorate it for the help system
    let nameAtom = Atom(schemePropName);
    augmentFunctionInfo(nameAtom, { group: "web-gfx", gfxApi: jsPropName,
      implStr:`(${jsGfxContextPropFnName} gfx-context [value])`, ...opts });
  }

  eval_string(`
      (defmacro [gfx-save forms]
        (list
          'finally [ (list gfx-context-save 'gfx-context) ]
            (list gfx-context-restore 'gfx-context)
            ...forms)) `);

  gfxFunction("save", "save",
    `function gfx_context_save(gfx_context, x = 0, y = 0) { return gfx_context.translate(x, y) }`, 
      { blurb: `Pushes the current context on a stack so that it can later be restored using ` +
                `gfx-context-restore. You should generally use gfx-save instead since it bundles ` +
                `gfx-context-save and gfx-context-restore in a structured way.`, omitGfxMacro: true });

  gfxFunction("restore", "restore",
    `function gfx_context_restore(gfx_context, x = 0, y = 0) { return gfx_context.translate(x, y) }`, 
      { blurb: `Pops the tha context from a stack which has previously been pushed by  ` +
              `gfx-context-save. You should generally use gfx-save instead since it bundles ` +
              `gfx-context-save and gfx-context-restore in a structured way.`, omitGfxMacro: true });      

  gfxFunction("translate", "translate",
    `function gfx_context_translate(gfx_context, x = 0, y = 0) { return gfx_context.translate(x, y) }`,
     { blurb: `Translates the origin by "x" and "y".`});

  gfxFunction("scale", "scale",
     `function gfx_context_scale(gfx_context, width = 1, height = width) { return gfx_context.scale(width, height) }`,
      { blurb: `Scales the coordinate system by "width" and "height".`});
 
  gfxFunction("rotate", "rotate",
    `function gfx_context_rotate(gfx_context, angle = 0) { return gfx_context.rotate(angle) }`,
     { blurb: `Rotates the coordinate system by "angle"`});

  gfxFunction("begin-path", "beginPath",
    `function gfx_context_begin_path(gfx_context)  { return gfx_context.beginPath() }`,
    {});

  gfxFunction("close-path", "closePath",
    `function gfx_context_close_path(gfx_context)  { return gfx_context.closePath() }`,
    {});

   gfxFunction("clip", "clip",
    `function gfx_context_clip(gfx_context)  { return gfx_context.clip() }`,
    {});

  gfxFunction("is-point-in-path", "isPointInPath",
    `function gfx_is_point_in_path(gfx_context, ...params) { return gfx_context.isPointInPath(...params) }`,
    {});

  gfxFunction("is-point-in-stroke", "isPointInStroke",
    `function gfx_is_point_in_stroke(gfx_context, ...params) { return gfx_context.isPointInStroke(...params) }`,
    {});
  
  gfxFunction("move-to", "moveTo",
    `function gfx_context_move_to(gfx_context, x = 0, y = 0) { return gfx_context.moveTo(x, y) }`,
    {});
  
  gfxFunction("move-to", "lineTo",
    `function gfx_context_line_to(gfx_context, x = 1, y = 1) { return gfx_context.lineTo(x, y) }`,
    {});
  
  gfxFunction("bezier-curve-to", "bezierCurveTo",
    `function gfx_context_bezier_curve_to(gfx_context, cpx1 = 1, cpy1 = 0, cpx2 = 0, cpy2 = 1, x = 1, y = 1) {
       return gfx_context.bezierCurveTo(cpx1, cpy1, cpx2, cpy2, x, y) }`,
     {});

  gfxFunction("quadratic_curve_to", "quadraticCurveTo",
    `function gfx_context_quadratic_curve_to(gfx_context, cpx = 1, cpy = 0, x = 1, y = 1) {
       return gfx_context.quadraticCurveTo(cpx, cpy, x, y) }`,
     {});
     
  gfxFunction("arc", "arc",
    `function gfx_context_arc(gfx_context, x = .5, y = .5, radius = .5, startAngle = 0, endAngle = 2*pi) {
      return gfx_context.arc(x, y, radius, startAngle, endAngle) }`,
      {});

  gfxFunction("arc-to", "arcTo",
    `function gfx_context_arc_to(gfx_context, x1 = 1, y1 = 0, x2 = 1, y2 = 1, radius = 1) {
       return gfx_context.arcTo(x1, y1, x2, y2, radius) }`,
      {});

  gfxFunction("ellipse", "ellipse", // defaults to a circle inscribing (0,0,1,1)
    `function gfx_context_ellipse(gfx_context, x = .5, y = .5, radiusX = .5, radiusY = .5, startAngle = 0, endAngle = 2*pi, counterclockwise = false) {
      return gfx_context.ellipse(x, y, radiusX, radiusY, startAngle, endAngle) }`,
      {});
  
  gfxFunction("rect", "rect",
    `function gfx_context_rect(gfx_context, x = 0, y = 0, width = 1, height = 1) {
      return gfx_context.rect(x, y, width, height) }`,
      {});
    
  gfxFunction("round-rect", "roundRect",  // MDN doesn't show it but the spec says it exists!
    `function gfx_context_round_rect(gfx_context, x = 0, y = 0, width = 1, height = 1, radii = 0) {
      return gfx_context.roundRect(x, y, width, height, radii) }`,
      {});
      
  gfxFunction("fill-rect", "fillRect",
    `function gfx_context_fill_rect(gfx_context, x = 0, y = 0, width = 1, height = 1) {
      return gfx_context.fillRect(x, y, width, height) }`,
      {});
      
  gfxFunction("clear-rect", "clearRect",
    `function gfx_context_clear_rect(gfx_context, x = 0, y = 0, width = 1, height = 1) {
      return gfx_context.clearRect(x, y, width, height) }`,
      {});
        
  gfxFunction("stroke-rect", "strokeRect",
    `function gfx_context_stroke_rect(gfx_context, x = 0, y = 0, width = 1, height = 1) {
      return gfx_context.strokeRect(x, y, width, height) }`,
      {});
          
  gfxFunction("fill-text", "fillText",
    `function gfx_context_fill_text(gfx_context, x = 0, y = 0, text = "", maxWidth = optional) {
      return gfx_context.fillText(x, y, text, maxWidth) }`,
      {});
            
  gfxFunction("measure-text", "measureText",
    `function gfx_context_measure_text(gfx_context, text = "") {
      return gfx_context.measureText(text) }`,
      {});
              
    
  gfxProp("canvas-element", "canvas", {});
  gfxProp("canvas-width", "canvas.width", {});
  gfxProp("canvas-height", "canvas.height", {});
  gfxProp("line-width", "lineWidth", {});
  gfxProp("line-cap", "lineCcap", {});
  gfxProp("line-join", "lineJoin", {});
  gfxProp("miter-limit","miterLimit", {});

  gfxFunction("get-line-dash", "getLineDash",
    `function gfx_context_get_line_dash(gfx_context) {
      return gfx_context.getLineDash() }`,
      {});

  gfxFunction("set-line-dash", "setLineDash",
    `function gfx_context_set_line_dash(gfx_context, value = []) {
      return gfx_context.setLineDash(value) }`,
      {});
  

  gfxProp("line-dash-offset", "lineDashOffset", {});
  gfxProp("font", "font", {});
  gfxProp("text-align", "textAlign", {});
  gfxProp("text-baseline", "textBaseline", {});
  gfxProp("direction", "direction", {});
  gfxProp("fill-style", "fillStyle", {});
  gfxProp("stroke-style", "strokeStyle", {});

  gfxFunction("create-conic-gradient", "createConicGradient",
    `function gfx_context_create_conic_gradient(gfx_context, startAngle = 0, x = 0, y = 1) {
      return gfx_context.createConicGradient(startAngle, x, y) }`,
      {});

  gfxFunction("create-linear-gradient", "createLinearGradient",
    `function gfx_context_create_linear_gradient(gfx_context, x0 = 0, y0 = 0, x1 = 1, y1 = 1) {
      return gfx_context.createLinearGradient(x0, y0, x1, y1) }`,
      {});
  
  gfxFunction("create-radial-gradient", "createRadialGradient",
    `function gfx_context_create_radial_gradient(gfx_context, x0 = 0, y0 = 0, r0 = 1, x1 = 1, y1 = 1, r1 = 0) {
      return gfx_context.createRadialGradient(x0, y0, r0, x1, y1, r1) }`,
      {});
    
  gfxFunction("create-pattern", "createPattern",
    `function gfx_context_create_pattern(gfx_context, image = optional, repetition = "repeat") {
      return gfx_context.createPattern(image, repetition) }`,
      {});
      
  gfxProp("shadow-color", "shadowColor", {});
  gfxProp("shadow-offset-x", "shadowOffsetX", {});
  gfxProp("shadow-offset-y", "shadowOffsetY", {});
  gfxFunction("fill", "fill",
    `function gfx_context_fill(gfx_context) {
      return gfx_context.fill() }`,
      {});

  gfxFunction("stroke", "stroke",
    `function gfx_context_stroke(gfx_context) {
      return gfx_context.stroke() }`,
     {});

  gfxFunction("draw-focus-if-needed", "drawFocusIfNeeded",
    `function gfx_context_draw_focus_if_needed(gfx_context, ...params) {
      return gfx_context.drawFocusIfNeeded(...params) }`,
      {});

  gfxFunction("scroll-path-into-view", "scrollPathIntoView",
    `function gfx_context_scroll_path_into_view(gfx_context, ...params) {
      return gfx_context.scrollPathIntoView(...params) }`,
      {});
  
  gfxProp("global-alpha", "globalAlpha", {});
  gfxProp("global-composite-operation", "globalCompositeOperation", {});

  gfxFunction("draw-image", "drawImage",
    `function gfx_context_draw_image(gfx_context, ...params) {
      return gfx_context.drawImage(...params) }`,
      {});
  
  gfxFunction("create-image-data", "createImageData",
    `function gfx_context_create_image_data(gfx_context, ...params) {
      return gfx_context.createImageData(...params) }`,
      {});
    
  gfxFunction("get-image-data", "getImageData",
    `function gfx_context_get_image_data(gfx_context, sx = 0, sy = 0, sw = 1, sh = 1) {
      return gfx_context.getImageData(sx, sy, sw, sh) }`,
      {});
      
  gfxProp("image-smoothing-enabled", "imageSmoothingEnabled", {});
  gfxProp("image-smoothing-quality", "imageSmoothingQuality", {});

  return globalScope;
}