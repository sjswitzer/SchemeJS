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
// done. As a general rule, drawing functions should, by convention, take no placement
// or scale operations and instead draw inside of a 1x1 box with an origin
// at (0, 0). You can use any coordinate system you want in your drawing function
// by writing
//     (gfx-save (scale (/ 100) (/ 200)) draw-stuff here)
// to draw in a 100x200 box for instance. And if a client wants to place
// it at (15, 20) and have it be 30 x 40 in size, they can write
//     (gfx-save (translate 15 20) (scale 30 40) (your-function))
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
  const defineBinding = defineSchemeBindings ? (globalScope.defineBinding ?? required()) : _ => undefined;

  const string = globalScope.string ?? required();
  const exportAPI = globalScope.exportAPI ?? required();
  const list = globalScope.list ?? required();
  const isList = globalScope.isList ?? required();
  const FIRST = globalScope.FIRST ?? required();
  const REST = globalScope.REST ?? required();
  const Atom = globalScope.Atom ?? required();
  const js_try = globalScope.try ?? required();
  const js_finally = globalScope.finally ?? required();
  const MACRO_TAG = globalScope.MACRO_TAG ?? required();
  const BOTTOM = globalScope.BOTTOM; // Can't "require" it because "undefined" is indeed a bottom.
  function required() { throw "required" }

  const gfxContextAtom = Atom("gfx-context");
  const htmlDocumentAtom = Atom("html-document");

  function gfxFunction(boundName, name, functionName, params, opts = {}) {
    const fn = (gfx_context, ...args) => gfx_context[functionName](...args);
    exportAPI(name, macro, { tag: MACRO_TAG });
    function macro(params) {
      return list(fn, gfxContextAtom, ...params);
    }
    let paramStr = '', argStr = '', sep = '';
    for (let param of params) {
      let paramName = param;
      let defaultStr = '';
      if (isList(param)) {
        paramName = param[REST][FIRST].description;
        let paramDefault = param[REST][REST][FIRST];
        if (paramDefault === undefined) paramDefault = "optional";
        else paramDefault = string(paramDefault);
        defaultStr = ` = ${paramDefault}`;
      }
      paramStr += `${sep}${paramName}${defaultStr}`;
      argStr += `${sep}${paramName}`;
      sep = ', ';
    }
    defineBinding(boundName, name, { group: "web-gfx", gfxApi: functionName,
      implStr: `(${[paramStr]}) => gfx_context.${functionName}(${argStr})`, ...opts });
  }

  function opt(param, dflt) {
    let paramAtom = Atom(param);
    return list( ((paramVal, dflt) => paramVal !== undefined ? paramVal : dflt), paramAtom, dflt );
  }

  function gfxVarArgFunction(boundName, name, functionName, opts = {}) {
    const fn = (gfx_context, ...args) => gfx_context[functionName](...args);
    exportAPI(name, macro, { tag: MACRO_TAG });
    function macro(params) {
      return list(fn, gfxContextAtom, ...params);
    }
    defineBinding(boundName, name, { group: "web-gfx", gfxApi: functionName,
      implStr: `(...params) => gfx_context.${functionName}(...params)`, ...opts });
  }

  function gfxProp(boundName, name, propName, opts = {}) {
    // Normally I'd use a closure here but this lets me use a "propName" of "canvas.width".
    const fn = new Function('gfx_context', 'value',
      `let oldValue = gfx_context.${propName}; if (value !== undefined) gfx_context.${propName} = value; return oldValue`);
    exportAPI(name, macro, { tag: MACRO_TAG });
    function macro(params) {
      return list(fn, gfxContextAtom, ...params);
    }
    defineBinding(boundName, name, { group: "web-gfx", gfxApi: propName,
      implStr: `(value = optional) => { let oldValue = gfx_context.${propName}; if (value !== undefined) gfx_context.${propName} = value; return oldValue }`,
      ...opts });
  }

  exportAPI("gfx_save", gfx_save, { tag: MACRO_TAG });
  function gfx_save(params) {
    let forms = params;
    return list( js_finally, [ list( (gfx_context => gfx_context.restore()), gfxContextAtom ) ],
      list( (gfx_context => gfx_context.save()), gfxContextAtom ),
      ... forms );
  }
  defineBinding("gfx-save", "gfx_save", { group: "web-gfx", sample: `(gfx-save form ...)`,
    blurb: `Saves the graphics context, executes the forms, then restores the context afterward. ` +
           `you should generally use this instead of save-gfx-context and restore-gfx-context, though ` +
           `though those can be useful in a REPL.` });

  gfxFunction("translate", "translate", "translate", [ opt("x", 0), opt("y", 0) ]);
  gfxFunction("rotate", "rotate", "rotate", [ opt("angle", 0)]);

  exportAPI("scale", gfx_scale, { tag: MACRO_TAG });
  function gfx_scale(params) {
    let x = params[0] ?? 0, y = params[1] ?? x;
    return list( ((gfx_context, x, y) => gfx_context.scale(x, y)), gfxContextAtom, x, y );
  }
  defineBinding("scale", "gfx_scale", { group: "web-gfx", sample: `(gfx-save form ...)`,
    blurb: `Saves the graphics context, executes the forms, then restores the context afterward.` });

  gfxFunction("save-gfx-context", "save_gfx_context", "save", [], {
    blurb: `Saves the graphics context on a stack which can later be "popped" with ` +
           `pop-gfx-context. You should generally use gfx-save instead, but these operations ` +
           `can be useful in a REPL.` });
  gfxFunction("restore-gfx-context", "restore_gfx_context", "restore", [], {
    blurb: `Saves the graphics context on a stack which can later be "popped" with ` +
           `pop-gfx-context. You should generally use gfx-save instead, but these operations ` +
           `can be useful in a REPL.` });
  gfxFunction("begin-path", "begin_path", "beginPath", []);
  gfxFunction("close-path", "close_path", "closePath", []);
  gfxVarArgFunction("clip", "clip", "clip");
  gfxFunction("move-to", "move_to", "moveTo", [ opt("x", 0), opt("y", 0) ]);
  gfxFunction("line-to", "line_to", "lineTo", [ opt("x", 0), opt("y", 1) ]);
  gfxFunction("bezier-curve-to", "bezier_curve_to", "bezierCurveTo",
    [ opt("cp1x", 1), opt("cp1y", 0), opt("cp2x", 0), opt("cp2y", 1), opt("x", 1), opt("y", 1) ]);
  gfxFunction("quadratic_curve_to", "quadratic_curve_to", "quadraticCurveTo",
    [ opt("cpx", 1), opt("cpy", 0), opt("x", 1), opt("y", 1) ]);
  gfxFunction("arc", "arc", "arc",
    [ opt("x", 1), opt("y", 1), opt("radius", .5), opt("startAngle", 0), opt("endAngle", 2*pi), opt("counterclockwise", false) ]);
  gfxFunction("arc-to", "arc_to", "arcTo",
    [ opt("x1", 1), opt("y1", 0), opt("x2", 1), opt("y2", 1), opt("radius", 1) ]);
  gfxFunction("ellipse", "ellipse", "ellipse", // defaults to a circle inscribing (0,0,1,1)
    [ opt("x", .5), opt("y", .5), opt("radiusX", .5), opt("radiusY", .5),
      opt("rotation", 0), opt("startAngle", 0), opt("endAngle", 2*pi), opt("counterclockwise", false) ]);
  gfxFunction("rect", "rect", "rect",
    [ opt("x", 0), opt("y", 0), opt("width", 1), opt("height", 1) ]);
  gfxFunction("fill-rect", "fill_rect", "fillRect",
    [ opt("x", 0), opt("y", 0), opt("width", 1), opt("height", 1) ]);
  gfxFunction("clear-rect", "clear_rect", "clearRect",
    [ opt("x", 0), opt("y", 0), opt("width", 1), opt("height", 1) ]);
  gfxFunction("stroke-rect", "stroke_rect", "strokeRect",
    [ opt("x", 0), opt("y", 0), opt("width", 1), opt("height", 1) ]);
  gfxFunction("fill-text", "fill_text", "fillText",
    [ "text", opt("x", 0), opt("y", 0), opt("maxWidth", optional)]);
  gfxFunction("measure-text", "measure_text", "measureText", [ "text" ]);
  gfxProp("canvas-width", "canvas_width", "canvas.width");
  gfxProp("canvas-height", "canvas_height", "canvas.height");
  gfxProp("line-width", "line_width", "lineWidth");
  gfxProp("line-cap", "line_cap", "lineCap");
  gfxProp("line-join", "line_join", "lineJoin");
  gfxProp("miter-limit", "miter_limit", "miterLimit");
  gfxFunction("get-line-dash", "get_line_dash", "getLineDash", []);
  gfxFunction("set-line-dash", "set_line_dash", "setLineDash", ["value"]);
  gfxProp("line-dash-offset", "line_dash_offset", "lineDashOffset");
  gfxProp("font", "font", "font");
  gfxProp("text-align", "text_align", "textAlign");
  gfxProp("text-baseline", "text_baseline", "textBaseline");
  gfxProp("direction", "direction", "direction");
  gfxProp("fill-style", "fill_style", "fillStyle");
  gfxProp("stroke-style", "stroke_style", "strokeStyle");
  gfxFunction("create-conic-gradient", "create_conic_gradient", "createConicGradient",
    [ opt("startAngle", 0), opt("x", 0), opt("y", 1) ]);
  gfxFunction("create-linear-gradient", "create_linear_gradient", "createLinearGradient",
    [ opt("x0", 0), opt("y0", 0), opt("x1", 1), opt("y1", 1) ]);
  gfxFunction("create-radial-gradient", "create_radial_gradient", "createRadialGradient",
    [ opt("x0", 0), opt("y0", 0), opt("r0", 1), opt("x1", 1), opt("y1", 1), opt("r1", 0) ]);
  gfxFunction("create-pattern", "create_pattern", "createPattern", [ "image", opt("repetition", "repeat") ]);
  gfxProp("shadow-color", "shadow_color", "shadowColor");
  gfxProp("shadow-offset-x", "shadow_offset_x", "shadowOffsetX");
  gfxProp("shadow-offset-y", "shadow_offset_y", "shadowOffsetY");
  gfxFunction("fill", "fill", "fill", []);
  gfxFunction("stroke", "stroke", "stroke", []);
  gfxFunction("draw-focus-if-needed", "draw_focus_if_needed", "drawFocusIfNeeded", []);
  gfxProp("global-alpha", "global_alpha", "globalAlpha");
  gfxProp("global-composite-operation", "global_composite_operation", "globalCompositeOperation");
  gfxFunction("get-image-data", "get_image_data", "getImageData",
    [ opt("sx", 0), opt("sy", 0), opt("sw", 1), opt("sh", 1) ]);
  gfxProp("image-smoothing-enabled", "image_smoothing_enabled", "imageSmoothingEnabled");
  gfxProp("image-smoothing-quality", "image_smoothing_quality", "imageSmoothingQuality");

  // Buncha APIs with variable args
  function gfxVarArgFunction(boundName, name, functionName, opts = {}) {};
  gfxVarArgFunction("draw-focus-if-needed", "draw_focus_if_needed", "drawFocusIfNeeded");
  gfxVarArgFunction("scroll-path-into-view", "scroll_path_into_view", "scrollPathIntoView");
  gfxVarArgFunction("is-point-in-path", "is_point_in_path", "isPointInPath");
  gfxVarArgFunction("is-point-in-stroke", "is_point_in_stroke", "isPointInStroke");
  gfxVarArgFunction("draw-image", "draw_image", "drawImage");
  gfxVarArgFunction("create-image-data", "create_image_data", "createImageData");

  return globalScope;
}