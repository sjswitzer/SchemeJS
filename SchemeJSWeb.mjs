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
  const defineSchemeBindings = schemeOpts.defineSchemeBindings;
  const defineBinding = defineSchemeBindings? (globalScope.defineBinding ?? required()) : _ => undefined;
  const string = globalScope.string ?? required();
  const exportAPI = globalScope.exportAPI ?? required();
  const list = globalScope.list ?? required();
  const Pair = globalScope.Pair ?? required();
  const call = globalScope.call ?? required();
  const isList = globalScope.isList ?? required();
  const isAtom = globalScope.isAtom ?? required();
  const def = globalScope.def ?? required;
  const FIRST = globalScope.FIRST ?? required();
  const REST = globalScope.REST ?? required();
  const Atom = globalScope.Atom ?? required();
  const js_finally = globalScope.finally ?? required();
  const MACRO_TAG = globalScope.MACRO_TAG ?? required();
  const restParamStr = schemeOpts.restParamStr ?? required();
  const BOTTOM = globalScope.BOTTOM; // Can't "require" it because "undefined" is indeed a bottom.
  function required() { throw "required" }

  const gfxContextAtom = Atom("gfx-context");
  const htmlDocumentAtom = Atom("html-document");
  const restParamAtom = Atom(restParamStr);
  const pi = Math.PI;

  // Defines the graphics function and a macro to call it
  // This is how you write scheme with no parser or bindings :)
  function gfxFunction(boundName, name, jsFunctionName, params, opts = {}) {
    let paramStr = '', argStr = '', sep = '', nextIsRest = false;
    for (let param of params) {
      let paramName = param;
      let defaultStr = '';
      if (isList(param)) {
        paramName = param[FIRST].description;
        let paramDefault = param[REST][FIRST];
        if (paramDefault === 2*pi) paramDefault = "2*pi";
        if (paramDefault === undefined) paramDefault = "optional";
        else paramDefault = string(paramDefault);
        defaultStr = ` = ${paramDefault}`;
      } else if (param === restParamAtom) {
        nextIsRest = true;
        continue;
      } else if (nextIsRest) {
        paramName = `...${param.description}`;
      } else {
        paramName = param.description;
      }
      paramStr += `${sep}${paramName}${defaultStr}`;
      argStr += `${sep}${paramName}`;
      sep = ', ';
    }
    let commaParamStr = paramStr ? `, ${paramStr}` : ''
    let jsGfxContextFnName = `gfx_context_${jsFunctionName}`;
    let gfxContextFnName = `gfx-context-${boundName}`, gfxContextFnAtom = Atom(gfxContextFnName);
    let fnBody =  `let pi = Math.PI, optional = undefined; ` +
      `return function ${jsGfxContextFnName} (gfx_context${commaParamStr}) { ` +
      `return gfx_context.${jsFunctionName}(${argStr}) }`;
    let fn = new Function(fnBody);
    exportAPI(jsGfxContextFnName, jsGfxContextFnName, { requiredCount: 0 });
    defineBinding(gfxContextFnName, fn, { group: "web-gfx-context", gfxApi: jsGfxContextFnName,
      implStr: `(gfx_context${commaParamStr} => gfx_context.${jsFunctionName}(${argStr})`, ...opts });

    // Not using defMacro so I can give it some descriptive info in defBinding, below
    exportAPI(name, params => new Pair(gfxContextFnAtom, params), { tag: MACRO_TAG });
    defineBinding(boundName, name, { group: "web-gfx", gfxApi: jsFunctionName,
      implStr: `(${[paramStr]}) => gfx_context.${jsFunctionName}(${argStr})`, ...opts });
  }

  function gfxProp(boundName, name, propName, opts = {}) {
    const fn = new Function('gfx_context', 'value',
      `let oldValue = gfx_context.${propName}; if (value !== undefined) gfx_context.${propName} = value; return oldValue`);
    exportAPI(name, params => new Pair(gfxContextFnAtom, params), { tag: MACRO_TAG });
    defineBinding(boundName, name, { group: "web-gfx", gfxApi: propName,
      implStr: `(value = optional) => { let oldValue = gfx_context.${propName}; if (value !== undefined) gfx_context.${propName} = value; return oldValue }`,
      ...opts });
  }

  exportAPI("gfx_save", gfx_save, { tag: MACRO_TAG });
  function gfx_save(params) {
    let forms = params;
    return list( js_finally, [ list( (gfx_context => gfx_context.restore()), gfxContextAtom ) ],
      list( (gfx_context => gfx_context.save()), gfxContextAtom ),
      ...forms );
  }
  defineBinding("gfx-save", "gfx_save", { group: "web-gfx", sample: `(gfx-save form ...)`,
    blurb: `Saves the graphics context, executes the forms, then restores the context afterward. ` +
           `You should generally use this instead of save-gfx-context and restore-gfx-context, though ` +
           `though those can be useful in a REPL.` });

  gfxFunction("translate", "translate", "translate", [ [Atom("x"), 0], [Atom("y"), 0] ], {
    blurb: `Translates the origin by "x" and "y"`});
  gfxFunction("rotate", "rotate", "rotate", [ [Atom("angle"), 0] ], {
    blurb: `Rotates the coordinate system by "angle"`});
  gfxFunction("scale", "scale", "scale", [ [Atom("width"), 1], [Atom("height"), Atom("width")] ], {
    blurb: `Scales the coordinate system by "width" and "height`});
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
  gfxVarArgFunction("clip", "clip", "clip", []);
  gfxFunction("is-point-in-path", "is_point_in_path", "isPointInPath", [restParamAtom, Atom("params")]);
  gfxFunction("is-point-in-stroke", "is_point_in_stroke", "isPointInStroke", [restParamAtom, Atom("params")]);
  gfxFunction("move-to", "move_to", "moveTo", [ [Atom("x"), 0], [Atom("y"), 0] ]);
  gfxFunction("line-to", "line_to", "lineTo", [ [Atom("x"), 0], [Atom("y"), 1] ]);
  gfxFunction("bezier-curve-to", "bezier_curve_to", "bezierCurveTo",
    [ [Atom("cp1x"), 1], [Atom("cp1y"), 0], [Atom("cp2x"), 0], [Atom("cp2y"), 1], [Atom("x"), 1], [Atom("y"), 1] ]);
  gfxFunction("quadratic_curve_to", "quadratic_curve_to", "quadraticCurveTo",
    [ [Atom("cpx"), 1], [Atom("cpy"), 0], [Atom("x"), 1], [Atom("y"), 1] ]);
  gfxFunction("arc", "arc", "arc",
    [ [Atom("x"), 1], [Atom("y"), 1], [Atom("radius"), .5], [Atom("startAngle"), 0], [Atom("endAngle"), .2*pi], [Atom("counterclockwise"), false] ]);
  gfxFunction("arc-to", "arc_to", "arcTo",
    [ [Atom("x1"), 1], [Atom("y1"), 0], [Atom("x2"), 1], [Atom("y2"), 1], [Atom("radius"), 1] ]);
  gfxFunction("ellipse", "ellipse", "ellipse", // defaults to a circle inscribing (0,0,1,1)
    [ [Atom("x"), .5], [Atom("y"), .5], [Atom("radiusX"), .5], [Atom("radiusY"), .5],
      [Atom("rotation"), 0], [Atom("startAngle"), 0], [Atom("endAngle"), .2*pi], [Atom("counterclockwise"), false] ]);
  gfxFunction("rect", "rect", "rect",
    [ [Atom("x"), 0], [Atom("y"), 0], [Atom("width"), 1], [Atom("height"), 1] ]);
  gfxFunction("fill-rect", "fill_rect", "fillRect",
    [ [Atom("x"), 0], [Atom("y"), 0], [Atom("width"), 1], [Atom("height"), 1] ]);
  gfxFunction("clear-rect", "clear_rect", "clearRect",
    [ [Atom("x"), 0], [Atom("y"), 0], [Atom("width"), 1], [Atom("height"), 1] ]);
  gfxFunction("stroke-rect", "stroke_rect", "strokeRect",
    [ [Atom("x"), 0], [Atom("y"), 0], [Atom("width"), 1], [Atom("height"), 1] ]);
  gfxFunction("fill-text", "fill_text", "fillText",
    [ "text", [Atom("x"), 0], [Atom("y"), 0], [Atom("maxWidth"), optional]]);
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
    [ [Atom("startAngle"), 0], [Atom("x"), 0], [Atom("y"), 1] ]);
  gfxFunction("create-linear-gradient", "create_linear_gradient", "createLinearGradient",
    [ [Atom("x0"), 0], [Atom("y0"), 0], [Atom("x1"), 1], [Atom("y1"), 1] ]);
  gfxFunction("create-radial-gradient", "create_radial_gradient", "createRadialGradient",
    [ [Atom("x0"), 0], [Atom("y0"), 0], [Atom("r0"), 1], [Atom("x1"), 1], [Atom("y1"), 1], [Atom("r1"), 0] ]);
  gfxFunction("create-pattern", "create_pattern", "createPattern", [ "image", [Atom("repetition"), "repeat"] ]);
  gfxProp("shadow-color", "shadow_color", "shadowColor");
  gfxProp("shadow-offset-x", "shadow_offset_x", "shadowOffsetX");
  gfxProp("shadow-offset-y", "shadow_offset_y", "shadowOffsetY");
  gfxFunction("fill", "fill", "fill", []);
  gfxFunction("stroke", "stroke", "stroke", []);
  gfxFunction("draw-focus-if-needed", "draw_focus_if_needed", "drawFocusIfNeeded", [restParamAtom, Atom("params")]);
  gfxFunction("scroll-path-into-view", "scroll_path_into_view", "scrollPathIntoView", [restParamAtom, Atom("params")]);
  gfxProp("global-alpha", "global_alpha", "globalAlpha");
  gfxProp("global-composite-operation", "global_composite_operation", "globalCompositeOperation");
  gfxFunction("draw-image", "draw_image", "drawImage", [restParamAtom, Atom("params")]);
  gfxFunction("create-image-data", "create_image_data", "createImageData", [restParamAtom, Atom("params")]);
  gfxFunction("get-image-data", "get_image_data", "getImageData",
    [ [Atom("sx"), 0], [Atom("sy"), 0], [Atom("sw"), 1], [Atom("sh"), 1] ]);
  gfxProp("image-smoothing-enabled", "image_smoothing_enabled", "imageSmoothingEnabled");
  gfxProp("image-smoothing-quality", "image_smoothing_quality", "imageSmoothingQuality");

  return globalScope;
}