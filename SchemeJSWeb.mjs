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
  const MACRO_TAG = globalScope.MACRO_TAG ?? required();
  const BOTTOM = globalScope.BOTTOM; // Can't "require" it because "undefined" is indeed a bottom.
  function required() { throw "required" }

  const gfxContextAtom = Atom("gfx-context");
  const htmlDocumentAtom = Atom("html-document");

  function gfxFunction(boundName, name, functionName, params, opts = {}) {
    exportAPI(name, macro, { tag: MACRO_TAG });
    function macro(params) {
      const fn = (gfx_context, ...args) => gfx_context[functionName](...args);
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

  function gfxProp(boundName, name, propName, opts = {}) {
    exportAPI(name, macro, { tag: MACRO_TAG });
    function macro(params) {
      const fn = (gfx_context, value = optional) => { let oldValue = gfx_context[propName]; if (value !== undefined) gfx_context[propName] = value; return oldValue };
      return list(fn, gfxContextAtom, ...params);
    }
    defineBinding(boundName, name, { group: "web-gfx", gfxApi: propName,
      implStr: `(value = optional) => { let oldValue = gfx_context.${propName}; if (value !== undefined) gfx_context.${propName} = value; return oldValue }`,
      ...opts });
  }

  gfxFunction("move-to", "move_to", "moveTo", [ opt("x", 0), opt("y", 0) ]);
  gfxFunction("line-to", "line_to", "lineTo", [ opt("x", 0), opt("y", 1) ]);
  gfxFunction("fill-rect", "fill_rect", "fillRect",
    [ opt("x", 0), opt("y", 0), opt("width", 1), opt("height", 1) ]);
  gfxFunction("clear-rect", "clear_rect", "clearRect",
    [ opt("x", 0), opt("y", 0), opt("width", 1), opt("height", 1) ]);
  gfxFunction("stroke-rect", "stroke_rect", "strokeRect",
    [ opt("x", 0), opt("y", 0), opt("width", 1), opt("height", 1) ]);
  gfxFunction("fill-text", "fill_text", "fillText",
    [ "text", opt("x", 0), opt("y", 0), opt("maxWidth", optional)]);
  gfxFunction("measure-text", "measure_text", "measureText", [ "text" ]);
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
  gfxFunction("begin-path", "begin_path", "beginPath", []);
  gfxFunction("close-path", "close_path", "closePath", []);
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
  gfxFunction("fill", "fill", "fill", []);
  gfxFunction("stroke", "stroke", "stroke", []);
  gfxFunction("draw-focus-if-needed", "draw_focus_if_needed", "drawFocusIfNeeded", []);
  gfxProp("global-alpha", "global_alpha", "globalAlpha");
  gfxProp("global-composite-operation", "global_composite_operation", "globalCompositeOperation");
  gfxFunction("get-image-data", "get_image_data", "getImageData",
    [ opt("sx", 0), opt("sy", 0), opt("sw", 1), opt("sh", 1) ]);
  gfxProp("image-smoothing-enabled", "image_smoothing_enabled", "imageSmoothingEnabled");
  gfxProp("image-smoothing-quality", "image_smoothing_quality", "imageSmoothingQuality");
  
  gfxProp("xxx", "xxx", "xxx");
  gfxProp("xxx", "xxx", "xxx");
  gfxProp("xxx", "xxx", "xxx");
    gfxFunction("xxx", "xxx", "xxx",
    []);

  gfxFunction("xxx", "xxx", "xxx", []);
  gfxFunction("xxx", "xxx", "xxx", []);
  gfxFunction("xxx", "xxx", "xxx", []);


  exportAPI("draw_focus_if_needed", draw_focus_if_needed, { compileHook: CtxFnHookHook('drawFocusIfNeeded') });
  function draw_focus_if_needed(...params) { return this[gfxContextAtom].drawFocusIfNeeded(...params) }

  exportAPI("scroll_path_into_view", scroll_path_into_view, { compileHook: CtxFnHookHook('scrollPathIntoView') });
  function scroll_path_into_view(...params) { return this[gfxContextAtom].scrollPathIntoView(...params) }

  exportAPI("is_point_in_path", is_point_in_path, { compileHook: CtxFnHookHook('isPointInPath') });
  function is_point_in_path(...params) { return this[gfxContextAtom].isPointInPath(...params) }

  exportAPI("is_point_in_stroke", is_point_in_stroke, { compileHook: CtxFnHookHook('isPointInStroke') });
  function is_point_in_stroke(...params) { return this[gfxContextAtom].isPointInStroke(...params) }

  exportAPI("clip", clip, { compileHook: CtxFnHookHook('clip') });
  function clip(...params) { return this[gfxContextAtom].clip(...params) }

  exportAPI("draw_image", draw_image, { compileHook: CtxFnHookHook('drawImage') });
  function draw_image(...params) { return this[gfxContextAtom].drawImage(...params) }

  exportAPI("create_image_data", createImageData, { compileHook: CtxFnHookHook('createImageData') });
  function createImageData(...params) { return this[gfxContextAtom].createImageData(...params) }




  // When there are macros, this can be rewritten as a LET binding, probably
  exportAPI("gfx_save", gfx_save, { evalArgs: 0, compileHook: gfx_save_hook });
  function gfx_save(...forms) {
    let scope = this, canvasRenderingContext = scope[gfxContextAtom];
    let RETURN_SYMBOL = globalScope.RETURN_SYMBOL;
    if (!RETURN_SYMBOL) throw "required symbol"
    canvasRenderingContext.save();
    try {
      let res = BOTTOM;
      for (let form of forms) {
        res = scope._eval(form);
        if (this[RETURN_SYMBOL]) return;
      }
      return res;
    } finally {
      canvasRenderingContext.restore();
    }
    return canvasRenderingContext;
  }
  function gfx_save_hook(args, ssaScope, tools) {
    let compileEval = tools.compileEval, emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
    let ssaGfxContext = compileEval(gfxContextAtom, ssaScope, tools);
    let ssaGfxContextAtom = use(bind(gfxContextAtom));
    emit(`${ssaGfxContext}.save();`);
    ssaScope = newScope(ssaScope, 'gfx-save-scope');
    ssaScope[gfxContextAtom] = ssaGfxContext;
    let saveSsaScope = ssaScope, scopeLines = [];
    let ssaTmpScope = newTemp("scope_tmp");
    scopeLines.push(emit(`let ${ssaTmpScope} = scope;`));
    emit(`try {`);
    let saveIndent = tools.indent;
    tools.indent += '  ';
    scopeLines.push(emit(`let scope = ${ssaTmpScope};`));
    scopeLines.push(emit(`scope[${ssaGfxContextAtom}] = ${ssaGfxContext};`));
    for (let arg of args)
      compileEval(arg, ssaScope, tools);
    tools.indent = saveIndent;
    emit(`} finally { ${ssaGfxContext}.restore() }`);
    if (ssaScope.dynamicScopeUsed)
      saveSsaScope.dynamicScopeUsed = true;
    else
      tools.deleteEmitted(scopeLines);
    return ssaGfxContext;
  }

  function CtxFnHookHook(fName, opts = {}) {
    let dup1 = opts.dup1 ?? false;
    return function crcHookFn(args, ssaScope, tools) {
      let compileEval = tools.compileEval, emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
      let ssaCRC = ssaScope[gfxContextAtom];
      if (!ssaCRC)
        ssaCRC = compileEval(gfxContextAtom, ssaScope, tools);
      let ssaArgStr = '', sep = '';
      if (dup1) {
        let ssaArg1Val = newTemp('dupFirst');
        emit(`let ${ssaArg1Val} = ${args[1]};`);
        emit(`if (${ssaArg1Val} === undefined) ${ssaArg1Val} = ${args[0]}`);
        args[1] = ssaArg1Val;
      }
      for (let arg of args)
        ssaArgStr += `${sep}${arg}`, sep = ', ';
      emit(`${ssaCRC}.${fName}(${ssaArgStr});`);
      return ssaCRC;
    }
  }

  function CanvasRenderingContextProperty(property) {
    return function get_set_crc_property(newValue = optional) {
      let canvasRenderingContext = this[gfxContextAtom];
      let value = canvasRenderingContext[property];
      if (newValue !== undefined)
        canvasRenderingContext[property] = newValue;
      return value;
    }
  }

  function CanvasRenderingContextPropertyHook(propName) {
    return function crcHookProp(args, ssaScope, tools) {
      let compileEval = tools.compileEval, emit = tools.emit, newTemp = tools.newTemp, bind = tools.bind, use = tools.use;
      let ssaCRC = ssaScope[gfxContextAtom];
      if (!ssaCRC)
        ssaCRC = compileEval(gfxContextAtom, ssaScope, tools);
      let ssaRes = newTemp(propName + '_value');
      emit(`let ${ssaRes} = ${ssaCRC}.${propName};`);
      if (args.length > 0) {
        let ssaVal = args[0];
        emit(`if (${ssaVal} !== undefined)`);
        emit(`  ${ssaCRC}.${propName} = ${ssaVal};`);
      }
      return ssaRes;
    }
  }

  exportAPI("translate", translate, { compileHook: CtxFnHookHook('translate') });
  function translate(x, y) { return this[gfxContextAtom].translate(x, y) }

  exportAPI("scale", scale, { compileHook: CtxFnHookHook('scale', { dup1: true }) });
  function scale(width, height = width) { return this[gfxContextAtom].scale(width, height) }

  exportAPI("rotate", rotate, { compileHook: CtxFnHookHook('rotate') });
  function rotate(theta) { return this[gfxContextAtom].rotate(theta) }

  // You probably shouldn't be using these (use gfx-save instead!),
  // but for completeness...
  exportAPI("save_context", save_context, { compileHook: CtxFnHookHook('save') });
  function save_context() { return this[gfxContextAtom].save() }

  exportAPI("restore_context", restore_context, { compileHook: CtxFnHookHook('restore') });
  function restore_context() { return this[gfxContextAtom].restore() }

  exportAPI("canvas_width", canvas_width, { compileHook: CanvasRenderingContextPropertyHook('canvas.width') });
  function canvas_width() { return this[gfxContextAtom].canvas.width; }

  exportAPI("canvas_height", canvas_height, { compileHook: CanvasRenderingContextPropertyHook('canvas.height') });
  function canvas_height() { return this[gfxContextAtom].canvas.height }

  //
  // Bindings!
  //

  if (defineSchemeBindings) {
    defineBinding("VERSION", "VERSION", {
      group: "main", sample: `VERSION`,
      blurb: `The SchemeJSWeb version`
    });

    defineBinding("gfx-save", "gfx_save", { group: "web-gfx", sample: `(gfx-save form ...)`,
      blurb: `Saves the graphics context, executes the forms then restores the context. ` +
             `Returns the value of the last form.` });
    defineBinding("canvas-width", "canvas_width", { group: "web-gfx",
      sample: `(canvas width) -or- (canvas-width new-width)`,
      blurb:  `Returns the current canvas width and optionally sets it to a new value.` });
    defineBinding("canvas-height", "canvas_height", { group: "web-gfx",
      sample: `(canvas height) -or- (canvas-height new-width) in "pixels" and optionally sets it to a new value.` });

    //
    // Complete Canvas 2D drawing APIs:
    //

    // You probably shouldn't be using these two operations (use gfx-save instead!),
    // but for completeness...
    defineBinding("save-context", "save_context", { group: "web-gfx", gfxApi: 'save',
      sample: `(save-context)`,
      blurb: `Saves the current graphics state until the corresponding call to ` +
              `(restore-context). Generally, you should avoid these operations and use (gfx-save form ...)` +
              `which saves the context, invokes the forms, then restores it when done.`});
    defineBinding("restore-context", "restore_context", { group: "web-gfx", gfxApi: 'restore',
      sample: `(save-context)`,
      blurb: `Restores the graphics state saved at the corresponding call to ` +
            `(save-context). Generally, you should avoid these operations and use (gfx-save form ...)` +
            `which saves the context, invokes the forms, then restores it when done.` });

    // But all of these are just fine...
    defineBinding("translate", "translate", { group: "web-gfx", gfxApi: 'translate' })
    defineBinding("scale", "scale", { group: "web-gfx", gfxApi: 'scale' });
    defineBinding("rotate", "rotate", { group: "web-gfx", gfxApi: 'rotate' });
    defineBinding("fill-rect", "fill_rect", { group: "web-gfx", gfxApi: 'fillRect' });
    defineBinding("clear-rect", "clear_rect", { group: "web-gfx", gfxApi: 'clearRect' });
    defineBinding("stroke-rect", "stroke_rect", { group: "web-gfx", gfxApi: 'strokeRect' });
    defineBinding("fill-text", "fill_text", { group: "web-gfx", gfxApi: 'fillText' });
    defineBinding("measure-text", "measure_text", { group: "web-gfx", gfxApi: 'measureText' });
    defineBinding("line-width", "line_width", { group: "web-gfx", gfxApi: 'lineWidth' });
    defineBinding("line-cap", "line_cap", { group: "web-gfx", gfxApi: 'lineCap' });
    defineBinding("line-join", "line_join", { group: "web-gfx", gfxApi: 'lineJoin' });
    defineBinding("miter-limit", "miter_limit", { group: "web-gfx", gfxApi: 'miterLimit' });
    defineBinding("get-line-dash", "get_line_dash", { group: "web-gfx", gfxApi: 'getLineDash' });
    defineBinding("set-line-dash", "set_line_dash", { group: "web-gfx", gfxApi: 'setLineDash' });
    defineBinding("line-dash-offset", "line_dash_offset", { group: "web-gfx", gfxApi: 'lineDashOffser' });
    defineBinding("font", "font", { group: "web-gfx", gfxApi: 'font' });
    defineBinding("text-align", "text_align", { group: "web-gfx", gfxApi: 'textAlign' });
    defineBinding("text-baseline", "text_baseline", { group: "web-gfx", gfxApi: 'textBaseline' });
    defineBinding("direction", "direction", { group: "web-gfx", gfxApi: 'direction' })
    defineBinding("fill-style", "fill_style", { group: "web-gfx", gfxApi: 'fillStyle' });
    defineBinding("stroke-style", "stroke_style", { group: "web-gfx", gfxApi: 'strokeStyle' });
    defineBinding("create-conic-gradient", "create_conic_gradient", { group: "web-gfx", gfxApi: 'createConicGradient' });
    defineBinding("create-linear-gradient", "create_linear_gradient", { group: "web-gfx", gfxApi: 'createLinearGradient' });
    defineBinding("create-radial-gradient", "create_radial_gradient", { group: "web-gfx", gfxApi: 'createRadialGradient' });
    defineBinding("create-pattern", "create_pattern", { group: "web-gfx", gfxApi: 'createPattern' });
    defineBinding("shadow-color", "shadow_color", { group: "web-gfx", gfxApi: 'shadowColor' });
    defineBinding("shadow-offset-x", "shadow_offset_x", { group: "web-gfx", gfxApi: 'shadowOffsetX' });
    defineBinding("shadow-offset-y", "shadow_offset_y", { group: "web-gfx", gfxApi: 'shadowOffsetY' });
    defineBinding("begin-path", "begin_path", { group: "web-gfx", gfxApi: 'beginPath' });
    defineBinding("close-path", "close_path", { group: "web-gfx", gfxApi: 'closePath' });
    //defineBinding("move-to", "move_to", { group: "web-gfx", gfxApi: 'moveTo' });
    //defineBinding("line-to", "line_to", { group: "web-gfx", gfxApi: 'lineTo' });
    defineBinding("bezier-curve-to", "bezier_curve_to", { group: "web-gfx", gfxApi: 'bezierCurveTo' });
    defineBinding("quadratic-curve-to", "quadratic_curve_to", { group: "web-gfx", gfxApi: 'quadraticCurveTo' });
    defineBinding("arc", "arc", { group: "web-gfx", gfxApi: 'arc' });
    defineBinding("arc-to", "arc_to", { group: "web-gfx", gfxApi: 'arcTo' });
    defineBinding("ellipse", "ellipse", { group: "web-gfx", gfxApi: 'ellipse' });
    defineBinding("rect", "rect", { group: "web-gfx", gfxApi: 'rect' });
    defineBinding("fill", "fill", { group: "web-gfx", gfxApi: 'fill' });
    defineBinding("stroke", "stroke", { group: "web-gfx", gfxApi: 'stroke' });
    defineBinding("draw-focus-if-needed", "draw_focus_if_needed", { group: "web-gfx", gfxApi: 'drawFocusIfNeeded' });
    defineBinding("scroll-path-into-view", "scroll_path_into_view", { group: "web-gfx", gfxApi: 'scrollPathIntoView' });
    defineBinding("is-point-in-path", "is_point_in_path", { group: "web-gfx", gfxApi: 'isPointInPath' });
    defineBinding("is-point-in-stroke", "is_point_in_stroke", { group: "web-gfx", gfxApi: 'isPointInStroke' });
    defineBinding("clip", "clip", { group: "web-gfx", gfxApi: 'clip' });
    defineBinding("global-alpha", "global_alpha", { group: "web-gfx", gfxApi: 'globalAlpha' });
    defineBinding("global-composite-operation", "global_composite_operation", { group: "web-gfx", gfxApi: 'globalCompositeOperation' });
    defineBinding("draw-image", "draw_image", { group: "web-gfx", gfxApi: 'drawImage' });
    defineBinding("create-image-data", "create_image_data", { group: "web-gfx", gfxApi: 'createImageData' });
    defineBinding("get-image-data", "get_image_data", { group: "web-gfx", gfxApi: 'getImageData' });
    defineBinding("put-image-data", "put_image_data", { group: "web-gfx", gfxApi: 'putImageData' });
    defineBinding("image-smoothing-enabled", "image_smoothing_enabled", { group: "web-gfx", gfxApi: 'imageSmoothingEnabled' });
    defineBinding("image_smoothing_quality", "image_smoothing_quality", { group: "web-gfx", gfxApi: 'imageSmoothingQuality' });
  }

  return globalScope;
}