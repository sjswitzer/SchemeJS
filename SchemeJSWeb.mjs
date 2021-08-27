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
        if (typeof paramDefault === 'symbol') paramDefault = paramDefault.description;
        defaultStr = ` = ${paramDefault}`;
      }
      paramStr += `${sep}${paramName}${defaultStr}`;
      argStr += `${sep}${paramName}`;
      sep = ', ';
    }
    defineBinding(boundName, name, { group: "web-gfx", gfxApi: functionName,
      implStr: `(${[paramStr]}) => gfx_context.${functionName}(${argStr})` });
  }
  
  /*
  exportAPI("gfx_opt", gfx_opt, { tag: MACRO_TAG });
  function gfx_opt(params) {
    let param = params[0], dflt = params[1];
    let paramAtom = Atom(param);
    return list( ((paramVal, dflt) => paramVal !== undefined ? paramVal : dflt), paramAtom, dflt );
  }
  const opt = (...params) => globalScope.gfx_opt(params);
  */
  function opt(param, dflt) {
    let paramAtom = Atom(param);
    return list( ((paramVal, dflt) => paramVal !== undefined ? paramVal : dflt), paramAtom, dflt );
  }

  gfxFunction("move-to", "move_to", "moveTo", [ opt("x", 0), opt("y", 0)]);
  gfxFunction("line-to", "line_to", "lineTo", [ opt("x", 0), opt("y", 1)]);

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

  exportAPI("fill_rect", fill_rect, { compileHook: CtxFnHookHook('fillRect') });
  function fill_rect(x = 0, y = 0, width = 1, height = 1) { return this[gfxContextAtom].fillRect(x, y, width, height) }
  
  exportAPI("clear_rect", clear_rect, { compileHook: CtxFnHookHook('clearRect') });
  function clear_rect(x = 0, y = 0, width = 1, height = 1) { return this[gfxContextAtom].clearRect(x, y, width, height) }
  
  exportAPI("stroke_rect", stroke_rect, { compileHook: CtxFnHookHook('strokeRect') });
  function stroke_rect(x = 0, y = 0, width = 1, height = 1) { return this[gfxContextAtom].strokeRect(x, y, width, height) }

  exportAPI("fill_text", fill_text, { compileHook: CtxFnHookHook('fillText') });
  function fill_text(text, x = 0, y = 0 , maxWidth = optional) { return this[gfxContextAtom].fillText(text, x, y , maxWidth) }

  exportAPI("measure_text", measure_text, { compileHook: CtxFnHookHook('measureText') });
  function measure_text(text) { return this[gfxContextAtom].measureText(text) }

  const line_width = CanvasRenderingContextProperty("lineWidth");
  exportAPI("line_width", line_width, { compileHook: CanvasRenderingContextPropertyHook('lineWidth') });

  const line_cap = CanvasRenderingContextProperty("lineCap");
  exportAPI("line_cap", line_cap, { compileHook: CanvasRenderingContextPropertyHook('lineCap') });

  const line_join = CanvasRenderingContextProperty("lineJoin");
  exportAPI("line_join", line_join, { compileHook: CanvasRenderingContextPropertyHook('lineJoin') });

  const miter_limit = CanvasRenderingContextProperty("miterLimit");
  exportAPI("miter_limit", miter_limit, { compileHook: CanvasRenderingContextPropertyHook('miterLimit') });

  exportAPI("get_line_dash", get_line_dash, { compileHook: CtxFnHookHook('getLineDash') });
  function get_line_dash() { return this[gfxContextAtom].getLineDash() }

  exportAPI("set_line_dash", set_line_dash, { compileHook: CtxFnHookHook('setLineDash') });
  function set_line_dash(segments) { this[gfxContextAtom].setLineDash(segments) }

  const line_dash_offset = CanvasRenderingContextProperty("lineDashOffset");
  exportAPI("line_dash_offset", line_dash_offset, { compileHook: CanvasRenderingContextPropertyHook('lineDashOffset') });
  
  const font = CanvasRenderingContextProperty("font");
  exportAPI("font", font, { compileHook: CanvasRenderingContextPropertyHook('font') });

  const text_align = CanvasRenderingContextProperty("textAlign");
  exportAPI("text_align", text_align, { compileHook: CanvasRenderingContextPropertyHook('textAlign') });

  const text_baseline = CanvasRenderingContextProperty("textBaseline");
  exportAPI("text_baseline", text_baseline, { compileHook: CanvasRenderingContextPropertyHook('textBaseline') });

  const direction = CanvasRenderingContextProperty("direction");
  exportAPI("direction", direction, { compileHook: CanvasRenderingContextPropertyHook('direction') });

  const fill_style = CanvasRenderingContextProperty("fillStyle");
  exportAPI("fill_style", fill_style, { compileHook: CanvasRenderingContextPropertyHook('fillStyle') });

  const stroke_style = CanvasRenderingContextProperty("strokeStyle");
  exportAPI("stroke_style", stroke_style, { compileHook: CanvasRenderingContextPropertyHook('strokeStyle') });

  exportAPI("create_conic_gradient", create_conic_gradient, { compileHook: CtxFnHookHook('createConicGradient') });
  function create_conic_gradient(startAngle = 0, x = 0, y = 0) { return this[gfxContextAtom].createConicGradient(startAngle, x, y) }

  exportAPI("create_linear_gradient", create_linear_gradient, { compileHook: CtxFnHookHook('createLinearGradient') });
  function create_linear_gradient(x0 = 0, y0 = 0, x1 = 1, y1 = 1) { return this[gfxContextAtom].createLinearGradient(x0, y0, x1, y1) }

  exportAPI("create_radial_gradient", create_radial_gradient, { compileHook: CtxFnHookHook('createRadialGradient') });
  function create_radial_gradient(x0 = 0, y0 = 0, r0 = 1, x1 = 0, y1 = 0, r1 = 0) { return this[gfxContextAtom].createRadialGradient(x0, y0, r0, x1, y1, r1) }

  exportAPI("create_pattern", create_pattern, { compileHook: CtxFnHookHook('createPattern') });
  function create_pattern(image, repetition = "repeat") { this[gfxContextAtom].createPattern(image, repetition) }

  const shadow_color = CanvasRenderingContextProperty("shadowColor");
  exportAPI("shadow_color", shadow_color, { compileHook: CanvasRenderingContextPropertyHook('shadowColor') });

  const shadow_offset_x = CanvasRenderingContextProperty("shadowOffsetX");
  exportAPI("shadow_offset_x", shadow_offset_x, { compileHook: CanvasRenderingContextPropertyHook('shadowOffsetX') });

  const shadow_offset_y = CanvasRenderingContextProperty("shadowOffsetY");
  exportAPI("shadow_offset_y", shadow_offset_y, { compileHook: CanvasRenderingContextPropertyHook('shadowOffsetY') });

  exportAPI("begin_path", begin_path, { compileHook: CtxFnHookHook('beginPath') });
  function begin_path() { return this[gfxContextAtom].beginPath() }

  exportAPI("close_path", close_path, { compileHook: CtxFnHookHook('closePath') });
  function close_path() { this[gfxContextAtom].closePath() }

  exportAPI("move_to", move_to, { compileHook: CtxFnHookHook('moveTo') });
  function move_to(x = 0, y = 0) { return this[gfxContextAtom].moveTo(x, y) }

  // consistency demands defaults of 1,1, but usability suggests 0, 0
  exportAPI("line_to", line_to, { compileHook: CtxFnHookHook('lineTo') });
  function line_to(x = 0, y = 0) { return this[gfxContextAtom].lineTo(x, y) }

  exportAPI("bezier_curve_to", bezier_curve_to, { compileHook: CtxFnHookHook('bezierCurveTo') });
  function bezier_curve_to(cp1x = 1, cp1y = 0, cp2x = 0 , cp2y = 1, x = 1, y = 1) { return this[gfxContextAtom].bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y) }

  exportAPI("quadratic_curve_to", quadratic_curve_to, { compileHook: CtxFnHookHook('quadraticCurveTo') });
  function quadratic_curve_to(cpx = 1, cpy = 0, x = 1, y = 1) { return this[gfxContextAtom].quadraticCurveTo(cpx, cpy, x, y) }

  exportAPI("arc", arc, { compileHook: CtxFnHookHook('arc') });
  function arc(x = .5, y = .5, radius = .5, startAngle = 0, endAngle = 2*Math.pi, counterclockwise = false) { return this[gfxContextAtom].arc(x, y, radius, startAngle, endAngle, counterclockwise) }

  exportAPI("arc_to", arc_to, { compileHook: CtxFnHookHook('arcTo') });
  function arc_to(x1 = 1, y1 = 0, x2 = 1, y2 = 1, radius = 1) { return this[gfxContextAtom].arcTo(x1, y1, x2, y2, radius) }

  // defaults to a circle inscribing (0,0,1,1)
  exportAPI("ellipse", ellipse, { compileHook: CtxFnHookHook('ellipse') });
  function ellipse(x = .5, y = .5, radiusX = .5, radiusY = .5, rotation = 0, startAngle = 0, endAngle = 2*pi, counterclockwise = false) { return this[gfxContextAtom].ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle, counterclockwise) }

  exportAPI("rect", rect, { compileHook: CtxFnHookHook('rect') });
  function rect(x = 0, y, width, height) { return this[gfxContextAtom].rect(x, y, width, height) }

  exportAPI("fill", fill, { compileHook: CtxFnHookHook('fill') });
  function fill(...params) { return this[gfxContextAtom].fill(...params) }

  exportAPI("stroke", stroke, { compileHook: CtxFnHookHook('stroke') });
  function stroke(...params) { return this[gfxContextAtom].stroke(...params) }

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

  const global_alpha = CanvasRenderingContextProperty("globalAlpha");
  exportAPI("global_alpha", global_alpha, { compileHook: CanvasRenderingContextPropertyHook('globalAlpha') });

  const global_composite_operation = CanvasRenderingContextProperty("globalCompositeOperation");
  exportAPI("global_composite_operation", global_composite_operation, { compileHook: CanvasRenderingContextPropertyHook('globalCompositeOperation') });

  exportAPI("draw_image", draw_image, { compileHook: CtxFnHookHook('drawImage') });
  function draw_image(...params) { return this[gfxContextAtom].drawImage(...params) }

  exportAPI("create_image_data", createImageData, { compileHook: CtxFnHookHook('createImageData') });
  function createImageData(...params) { return this[gfxContextAtom].createImageData(...params) }

  exportAPI("get_image_data", get_image_data, { compileHook: CtxFnHookHook('getImageData') });
  function get_image_data(sx, sy, sw, sh) { return this[gfxContextAtom].getImageData(sx, sy, sw, sh) }

  exportAPI("put_image_data", put_image_data, { compileHook: CtxFnHookHook('putImageData') });
  function put_image_data(...params) { return this[gfxContextAtom].putImageData(...params) }

  const image_smoothing_enabled = CanvasRenderingContextProperty("imageSmoothingEnabled");
  exportAPI("image_smoothing_enabled", image_smoothing_enabled, { compileHook: CanvasRenderingContextPropertyHook('imageSmoothingEnabled') });

  const image_smoothing_quality = CanvasRenderingContextProperty("imageSmoothingQuality");
  exportAPI("image_smoothing_quality", image_smoothing_quality, { compileHook: CanvasRenderingContextPropertyHook('imageSmoothingQuality') });

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