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

  const string = globalScope.string ?? required();
  const exportAPI = globalScope.exportAPI ?? required();
  const Atom = globalScope.Atom ?? required();
  const BOTTOM = globalScope.BOTTOM; // Can't "require" it because "undefined" is indeed a bottom.
  function required() { throw "required" }

  const gfxContextAtom = Atom("gfx-context");
  const htmlDocumentAtom = Atom("html-document");

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

  function CanvasRenderingContextFunction(scope, fn) {
    let canvasRenderingContext = scope[gfxContextAtom];
    fn(canvasRenderingContext);
    return canvasRenderingContext;
  }

  function CanvasRenderingContextFunctionHook(fName, opts = {}) {
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

  exportAPI("translate", translate, { compileHook: CanvasRenderingContextFunctionHook('translate') });
  function translate(x, y) {
    let scope = this, canvasRenderingContext = scope[gfxContextAtom];
    canvasRenderingContext.translate(x, y);
    return canvasRenderingContext;
  }

  exportAPI("scale", scale, { compileHook: CanvasRenderingContextFunctionHook('scale', { dup1: true }) });
  function scale(width, height = width) {
    let scope = this, canvasRenderingContext = scope[gfxContextAtom];
    canvasRenderingContext.scale(width, height);
    return canvasRenderingContext;
  }

  exportAPI("rotate", rotate, { compileHook: CanvasRenderingContextFunctionHook('rotate') });
  function rotate(theta) {
    let scope = this, canvasRenderingContext = scope[gfxContextAtom];
    canvasRenderingContext.rotate(theta);
    return canvasRenderingContext;
  }

  // You probably shouldn't be using these (use gfx-save instead!),
  // but for completeness...
  exportAPI("save_context", save_context, { compileHook: CanvasRenderingContextFunctionHook('save') });
  function save_context() { return CanvasRenderingContextFunction(this, ctx => ctx.save()) }

  exportAPI("restore_context", restore_context, { compileHook: CanvasRenderingContextFunctionHook('restore') });
  function restore_context() { return CanvasRenderingContextFunction(this, ctx => ctx.restore()) }

  exportAPI("canvas_width", canvas_width, { compileHook: CanvasRenderingContextPropertyHook('canvaswidth') });
  function canvas_width() {
    let ctx = this[gfxContextAtom];
    return ctx.canvas.width;
  };

  exportAPI("canvas_height", canvas_height, { compileHook: CanvasRenderingContextPropertyHook('canvas.height') });
  function canvas_height() {
    let ctx = this[gfxContextAtom];
    return ctx.canvas.height;
  };

  exportAPI("fill_rect", fill_rect, { compileHook: CanvasRenderingContextFunctionHook('fillRect') });
  function fill_rect(x = 0, y = 0, width = 1, height = 1) { return CanvasRenderingContextFunction(this, ctx => ctx.fillRect(x, y, width, height)) }
  
  exportAPI("clear_rect", clear_rect, { compileHook: CanvasRenderingContextFunctionHook('clearRect') });
  function clear_rect(x = 0, y = 0, width = 1, height = 1) { return CanvasRenderingContextFunction(this, ctx => ctx.clearRect(x, y, width, height)) }
  
  exportAPI("stroke_rect", stroke_rect, { compileHook: CanvasRenderingContextFunctionHook('strokeRect') });
  function stroke_rect(x = 0, y = 0, width = 1, height = 1) {
    return CanvasRenderingContextFunction(this, ctx => ctx.strokeRect(x, y, width, height))
  }

  exportAPI("fill_text", fill_text, { compileHook: CanvasRenderingContextFunctionHook('fillText') });
  function fill_text(text, x = 0, y = 0 , maxWidth = optional) {
    return CanvasRenderingContextFunction(this, ctx => ctx.fillText(text, x, y , maxWidth))
  }

  exportAPI("measure_text", measure_text, { compileHook: CanvasRenderingContextFunctionHook('measureText') });
  function measure_text(text) { return CanvasRenderingContextFunction(this, ctx => ctx.measureText(text)) }

  const line_width = CanvasRenderingContextProperty("lineWidth");
  exportAPI("line_width", line_width, { compileHook: CanvasRenderingContextPropertyHook('lineWidth') });

  const line_cap = CanvasRenderingContextProperty("lineCap");
  exportAPI("line_cap", line_cap, { compileHook: CanvasRenderingContextPropertyHook('lineCap') });

  const line_join = CanvasRenderingContextProperty("lineJoin");
  exportAPI("line_join", line_join, { compileHook: CanvasRenderingContextPropertyHook('lineJoin') });

  const miter_limit = CanvasRenderingContextProperty("miterLimit");
  exportAPI("miter_limit", miter_limit, { compileHook: CanvasRenderingContextPropertyHook('miterLimit') });

  exportAPI("get_line_dash", get_line_dash, { compileHook: CanvasRenderingContextFunctionHook('getLineDash') });
  function get_line_dash() { return CanvasRenderingContextFunction(this, ctx => ctx.getLineDash) }

  exportAPI("set_line_dash", set_line_dash, { compileHook: CanvasRenderingContextFunctionHook('setLineDash') });
  function set_line_dash(segments) { CanvasRenderingContextFunction(this, ctx => ctx.setLineDash(segments)) }

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

  exportAPI("create_conic_gradient", create_conic_gradient, { compileHook: CanvasRenderingContextFunctionHook('createConicGradient') });
  function create_conic_gradient(startAngle = 0, x = 0, y = 0) {
    return CanvasRenderingContextFunction(this, ctx => ctx.createConicGradient(startAngle, x, y))
  }

  exportAPI("create_linear_gradient", create_linear_gradient, { compileHook: CanvasRenderingContextFunctionHook('createLinearGradient') });
  function create_linear_gradient(x0 = 0, y0 = 0, x1 = 1, y1 = 1) {
    return CanvasRenderingContextFunction(this, ctx => ctx.createLinearGradient(x0, y0, x1, y1))
  }

  exportAPI("create_radial_gradient", create_radial_gradient, { compileHook: CanvasRenderingContextFunctionHook('createRadialGradient') });
  function create_radial_gradient(x0 = 0, y0 = 0, r0 = 1, x1 = 0, y1 = 0, r1 = 0) {
    return CanvasRenderingContextFunction(this, ctx => ctx.createRadialGradient(x0, y0, r0, x1, y1, r1))
  }

  exportAPI("create_pattern", create_pattern, { compileHook: CanvasRenderingContextFunctionHook('createPattern') });
  function create_pattern(image, repetition = "repeat") { CanvasRenderingContextFunction(this, ctx => ctx.createPattern(image, repetition)) }

  const shadow_color = CanvasRenderingContextProperty("shadowColor");
  exportAPI("shadow_color", shadow_color, { compileHook: CanvasRenderingContextPropertyHook('shadowColor') });

  const shadow_offset_x = CanvasRenderingContextProperty("shadowOffsetX");
  exportAPI("shadow_offset_x", shadow_offset_x, { compileHook: CanvasRenderingContextPropertyHook('shadowOffsetX') });

  const shadow_offset_y = CanvasRenderingContextProperty("shadowOffsetY");
  exportAPI("shadow_offset_y", shadow_offset_y, { compileHook: CanvasRenderingContextPropertyHook('shadowOffsetY') });

  exportAPI("begin_path", begin_path, { compileHook: CanvasRenderingContextFunctionHook('beginPath') });
  function begin_path() { return CanvasRenderingContextFunction(this, ctx => ctx.beginPath()) }

  exportAPI("close_path", close_path, { compileHook: CanvasRenderingContextFunctionHook('closePath') });
  function close_path() { return CanvasRenderingContextFunction(this, ctx => ctx.closePath()) }

  exportAPI("move_to", move_to, { compileHook: CanvasRenderingContextFunctionHook('moveTo') });
  function move_to(x = 0, y = 0) { return CanvasRenderingContextFunction(this, ctx => ctx.moveTo(x, y)) }

  // consistency demands defaults of 1,1, but usability suggests 0, 0
  exportAPI("line_to", line_to, { compileHook: CanvasRenderingContextFunctionHook('lineTo') });
  function line_to(x = 0, y = 0) { return CanvasRenderingContextFunction(this, ctx => ctx.lineTo(x, y)) }

  exportAPI("bezier_curve_to", bezier_curve_to, { compileHook: CanvasRenderingContextFunctionHook('bezierCurveTo') });
  function bezier_curve_to(cp1x = 1, cp1y = 0, cp2x = 0 , cp2y = 1, x = 1, y = 1) {
    return CanvasRenderingContextFunction(this, ctx => ctx.bezierCurveTo(cp1x, cp1y, cp2x, cp2y, x, y))
  }

  exportAPI("quadratic_curve_to", quadratic_curve_to, { compileHook: CanvasRenderingContextFunctionHook('quadraticCurveTo') });
  function quadratic_curve_to(cpx = 1, cpy = 0, x = 1, y = 1) {
    return CanvasRenderingContextFunction(this, ctx => ctx.quadraticCurveTo(cpx, cpy, x, y))
  }

  exportAPI("arc", arc, { compileHook: CanvasRenderingContextFunctionHook('arc') });
  function arc(x = .5, y = .5, radius = .5, startAngle = 0, endAngle = 2*Math.pi, counterclockwise = false) {
    return CanvasRenderingContextFunction(this, ctx => ctx.arc(x, y, radius, startAngle, endAngle, counterclockwise))
  }

  exportAPI("arc_to", arc_to, { compileHook: CanvasRenderingContextFunctionHook('arcTo') });
  function arc_to(x1 = 1, y1 = 0, x2 = 1, y2 = 1, radius = 1) { return CanvasRenderingContextFunction(this, ctx => ctx.arcTo(x1, y1, x2, y2, radius)) }

  // defaults to a circle inscribing (0,0,1,1)
  exportAPI("ellipse", ellipse, { compileHook: CanvasRenderingContextFunctionHook('ellipse') });
  function ellipse(x = .5, y = .5, radiusX = .5, radiusY = .5, rotation = 0, startAngle = 0, endAngle = 2*pi, counterclockwise = false) {
    return CanvasRenderingContextFunction(this, ctx => ctx.ellipse(x, y, radiusX, radiusY, rotation, startAngle, endAngle, counterclockwise))
  }

  exportAPI("rect", rect, { compileHook: CanvasRenderingContextFunctionHook('rect') });
  function rect(x = 0, y, width, height) { return CanvasRenderingContextFunction(this, ctx => ctx.rect(x, y, width, height)) }

  exportAPI("fill", fill, { compileHook: CanvasRenderingContextFunctionHook('fill') });
  function fill(...params) { return CanvasRenderingContextFunction(this, ctx => ctx.fill(...params)) }

  exportAPI("stroke", stroke, { compileHook: CanvasRenderingContextFunctionHook('stroke') });
  function stroke(...params) { return CanvasRenderingContextFunction(this, ctx => ctx.stroke(...params)) }

  exportAPI("draw_focus_if_needed", draw_focus_if_needed, { compileHook: CanvasRenderingContextFunctionHook('drawFocusIfNeeded') });
  function draw_focus_if_needed(...params) { return CanvasRenderingContextFunction(this, ctx => ctx.drawFocusIfNeeded(...params)) }

  exportAPI("scroll_path_into_view", scroll_path_into_view, { compileHook: CanvasRenderingContextFunctionHook('scrollPathIntoView') });
  function scroll_path_into_view(...params) { return CanvasRenderingContextFunction(this, ctx => ctx.scrollPathIntoView(...params)) }

  exportAPI("is_point_in_path", is_point_in_path, { compileHook: CanvasRenderingContextFunctionHook('isPointInPath') });
  function is_point_in_path(...params) { return CanvasRenderingContextFunction(this, ctx => ctx.isPointInPath(...params)) }

  exportAPI("is_point_in_stroke", is_point_in_stroke, { compileHook: CanvasRenderingContextFunctionHook('isPointInStroke') });
  function is_point_in_stroke(...params) { return CanvasRenderingContextFunction(this, ctx => ctx.isPointInStroke(...params)) }

  exportAPI("clip", clip, { compileHook: CanvasRenderingContextFunctionHook('clip') });
  function clip(...params) { return CanvasRenderingContextFunction(this, ctx => ctx.clip(...params)) }

  const global_alpha = CanvasRenderingContextProperty("globalAlpha");
  exportAPI("global_alpha", global_alpha, { compileHook: CanvasRenderingContextPropertyHook('globalAlpha') });

  const global_composite_operation = CanvasRenderingContextProperty("globalCompositeOperation");
  exportAPI("global_composite_operation", global_composite_operation, { compileHook: CanvasRenderingContextPropertyHook('globalCompositeOperation') });

  exportAPI("draw_image", draw_image, { compileHook: CanvasRenderingContextFunctionHook('drawImage') });
  function draw_image(...params) { return CanvasRenderingContextFunction(this, ctx => ctx.drawImage(...params)) }

  exportAPI("create_image_data", createImageData, { compileHook: CanvasRenderingContextFunctionHook('createImageData') });
  function createImageData(...params) { return CanvasRenderingContextFunction(this, ctx => ctx.createImageData(...params)) }

  exportAPI("get_image_data", get_image_data, { compileHook: CanvasRenderingContextFunctionHook('getImageData') });
  function get_image_data(sx, sy, sw, sh) { return CanvasRenderingContextFunction(this, ctx => ctx.getImageData(sx, sy, sw, sh)) }

  exportAPI("put_image_data", put_image_data, { compileHook: CanvasRenderingContextFunctionHook('putImageData') });
  function put_image_data(...params) { return CanvasRenderingContextFunction(this, ctx => ctx.putImageData(...params)) }

  const image_smoothing_enabled = CanvasRenderingContextProperty("imageSmoothingEnabled");
  exportAPI("image_smoothing_enabled", image_smoothing_enabled, { compileHook: CanvasRenderingContextPropertyHook('imageSmoothingEnabled') });

  const image_smoothing_quality = CanvasRenderingContextProperty("imageSmoothingQuality");
  exportAPI("image_smoothing_quality", image_smoothing_quality, { compileHook: CanvasRenderingContextPropertyHook('imageSmoothingQuality') });

  //
  // Bindings!
  //

  if (defineSchemeBindings) {
    const defineBinding = globalScope.defineBinding ?? required();

    defineBinding("VERSION", "VERSION", {
      group: "main", sample: `VERSION`,
      blurb: `The SchemeJSWeb version`
    });

    // You probably shouldn't be using these (use gfx-save instead!),
    // but for completeness...
    defineBinding("save-context", "save_context", { group: "web-gfx" });
    defineBinding("restore-context", "restore_context", { group: "web-gfx" });

    // Complete Canvas 2D drawing APIs:
    defineBinding("gfx-save", "gfx_save", { group: "web-gfx" });
    defineBinding("translate", "translate", { group: "web-gfx" })
    defineBinding("scale", "scale", { group: "web-gfx" });
    defineBinding("rotate", "rotate", { group: "web-gfx" });
    defineBinding("canvas-width", "canvas_width", { group: "web-gfx" });
    defineBinding("canvas-height", "canvas_height", { group: "web-gfx" });
    defineBinding("fill-rect", "fill_rect", { group: "web-gfx" });
    defineBinding("clear-rect", "clear_rect", { group: "web-gfx" });
    defineBinding("stroke-rect", "stroke_rect", { group: "web-gfx" });
    defineBinding("fill-text", "fill_text", { group: "web-gfx" });
    defineBinding("measure-text", "measure_text", { group: "web-gfx" });
    defineBinding("line-width", "line_width", { group: "web-gfx" });
    defineBinding("line-cap", "line_cap", { group: "web-gfx" });
    defineBinding("line-cap", "line_cap", { group: "web-gfx" });
    defineBinding("line-join", "line_join", { group: "web-gfx" });
    defineBinding("miter-limit", "miter_limit", { group: "web-gfx" });
    defineBinding("get-line-dash", "get_line_dash", { group: "web-gfx" });
    defineBinding("set-line-dash", "set_line_dash", { group: "web-gfx" });
    defineBinding("line-dash-offset", "line_dash_offset", { group: "web-gfx" });
    defineBinding("font", "font", { group: "web-gfx" });
    defineBinding("text-align", "text_align", { group: "web-gfx" });
    defineBinding("text-baseline", "text_baseline", { group: "web-gfx" });
    defineBinding("direction", "direction", { group: "web-gfx" })
    defineBinding("fill-style", "fill_style", { group: "web-gfx" });
    defineBinding("stroke-style", "stroke_style", { group: "web-gfx" });
    defineBinding("create-conic-gradient", "create_conic_gradient", { group: "web-gfx" });
    defineBinding("create-linear-gradient", "create_linear_gradient", { group: "web-gfx" });
    defineBinding("create-radial-gradient", "create_radial_gradient", { group: "web-gfx" });
    defineBinding("create-pattern", "create_pattern", { group: "web-gfx" });
    defineBinding("shadow-color", "shadow_color", { group: "web-gfx" });
    defineBinding("shadow-offset-x", "shadow_offset_x", { group: "web-gfx" });
    defineBinding("shadow-offset-y", "shadow_offset_y", { group: "web-gfx" });
    defineBinding("begin-path", "begin_path", { group: "web-gfx" });
    defineBinding("close-path", "close_path", { group: "web-gfx" });
    defineBinding("move-to", "move_to", { group: "web-gfx" });
    defineBinding("line-to", "line_to", { group: "web-gfx" });
    defineBinding("bezier-curve-to", "bezier_curve_to", { group: "web-gfx" });
    defineBinding("quadratic-curve-to", "quadratic_curve_to", { group: "web-gfx" });
    defineBinding("arc", "arc", { group: "web-gfx" });
    defineBinding("arc-to", "arc_to", { group: "web-gfx" });
    defineBinding("ellipse", "ellipse", { group: "web-gfx" });
    defineBinding("rect", "rect", { group: "web-gfx" });
    defineBinding("fill", "fill", { group: "web-gfx" });
    defineBinding("stroke", "stroke", { group: "web-gfx" });
    defineBinding("draw-focus-if-needed", "draw_focus_if_needed", { group: "web-gfx" });
    defineBinding("scroll-path-into-view", "scroll_path_into_view", { group: "web-gfx" });
    defineBinding("scroll-path-into-view", "scroll_path_into_view", { group: "web-gfx" });
    defineBinding("is-point-in-path", "is_point_in_path", { group: "web-gfx" });
    defineBinding("is-point-in-stroke", "is_point_in_stroke", { group: "web-gfx" });
    defineBinding("clip", "clip", { group: "web-gfx" });
    defineBinding("global-alpha", "global_alpha", { group: "web-gfx" });
    defineBinding("global-composite-operation", "global_composite_operation", { group: "web-gfx" });
    defineBinding("draw-image", "draw_image", { group: "web-gfx" });
    defineBinding("create-image-data", "create_image_data", { group: "web-gfx" });
    defineBinding("get-image-data", "get_image_data", { group: "web-gfx" });
    defineBinding("put-image-data", "put_image_data", { group: "web-gfx" });
    defineBinding("image-smoothing-enabled", "image_smoothing_enabled", { group: "web-gfx" });
    defineBinding("image_smoothing_quality", "image_smoothing_quality", { group: "web-gfx" });
  }

  return globalScope;
}