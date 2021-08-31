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
// To faclitate this convention, the defaults of all of the drawing primitives have been
// set so that, assuming the current point is (0,0) the resulting figure will be bounded
// by (0,0,1,1). This occasionally leads to strange defaults, for instance the defaults
// for line-to are (1,1).
//

import * as SchemeJS from './SchemeJS.mjs';

export const VERSION = SchemeJS.VERSION;

//
// Creates a SchemeJSWeb instance.
//
export function createInstance(schemeOpts = {}) {
  let globalScope = SchemeJS.createInstance(schemeOpts);
  const defineSchemeBindings = schemeOpts.defineSchemeBindings ?? true;
  const webApiMacros = schemeOpts.webApiMacros ?? true;
  if (!defineSchemeBindings)
    return globalScope;

  const string = globalScope.string ?? required();
  const exportAPI = globalScope.exportAPI ?? required();
  const augmentFunctionInfo = globalScope.augmentFunctionInfo ?? required();
  const Atom = globalScope.Atom ?? required();
  const isList = globalScope.isList ?? required();
  const FIRST = globalScope.FIRST ?? required();
  const parseSExpr = globalScope.parseSExpr ?? required();
  function required() { throw "required" }

  const eval_string = str => globalScope.eval_string(str);

  // Defines the context function and a macro to call it with the context
  function contextFunction(category, name, jsFunctionName, paramStr, opts = {}) {
    let context = `${category}-context`;
    let schemeGfxContextFnName = `${context}-${name}`;
    // Maka an arg string from the param str
    let parsedParams = parseSExpr(`[ ${paramStr} ]`), argStr = '';
    for (let param of parsedParams) {
      if (isList) argStr += ` ${string(param[FIRST])}`;
      else argStr += ` ${string(param)}`;
    }

    eval_string(`
        (compile [${schemeGfxContextFnName} ${context} ${paramStr}]
          (@! ${context} "${jsFunctionName}" ${argStr}) )`);

    if (webApiMacros) {
      // Define a macro that adds the graphics context and calls it
      eval_string(`
          (defmacro [${name} params] 
            (cons ${schemeGfxContextFnName} (cons '${context} params))) `);

      // Decorate it for the help system
      let nameAtom = Atom(name);
      augmentFunctionInfo(nameAtom,  { group: `web-${category}`, [`${context}Api`]: jsFunctionName,
        implStr: `(${schemeGfxContextFnName} ${context} ...params)`, ...opts });
    }
  }
  
  function contextProperty(category, schemePropName, jsPropName, opts = {}) {
    let context = `${category}-context`;
    eval_string(`
        (defmacro [${schemePropName} args]
          (? (> 0 (length args))
            (list '@ '${context} "${jsPropName}")
            (list 'prog1
              (list '@ '${context} "${jsPropName}")
              (list '@= '${context} "${jsPropName}" (car args))
            )
          )) `);
    // Decorate it for the help system
    let nameAtom = Atom(schemePropName);
    augmentFunctionInfo(nameAtom, { group: `web-${category}`, [`${context}Api`]: jsPropName, ...opts });
  }

  //
  // CanvasRenderingContext2D API
  //    https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D
  //

  function gfxFunction(name, jsFunctionName, paramStr, opts = {}) {
    return contextFunction("gfx", name, jsFunctionName, paramStr, opts);
  }

  function gfxProperty(schemePropName, jsPropName, opts = {}) {
    return contextProperty("gfx", schemePropName, jsPropName, opts);
  }

  eval_string(`
      (defmacro [gfx-save forms]
        (list
          'finally [ (list gfx-context-restore 'gfx-context) ]
            (list gfx-context-save 'gfx-context)
            ...forms)) `);

  gfxFunction("save", "save", '',
      { blurb: `Pushes the current context on a stack so that it can later be restored using ` +
                `gfx-context-restore. You should generally use gfx-save instead since it bundles ` +
                `gfx-context-save and gfx-context-restore in a structured way.`, omitGfxMacro: true });

  gfxFunction("restore", "restore", '',
      { blurb: `Pops the tha context from a stack which has previously been pushed by  ` +
              `gfx-context-save. You should generally use gfx-save instead since it bundles ` +
              `gfx-context-save and gfx-context-restore in a structured way.`, omitGfxMacro: true });      

  gfxFunction("translate", "translate", '[x 0] [y 0]',
     { blurb: `Translates the origin by "x" and "y".`});

  gfxFunction("scale", "scale", '[width 1] [height width]',
      { blurb: `Scales the coordinate system by "width" and "height".`});
 
  gfxFunction("rotate", "rotate", '[angle 0]',
     { blurb: `Rotates the coordinate system by "angle"`});

  gfxFunction("begin-path", "beginPath", '');
  gfxFunction("close-path", "closePath", '');
  gfxFunction("clip", "clip", '');
  gfxFunction("is-point-in-path", "isPointInPath", '...params');
  gfxFunction("is-point-in-stroke", "isPointInStroke", '...params');
  gfxFunction("move-to", "moveTo", '[x 0] [y 0]');
  gfxFunction("line-to", "lineTo", '[x 1] [y 1]');
  gfxFunction("bezier-curve-to", "bezierCurveTo", '[cpx1 1] [cpy1 0] [cpx2 0] [cpy2 1] [x 1] [y 1]');
  gfxFunction("quadratic-curve-to", "quadraticCurveTo", '[cpx 1] [cpy 0] [x 1] [y 1]');
  gfxFunction("arc", "arc", '[x .5] [y .5] [radius .5] [start-angle 0] [end-angle (* 2 *pi*)]');
  gfxFunction("arc-to", "arcTo", '[x1 1] [y1 0] [x2 1] [y2 1] [radius 1]');
  // defaults to a circle inscribing (0,0,1,1)
  gfxFunction("ellipse", "ellipse", '[x .5] [y .5] [radius-x .5] [radius-y .5] [start-angle 0] [end-angle (* 2 *pi*)] [counterclockwise false]');
  gfxFunction("rect", "rect", '[x 0] [y 0] [width 1] [height 1]');
  // MDN doesn't show it but the spec says it exists!
  gfxFunction("round-rect", "roundRect", '[x 0] [y 0] [width 1] [height 1] [radii .1]');
  gfxFunction("fill-rect", "fillRect", '[x 0] [y 0] [width 1] [height 1]');
  gfxFunction("clear-rect", "clearRect", '[x 0] [y 0] [width 1] [height 1]');
  gfxFunction("stroke-rect", "strokeRect", '[x 0] [y 0] [width 1] [height 1]');
  gfxFunction("fill-text", "fillText", '[text ""] [x 0] [y 0] [max-width]');
  gfxFunction("measure-text", "measureText", '[text]');
  gfxProperty("canvas-element", "canvas");
  gfxProperty("line-width", "lineWidth");
  gfxProperty("line-cap", "lineCcap");
  gfxProperty("line-join", "lineJoin");
  gfxProperty("miter-limit","miterLimit");
  gfxFunction("get-line-dash", "getLineDash", '');
  gfxFunction("set-line-dash", "setLineDash", '[value []]');
  gfxProperty("line-dash-offset", "lineDashOffset");
  gfxProperty("font", "font");
  gfxProperty("text-align", "textAlign");
  gfxProperty("text-baseline", "textBaseline");
  gfxProperty("direction", "direction");
  gfxProperty("fill-style", "fillStyle");
  gfxProperty("stroke-style", "strokeStyle");
  gfxFunction("create-conic-gradient", "createConicGradient", '[start-angle 0] [x 0] [y 1]');
  gfxFunction("create-linear-gradient", "createLinearGradient", '[x0 0] [y0 0] [x1 1] [y1 1]');
  gfxFunction("create-radial-gradient", "createRadialGradient", '[x0 0] [y0 0] [r0 1] [x1 1] [y1 1] [r1 0]');
  gfxFunction("create-pattern", "createPattern", '[image] [repetition "repeat"]');
  gfxProperty("shadow-color", "shadowColor");
  gfxProperty("shadow-offset-x", "shadowOffsetX");
  gfxProperty("shadow-offset-y", "shadowOffsetY");
  gfxFunction("fill", "fill", '');
  gfxFunction("stroke", "stroke", '');
  gfxFunction("draw-focus-if-needed", "drawFocusIfNeeded", '...params');
  gfxFunction("scroll-path-into-view", "scrollPathIntoView", '...params');
  gfxProperty("global-alpha", "globalAlpha");
  gfxProperty("global-composite-operation", "globalCompositeOperation");
  gfxFunction("draw-image", "drawImage", '...params');
  gfxFunction("create-image-data", "createImageData", '...params');
  gfxFunction("get-image-data", "getImageData", '[sx 0] [sy 0] [sw 1] [sh 1]');
  gfxProperty("image-smoothing-enabled", "imageSmoothingEnabled");
  gfxProperty("image-smoothing-quality", "imageSmoothingQuality");

  // canvas-width and canvas-height are actually properties of the context's canvas
  if (webApiMacros) {
    eval_string(`
        (defmacro [canvas-width args]
          (? (> 0 (length args))
            (list '@@ 'gfx-context "canvas" "width")
            (list 'prog1
              (list '@@ 'gfx-context "canvas" "width")
              (list '@@= 'gfx-context "canvas" "width" (car args))
            )
          )) `);
    augmentFunctionInfo(Atom("canvas-width"), { group: "web-gfx" });

    eval_string(`
        (defmacro [canvas-height args]
          (? (> 0 (length args))
            (list '@@ 'gfx-context "canvas" "height")
            (list 'prog1
              (list '@@ 'gfx-context "canvas" "height")
              (list '@@= 'gfx-context "canvas" "height" (car args))
            )
          )) `);
    augmentFunctionInfo(Atom("canvas-height"), { group: "web-gfx" });
  }

  //
  // HTML DOM API
  //

  // Document APIs   https://developer.mozilla.org/en-US/docs/Web/API/Document

  function htmlDocumentFunction(name, jsFunctionName, paramStr, opts = {}) {
    return contextFunction("document", name, jsFunctionName, paramStr, opts);
  }

  function htmlDocumentProperty(schemePropName, jsPropName, opts = {}) {
    return contextProperty("document", schemePropName, jsPropName, opts);
  }

  eval_string(`
      (defn [html-create-element document [tag "div"] ...children]
        (let [[element (@! document "createElement" tag)]]
          (for-of child children
            (cond
              [ (|| (instanceof child Node) (string? child))
                (@! element "append" child) ]
              [ (object? child)
                (for-in attr value
                  (@! element "setAttribute" (atom? attr (attr.description) attr) value)
                ) ]
              [ true (throw (new Error (+ "Bad element content: " child))) ]
            )
          )
          element
        )
      ) `);
    eval_string(`
        (defmacro [html-element args]
          (cons html-create-element (cons 'html-document args))
        )`);

  // Access from globalThis or it'll fail unit tests in Node.js
  globalScope[Atom('html-document')] = globalThis.document;
  globalScope[Atom('browser-window')] = globalThis.window;

  return globalScope;
}