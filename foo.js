function foo() {
"use strict";
// params: (bound, resolveUnbound, invokeUnbound)
let scope = this;
let string = bound["string"];
let NIL = bound["NIL"];
let schemeTrue = bound["schemeTrue"];
let isList = bound["isList"];
let Pair = bound["Pair"];
let Atom = bound["Atom"];
let newScope = bound["newScope"];
let FIRST = bound["FIRST"];
let REST = bound["REST"];
let LIST = bound["LIST"];
let ITERATE_AS_LIST = bound["ITERATE_AS_LIST"];
let MORELIST = bound["MORELIST"];
let COMPILED = bound["COMPILED"];
let PARAMETER_DESCRIPTOR = bound["PARAMETER_DESCRIPTOR"];
let COMPILE_INFO = bound["COMPILE_INFO"];
let CLOSURE_ATOM = bound["CLOSURE_ATOM"];
let _gfx$dash_context_atom = bound["_gfx$dash_context_atom"];
let _o$dash_scope$dash_canvas_atom = bound["_o$dash_scope$dash_canvas_atom"];
let _time$dash_domain$dash_data_atom = bound["_time$dash_domain$dash_data_atom"];
let _frequency$dash_domain$dash_data_atom = bound["_frequency$dash_domain$dash_data_atom"];
let _for$dash_in_atom = bound["_for$dash_in_atom"];
let for-in_args = bound["for-in_args"];
let _for$dash_in_args = bound["_for$dash_in_args"];
let closureForm = bound["closureForm"];
let _COMPILE_INFO = bound["_COMPILE_INFO"];
function _o$dash_scope(_gfx$dash_context) { // COMPILED o-scope, req: 1, eval: *
  // (λ (gfx-context) . [(let ((time-domain-data (@ o-scope-canvas (quote time-domain-data))) (
  // frequency-domain-data (@ o-scope-canvas (quote frequency-domain-data)))) (gfx-save (scale (/ (
  // canvas-width) (+ 1 (@ frequency-domain-data "length"))) (/ (canvas-height) 256)) (fill-style "#a22") 
  // (for-in i value frequency-domain-data (fill-rect i (- 256 value) 2 value))) (gfx-save (scale (/ (
  // canvas-width) (+ 1 (@ time-domain-data "length"))) (/ (canvas-height) 256)) (stroke-style "#8f8") (
  // begin-path) (for-in i value time-domain-data (line-to i value)) (stroke)))])
  let _letrec0 = NIL; { // letrec
    let _time$dash_domain$dash_data = NIL;
    let __result; {
      let a = resolveUnbound(_o$dash_scope$dash_canvas_atom);
      let b = _time$dash_domain$dash_data_atom;
      __result = (a[b]);
    }
    _time$dash_domain$dash_data = __result;
    let _frequency$dash_domain$dash_data = NIL;
    let __result2; {
      let a = resolveUnbound(_o$dash_scope$dash_canvas_atom);
      let b = _frequency$dash_domain$dash_data_atom;
      __result2 = (a[b]);
    }
    _frequency$dash_domain$dash_data = __result2;
    _gfx$dash_context.save();
    try {
      let _canvas_widget = contextToSchemeWidget(_gfx$dash_context);
      let _width_value = _canvas_widget.width;
      let __result5; {
        let a = _frequency$dash_domain$dash_data;
        let b = "length";
        __result5 = (a[b]);
      }
      let _add6 = (1 + __result5)
      let _div7 = (_width_value / _add6)
      let _canvas_widget9 = contextToSchemeWidget(_gfx$dash_context);
      let _height_value = _canvas_widget9.height;
      let _div10 = (_height_value / 256)
      let _dupFirst = _div10;
      if (_dupFirst === undefined) _dupFirst = _div7
      _gfx$dash_context.scale(_div7, _dupFirst);
      let _fillStyle_value = _gfx$dash_context.fillStyle;
      if ("#a22" !== undefined)
        _gfx$dash_context.fillStyle = "#a22";
      let _for$dash_in_result = invokeUnbound(resolveUnbound(_for$dash_in_atom), for-in_args);
    } finally { _gfx$dash_context.restore() }
    _letrec0 = _gfx$dash_context;
    _gfx$dash_context.save();
    try {
      let _canvas_widget16 = contextToSchemeWidget(_gfx$dash_context);
      let _width_value17 = _canvas_widget16.width;
      let __result20; {
        let a = _time$dash_domain$dash_data;
        let b = "length";
        __result20 = (a[b]);
      }
      let _add21 = (1 + __result20)
      let _div22 = (_width_value17 / _add21)
      let _canvas_widget25 = contextToSchemeWidget(_gfx$dash_context);
      let _height_value26 = _canvas_widget25.height;
      let _div27 = (_height_value26 / 256)
      let _dupFirst28 = _div27;
      if (_dupFirst28 === undefined) _dupFirst28 = _div22
      _gfx$dash_context.scale(_div22, _dupFirst28);
      let _strokeStyle_value = _gfx$dash_context.strokeStyle;
      if ("#8f8" !== undefined)
        _gfx$dash_context.strokeStyle = "#8f8";
      _gfx$dash_context.beginPath();
      let _for$dash_in_result30 = invokeUnbound(resolveUnbound(_for$dash_in_atom), _for$dash_in_args);
      _gfx$dash_context.stroke();
    } finally { _gfx$dash_context.restore() }
    _letrec0 = _gfx$dash_context;
  }
  return _letrec0;
}
// evalCount: MAX_INTEGER, requiredCount: 1
_o$dash_scope[PARAMETER_DESCRIPTOR] = -1048320;
_o$dash_scope[COMPILE_INFO] = _COMPILE_INFO;
// (%%closure "PATCH" (gfx-context) . [(let ((time-domain-data (@ o-scope-canvas (quote time-domain-data))) 
// (frequency-domain-data (@ o-scope-canvas (quote frequency-domain-data)))) (gfx-save (scale (/ (
// canvas-width) (+ 1 (@ frequency-domain-data "length"))) (/ (canvas-height) 256)) (fill-style "#a22") 
// (for-in i value frequency-domain-data (fill-rect i (- 256 value) 2 value))) (gfx-save (scale (/ (
// canvas-width) (+ 1 (@ time-domain-data "length"))) (/ (canvas-height) 256)) (stroke-style "#8f8") (
// begin-path) (for-in i value time-domain-data (line-to i value)) (stroke)))])
_o$dash_scope[FIRST] = closureForm[FIRST];
_o$dash_scope[REST] = new Pair(scope, closureForm[REST][REST]);
_o$dash_scope[COMPILED] = "o-scope"
_o$dash_scope[LIST] = _o$dash_scope[ITERATE_AS_LIST] = _o$dash_scope[MORELIST] = _o$dash_scope[CLOSURE_ATOM] = true;
return _o$dash_scope;}