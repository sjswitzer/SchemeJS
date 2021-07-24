//
// Custom Close-Button Element
//
//   There's no good way to make a close button using fonts,
//   so it has come to this.
//
// Copyright 2021 Stan Switzer
//   This work is licensed under a Creative Commons Attribution-ShareAlike
//   4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
//
function drawCloseButton(ctx, width, height, opts = {}) {
  function pf(s) { // like parseFloat, but returns undefined for non-numbers
    let res = parseFloat(s);
    if (isFinite(res))
      return res;
    return undefined;
  }
  let color = opts.color || 'black';
  let boxColor = opts.boxColor;
  if (boxColor === 'rgba(0, 0, 0, 0)' || boxColor === 'rgba(0,0,0,0)') // "transparent"
    boxColor = undefined;
  let r = pf(opts.boxRadius) ?? 20;
  let ll = (pf(opts.xSize) ?? (boxColor ? 80 : 100)) / 2;
  let tt = (pf(opts.xThick) ?? ll/1.5)/2;
  let cc = pf(opts.xCap) ?? tt/2;
  ll = ll - tt - cc/2;
  let magic = 0.551915024494; // https://spencermortensen.com/articles/bezier-circle/
  let m = magic * cc;

  ctx.save();
  // scale to a 100x100 coord system with the origin in the center
  // The "-1" is to keep a pixel from being sheered off the bottom and right edges.
  ctx.scale((width-1)/100, (height-1)/100);
  ctx.translate(50, 50);
  if (boxColor) {
    let b = 50;
    ctx.fillStyle = boxColor;
    ctx.beginPath();
    ctx.moveTo(-b+r, -b);
    ctx.lineTo(b-r, -b);
    ctx.arcTo(b, -b, b, b+r, r);
    ctx.lineTo(b, b-r);
    ctx.arcTo(b, b, b-r, b, r);
    ctx.lineTo(-b+r, b);
    ctx.arcTo(-b, b, -b, b-r, r);
    ctx.lineTo(-b, -b+r);
    ctx.arcTo(-b, -b, -b+r, -b, r);
    ctx.closePath();
    ctx.fill();
  }
  ctx.fillStyle = color;
  ctx.beginPath();
  ctx.moveTo(tt, 0);
  for (let i = 0; i < 4; ++i) {
    ctx.lineTo(tt+ll, ll);
    let mid = ll + tt/2 + cc
    ctx.bezierCurveTo(tt+ll+m, ll+m, mid+m, mid-m, mid, mid);
    ctx.bezierCurveTo(mid-m, mid+m, ll+m, tt+ll+m, ll, tt+ll);
    // Arcs are not as pretty
    // ctx.arcTo(tt+ll+c, ll+cc, mid, mid, cc);
    // ctx.arcTo(ll+cc, tt+ll+cc, ll, tt+ll, cc);
    ctx.lineTo(0, tt);
    ctx.rotate(Math.PI/2);
  }
  ctx.closePath();
  ctx.fill();
  ctx.restore();
}

class MRUCache {
  constructor(size = 20) {
    this.size = size;  // user can read/change
    this.clear();
  }
  get(signature, fn) {
    let val = this._cache.get(signature);
    if (!val) {
      val = this._backCache.get(signature);
      if (val)  // promote item from backup cache to main cache
        this._cache.set(signature, val);
      if (!val && fn) {
        val = fn(signature);
        if (val !== undefined)
          this.set(signature, fn(signature));
      }
    }
    return val;
  }
  set(signature, val) {
    if (this._cache.size >= this.size) {
      // console.info("cache evicting");
      let tmp = this._backCache;
      this._backCache = this._cache;
      this._cache = tmp;
      this._cache.clear(); 
    }
    this._cache.set(signature, val);
  }
  clear() {
    this._cache = new Map();
    this._backCache = new Map();
  }
}

export class CloseButtonElement extends HTMLElement {
  constructor() {
    super();
    let root = this.attachShadow({ mode: 'open' });
    let canvas = document.createElement('canvas');
    canvas.width = canvas.height = 1;
    root.appendChild(canvas);
    this._canvas = canvas;
    this._signature = undefined;
    this._renderCount = 0;
    this._animating = false;
    this._startClosure = event => this._startAnimation(event);
    this._endClosure = event => this._stopAnimation(event);
    this._iterationClosure = event => this._iteration(event);
    this._animateClosure = timestamp => this._render(CloseButtonElement.cacheAnimation);
  }

  // User can set to null or install a cache of a different size.
  // The cache doesn't appear to help much for transitions or animations because
  // the signatures do not often repeat. Its more helpful when rendering
  // lots of identical buttons.
  static cache = new MRUCache(10);
  static cacheAnimation = false;
  static cacheStatic = true;
  static logging = false;  // Can turn this on in the debugger

  // Handle ordinary, non-transition non-animation, resizes
  static _resizeObserver = new ResizeObserver((entries, observer) => {
    for (let entry of entries) {
      if (CloseButtonElement.logging)
        console.info(`CloseButtonElement resized`);
      entry.target._render(CloseButtonElement.cacheStatic);
    }
  });

  connectedCallback() {
    if (CloseButtonElement.logging)
      console.info(`CloseButtonElement connected`);
    // We may never be animated, so just register these to start with
    // then add the end and cancel listeners if or when an animation starts
    this.addEventListener('transitionstart', this._startClosure);
    this.addEventListener('animationstart', this._startClosure);
    CloseButtonElement._resizeObserver.observe(this);
    this._render(CloseButtonElement.cacheStatic);
  }

  disconnectedCallback() {
    if (CloseButtonElement.logging)
      console.info(`CloseButtonElement disconnected`);
    CloseButtonElement._resizeObserver.unobserve(this);
    this._stopAnimation();
    this.removeEventListener('transitionstart', this._startClosure);
    this.removeEventListener('animationstart', this._startClosure);
  }

  _startAnimation(event) {
    if (this._animating)
      return;
    if (CloseButtonElement.logging)
      console.info(`CloseButtonElement ${event.type}`);
    this._animating = true;
    this.addEventListener('transitionend', this._endClosure);
    this.addEventListener('transitioncancel', this._endClosure);
    this.addEventListener('animationend', this._endClosure);
    this.addEventListener('animationcancel', this._endClosure);
    this.addEventListener('animationiteration', this._iterationClosure);
    this._renderCount = 0;
    this._render(CloseButtonElement.cacheAnimation);
  }

  _stopAnimation(event) {
    if (!this._animating)
      return;
    if (CloseButtonElement.logging)
      console.info(`CloseButtonElement ${event.type} ${this._renderCount}`);
    this._animating = false;
    this.removeEventListener('transitionend', this._endClosure);
    this.removeEventListener('transitioncancel', this._endClosure);
    this.removeEventListener('animationend', this._endClosure);
    this.removeEventListener('animationcancel', this._endClosure);
    this.removeEventListener('animationiteration', this._iterationClosure);
    this._renderCount = 0;
    this._render(CloseButtonElement.cacheAnimation);
  }

  _iteration(event) { // Purely for instrumentation
    if (CloseButtonElement.logging) {
      console.info(`CloseButtonElement ${event.type} ${event.animationName} ${this._renderCount}`);
      this._renderCount = 0;
    }
  }

  _render(useCache) {
    if (this._animating)
      requestAnimationFrame(this._animateClosure);
    let style = getComputedStyle(this);
    let size = 1.2 * parseFloat(style.fontSize);
    let color = style.getPropertyValue('color'.trim());
    let xSize = style.getPropertyValue('--close-button-x-size').trim();
    let xThick = style.getPropertyValue('--close-button-x-thick').trim();
    let xCap = style.getPropertyValue('--close-button-x-cap').trim();
    let boxColor = style.getPropertyValue('--close-button-box-color').trim();
    let boxRadius = style.getPropertyValue('--close-button-box-radius').trim();
    let opts = { size, color, xSize, xThick, xCap, boxColor, boxRadius };
    let signature = Object.values(opts).join('/');
    if (this._signature !== signature) {
      this._signature = signature;
      ++this._renderCount;
      this._canvas.setAttribute('width', size);
      this._canvas.setAttribute('height', size);
      const ctx = this._canvas.getContext('2d');
      ctx.clearRect(0, 0, size, size);
      if (useCache && CloseButtonElement.cache) {
        let canvas = CloseButtonElement.cache.get(signature, () => {
          let canvas = document.createElement('canvas');
          canvas.setAttribute('width', size);
          canvas.setAttribute('height', size);
          const ctx = canvas.getContext('2d');
          drawCloseButton(ctx, size, size, opts);
          return canvas;
        });
        ctx.drawImage(canvas, 0, 0);
      } else {
        drawCloseButton(ctx, size, size, opts);
      }
    }
  }
};

customElements.define('close-button', CloseButtonElement);
if (CSS.registerProperty) {
  // "namespace" the properties with the custom element name
  CSS.registerProperty({ name: '--close-button-box-color',
      syntax: '<color>', inherits: false, initialValue: 'transparent' });
  CSS.registerProperty({ name: '--close-button-box-radius',
      syntax: '<percentage>', inherits: false, initialValue: '20%' });
  CSS.registerProperty({ name: '--close-button-x-size',
      syntax: '<percentage>', inherits: false, initialValue: '90%' });
  CSS.registerProperty({ name: '--close-button-x-thick',
      syntax: '<percentage>', inherits: false, initialValue: '30%' });
  CSS.registerProperty({ name: '--close-button-x-cap',
      syntax: '<percentage>', inherits: false, initialValue: '10%' });
}
// Emulate a UA stylesheet for the element
let stylesheet = document.createElement("style");
stylesheet.textContent = `close-button { cursor: pointer; }`;
document.head.prepend(stylesheet);