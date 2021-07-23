<!DOCTYPE html>
<!--
  Stand-alone test for <close-button> element.

  Copyright 2021 Stan Switzer
    This work is licensed under a Creative Commons Attribution-ShareAlike
    4.0 International License. https://creativecommons.org/licenses/by-sa/4.0/
-->
<meta charset="utf-8">
  <body style="font: 100% caption, sans-serif">
    <style>
      close-button.bluebox {
        color: white;
        vertical-align: -20%;
        --close-button-box-color: blue;
        --close-button-x-size: 75%;
      }

      close-button.delete-text {
        color: white;
        --close-button-box-color: #8e8d92;
        --close-button-box-radius: 50%;
        --close-button-x-size: 50%;
      }

      close-button.transition {
        font-size: 3em;
        color: white;
        --close-button-box-color: #8e8d92;
        --close-button-box-radius: 50%;
        --close-button-x-size: 50%;
        --close-button-x-thick: 20%;
        transition: 2s;
        transition-property: font-size, color, --close-button-box-color, --close-button-box-radius,
          --close-button-x-size, --close-button-x-thick, --close-button-x-cap;
      }
      close-button.transition:hover {
        vertical-align: -20%;
        font-size: 5em;
        color: black;
        --close-button-box-color: red;
        --close-button-box-radius: 10%;
        --close-button-x-size: 80%;
        --close-button-x-cap: 0%;
        --close-button-x-thick: 40%;
      }

      close-button.animation {
        animation-duration: 2s;
        animation-name: allprops;
        animation-iteration-count: infinite;
        animation-direction: alternate;
        font-size: 3em;
        color: yellow;
        --close-button-box-color: #8e8d92;
        --close-button-box-radius: 50%;
        --close-button-x-size: 50%;
        --close-button-x-thick: 20%;
      }
      @keyframes allprops {
        from {
        }
        to {
          font-size: 5em;
          color: black;
          --close-button-box-color: rgb(230, 51, 176);
          --close-button-box-radius: 5%;
          --close-button-x-size: 80%;
          --close-button-x-cap: 0%;
          --close-button-x-thick: 40%;
        }
      }
    </style>
    <p> Drawn close-boxes:
    <p> <canvas id="canvas1" width="200" height="200">What?</canvas>
        <canvas id="canvas2" width="200" height="200">What?</canvas>
        <canvas id="canvas3" width="200" height="200">What?</canvas>
        <canvas id="canvas4" width="200" height="200">What?</canvas>
    <p> Close-button elements:
    <p> <close-button></close-button> Text
        <close-button></close-button> Text
        <close-button></close-button> Text
    <p> Close-box size and X color are based on the font-size and color properties:
    <p style="font-size: 2em">
        <close-button class="bluebox"></close-button> Text
        <close-button class="bluebox"></close-button> Text
        <close-button class="bluebox"></close-button> Text
    <p> <close-button class="delete-text"></close-button> Text
        <close-button class="delete-text"></close-button> Text
        <close-button class="delete-text"></close-button> Text
    <p> Transitions and Animations only work for custom properties in up-to-date Blink-based
        browsers. So in Chrome, you'll see every parameter of the box transition or animate
        but in other browsers you'll only see the size and X color animate.
    <p> <close-button class="transition"></close-button> Transition (hover over the button)
    <p> <close-button class="animation"></close-button> Animation
    <script>
      function drawCloseButtonOnCanvas(canvasName, opts) {
        const canvas = document.getElementById(canvasName);
        const ctx = canvas.getContext('2d');
        ctx.save();
        ctx.fillStyle = '#eee';  // so you can see which parts are transparent
        ctx.fillRect(0, 0, canvas.width, canvas.height);
        ctx.restore();
        drawCloseButton(ctx, canvas.width, canvas.height, opts);
      }
      drawCloseButtonOnCanvas('canvas1');
      drawCloseButtonOnCanvas('canvas2', { xCap: 0 });
      drawCloseButtonOnCanvas('canvas3', { boxColor: '#f88' });
      drawCloseButtonOnCanvas('canvas4', { boxColor: '#66B', xThick: 30, color: 'white' });

      //
      // Custom close button element
      //   There's no good way to make a close button using fonts,
      //   so it has come to this.
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

      class CloseButtonElement extends HTMLElement {
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
  </script>
</body>