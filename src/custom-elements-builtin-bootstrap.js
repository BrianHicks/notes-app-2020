// bootstrap from https://github.com/ungap/custom-elements-builtin README
(function() {
  function polyfill() {
    var el = document.createElement("script");
    el.src = "/script/custom-elements-builtin.js";
    document.body.appendChild(el);
  }

  if (this.customElements) {
    try {
      customElements.define(
        "built-in",
        document.createElement("p").constructor,
        {
          extends: "p"
        }
      );
    } catch (_) {
      // only WebKit or Safari
      polyfill();
    }
  } else {
    // only legacy browsers
    polyfill();
  }
})();
