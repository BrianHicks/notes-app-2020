// https://github.com/ungap/custom-elements-builtin README
if (this.customElements) {
  try {
    customElements.define('built-in', document.createElement('p').constructor, {'extends':'p'});
  } catch(_) {
    // only WebKit or Safari
    require('@ungap/custom-elements-builtin');
  }
} else {
  // only legacy browsers
  require('@ungap/custom-elements-builtin');
}
