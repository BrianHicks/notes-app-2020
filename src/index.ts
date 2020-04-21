// custom element
class NoteInput extends HTMLTextAreaElement {
  connectedCallback() {
    var self = this;
    
    document.addEventListener('selectionchange', function(event) {
      if (event.target.activeElement !== self) { return; }

      self.dispatchEvent(new CustomEvent('note-input-selectionchange', { detail: {
        selectionStart: self.selectionStart,
        selectionEnd: self.selectionEnd,
        length: self.textLength
      })});
    });
  }
}

customElements.define('note-input', NoteInput, { extends: 'textarea' });

// go!
import { Elm } from './Main.elm'
Elm.Main.init()
