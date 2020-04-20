// custom element
class NoteInput extends HTMLTextAreaElement {
  connectedCallback() {
    console.log('connected');
    console.log(this);
    var self = this;
    
    document.addEventListener('selectionchange', function(event) {
      // if (event.target.activeElement !== self) { return; }
      const range = document.getSelection().getRangeAt(0);
      console.log(range.getBoundingClientRect());
      // self.dispatchEvent(new CustomEvent('select', { detail: {
      //     start: {
      //         node: range.startContainer,
      //         offset: self.offsetUntil(range.startContainer) + range.startOffset
      //     },
      //     end: {
      //         node: range.endContainer,
      //         offset: self.offsetUntil(range.endContainer) + range.endOffset
      //     },
      //     originalEvent: range,
      // }}));
    });
  }
}

customElements.define('note-input', NoteInput, { extends: 'textarea' });

// go!
import { Elm } from './Main.elm'
Elm.Main.init()
