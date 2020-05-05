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

// storage
import PouchDB from 'pouchdb';
var db = new PouchDB('http://localhost:5984/notes');

// go!
import { Elm } from './Main.elm'
var app = Elm.Main.init()

app.ports.persistLogEntry.subscribe((logItem) => {
  console.log(logItem);
})
