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
import { Elm } from './Main.elm'

var db = new PouchDB('notes');
// TODO: set up syncing

db.allDocs({ include_docs: true }).then(allDocs => {
  var app = Elm.Main.init({
    flags: {
      seed: Date.now(),
      events: allDocs.rows
    }
  })
  
  app.ports.put.subscribe((item) => {
    db.put(item)
      .then(success => app.ports.putSuccessfully.send(success))
      .catch(err => console.error(err));
  });
})

// go!

