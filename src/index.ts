// custom element
class NodeInput extends HTMLTextAreaElement {
  connectedCallback() {
    var self = this;

    // TODO: remove this in the disconnectedCallback (or whatever it's called)
    document.addEventListener('selectionchange', function(event) {
      if (event.target.activeElement !== self) { return; }

      self.dispatchEvent(new CustomEvent('note-input-selectionchange', { detail: {
        start: self.selectionStart,
        end: self.selectionEnd
      })});
    });
  }
}

customElements.define('node-input', NodeInput, { extends: 'textarea' });

// storage
import PouchDB from 'pouchdb';
import { Elm } from './Main.elm'

var db = new PouchDB('notes');
// TODO: set up syncing

(async function main() {
  // get flags
  var allDocs = await db.allDocs({ include_docs: true })

  // initialize Elm
  var app = Elm.Main.init({
    flags: {
      seed: Date.now(),
      rows: allDocs.rows
    }
  })

  // set up ports
  app.ports.put.subscribe((item) => {
    db.put(item)
      .then(success => app.ports.putSuccessfully.send(success))
      .catch(err => console.error(err));
  });
})();
