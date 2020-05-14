// custom element
class NodeInput extends HTMLTextAreaElement {
  connectedCallback() {
    var element = this;

    this.style.height = this.scrollHeight + "px";
    this.addEventListener("input", function() {
      console.log(this);
      this.style.height = this.scrollHeight + "px";
    });

    // TODO: remove this in the disconnectedCallback (or whatever it's called)
    document.addEventListener("selectionchange", function(event) {
      if (event.target.activeElement !== element) {
        return;
      }

      element.dispatchEvent(
        new CustomEvent("note-input-selectionchange", {
          detail: {
            start: element.selectionStart,
            end: element.selectionEnd
          }
        })
      );
    });
  }
}

customElements.define("node-input", NodeInput, { extends: "textarea" });

var db = new PouchDB("notes");
// TODO: set up syncing

db.allDocs({ include_docs: true }, function(err, allDocs) {
  // initialize Elm
  var app = Elm.Main.init({
    flags: {
      now: new Date().getTime(),
      rows: allDocs.rows
    }
  });

  // set up ports
  app.ports.put.subscribe(item => {
    db.put(item)
      .then(success => app.ports.putSuccessfully.send(success))
      .catch(err => console.error(err));
  });
});
