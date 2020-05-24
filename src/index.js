// custom element
class NodeInput extends HTMLTextAreaElement {
  connectedCallback() {
    var element = this;

    this.style.height = this.scrollHeight + "px";
    this.addEventListener("input", function () {
      this.style.height = this.scrollHeight + "px";
    });

    // TODO: remove this in the disconnectedCallback (or whatever it's called)
    document.addEventListener("selectionchange", function (event) {
      if (event.target.activeElement !== element) {
        return;
      }

      element.dispatchEvent(
        new CustomEvent("note-input-selectionchange", {
          detail: {
            start: element.selectionStart,
            end: element.selectionEnd,
          },
        })
      );
    });
  }
}

customElements.define("node-input", NodeInput, { extends: "textarea" });

// Go!
import * as Elm from "./elm.js";
import { PouchDB } from "./pouchdb.js";

var db = new PouchDB("notes");
// TODO: set up syncing

(async function () {
  let allDocs = await db.allDocs({ include_docs: true });

  let settings = null;
  try {
    settings = await db.get("_local/settings");
  } catch (err) {
    if (err.status !== 404) {
      throw err;
    }
  }

  var app = Elm.Main.init({
    flags: {
      now: new Date().getTime(),
      documents: allDocs.rows,
      settings: settings,
    },
  });

  // saving
  app.ports.put.subscribe((item) => {
    db.put(item)
      .then((success) => app.ports.putSuccessfully.send(success))
      .catch((err) => console.error(err));
  });

  // syncing
  app.ports.syncOnce.subscribe((url) => {
    db.replicate.from(url).on("complete", (info) => {
      db.sync(url, { retry: true })
        .on("change", (info) => console.log("change", info))
        .on("paused", (err) => console.log("paused", err))
        .on("active", () => console.log("active"))
        .on("denied", (err) => console.log("denied", err))
        .on("complete", () => console.log("complete"))
        .on("error", (err) => console.log("error", err));
    });
  });
})();
