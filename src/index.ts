// custom element
class NoteInput extends HTMLTextAreaElement {
    connectedCallback() {
        console.log('connected');
        console.log(this);
    }
}

customElements.define('note-input', NoteInput, { extends: 'textarea' });

// go!
import { Elm } from './Main.elm'
Elm.Main.init()
