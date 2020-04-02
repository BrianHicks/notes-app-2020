# Notes

This is my implementation of a note-taking app, organized around the following principles:

- **Linking creates structure as necessary.**
  Having to choose a category or folder for notes just makes me tired.
  I want to write and connect stuff as I think of it.
  Structure will emerge where it needs to!
  I should also be able to see all linked/unlinked references to a note.

- **Notes are for _me_.**
  Sharing is important but secondary, and exporting should be fine in most cases.
  Importing is pretty important here too!

- **If I can't organize things I need to do, I'm not going to use the tool.**
  It should handle TODOs nicely (due/defer, mark/complete/cancel, recurrence rules?)
  It should also be possible to reorder/filter todos in some sort of aggregate view.

- **Aggregate views to reveal structure.**
  These look like queries.
  Maybe I want to see only the first marked TODO in each note?
  Should be doable.

- **Anything I can see, I can edit.**
  There should be as few read-only views in the app as I can manage.

- **Keyboard shortcuts for the win.**
  I want to be able to navigate completely with the keyboard.
  Vim-ish keybindings, if possible!

I'm not 100% sure about whether this should be plain text editing or a tree/outline editor.
For one thing, I think that flatter is better when it comes to information.
If I'm going to have links create structure, why have an outline?
But on the other hand, infinite parenthetical asides are really great for capturing the overview of a thought and being able to drill down as necessary.
It's also way easier to narrow a search to a specific node in an outline view; you're more likely to get exactly the thought you were searching for or referring to.

I feel like a hybrid approach might be nice too: outline view by default, with the ability to break out into a document view, or focus into it.
Roam does this, but I'm not really a fan of the UX for enabling/disabling it.

## Prior Art

- [Roam](https://roamresearch.com) does bidirectional links amazingly well.
- [Bear](https://bear.app) has a really nice user experience.
