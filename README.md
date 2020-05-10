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

Some other ideas...

- It'd be cool to be able to select/export a group of notes.
  These could either be viewed in a read-only mode or imported into someone else's database.
  I guess it would also be possible to structure the export with CRDTs for easy merges.
- It'd be cool to persistently save a database somewhere shared, but that potentially violates the "no formal structure" principle so it needs to be done carefully.

## Technical Ideas

Just jotting down some ideas prior to actually building this.

I think it may make sense to store all note parts in a big dictionary.
Pros: simple, uniform, `O(log n)` access for a single node.
Cons: Potentially costly to retrieve all the nodes in the document.

So I guess ideally I'd like to assign IDs in a way that are likely to be serial.
Then selecting a subtree would maybe be easier.
The [LSEQ](https://hal.archives-ouvertes.fr/hal-00921633/document) algorithm might be appropriate here?

Of course, I could also keep a second index with all the contents by note ID...

## Next things to do

- [ ] tree editing
   - [x] ability to move a node up and down as well as in and out
       - [x] moving up and down to siblings. Uncontroversial!
       - [x] if you're at the first child, moving up goes to the parent.
       - [x] if you're at the last child, moving down goes to the parent's next sibling.
   - [ ] moving non-edit focus around
       - [ ] hitting up and down keys should go to the previous/next sibling
           - [ ] this might mean adding rich text support with like elm-rte
   - refinements on existing behavior
     - [ ] move to the previous sibling when I backspace through a node
     - [ ] hitting tab/shift-tab should preserve the cursor position, if possible
         - Html.Keyed? Blocking keyup defaults as well as keydown?
- [ ] nice searching for the links
- [x] persistence
- [ ] nice design
- [ ] order notes according to most recently updated
- [ ] allow multiple notes to be edited concurrently
- [ ] come up with a consistent design for keybindings (e.g. alt moves edit, alt+shift moves node)

Someday/maybe

- [ ] exporting to
  - (likely) markdown
  - (less likely) rich text of some kind
- [ ] metadata
- [x] at some point, I want to move away from using ints for IDs and go to UUIDs. Possible UUID1s?
- [ ] It'd be super slick to do an idea-fight kind of thing where one could order the children of a node.

### Notes to Self

elm-rte-toolkit looks cool, and it may make sense to add it later, but don't go for the complexity now!
Keep things simple as long as possible!

## Layout


```
┌────────────────────────────┬─────────────────────────────────────────┬────────────────────────────┐
│    Logo and menu items     │                                         │                            │
│                            │                                         │                            │
├────────────────────────────┤     note title (biggish! H1!)           │       note inspector       │
│Title title                 │                                         │                            │
│first bullet point, second  │                                         │ holds note metadata, edits │
├────────────────────────────┤     • first bullet point                │   user metadata for both   │
│Title title                 │     • second bullet point               │selected node and in general│
│first bullet point, second  │     • third bullet point                │                            │
├────────────────────────────┤       • sub bullet                      │                            │
│Title title                 │       • sub bullet                      ├────────────────────────────┤
│first bullet point, second  │     • conclusion                        │                            │
├────────────────────────────┤                                         │                            │
│Title title                 │  ┌───────────────────────────────────┐  │                            │
│first bullet point, second  │  │                                   │  │                            │
├────────────────────────────┤  │                                   │  │                            │
│Title title                 │  │            links space            │  │                            │
│first bullet point, second  │  │                                   │  │                            │
├────────────────────────────┤  │ holds incoming links to this note │  │      references space      │
│Title title                 │  │        for easy navigation        │  │                            │
│first bullet point, second  │  │                                   │  │  holds pinned items in an  │
├────────────────────────────┤  │                                   │  │      editable format       │
│                            │  └───────────────────────────────────┘  │                            │
│                            │                                         │                            │
│                            │                                         │                            │
│                            │                                         │                            │
│                            │                                         │                            │
│                            │                                         │                            │
│                            │                                         │                            │
│                            │                                         │                            │
└────────────────────────────┴─────────────────────────────────────────┴────────────────────────────┘
```

## Prior Art

- [Roam](https://roamresearch.com) does bidirectional links amazingly well.
- [Bear](https://bear.app) has a really nice user experience.
