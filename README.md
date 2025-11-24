# Glemax: Emacs Reimplemented on Modern Hardware

<img src="assets/uwu.png" width="200" align="right" />

Glemax is a faithful, function-by-function reimplementation of GNU Emacs, built from scratch in C with a Vulkan rendering engine. This is not an Emacs-inspired editor—it is Emacs itself, rebuilt on a foundation designed for modern hardware.

## For Emacs Users

If you're proficient at using Emacs for text editing, you'll feel right at home. Every editing command you've learned works exactly as you expect. The muscle memory transfers completely.

`C-n` moves down a line. `C-k` kills to end of line or the whole line if `kill-whole-line` is #t, or backward to beginning with `C-0 C-k`. `C-3 M-f` moves forward three words. `C-x 2` splits the window horizontally. `C-x o` cycles through windows. Everything works.

The keychord system is complete: multi-key sequences like `C-x C-f`, prefix arguments that propagate correctly, lambdas with arguments bound to keys. Window management behaves identically—splits maintain correct proportions, `C-x 0` deletes the current window, `C-x 1` deletes others, `C-x +` balances sizes.

Movement commands handle all the edge cases you're used to. Words stop at the right boundaries. Paragraphs find blank lines. S-expressions understand nesting, strings, and comments. The kill ring appends consecutive kills just like Emacs, the implementation tracks `last-command` to get this right.

## For Elisp Programmers

The extension language is Guile Scheme instead of Emacs Lisp. This brings cleaner syntax and better tooling while keeping the Lisp spirit that makes Emacs programmable.

The core editing primitives are here: `point`, `mark`, `buffer-size`, `line-beginning-position`, `line-end-position`, `current-column`. The APIs match Emacs. There are naming changes for Scheme conventions, predicates end in `?` instead of `p`, so `bobp` becomes `bob?`, `bolp` becomes `bol?`, `mark-active-p` becomes `mark-active?`. Small adjustments.

Buffers and windows are first-class foreign objects with proper identity:

```scheme
(eq? (current-buffer) (current-buffer))  ; => #t
(eq? (next-window) (next-window))        ; => #t
```

The same buffer or window always returns the same Scheme object. This is implemented through object caching, when a C pointer is wrapped for Scheme, we check a hash table first. If we've already created a foreign object for this pointer, we return it. This gives proper object identity semantics.

You can manipulate these objects directly:

```scheme
;; Get windows and buffers
(define win (selected-window))
(define buf (window-buffer win))

;; Query their state
(window-point win)           ; => 131
(buffer-size)                ; => 512
(window-width win)           ; => 107

;; Modify them
(set-window-point win 100)
(set-window-buffer win (get-buffer "*Messages*"))

;; Iterate over all windows
(for-each
  (lambda (w)
    (message "Window showing ~a at point ~a"
             (buffer-name (window-buffer w))
             (window-point w)))
  (window-list))
```

Commands are C functions wrapped with optional argument handling. Call `(forward-char 5)` and it sets the prefix argument before calling the C function. Bind it to a key and it uses the global prefix argument from `C-u`. This matches Emacs semantics exactly.

Documentation strings work:

```scheme
(define (insert-timestamp)
  "Insert the current Unix timestamp at point."
  (insert (number->string (current-time))))

(keychord-bind "C-c t" insert-timestamp)
```

Variables work as expected:

```scheme
(define kill-whole-line #t)
(set-var-doc! kill-whole-line
  "If non-false, `kill-line' with no arg at start of line kills the whole line.")
```

The C code queries these with `scm_get_bool("kill-whole-line", false)`. No FFI complexity.

## Why Rewrite Emacs?

GNU Emacs is brilliant but carries four decades of architectural assumptions. **Attempting to bolt a modern GPU rendering pipeline onto Emacs's core display engine is like putting a jet engine on a horse-drawn carriage. To truly unlock the potential of modern hardware, we need a new chassis built from the ground up.**

What if we kept everything that makes Emacs great but rebuilt it on modern foundations?

- Render at thousands of frames per second using GPU acceleration
- Minimize garbage collection with efficient C functions
- Support 3D geometry rendering in buffers
- Enable shader-based text styling
- Maintain 100% behavioral compatibility with GNU Emacs
- **Provide the best Guile Scheme editing experience possible**

Glemax is designed from the ground up to be the ultimate editor for Guile Scheme development. With native Scheme integration, instant evaluation, proper object identity, it's built to make Scheme programming feel as fluid and powerful as it deserves to be.

This isn't about making an "Emacs-like" editor. I'm reimplementing every function, one by one, matching GNU Emacs behavior and API exactly. Every edge case. Every prefix argument behavior. Everything.

## Faces, Text Properties, and Theming

Glemax includes a complete face and text property system with a powerful theming engine. Faces define how text appears, every character is rendered with a face controlling its colors, font variant, and styling attributes.

### Basic Face Queries

```scheme
;; Query face properties
(face-foreground 'default)    ; => "#ffffff"
(face-background 'default)    ; => "#000000"

;; Modify faces directly
(set-face-foreground! 'mode-line "#FF0000")
(set-face-background! 'cursor "#505050")
```

Built-in faces include: `default`, `cursor`, `mode-line`, `mode-line-active`, `mode-line-inactive`, `fringe`, `window-divider`, `bold`, `italic`, `bold-italic`, and `visible-mark`.
Many more are expected to be added over time.


### Text Properties

Glemax has a proper text properties implementation. Apply faces to specific regions of text to create syntax highlighting, emphasis, or custom styling:

```scheme
;; Highlight text from position 10 to 50 with the bold face
(put-text-property 0 20 'face 'bold)

;; Query the face at a position
(get-text-property 25 'face)  ; => bold

;; Remove properties from a region
(remove-text-properties 10 50 'face)
```

Text properties automatically adjust when you insert or delete text, maintaining correct highlighting as the buffer changes. The implementation handles all edge cases: splitting properties, merging adjacent properties, and efficient updates during editing.

### Defining and Loading Themes

Create custom themes with full control over all visual elements:

```scheme
;; Define a custom theme
(deftheme 'my-theme "My personal color scheme")

(custom-theme-set-faces 'my-theme
  '(default ((t (:foreground "#E0E0E0" :background "#1A1A1A"))))
  '(cursor  ((t (:background "#00FF00"))))
  '(mode-line ((t (:foreground "#FFFFFF" :background "#333333")))))

;; Load and enable the theme
(load-theme 'my-theme)
```

### Shipped Themes

Glemax comes with carefully crafted themes:

```scheme
(load-theme 'modus-vivendi)      ; High-contrast black background
(load-theme 'nord)               ; Cool, arctic-inspired palette
(load-theme 'kaolin-dark)        ; Dark jade theme
(load-theme 'kaolin-ocean)       ; Dark blue variant
(load-theme 'kaolin-temple)      ; Warm, terrestrial colors
(load-theme 'dark-one)           ; Inspired by Atom One Dark
(load-theme 'city-lights)        ; Atom's City Lights theme
(load-theme 'molokai)            ; Classic Molokai port
(load-theme 'monokai-ristretto)  ; Refined Monokai
```

### Multiple Theme Layering

Just like Emacs, Glemax supports multiple active themes simultaneously. Themes are layered—later themes override settings from earlier themes:

```scheme
;; Load base theme
(load-theme 'dark-one)

;; Layer on customizations - these override dark-one's settings
(deftheme 'my-tweaks "Personal adjustments")
(custom-theme-set-faces 'my-tweaks
  '(cursor ((t (:background "#FF00FF"))))
  '(mode-line ((t (:background "#000000")))))
(enable-theme 'my-tweaks)

;; Check what's active
(custom-enabled-themes)  ; => (my-tweaks dark-one)

;; Disable specific themes
(disable-theme 'my-tweaks)

;; Cool trick: disable all themes at once
(map disable-theme (custom-enabled-themes))
```

When you disable all themes, Glemax returns to its base theme: clean black text on white background, just like a fresh Emacs install.

### Theme System Features

The theme engine is production-quality:

- **Proper inheritance**: Faces automatically inherit from `default` unless explicitly overridden
- **Color space correctness**: Colors are stored in linear RGB and converted properly for display
- **Live updates**: Changes apply immediately across all buffers and windows
- **Multiple layers**: Stack themes for base + customizations workflow
- **Reset capability**: Always return cleanly to base theme

This is the foundation for syntax highlighting, semantic coloring, and complete visual customization.

## Architecture

### Text Storage: Ropes

Buffers use a rope data structure, a balanced binary tree where leaves contain strings. This gives:

- O(log n) insertion and deletion anywhere
- No reallocation or copying
- Efficient iteration

The rope caches total length, so `buffer-size` is O(1).

### Window Management: Binary Space Partition

Windows form a binary tree. Each window is either a leaf (displays a buffer, has a point) or a split (has two children and a split ratio).

Operations are natural:
- Split: replace leaf with split node, create two children
- Delete: remove leaf, promote sibling
- Resize: adjust split ratios

Geometry is calculated recursively from the root. When you resize or delete, we recalculate the tree. Fast enough to be imperceptible.

Each window tracks its own point. Split a window and both start with the same point, but they diverge as you edit. The selected window's point syncs with `current-buffer->pt`. This matches Emacs exactly.

### Extension Language: Guile Scheme

Guile is a serious Lisp implementation with proper lexical scoping, hygienic macros, a module system, and good performance.

The C/Scheme boundary is clean. Functions are wrapped as procedures. Foreign objects (buffers, windows) use type descriptors. We define commands with two macros:

```c
// Define the Scheme wrapper with documentation
DEFINE_SCM_COMMAND(scm_forward_char, forward_char,
  "Move point N characters forward (backward if N is negative).");

// Register in Scheme
REGISTER_COMMAND("forward-char", scm_forward_char);
```

The first generates a function handling optional prefix arguments. The second registers it with Guile and attaches documentation. Minimal boilerplate.

### Keychords

The system supports:
- Multi-key sequences: `C-x C-f`, `C-x r t`
- Prefix arguments: `C-u`, digit arguments, negatives
- Key/action separation on press/release/repeat
- Scheme and C function binding

Press `C-x` and the system waits for the next key. Press `C-f` and it executes `C-x C-f`. Press something unbound and it resets.

Prefix arguments are global, consumed by commands. `C-3 M-f` sets `prefix-arg` to 3, calls `forward-word`, which reads it, moves three words, and resets to 1.

## Configuration Example

This works right now:

```scheme
;; Editing
(keychord-bind "C-n" next-line)
(keychord-bind "C-p" previous-line)
(keychord-bind "C-f" forward-char)
(keychord-bind "C-b" backward-char)
(keychord-bind "M-f" forward-word)
(keychord-bind "M-b" backward-word)

;; Killing
(keychord-bind "C-k" kill-line)
(keychord-bind "C-w" kill-region)
(keychord-bind "M-d" kill-word)
(keychord-bind "C-y" yank)

;; Windows
(keychord-bind "C-x 2" split-window-below)
(keychord-bind "C-x 3" split-window-right)
(keychord-bind "C-x 0" delete-window)
(keychord-bind "C-x o" other-window)

;; Custom commands
(define (mark-whole-buffer)
  "Put point at beginning and mark at end of buffer."
  (set-mark (buffer-size))
  (beginning-of-buffer))

(keychord-bind "C-x h" mark-whole-buffer)

;; Commands with arguments
(keychord-bind "C-T" (lambda () (transpose-chars -1)))

;; Helper functions
(define (bob?) (= (point) 0))
(define (eob?) (= (point) (buffer-size)))
(define (bol?) (= (point) (line-beginning-position)))
(define (eol?) (= (point) (line-end-position)))

(define (delete-horizontal-space)
  "Delete all spaces and tabs around point."
  (while (and (not (bob?))
              (let ((ch (char-before)))
                (or (= ch 32) (= ch 9))))
    (delete-backward-char))
  (when (= prefix-arg 1)
    (while (and (not (eob?))
                (let ((ch (char-after)))
                  (or (= ch 32) (= ch 9))))
      (delete-char))))

(keychord-bind "M-\\" delete-horizontal-space)

;; Variables
(define blink-cursor-mode #t)
(define electric-pair-mode #t)
(define kill-whole-line #t)

;; Theme
(load-theme 'modus-vivendi)
```

## Design Principles

### Emacs Compatibility First

Every command must behave exactly like GNU Emacs. Not "close enough"—exactly. Edge cases, prefix arguments, point movement, mark activation—all identical.

When implementing:
1. Read the Emacs Lisp manual
2. Test behavior in GNU Emacs
3. Implement the C function
4. Test against GNU Emacs again
5. Fix any differences

If behavior differs, (most of the times) it's a bug.

### Build on Solid Foundations

No feature without correct infrastructure. No syntax highlighting before faces. No modes before keymaps. No packages before the standard library exists.

Slower than rushing to parity, but prevents technical debt. We can design correctly from the start.

### Performance as a Feature

Text editing should be instant. Cursor never stutters. Window operations imperceptible.

This requires efficient data structures, minimal allocation, cache-friendly layouts, GPU acceleration where appropriate. Profile regularly. Optimize hot paths. But correctness before optimization.

### Visual Fidelity

Every UI element is themeable. Colors are managed correctly in linear RGB space for accurate rendering. The face system provides fine-grained control over text appearance, from simple color changes to font variants. Text properties adjust seamlessly during editing.

### Self-Documenting from the Start

Every function has documentation. Every variable is documented. The system is explorable.

Commands have docstrings in C. Variables can have documentation. Eventually `C-h f` and `C-h v` will work.

## Build and Run

First clone build and install the obsidian engine library
```bash
git clone https://github.com/laluxx/obsidian
cd obsidian
sudo make install
cd ..
```

Make sure you have Vulkan validation layers installed
```bash
sudo pacman -S vulkan-validation-layers
```

Then clone and build glemax
```bash
git clone https://github.com/laluxx/glemax
cd glemax
make
./glemax
```

The init file location is checked in this order:
1. `./init.scm` Faster for development
2. `./.glemax`
3. `./.glemax.d/init.scm`
4. `~/.config/glemax/init.scm`

## Contributing

Contributions welcome. Understand the goals:

1. Match GNU Emacs behavior exactly. Test against real Emacs.
2. Build on solid foundations. Don't add features needing missing infrastructure.
3. Write clear, documented code. Explain what functions do and why.
4. Follow existing patterns. Use the same macros, error handling, naming.

If you know C and Emacs, you can contribute.

## Why "Glemax"?

- **G** for [Guile](https://www.gnu.org/software/guile/)
- **L** for Graphics Library
- **E** for Emacs
- **MAX** for maximizing what Emacs could be

The name captures what this project is: Guile Scheme as the extension language, modern graphics libraries for rendering, Emacs as the foundation, and maximizing what Emacs can be, pushing it to its limits in performance, capability, and possibility.

---

Glemax is Emacs rebuilt on modern foundations. Fast, clean, extensible.

If you're an Emacs user, you should feel at home. If you're an Elisp programmer, the transition to Scheme is straightforward. If you care about performance and capability, Glemax offers both. If you want the best Guile Scheme editing experience, Glemax is built for you.

The project is young, but the foundations are solid.
