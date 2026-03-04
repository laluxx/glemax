;;; dired.scm --- Directory editor

;;; Commentary:

;; Dired mode for glemax. Uses ls --dired for directory listings.
;; Faces use defface with :inherit so they follow the active theme.

;;; Code:

(use-modules (ice-9 popen)
             (ice-9 rdelim))

;;; Variables

(defvar dired-listing-switches "-al"
  "Switches passed to ls for Dired. MUST contain -l.")

(defvar dired-auto-revert-buffer #f
  "Non-nil means revert the Dired buffer before listing.")

(defvar dired-kill-when-opening-new-dired-buffer #f
  "Non-nil means kill current Dired buffer when opening a new one.")

(defvar dired-dwim-target #f
  "If non-nil, Dired tries to guess a default target directory.")

(defvar dired-recursive-deletes 'top
  "Whether Dired deletes directories recursively.")

(defvar dired-recursive-copies 'top
  "Whether Dired copies directories recursively.")

(defvar dired-copy-preserve-time #t
  "Non-nil means preserve file timestamps when copying.")

(defvar dired-use-ls-dired #t
  "Non-nil means pass --dired to ls.")

(defvar dired-hide-details-mode #f
  "Non-nil means hide permission/size/date columns.")

;;; Buffer-local variables

(defvar-local dired-directory nil
  "Absolute directory path this Dired buffer is showing (with trailing /).")

(defvar-local dired-marked-files '()
  "List of absolute paths of marked files in this Dired buffer.")

;;; Faces

(defface dired-header
  '((t (:inherit font-lock-type-face)))
  "Face used for directory headers.")

(defface dired-mark
  '((t (:inherit font-lock-constant-face)))
  "Face used for Dired mark characters.")

(defface dired-marked
  '((t (:inherit warning)))
  "Face used for marked file names.")

(defface dired-flagged
  '((t (:inherit error)))
  "Face used for files flagged for deletion.")

(defface dired-warning
  '((t (:inherit font-lock-warning-face)))
  "Face used to highlight warnings in Dired.")

(defface dired-perm-write
  '((t (:inherit font-lock-comment-delimiter-face)))
  "Face used for group/world-writable permission bits.")

(defface dired-directory
  '((t (:inherit font-lock-function-name-face)))
  "Face used for subdirectory names.")

(defface dired-symlink
  '((t (:inherit font-lock-keyword-face)))
  "Face used for symbolic link names.")

(defface dired-broken-symlink
  '((t (:foreground "yellow1" :background "red1" :weight bold)))
  "Face used for broken symbolic links.")

(defface dired-executable
  '((t (:inherit font-lock-function-name-face :weight bold)))
  "Face used for executable file names.")

(defface dired-special
  '((t (:inherit font-lock-variable-name-face)))
  "Face used for special files (sockets, pipes, block/char devices).")

(defface dired-ignored
  '((t (:inherit shadow)))
  "Face used for ignored/object file names.")

;;; Internal: ls helpers

(define (dired--run-ls quoted-dir switches)
  "Run ls SWITCHES -- QUOTED-DIR and return output as a string."
  (let* ((flags (if dired-use-ls-dired
                    (string-append switches " --dired")
                    switches))
         (cmd  (string-append "ls " flags " -- " quoted-dir))
         (port (open-input-pipe cmd))
         (out  (let loop ((acc ""))
                 (let ((line (read-line port)))
                   (if (eof-object? line) acc
                       (loop (string-append acc line "\n")))))))
    (close-pipe port)
    out))

(define (dired--ignored-name? name)
  "Return #t if NAME should use the dired-ignored face."
  (or (string-suffix? ".o"   name)
      (string-suffix? ".a"   name)
      (string-suffix? ".so"  name)
      (string-suffix? ".pyc" name)
      (string-suffix? "~"    name)
      (string-suffix? ".elc" name)))

(define (dired--skip-fields str n)
  "Skip N whitespace-separated fields in STR.
Returns the character index where field N begins, or #f."
  (let ((len (string-length str)))
    (let loop ((i 0) (fields 0) (in-space #t))
      (cond
       ((>= i len) #f)
       ((char=? (string-ref str i) #\space)
        (loop (+ i 1) fields #t))
       (in-space
        (if (= fields n)
            i
            (loop (+ i 1) (+ fields 1) #f)))
       (else
        (loop (+ i 1) fields #f))))))

;;; Internal: buffer insertion

(define (dired--insert-listing dir switches)
  "Insert an ls listing for DIR (with trailing /) using SWITCHES."
  (let* ((quoted (string-append "\"" dir "\""))
         (output (dired--run-ls quoted switches))
         (lines  (string-split output #\newline)))

    ;; Header — 2 spaces indent
    (let ((hstart (point)))
      (insert (string-append "  " dir ":\n"))
      (put-text-property hstart (point) 'face
                         (face-id-from-name "dired-header")))

    (for-each
     (lambda (line)
       (when (and (> (string-length line) 0)
                  ;; Skip "total NNN" — scan past leading spaces then check
                  (not (let loop ((i 0))
                         (cond ((>= i (string-length line)) #f)
                               ((char=? (string-ref line i) #\space) (loop (+ i 1)))
                               (else (string-prefix? "total" (substring line i))))))
                  (not (string-prefix? "//DIRED" line))
                  (not (string-prefix? "//DIRED-OPTIONS" line)))
         (let* ((line-start (point))
                (ftype      (string-ref line 0))
                (fname-col  (dired--skip-fields line 8)))
           (insert line)
           (insert "\n")
           (when (and fname-col (< fname-col (string-length line)))
             (let* ((full-name  (substring line fname-col))
                    (arrow      (string-contains full-name " -> "))
                    (name       (if arrow
                                    (substring full-name 0 arrow)
                                    full-name))
                    (buf-start  (+ line-start fname-col))
                    (buf-end    (+ buf-start (string-length name)))
                    (face-name
                     (cond
                      ((char=? ftype #\d)              "dired-directory")
                      ((char=? ftype #\l)              "dired-symlink")
                      ((and (char=? ftype #\-)
                            (> (string-length line) 3)
                            (char=? (string-ref line 3) #\x))
                                                       "dired-executable")
                      ((memv ftype '(#\p #\s #\b #\c)) "dired-special")
                      ((dired--ignored-name? name)     "dired-ignored")
                      (else #f))))
               (when face-name
                 (put-text-property buf-start buf-end 'face
                                    (face-id-from-name face-name))))))))
     lines)))

;; (define (dired--insert-listing dir switches)
;;   "Insert an ls listing for DIR (with trailing /) using SWITCHES."
;;   (let* ((quoted (string-append "\"" dir "\""))
;;          (output (dired--run-ls quoted switches))
;;          (lines  (string-split output #\newline)))

;;     ;; Header — 2 spaces indent
;;     (let ((hstart (point)))
;;       (insert (string-append "  " dir ":\n"))
;;       (put-text-property hstart (point) 'face
;;                          (face-id-from-name "dired-header")))

;;     (for-each
;;      (lambda (line)
;;        (when (and (> (string-length line) 0)
;;                   ;; Skip "total NNN" line
;;                   (not (string-prefix? "total" (string-trim line)))
;;                   ;; Skip --dired metadata
;;                   (not (string-prefix? "//DIRED" line))
;;                   (not (string-prefix? "//DIRED-OPTIONS" line)))
;;          (let* ((line-start (point))
;;                 (ftype      (string-ref line 0))
;;                 (fname-col  (dired--skip-fields line 8)))
;;            (insert line)
;;            (insert "\n")
;;            (when (and fname-col (< fname-col (string-length line)))
;;              (let* ((full-name  (substring line fname-col))
;;                     (arrow      (string-contains full-name " -> "))
;;                     (name       (if arrow
;;                                     (substring full-name 0 arrow)
;;                                     full-name))
;;                     (buf-start  (+ line-start fname-col))
;;                     (buf-end    (+ buf-start (string-length name)))
;;                     (face-name
;;                      (cond
;;                       ((char=? ftype #\d)
;;                        "dired-directory")
;;                       ((char=? ftype #\l)
;;                        "dired-symlink")
;;                       ((and (char=? ftype #\-)
;;                             (> (string-length line) 3)
;;                             (char=? (string-ref line 3) #\x))
;;                        "dired-executable")
;;                       ((memv ftype '(#\p #\s #\b #\c))
;;                        "dired-special")
;;                       ((dired--ignored-name? name)
;;                        "dired-ignored")
;;                       (else #f))))
;;                (when face-name
;;                  (put-text-property buf-start buf-end 'face
;;                                     (face-id-from-name face-name))))))))
;;      lines)))

;;; Internal: point / file queries

(define (dired--current-line-filename)
  "Return the bare filename on the current line, or #f."
  (let* ((bol  (line-beginning-position))
         (eol  (line-end-position))
         (line (buffer-substring bol eol)))
    (and (>= (string-length line) 10)
         (let ((col (dired--skip-fields line 8)))
           (and col (substring line col))))))

(define (dired--current-file-full-path)
  "Return the absolute path of the file on the current line, or #f."
  (let ((fname (dired--current-line-filename))
        (dir   (buffer-local-value 'dired-directory)))
    (and fname dir
         (let* ((arrow  (string-contains fname " -> "))
                (actual (string-trim-right
                         (if arrow
                             (substring fname 0 arrow)
                             fname))))
           (string-append dir actual)))))

;;; Internal: move point to first real entry (skip . and ..)

(define (dired--goto-first-entry)
  "Move point to the filename column of the first non-dot entry."
  (goto-char 0)
  ;; Skip header line
  (next-line 1)
  ;; Skip . and .. entries
  (let loop ()
    (let ((fname (dired--current-line-filename)))
      (when (and fname
                 (or (string=? (string-trim-right fname) ".")
                     (string=? (string-trim-right fname) "..")))
        (next-line 1)
        (loop))))
  ;; Move point to the filename column
  (let* ((bol  (line-beginning-position))
         (line (buffer-substring bol (line-end-position)))
         (col  (dired--skip-fields line 8)))
    (when col
      (goto-char (+ bol col))
      (set-goal-column))))

;;; Internal: mark character helpers

(define (dired--set-mark-char ch face-name)
  "Replace the first character of the current line with CH."
  (let ((bol (line-beginning-position)))
    (goto-char bol)
    (delete-char 1)
    (let ((mark-start (point)))
      (insert (string ch))
      (when face-name
        (put-text-property mark-start (point) 'face
                           (face-id-from-name face-name))))))

;;; Internal: target file list

(define (dired--get-target-files)
  "Return marked files, or a singleton list of the file at point."
  (if (not (null? dired-marked-files))
      dired-marked-files
      (let ((f (dired--current-file-full-path)))
        (if f (list f) '()))))

;;; Navigation commands

(define (dired-next-line n)
  "Move N lines forward in the Dired listing."
  (next-line (if (number? n) n 1)))

(define (dired-previous-line n)
  "Move N lines backward in the Dired listing."
  (previous-line (if (number? n) n 1)))

(define (dired-up-directory)
  "Navigate to the parent directory."
  (let* ((dir     (buffer-local-value 'dired-directory))
         (trimmed (string-trim-right dir (lambda (c) (char=? c #\/))))
         (parent  (file-name-directory trimmed)))
    (if parent
        (dired parent)
        (message "Already at root"))))

;;; Visit commands

(define (dired-find-file)
  "Visit the file or directory on the current line."
  (let ((path (dired--current-file-full-path)))
    (if path
        (find-file path)
        (message "No file on this line"))))

(define (dired-find-file-other-window)
  "Visit the file on the current line in the other window."
  (let ((path (dired--current-file-full-path)))
    (if (not path)
        (message "No file on this line")
        (begin
          (split-window-sensibly)
          (other-window 1)
          (find-file path)))))


(define (dired-jump)
  "Jump to Dired buffer corresponding to current buffer.
If in a buffer visiting a file, Dired that file's directory and
move to that file's line in the directory listing.

If the current buffer isn't visiting a file, Dired `default-directory'.

If in Dired already, pop up a level and goto old directory's line.
In case the proper Dired file line cannot be found, refresh the Dired
buffer and try again.

When OTHER-WINDOW is non-nil, jump to Dired buffer in other window.

When FILE-NAME is non-nil, jump to its line in Dired.
Interactively with prefix argument, read FILE-NAME."
  (find-file default-directory))


;;; Mark / unmark commands

(define (dired-mark)
  "Mark the file on the current line with `*'."
  (let ((path (dired--current-file-full-path)))
    (when path
      (setq dired-marked-files (cons path dired-marked-files))
      (dired--set-mark-char #\* "dired-mark")
      (dired-next-line 1))))

(define (dired-unmark)
  "Unmark the file on the current line."
  (let ((path (dired--current-file-full-path)))
    (when path
      (setq dired-marked-files
            (filter (lambda (f) (not (string=? f path)))
                    dired-marked-files))
      (dired--set-mark-char #\space #f)
      (dired-next-line 1))))

(define (dired-unmark-backward)
  "Move up one line and unmark it."
  (dired-previous-line 1)
  (dired-unmark)
  (dired-previous-line 1))

(define (dired-flag-file-deletion)
  "Flag the file on the current line for deletion with `D'."
  (let ((path (dired--current-file-full-path)))
    (when path
      (dired--set-mark-char #\D "dired-flagged")
      (dired-next-line 1))))

(define (dired-unmark-all-marks)
  "Remove all marks by reverting the buffer."
  (dired-revert))

(define (dired-toggle-marks)
  "Toggle marks: `*' becomes unmarked and unmarked becomes `*'."
  (beginning-of-buffer)
  (while (< (point) (- (buffer-size) 1))
    (let* ((bol (line-beginning-position))
           (ch  (if (> (buffer-size) bol)
                    (buffer-substring bol (+ bol 1))
                    " ")))
      (cond
       ((string=? ch "*")
        (dired--set-mark-char #\space #f))
       ((string=? ch " ")
        (when (dired--current-file-full-path)
          (dired--set-mark-char #\* "dired-mark")))))
    (next-line 1)))

;;; File operation commands

(define (dired-do-delete)
  "Delete marked files, or the file at point if none are marked."
  (let ((files (dired--get-target-files)))
    (if (null? files)
        (message "No files to delete")
        (begin
          (for-each (lambda (f)
                      (system (string-append "rm -rf -- \"" f "\""))
                      (message (string-append "Deleted: " f)))
                    files)
          (setq dired-marked-files '())
          (dired-revert)))))

(define (dired-do-copy)
  "Copy marked files (or file at point) to a destination."
  (let* ((files (dired--get-target-files))
         (dest  (read-file-name "Copy to: ")))
    (when (and (not (null? files)) dest (not (string=? dest "")))
      (for-each (lambda (f)
                  (let ((cmd (string-append
                              "cp " (if dired-copy-preserve-time "-rp" "-r")
                              " -- \"" f "\" \"" dest "\"")))
                    (system cmd)
                    (message (string-append "Copied " f " -> " dest))))
                files)
      (dired-revert))))

(define (dired-do-rename)
  "Rename/move marked files (or file at point) to a destination."
  (let* ((files (dired--get-target-files))
         (dest  (read-file-name "Rename to: ")))
    (when (and (not (null? files)) dest (not (string=? dest "")))
      (for-each (lambda (f)
                  (system (string-append "mv -- \"" f "\" \"" dest "\""))
                  (message (string-append "Renamed " f " -> " dest)))
                files)
      (setq dired-marked-files '())
      (dired-revert))))

(define (dired-do-chmod)
  "Change permissions of marked files (or file at point)."
  (let* ((files (dired--get-target-files))
         (mode  (read-from-minibuffer "Change mode (e.g. 755): ")))
    (when (and (not (null? files)) mode (not (string=? mode "")))
      (for-each (lambda (f)
                  (system (string-append "chmod " mode " -- \"" f "\"")))
                files)
      (dired-revert))))

(define (dired-do-shell-command)
  "Run a shell command on marked files (or file at point)."
  (let* ((files (dired--get-target-files))
         (cmd   (read-from-minibuffer "Shell command: ")))
    (when (and (not (null? files)) cmd (not (string=? cmd "")))
      (let ((full-cmd
             (string-append cmd " "
                            (string-join
                             (map (lambda (f) (string-append "\"" f "\""))
                                  files)
                             " "))))
        (system full-cmd)
        (message (string-append "Done: " cmd))))))

(define (dired-do-compress)
  "Compress or decompress marked files (or file at point)."
  (let ((files (dired--get-target-files)))
    (for-each
     (lambda (f)
       (cond
        ((or (string-suffix? ".tar.gz" f)
             (string-suffix? ".tgz"    f)) (system (string-append "tar xzf \"" f "\"")))
        ((string-suffix? ".tar.bz2" f)     (system (string-append "tar xjf \"" f "\"")))
        ((string-suffix? ".tar.xz"  f)     (system (string-append "tar xJf \"" f "\"")))
        ((string-suffix? ".gz"      f)     (system (string-append "gunzip \""  f "\"")))
        ((string-suffix? ".zip"     f)     (system (string-append "unzip \""   f "\"")))
        (else                              (system (string-append "gzip \""    f "\"")))))
     files)
    (dired-revert)))

(define (dired-create-directory)
  "Create a new directory inside the current Dired directory."
  (let* ((dir  (buffer-local-value 'dired-directory))
         (name (read-from-minibuffer "Create directory: ")))
    (when (and name (not (string=? name "")))
      (system (string-append
               "mkdir -p -- \""
               (if (string-prefix? "/" name) name (string-append dir name))
               "\""))
      (dired-revert))))

;;; View / display commands

(define (dired-revert)
  "Revert the current Dired buffer, discarding all marks."
  (let ((dir (buffer-local-value 'dired-directory)))
    (when dir
      (setq dired-marked-files '())
      (when (> (buffer-size) 0)
        (goto-char 0)
        (delete-region))
      (dired--insert-listing dir dired-listing-switches)
      (dired--goto-first-entry))))

(define (dired-sort-toggle)
  "Toggle between sorting by name and sorting by date (-t)."
  (if (string-contains dired-listing-switches "t")
      (setq dired-listing-switches
            (list->string
             (filter (lambda (c) (not (char=? c #\t)))
                     (string->list dired-listing-switches))))
      (setq dired-listing-switches
            (string-append dired-listing-switches "t")))
  (dired-revert))

(define (dired-hide-details-toggle)
  "Toggle display of permission / owner / size columns."
  (setq dired-hide-details-mode (not dired-hide-details-mode))
  (message (if dired-hide-details-mode "Details hidden" "Details shown")))

(define (dired-summary)
  "Show a one-line summary of the file at point in the echo area."
  (let ((path (dired--current-file-full-path)))
    (if (not path)
        (message "No file on this line")
        (let* ((port   (open-input-pipe
                        (string-append "ls -lah -- \"" path "\"")))
               (result (read-line port)))
          (close-pipe port)
          (message (if (eof-object? result) "No info available" result))))))

(define (dired-goto-file filename)
  "Move point to FILENAME in the listing. Returns #t if found."
  (goto-char 0)
  (let loop ()
    (let ((path (dired--current-file-full-path)))
      (cond
       ((and path (string=? path filename)) #t)
       ((>= (point) (- (buffer-size) 1))   #f)
       (else (next-line 1) (loop))))))

;;; Window helper

(define (quit-window)
  "Quit the current window: delete it if others exist, else bury buffer."
  (if (> (length (window-list)) 1)
      (delete-window)
      (switch-to-buffer (other-buffer))))

;;; Keymap

(define dired-mode-map (make-sparse-keymap))

(define-key dired-mode-map "n"      dired-next-line)
(define-key dired-mode-map "p"      dired-previous-line)
(define-key dired-mode-map "<down>" dired-next-line)
(define-key dired-mode-map "<up>"   dired-previous-line)
(define-key dired-mode-map "^"      dired-up-directory)
(define-key dired-mode-map "RET"    dired-find-file)
(define-key dired-mode-map "f"      dired-find-file)
(define-key dired-mode-map "o"      dired-find-file-other-window)
(define-key dired-mode-map "m"      dired-mark)
(define-key dired-mode-map "u"      dired-unmark)
(define-key dired-mode-map "U"      dired-unmark-all-marks)
(define-key dired-mode-map "t"      dired-toggle-marks)
(define-key dired-mode-map "d"      dired-flag-file-deletion)
(define-key dired-mode-map "DEL"    dired-unmark-backward)
(define-key dired-mode-map "D"      dired-do-delete)
(define-key dired-mode-map "C"      dired-do-copy)
(define-key dired-mode-map "R"      dired-do-rename)
(define-key dired-mode-map "M"      dired-do-chmod)
(define-key dired-mode-map "Z"      dired-do-compress)
(define-key dired-mode-map "!"      dired-do-shell-command)
(define-key dired-mode-map "+"      dired-create-directory)
(define-key dired-mode-map "g"      dired-revert)
(define-key dired-mode-map "s"      dired-sort-toggle)
(define-key dired-mode-map "("      dired-hide-details-toggle)
(define-key dired-mode-map "?"      dired-summary)
(define-key dired-mode-map "q"      quit-window)

;; Global keymap
(keymap-global-set "C-x C-j" dired-jump)


;;; Major mode

(define-derived-mode dired-mode nil "Dired"
  "Major mode for directory listings produced by ls."
  (setq truncate-lines #t)
  (use-local-map dired-mode-map))

;;; Entry point

(define (dired dirname)
  "Open a Dired buffer for DIRNAME."
  (let* ((dir      (expand-file-name
                    (if (string-suffix? "/" dirname)
                        dirname
                        (string-append dirname "/"))))
         (buf-name (let* ((trimmed (string-trim-right dir (lambda (c) (char=? c #\/))))
                          (slash   (string-rindex trimmed #\/)))
                     (if slash
                         (substring trimmed (+ slash 1))
                         trimmed)))
         (existing (get-buffer buf-name)))
    (if (and existing (not dired-auto-revert-buffer))
        (switch-to-buffer existing)
        (let ((buf (get-buffer-create buf-name)))
          (switch-to-buffer buf)
          (dired-mode)
          (setq dired-directory   dir)
          (setq default-directory dir)
          (dired--insert-listing dir dired-listing-switches)
          (dired--goto-first-entry)
          (read-only-mode)
          buf))))
