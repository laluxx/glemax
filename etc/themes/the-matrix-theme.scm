;;; the-matrix-theme.scm --- Green-on-black dark theme inspired by "The Matrix" movie


;;; Commentary:

;; "Unfortunately, no one can be told what The Matrix Theme is.
;; You'll have to see it for yourself." --Morpheus
;; I've made this almost monochrome green-on-black theme, because
;; it helps me focus. Syntax highlighting is implemented by different
;; font styles and a green base color which varies only in brightness
;; and luminosity, with additional clues in red and blue.

;;; Code:


(deftheme 'the-matrix "Green-on-black dark theme inspired by \"The Matrix\" movie")

(custom-theme-set-faces 'the-matrix
  '(default             ((t (:foreground "#00b25f" :background "#000000"))))
  '(mode-line           ((t (:foreground "#00733d" :background "#000000" :box "#00b25f"))))
  '(mode-line-active    ((t (:foreground "#00733d" :background "#000000" :box "#00b25f"))))
  '(mode-line-inactive  ((t (:foreground "#00733d" :background "#000000" :box "#004022"))))
  '(window-divider      ((t (                      :background "#000000"))))
  '(fringe              ((t (:foreground "#00b25f" :background "#01120a"))))
  '(cursor              ((t (:foreground "#000000" :background "#00e57a"))))
  '(visible-mark        ((t (:foreground "#000000" :background "#0081c7"))))
  '(error                                     ((t (:foreground "#cc0037"))))
  '(success                                   ((t (:foreground "#00cd6d"))))
  '(warning                                   ((t (:foreground "#0081c7"))))
  '(font-lock-bracket-face                    ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-builtin-face                    ((t (:foreground "#00b25f" :weight italic))))
  '(font-lock-comment-delimiter-face          ((t (:inherit font-lock-comment-face))))
  '(font-lock-comment-face                    ((t (:foreground "#00733d"))))
  '(font-lock-constant-face                   ((t (:inherit bold))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#00733d" :weight italic))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#00cd6d" :weight bold))));; TODO Extrabold
  '(font-lock-keyword-face                    ((t (:foreground "#00e57a" :weight bold))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  ;; '(font-lock-negation-char-face              ((t (:foreground ""))))
  '(font-lock-number-face                     ((t (:inherit default))))
  '(font-lock-operator-face                   ((t (:foreground "#00cd6d"))))
  '(font-lock-preprocessor-face               ((t (:inherit font-lock-builtin-face))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#00cd6d" :weight bold))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#00cd6d" :weight bold))))
  '(font-lock-string-face                     ((t (:foreground "#00b25f" :background "#011f11"))))
  '(font-lock-type-face                       ((t (:inherit bold))))
  '(font-lock-variable-name-face              ((t (:foreground "#00b25f" :weight bold))));; TODO Weight: black
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:foreground "#cc0037" :weight italic))))

  '(minibuffer-prompt                         ((t (:foreground "#00e57a" :weight bold))))
)


