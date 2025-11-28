;;; monokai-ristretto-theme.scm --- Port of Monokai Ristretto


;;; Commentary:



;;; Code:

(deftheme 'monokai-ristretto "Port of Monokai Ristretto")

(custom-theme-set-faces 'monokai-ristretto
  '(default             ((t (:foreground "#fff1f3" :background "#2c2525"))))
  '(mode-line           ((t (:foreground "#fff1f3" :background "#403838"))))
  '(mode-line-active    ((t (:foreground "#fff1f3" :background "#403838"))))
  '(mode-line-inactive  ((t (:foreground "#fff1f3" :background "#2c2525"))))
  '(window-divider      ((t (                      :background "#413a3a"))))
  '(fringe              ((t (:foreground "#5b5353" :background "#2c2525"))))
  '(cursor              ((t (:foreground "#2c2525" :background "#fff1f3"))))
  '(visible-mark        ((t (:foreground "#2c2525" :background "#adda78"))))
  '(error                                     ((t (:foreground "#fd6883"))))
  '(success                                   ((t (:foreground "#adda78"))))
  '(warning                                   ((t (:foreground "#f38d70"))))
  '(font-lock-bracket-face                    ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-builtin-face                    ((t (:foreground "#a8a9eb"))))
  '(font-lock-comment-delimiter-face          ((t (:inherit font-lock-comment-face))))
  '(font-lock-comment-face                    ((t (:foreground "#72696a"))))
  '(font-lock-constant-face                   ((t (:foreground "#a8a9eb"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#958e8f"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#adda78"))))
  '(font-lock-keyword-face                    ((t (:foreground "#85dacc"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#fd6883"))))
  '(font-lock-number-face                     ((t (:foreground "#a8a9eb"))))
  ;; '(font-lock-operator-face                   ((t (:foreground ""))))
  '(font-lock-preprocessor-face               ((t (:foreground "#fd6883" :inherit bold))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#fd6883" :inherit bold))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#fd6883" :inherit bold))))
  '(font-lock-string-face                     ((t (:foreground "#f9cc6c"))))
  '(font-lock-type-face                       ((t (:foreground "#85dacc"))))
  '(font-lock-variable-name-face              ((t (:foreground "#fff1f3"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:inherit warning))))
)
