;;; kaolin-dark-theme.scm --- A dark jade theme inspired by Sierra.vim


;;; Commentary:


;;; Code:

(deftheme 'kaolin-dark "A dark jade theme inspired by Sierra.vim")

(custom-theme-set-faces 'kaolin-dark
  '(default                                   ((t (:foreground "#e4e4e8" :background "#18181B"))))
  '(mode-line                                 ((t (:foreground "#babac4" :background "#222225"))))
  '(mode-line-active                          ((t (:foreground "#babac4" :background "#222225"))))
  '(mode-line-inactive                        ((t (:foreground "#545c5e" :background "#222225"))))
  '(window-divider                            ((t (                      :background "#2B2B2F"))))
  '(fringe                                    ((t (:foreground "#e4e4e8" :background "#18181B"))))
  '(cursor                                    ((t (:foreground "#18181B" :background "#e4e4e8"))))
  '(visible-mark                              ((t (:foreground "#18181B" :background "#968cc7"))))
  '(error                                     ((t (:foreground "#cd5c60"))))
  '(success                                   ((t (:foreground "#6fb593"))))
  '(warning                                   ((t (:foreground "#dbac66"))))
  '(font-lock-bracket-face                    ((t (:foreground "#6bd9db"))))
  '(font-lock-builtin-face                    ((t (:foreground "#80bcb6"))))
  '(font-lock-comment-delimiter-face          ((t (:foreground "#545c5e"))))
  '(font-lock-comment-face                    ((t (:foreground "#545c5e"))))
  '(font-lock-constant-face                   ((t (:foreground "#ab98b5"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#5D8272"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#80bcb6"))))
  '(font-lock-keyword-face                    ((t (:foreground "#4d9391"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#cd5c60"))))
  '(font-lock-number-face                     ((t (:foreground "#e4e4e8"))))
  '(font-lock-operator-face                   ((t (:foreground "#80bcb6"))))
  '(font-lock-preprocessor-face               ((t (:foreground "#9d81ba"))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#cd5c60"))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#cd5c60"))))
  '(font-lock-string-face                     ((t (:foreground "#6fb593"))))
  '(font-lock-type-face                       ((t (:foreground "#cd9575"))))
  '(font-lock-variable-name-face              ((t (:foreground "#968cc7"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:foreground "#dbac66"))))
)
