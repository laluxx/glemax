;;; kaolin-ocean-theme.scm --- Dark blue Kaolin theme


;;; Commentary:


;;; Code:

(deftheme 'kaolin-ocean "Dark blue Kaolin theme")

(custom-theme-set-faces 'kaolin-ocean
  '(default             ((t (:foreground "#e6e6e8" :background "#1a1a25"))))
  '(mode-line           ((t (:foreground "#bebec4" :background "#252534"))))
  '(mode-line-active    ((t (:foreground "#bebec4" :background "#252534"))))
  '(mode-line-inactive  ((t (:foreground "#545c5e" :background "#252534"))))
  '(window-divider      ((t (                      :background "#2f2f43"))))
  '(fringe              ((t (:foreground "#e6e6e8" :background "#1a1a25"))))
  '(cursor              ((t (:foreground "#1a1a25" :background "#F2F2F2"))))
  '(visible-mark        ((t (:foreground "#1a1a25" :background "#738FD7"))))
  '(error                                     ((t (:foreground "#e84c58"))))
  '(success                                   ((t (:foreground "#65E6A7"))))
  '(warning                                   ((t (:foreground "#dbac66"))))
  '(font-lock-bracket-face                    ((t (:foreground "#807f96"))))
  '(font-lock-builtin-face                    ((t (:foreground "#4ca6e8"))))
  '(font-lock-comment-delimiter-face          ((t (:foreground "#545c5e"))))
  '(font-lock-comment-face                    ((t (:foreground "#545c5e"))))
  '(font-lock-constant-face                   ((t (:foreground "#cea2ca"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#5D8272"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#6bd9db"))))
  '(font-lock-keyword-face                    ((t (:foreground "#738FD7"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#e84c58"))))
  '(font-lock-number-face                     ((t (:foreground "#eed891"))))
  '(font-lock-operator-face                   ((t (:foreground "#6bd9db"))))
  '(font-lock-preprocessor-face               ((t (:foreground "#9587DD"))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#eed891"))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#eed891"))))
  '(font-lock-string-face                     ((t (:foreground "#7CF083"))))
  '(font-lock-type-face                       ((t (:foreground "#d24b83"))))
  '(font-lock-variable-name-face              ((t (:foreground "#c79af4"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:foreground "#dbac66"))))
)
