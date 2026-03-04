;;; kaolin-ocean-theme.scm --- Dark blue Kaolin theme


;;; Commentary:


;;; Code:

(deftheme 'kaolin-ocean "Dark blue Kaolin theme")

(custom-theme-set-faces 'kaolin-ocean
  '(default             ((t (:foreground "#e6e6e8" :background "#1a1a25"))))
  '(mode-line           ((t (:foreground "#bebec4" :background "#252534" :box "#2f2f43")))); TODO :line-width 2
  '(mode-line-active    ((t (:foreground "#bebec4" :background "#252534" :box "#2f2f43")))); TODO :line-width 2
  '(mode-line-inactive  ((t (:foreground "#545c5e" :background "#252534" :box "#2f2f43")))); TODO :line-width 2
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
  '(font-lock-number-face                     ((t (:foreground "#e6e6e8"))))
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

  '(minibuffer-prompt                         ((t (:foreground "#738FD7" :weight bold))))
  '(region                                    ((t (:foreground "#bebec4" :background "#2E403B"))))

  '(shadow                                    ((t (:foreground "#545c5e"))))
  '(highlight                                 ((t (:foreground "#e6e6e8" :background "#545c5e"))))
  '(help-key-binding                          ((t (:foreground "LightBlue" :background "grey19" :box "grey35"))))
  '(completions-highlight                     ((t (:inherit highlight))))
  '(completions-annotations                   ((t (:foreground "#5D8272"))))
  '(completions-common-part                   ((t (:foreground "#6bd9db"))))
  '(completions-first-difference              ((t (:inherit bold))))

  '(rainbow-delimiters-depth-1-face           ((t (:foreground "#807f96"))))
  '(rainbow-delimiters-depth-2-face           ((t (:foreground "#9d81ba"))))
  '(rainbow-delimiters-depth-3-face           ((t (:foreground "#4d9391"))))
  '(rainbow-delimiters-depth-4-face           ((t (:foreground "#a0586c"))))
  '(rainbow-delimiters-depth-5-face           ((t (:foreground "#53859d"))))
  '(rainbow-delimiters-depth-6-face           ((t (:foreground "#5D8272"))))
  '(rainbow-delimiters-depth-7-face           ((t (:foreground "#cd9575"))))
  '(rainbow-delimiters-depth-8-face           ((t (:foreground "#91b9c7"))))
  '(rainbow-delimiters-depth-9-face           ((t (:foreground "#4ca6e8"))))
  '(rainbow-delimiters-unmatched-face         ((t (:foreground "#e84c58" :background "#832729"))))
  '(rainbow-delimiters-mismatched-face        ((t (:inherit rainbow-delimiters-unmatched-face))))

  '(escape-glyph                              ((t (:foreground "#6bd9db"))))

  '(isearch                                   ((t (:foreground "#0ed49b" :underline t :weight bold))))
  '(isearch-fail                              ((t (:foreground "#e84c58"))))
  '(lazy-highlight                            ((t (:foreground "#0ed49b" :background "#32324a"))))
)
