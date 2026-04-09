;;; kaolin-galaxy-theme.scm --- Bright theme based on one of the Sebastian Andaur arts.


;;; Commentary:


;;; Code:

(deftheme 'kaolin-galaxy "Bright theme based on one of the Sebastian Andaur arts.")

(custom-theme-set-faces 'kaolin-galaxy
  '(default             ((t (:foreground "#e6e6e8" :background "#212026"))))
  '(mode-line           ((t (:foreground "#bebec4" :background "#2a2931" :box "#31303A")))); TODO :line-width 2
  '(mode-line-active    ((t (:foreground "#bebec4" :background "#2a2931" :box "#31303A")))); TODO :line-width 2
  '(mode-line-inactive  ((t (:foreground "#615B75" :background "#2a2931" :box "#31303A")))); TODO :line-width 2
  '(window-divider      ((t (                      :background "#212026"))))
  '(fringe              ((t (:foreground "#e6e6e8" :background "#212026"))))
  '(cursor              ((t (:foreground "#e6e6e8" :background "#9587DD"))))
  '(visible-mark        ((t (:foreground "#e6e6e8" :background "#cea2ca"))))
  '(error                                     ((t (:foreground "#e84c58"))))
  '(success                                   ((t (:foreground "#6dd797"))))
  '(warning                                   ((t (:foreground "#f5c791"))))
  '(font-lock-bracket-face                    ((t (:foreground "#9d81ba"))))
  '(font-lock-builtin-face                    ((t (:foreground "#615B75"))))
  '(font-lock-comment-delimiter-face          ((t (:foreground "#615B75"))))
  '(font-lock-comment-face                    ((t (:foreground "#615B75"))))
  '(font-lock-constant-face                   ((t (:foreground "#f5c791"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#709688"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#cea2ca" :weight black))))
  '(font-lock-keyword-face                    ((t (:foreground "#9587DD" :weight black))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#e84c58"))))
  '(font-lock-number-face                     ((t (:foreground "#41b0f3"))))
  '(font-lock-operator-face                   ((t (:foreground "#cea2ca"))))
  '(font-lock-preprocessor-face               ((t (:foreground "#9587DD"))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground """#eed891"))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#41b0f3"))))
  '(font-lock-string-face                     ((t (:foreground "#65E6A7"))))
  '(font-lock-type-face                       ((t (:foreground "#ef6787" :weight bold))))
  '(font-lock-variable-name-face              ((t (:foreground "#cea2ca" :weight black))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:foreground "#f5c791"))))

  '(minibuffer-prompt                         ((t (:foreground "#9587DD" :weight bold))))
  '(region                                    ((t (:foreground "#bebec4" :background "#2B2638" :extend t))))

  '(shadow                                    ((t (:foreground "#615B75"))))
  '(highlight                                 ((t (:foreground "#212026" :background "#9587DD" :weight bold))))
  '(help-key-binding                          ((t (:foreground "#41b0f3" :weight black))))
  '(completions-highlight                     ((t (:inherit highlight))))
  '(completions-annotations                   ((t (:foreground "#709688"))))
  '(completions-common-part                   ((t (:foreground "#cea2ca"))))
  '(completions-first-difference              ((t (:inherit bold))))

  '(rainbow-delimiters-depth-1-face           ((t (:foreground "#9d81ba"))))
  '(rainbow-delimiters-depth-2-face           ((t (:foreground "#80bcb6"))))
  '(rainbow-delimiters-depth-3-face           ((t (:foreground "#c79af4"))))
  '(rainbow-delimiters-depth-4-face           ((t (:foreground "#807f96"))))
  '(rainbow-delimiters-depth-5-face           ((t (:foreground "#91b9c7"))))
  '(rainbow-delimiters-depth-6-face           ((t (:foreground "#687184"))))
  '(rainbow-delimiters-depth-7-face           ((t (:foreground "#c2b4a1"))))
  '(rainbow-delimiters-depth-8-face           ((t (:foreground "#845A84"))))
  '(rainbow-delimiters-depth-9-face           ((t (:foreground "#ef6787"))))
  '(rainbow-delimiters-unmatched-face         ((t (:foreground "#e84c58" :background "#832729"))))
  '(rainbow-delimiters-mismatched-face        ((t (:inherit rainbow-delimiters-unmatched-face))))

  '(escape-glyph                              ((t (:foreground "#9587DD"))))

  '(isearch                                   ((t (:foreground "#9587DD" :background "#2a2931" :weight bold))))
  '(isearch-fail                              ((t (:foreground "#e84c58"))))
  '(lazy-highlight                            ((t (:foreground "#e6e6e8" :background "#2a2931" :weight bold))))
)
