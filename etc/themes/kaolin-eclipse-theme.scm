;;; kaolin-eclipse-theme.scm --- Dark purple kaolin theme variant


;;; Commentary:


;;; Code:

(deftheme 'kaolin-eclipse "Dark purple kaolin theme variant")

(custom-theme-set-faces 'kaolin-eclipse
  '(default             ((t (:foreground "#F0EBE7" :background "#2B1D2B"))))
  '(mode-line           ((t (:foreground "#bebec4" :background "#332433" :box "#3D293D")))); TODO :line-width 2
  '(mode-line-active    ((t (:inherit mode-line))))
  '(mode-line-inactive  ((t (:foreground "#7A6884" :background "#332433" :box "#3D293D")))); TODO :line-width 2
  '(window-divider      ((t (                      :background "#2B1D2B"))))
  '(fringe              ((t (:foreground "#7A6884" :background "#2B1D2B"))))
  '(cursor              ((t (:foreground "#2B1D2B" :background "#9587DD"))))
  '(visible-mark        ((t (:foreground "#2B1D2B" :background "#C68EDE"))))
  '(error                                     ((t (:foreground "#e84c58"))))
  '(success                                   ((t (:foreground "#65E6A7"))))
  '(warning                                   ((t (:foreground "#f3c91f"))))
  '(font-lock-bracket-face                    ((t (:foreground "#a0586c"))))
  '(font-lock-builtin-face                    ((t (:foreground "#e361c3"))))
  '(font-lock-comment-delimiter-face          ((t (:foreground "#7A6884"))))
  '(font-lock-comment-face                    ((t (:foreground "#7A6884"))))
  '(font-lock-constant-face                   ((t (:foreground "#738FD7"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#807f96"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#fbaed2"))))
  '(font-lock-keyword-face                    ((t (:foreground "#C68EDE"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#e84c58"))))
  '(font-lock-number-face                     ((t (:foreground "#f5c791"))))
  '(font-lock-operator-face                   ((t (:foreground "#fbaed2"))))
  '(font-lock-preprocessor-face               ((t (:foreground "#41b0f3"))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground """#e361c3"))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#f5c791"))))
  '(font-lock-string-face                     ((t (:foreground "#6bd9db"))))
  '(font-lock-type-face                       ((t (:foreground "#6bd9db"))))
  '(font-lock-variable-name-face              ((t (:foreground "#9587DD"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:foreground "#f3c91f"))))

  '(minibuffer-prompt                         ((t (:foreground "#C68EDE" :weight bold))))
  '(region                                    ((t (:foreground "#93689A" :background "#251925" :weight bold :extend t))))

  '(shadow                                    ((t (:foreground "#7A6884"))))
  '(highlight                                 ((t (:foreground "#F0EBE7" :background "#7A6884"))))
  '(help-key-binding                          ((t (:foreground "#9587DD" :weight black))))
  '(completions-highlight                     ((t (:inherit highlight))))
  '(completions-annotations                   ((t (:foreground "#807f96"))))
  '(completions-common-part                   ((t (:foreground "#fbaed2"))))
  '(completions-first-difference              ((t (:inherit bold))))

  '(rainbow-delimiters-depth-1-face           ((t (:foreground "#a0586c"))))
  '(rainbow-delimiters-depth-2-face           ((t (:foreground "#9d81ba"))))
  '(rainbow-delimiters-depth-3-face           ((t (:foreground "#9587DD"))))
  '(rainbow-delimiters-depth-4-face           ((t (:foreground "#C68EDE"))))
  '(rainbow-delimiters-depth-5-face           ((t (:foreground "#80bcb6"))))
  '(rainbow-delimiters-depth-6-face           ((t (:foreground "#738FD7"))))
  '(rainbow-delimiters-depth-7-face           ((t (:foreground "#4d9391"))))
  '(rainbow-delimiters-depth-8-face           ((t (:foreground "#53859d"))))
  '(rainbow-delimiters-depth-9-face           ((t (:foreground "#cd9575"))))
  '(rainbow-delimiters-unmatched-face         ((t (:foreground "#e84c58" :background "#512634"))))
  '(rainbow-delimiters-mismatched-face        ((t (:inherit rainbow-delimiters-unmatched-face))))

  '(escape-glyph                              ((t (:foreground "#6bd9db"))))

  '(isearch                                   ((t (:foreground "#93689A" :background "#251925" :weight black))))
  '(isearch-fail                              ((t (:foreground "#e84c58"))))
  '(lazy-highlight                            ((t (:foreground "#7A6884" :underline "#4D3B4D"))))
)
