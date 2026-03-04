;;; doom-nord-theme.scm --- dark variant of Nord


;;; Commentary:



;;; Code:

(deftheme 'nord "dark variant of Nord")

(custom-theme-set-faces 'nord
  '(default             ((t (:foreground "#ECEFF4" :background "#2E3440"))))
  '(mode-line           ((t (:foreground "#ECEFF4" :background "#292e39"))))
  '(mode-line-active    ((t (:foreground "#ECEFF4" :background "#292e39"))))
  '(mode-line-inactive  ((t (:foreground "#9099AB" :background "#292e39"))))
  '(window-divider      ((t (                      :background "#1c2028"))))
  '(fringe              ((t (:foreground "#8FBCBB" :background "#2E3440"))))
  '(cursor              ((t (:foreground "#2E3440" :background "#81A1C1"))))
  '(visible-mark        ((t (:foreground "#2E3440" :background "#8FBCBB"))))
  '(error                                     ((t (:foreground "#BF616A"))))
  '(success                                   ((t (:foreground "#A3BE8C"))))
  '(warning                                   ((t (:foreground "#EBCB8B"))))
  '(font-lock-bracket-face                    ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-builtin-face                    ((t (:foreground "#81A1C1"))))
  '(font-lock-comment-delimiter-face          ((t (:inherit font-lock-comment-face))))
  '(font-lock-comment-face                    ((t (:foreground "#6f7787"))))
  '(font-lock-constant-face                   ((t (:foreground "#81A1C1"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#78808f"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#88C0D0"))))
  '(font-lock-keyword-face                    ((t (:foreground "#81A1C1"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#81A1C1"))))
  '(font-lock-number-face                     ((t (:foreground "#B48EAD"))))
  ;; '(font-lock-operator-face                   ((t (:foreground ""))))
  '(font-lock-preprocessor-face               ((t (:foreground "#81A1C1" :weight bold))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#81A1C1" :weight bold))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#81A1C1" :weight bold))))
  '(font-lock-string-face                     ((t (:foreground "#A3BE8C"))))
  '(font-lock-type-face                       ((t (:foreground "#8FBCBB"))))
  '(font-lock-variable-name-face              ((t (:foreground "#D8DEE9"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:inherit warning))))

  '(minibuffer-prompt                         ((t (:foreground "#81A1C1"))))
  '(region                                    ((t (:background "#434C5E"))))

  '(shadow                                    ((t (:foreground "#4C566A"))))
  '(highlight                                 ((t (:foreground "#191C25" :background "#81A1C1"))))
  '(help-key-binding                          ((t (:foreground "LightBlue" :background "grey19" :box "grey35"))))
  '(completions-highlight                     ((t (:inherit highlight))))
  '(completions-annotations                   ((t (:inherit shadow :slant italic))))
  '(completions-common-part                   ((t (:foreground "LightBlue"))))
  '(completions-first-difference              ((t (:inherit bold))))

  '(rainbow-delimiters-depth-1-face           ((t (:foreground "#81A1C1"))))
  '(rainbow-delimiters-depth-2-face           ((t (:foreground "#B48EAD"))))
  '(rainbow-delimiters-depth-3-face           ((t (:foreground "#A3BE8C"))))
  '(rainbow-delimiters-depth-4-face           ((t (:foreground "#5D80AE"))))
  '(rainbow-delimiters-depth-5-face           ((t (:foreground "#8FBCBB"))))
  '(rainbow-delimiters-depth-6-face           ((t (:foreground "#81A1C1"))))
  '(rainbow-delimiters-depth-7-face           ((t (:foreground "#B48EAD"))))
  '(rainbow-delimiters-depth-8-face           ((t (:foreground "#A3BE8C"))))
  '(rainbow-delimiters-depth-9-face           ((t (:foreground "#5D80AE"))))
  '(rainbow-delimiters-unmatched-face         ((t (:foreground "#2E3440" :background "#BF616A" :weight bold))))
  '(rainbow-delimiters-mismatched-face        ((t (:inherit rainbow-delimiters-unmatched-face))))

  '(escape-glyph                              ((t (:foreground "#88C0D0"))))

  '(isearch                                   ((t (:inherit lazy-highlight :weight bold))))
  '(isearch-fail                              ((t (:foreground "#191C25" :background "#BF616A" :weight bold))))
  '(lazy-highlight                            ((t (:foreground "#F0F4FC" :background "#5a7087"))))
)
