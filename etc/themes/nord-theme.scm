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
)
