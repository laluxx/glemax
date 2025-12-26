;;; molokai-theme.scm --- inspired by Tomas Restrepo's Molokai


;;; Commentary:



;;; Code:

(deftheme 'molokai "inspired by Tomas Restrepo's Molokai")

(custom-theme-set-faces 'molokai
  '(default             ((t (:foreground "#D6D6D4" :background "#1C1E1F"))))
  '(mode-line           ((t (:foreground "#d6d6d4" :background "#2d2e2e"))))
  '(mode-line-active    ((t (:foreground "#d6d6d4" :background "#2d2e2e"))))
  '(mode-line-inactive  ((t (:foreground "#4e4e4e" :background "#171819"))))
  '(window-divider      ((t (                      :background "#323435"))))
  '(fringe              ((t (:foreground "#4e4e4e" :background "#1c1e1f"))))
  '(cursor              ((t (:foreground "#1C1E1F" :background "#FB2874"))))
  '(visible-mark        ((t (:foreground "#1C1E1F" :background "#b6e63e"))))
  '(error                                     ((t (:foreground "#e74c3c"))))
  '(success                                   ((t (:foreground "#b6e63e"))))
  '(warning                                   ((t (:foreground "#e2c770"))))
  '(font-lock-bracket-face                    ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-builtin-face                    ((t (:foreground "#fd971f"))))
  '(font-lock-comment-delimiter-face          ((t (:inherit font-lock-comment-face))))
  '(font-lock-comment-face                    ((t (:foreground "#555556"))))
  '(font-lock-constant-face                   ((t (:foreground "#fd971f"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#7f7f80"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#b6e63e"))))
  '(font-lock-keyword-face                    ((t (:foreground "#fb2874"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#9c91e4"))))
  '(font-lock-number-face                     ((t (:foreground "#9c91e4"))))
  ;; '(font-lock-operator-face                   ((t (:foreground ""))))
  '(font-lock-preprocessor-face               ((t (:foreground "#9c91e4" :weight bold))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#9c91e4"))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#9c91e4"))))
  '(font-lock-string-face                     ((t (:foreground "#e2c770"))))
  '(font-lock-type-face                       ((t (:foreground "#66d9ef"))))
  '(font-lock-variable-name-face              ((t (:foreground "#fd971f"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:inherit warning))))

  '(minibuffer-prompt                         ((t (:foreground "#fd971f"))))
)
