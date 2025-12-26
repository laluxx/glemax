;;; tomorrow-night-theme.scm --- One of the dark variants of Tomorrow


;;; Commentary:



;;; Code:


(deftheme 'tomorrow-night "One of the dark variants of Tomorrow")

(custom-theme-set-faces 'tomorrow-night
  '(default             ((t (:foreground "#c5c8c6" :background "#1d1f21"))))
  '(mode-line           ((t (:foreground "#ffffff" :background "#0f1011"))))
  '(mode-line-active    ((t (:inherit mode-line))))
  '(mode-line-inactive  ((t (:foreground "#5a5b5a" :background "#1d1f21"))))
  '(window-divider      ((t (                      :background "#0d0d0d"))))
  '(fringe              ((t (:foreground "#3f4040" :background "#1d1f21"))))
  '(cursor              ((t (:foreground "#1d1f21" :background "#81a2be"))))
  '(visible-mark        ((t (:foreground "#1d1f21" :background "#b294bb"))))
  '(error                                     ((t (:foreground "#cc6666"))))
  '(success                                   ((t (:foreground "#b5bd68"))))
  '(warning                                   ((t (:foreground "#f0c674"))))
  '(font-lock-bracket-face                    ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-builtin-face                    ((t (:foreground "#81a2be"))))
  '(font-lock-comment-delimiter-face          ((t (:inherit font-lock-comment-face))))
  '(font-lock-comment-face                    ((t (:foreground "#5a5b5a"))))
  '(font-lock-constant-face                   ((t (:foreground "#de935f"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#717171"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#81a2be"))))
  '(font-lock-keyword-face                    ((t (:foreground "#b294bb"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#c5c8c6"))))
  '(font-lock-number-face                     ((t (:foreground "#de935f"))))
  ;; '(font-lock-operator-face                   ((t (:foreground ""))))
  '(font-lock-preprocessor-face               ((t (:foreground "#c5c8c6" :weight bold))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#c5c8c6" :weight bold))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#c5c8c6" :weight bold))))
  '(font-lock-string-face                     ((t (:foreground "#b5bd68"))))
  '(font-lock-type-face                       ((t (:foreground "#f0c674"))))
  '(font-lock-variable-name-face              ((t (:foreground "#cc6666"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:inherit warning))))

  '(minibuffer-prompt                         ((t (:foreground "#81a2be"))))
)
