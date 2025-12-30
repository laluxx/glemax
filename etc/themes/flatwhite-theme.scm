;;; flatwhite-theme.scm --- inspired by Atom's Flatwhite Syntax theme


;;; Commentary:



;;; Code:


(deftheme 'flatwhite "inspired by Atom's Flatwhite Syntax theme")

(custom-theme-set-faces 'flatwhite
  '(default             ((t (:foreground "#605a52" :background "#f7f3ee"))))
  '(mode-line           ((t (:foreground "#605a52" :background "#e4ddd2"))))
  '(mode-line-active    ((t (:inherit mode-line))))
  '(mode-line-inactive  ((t (:foreground "#93836c" :background "#dedad6"))))
  '(window-divider      ((t (                      :background "#c6bdb2"))))
  '(fringe              ((t (:foreground "#93836c" :background "#f7f3ee"))))
  '(cursor              ((t (:foreground "#f7f3ee" :background "#7382a0"))))
  '(visible-mark        ((t (:foreground "#f7f3ee" :background "#d8d4cd"))))
  '(error                                     ((t (:foreground "#955f5f"))))
  '(success                                   ((t (:foreground "#81895d"))))
  '(warning                                   ((t (:foreground "#957f5f"))))
  '(font-lock-bracket-face                    ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-builtin-face                    ((t (:foreground "#605a52"))))
  '(font-lock-comment-delimiter-face          ((t (:inherit font-lock-comment-face))))
  '(font-lock-comment-face                    ((t (:foreground "#b9a992"))))
  '(font-lock-constant-face                   ((t (:foreground "#465953" :background "#d2ebe3"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#9d8f7c"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#605a52"))))
  '(font-lock-keyword-face                    ((t (:foreground "#614c61" :background "#f1ddf1"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:inherit default))))
  '(font-lock-number-face                     ((t (:foreground "#957f5f"))))
  ;; '(font-lock-operator-face                   ((t (:foreground ""))))
  '(font-lock-preprocessor-face               ((t (:inherit default))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:inherit default))))
  '(font-lock-regexp-grouping-construct       ((t (:inherit default))))
  '(font-lock-string-face                     ((t (:foreground "#525643" :background "#e2e9c1"))))
  '(font-lock-type-face                       ((t (:inherit bold))))
  '(font-lock-variable-name-face              ((t (:foreground "#4c5361" :background "#dde4f2"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:foreground "#5b4343" :background "#f6cfcb"))))

  '(minibuffer-prompt                         ((t (:foreground "#7382a0"))))
  '(region                                    ((t (:background "#d8d4cd"))))
)
