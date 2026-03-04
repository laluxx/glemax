;;; kaolin-bubblegum-theme.scm --- Kaolin colorful theme with dark blue background


;;; Commentary:


;;; Code:

(deftheme 'kaolin-bubblegum "Kaolin colorful theme with dark blue background")

(custom-theme-set-faces 'kaolin-bubblegum
  '(default             ((t (:foreground "#D4D4D6" :background "#14171E"))))
  '(mode-line           ((t (:foreground "#bebec4" :background "#191D26" :box "#202430")))); TODO :line-width 2
  '(mode-line-active    ((t (:foreground "#bebec4" :background "#191D26" :box "#202430")))); TODO :line-width 2
  '(mode-line-inactive  ((t (:foreground "#454459" :background "#191D26" :box "#202430")))); TODO :line-width 2
  '(window-divider      ((t (                      :background "#202430"))))
  '(fringe              ((t (:foreground "#e6e6e8" :background "#14171e"))))
  '(cursor              ((t (:foreground "#14171E" :background "#D6A0D1"))))
  '(visible-mark        ((t (:foreground "#14171E" :background "#41b0f3"))))
  '(error                                     ((t (:foreground "#e55c7a"))))
  '(success                                   ((t (:foreground "#35BF88"))))
  '(warning                                   ((t (:foreground "#dbac66"))))
  '(font-lock-bracket-face                    ((t (:foreground "#11ccb2"))))
  '(font-lock-builtin-face                    ((t (:foreground "#63E8C1"))))
  '(font-lock-comment-delimiter-face          ((t (:foreground "#454459"))))
  '(font-lock-comment-face                    ((t (:foreground "#454459"))))
  '(font-lock-constant-face                   ((t (:foreground "#41b0f3"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#65a0a1"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#D6A0D1"))))
  '(font-lock-keyword-face                    ((t (:foreground "#9587DD"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#e55c7a"))))
  '(font-lock-number-face                     ((t (:foreground "#D4D4D6"))))
  '(font-lock-operator-face                   ((t (:foreground "#D6A0D1"))))
  '(font-lock-preprocessor-face               ((t (:foreground "#c79af4"))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#e361c3"))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#e361c3"))))
  '(font-lock-string-face                     ((t (:foreground "#62D2DB"))))
  '(font-lock-type-face                       ((t (:foreground "#11ccb2"))))
  '(font-lock-variable-name-face              ((t (:foreground "#41b0f3"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:foreground "#dbac66"))))

  '(minibuffer-prompt                         ((t (:foreground "#9587DD" :weight bold))))
  '(region                                    ((t (:foreground "#bebec4" :background "#272C3A"))))

  '(shadow                                    ((t (:foreground "#454459"))))
  '(highlight                                 ((t (:foreground "#e6e6e8" :background "#454459"))))
  '(help-key-binding                          ((t (:foreground "LightBlue" :background "grey19" :box "grey35"))))
  '(completions-highlight                     ((t (:inherit highlight))))
  '(completions-annotations                   ((t (:foreground "#65a0a1"))))
  '(completions-common-part                   ((t (:foreground "#D6A0D1"))))
  '(completions-first-difference              ((t (:inherit bold))))

  '(rainbow-delimiters-depth-1-face           ((t (:foreground "#11ccb2"))))
  '(rainbow-delimiters-depth-2-face           ((t (:foreground "#47ba99"))))
  '(rainbow-delimiters-depth-3-face           ((t (:foreground "#62D2DB"))))
  '(rainbow-delimiters-depth-4-face           ((t (:foreground "#9d81ba"))))
  '(rainbow-delimiters-depth-5-face           ((t (:foreground "#35BF88"))))
  '(rainbow-delimiters-depth-6-face           ((t (:foreground "#687184"))))
  '(rainbow-delimiters-depth-7-face           ((t (:foreground "#cd9575"))))
  '(rainbow-delimiters-depth-8-face           ((t (:foreground "#845A84"))))
  '(rainbow-delimiters-depth-9-face           ((t (:foreground "#47ba99"))))
  '(rainbow-delimiters-unmatched-face         ((t (:foreground "#e55c7a" :background "#832729"))))
  '(rainbow-delimiters-mismatched-face        ((t (:inherit rainbow-delimiters-unmatched-face))))

  '(escape-glyph                              ((t (:foreground "#6bd9db"))))

  '(isearch                                   ((t (:foreground "#f3c91f" :underline t :weight bold))))
  '(isearch-fail                              ((t (:foreground "#e55c7a"))))
  '(lazy-highlight                            ((t (:foreground "#f3c91f" :background "#272C3A"))))
)
