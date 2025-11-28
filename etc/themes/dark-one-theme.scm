;;; dak-one-theme.scm --- inspired by Atom One Dark


;;; Commentary:


;;; Code:

(deftheme 'dark-one "inspired by Atom One Dark")

(custom-theme-set-faces 'dark-one
  '(default             ((t (:foreground "#BBC2CF" :background "#282C34"))))
  '(mode-line           ((t (:foreground "#bbc2cf" :background "#1d2026"))))
  '(mode-line-active    ((t (:foreground "#bbc2cf" :background "#1d2026"))))
  '(mode-line-inactive  ((t (:foreground "#5B6268" :background "#21242b"))))
  '(window-divider      ((t (                      :background "#191b20"))))
  '(fringe              ((t (:foreground "#3f444a" :background "#282c34"))))
  '(cursor              ((t (:foreground "#282C34" :background "#51AFEF"))))
  '(visible-mark        ((t (:foreground "#282C34" :background "#c678dd"))))
  '(error                                     ((t (:foreground "#ff6c6b"))))
  '(success                                   ((t (:foreground "#98be65"))))
  '(warning                                   ((t (:foreground "#ECBE7B"))))
  '(font-lock-bracket-face                    ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-builtin-face                    ((t (:foreground "#c678dd"))))
  '(font-lock-comment-delimiter-face          ((t (:inherit font-lock-comment-face))))
  '(font-lock-comment-face                    ((t (:foreground "#5B6268"))))
  '(font-lock-constant-face                   ((t (:foreground "#a9a1e1"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#83898d"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#c678dd"))))
  '(font-lock-keyword-face                    ((t (:foreground "#51afef"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#51afef"))))
  '(font-lock-number-face                     ((t (:foreground "#da8548"))))
  ;; '(font-lock-operator-face                   ((t (:foreground ""))))
  '(font-lock-preprocessor-face               ((t (:foreground "#51afef"))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#51afef"))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#51afef"))))
  '(font-lock-string-face                     ((t (:foreground "#98be65"))))
  '(font-lock-type-face                       ((t (:foreground "#ECBE7B"))))
  '(font-lock-variable-name-face              ((t (:foreground "#dcaeea"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:inherit warning))))
)
