;;; city-lights-theme.scm --- inspired by Atom's City Lights theme


;;; Commentary:



;;; Code:

(deftheme 'city-lights "inspired by Atom's City Lights theme")

(custom-theme-set-faces 'city-lights
  '(default             ((t (:foreground "#A0B3C5" :background "#1D252C"))))
  '(mode-line           ((t (:foreground "#A0B3C5" :background "#181f25"))))
  '(mode-line-active    ((t (:foreground "#A0B3C5" :background "#181f25"))))
  '(mode-line-inactive  ((t (:foreground "#56697A" :background "#1D252C"))))
  '(window-divider      ((t (                      :background "#0b0e11"))))
  '(fringe              ((t (:foreground "#384551" :background "#1D252C"))))
  '(cursor              ((t (:foreground "#1D252C" :background "#51AFEF"))))
  '(visible-mark        ((t (:foreground "#1D252C" :background "#E27E8D"))))
  '(error                                     ((t (:foreground "#D95468"))))
  '(success                                   ((t (:foreground "#8BD49C"))))
  '(warning                                   ((t (:foreground "#EBBF83"))))
  '(font-lock-bracket-face                    ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-builtin-face                    ((t (:foreground "#5EC4FF"))))
  '(font-lock-comment-delimiter-face          ((t (:inherit font-lock-comment-face))))
  '(font-lock-comment-face                    ((t (:foreground "#41505E"))))
  '(font-lock-constant-face                   ((t (:foreground "#E27E8D"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#707b86"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#33CED8"))))
  '(font-lock-keyword-face                    ((t (:foreground "#5EC4FF"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#5EC4FF"))))
  '(font-lock-number-face                     ((t (:foreground "#E27E8D"))))
  ;; '(font-lock-operator-face                   ((t (:foreground ""))))
  '(font-lock-preprocessor-face               ((t (:foreground "#5EC4FF"))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#5EC4FF"))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#5EC4FF"))))
  '(font-lock-string-face                     ((t (:foreground "#539AFC"))))
  '(font-lock-type-face                       ((t (:foreground "#EBBF83"))))
  '(font-lock-variable-name-face              ((t (:foreground "#718CA1"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:inherit warning))))
)
