;;; kaolin-temple-theme.scm --- The terrestrial sphere imbues my spirit


;;; Commentary:


;;; Code:

(deftheme 'kaolin-temple "The terrestrial sphere imbues my spirit")

(custom-theme-set-faces 'kaolin-temple
  '(default             ((t (:foreground "#EEDCC1" :background "#2B2B2F"))))
  '(mode-line           ((t (:foreground "#bebec4" :background "#303035"))))
  '(mode-line-active    ((t (:foreground "#bebec4" :background "#303035"))))
  '(mode-line-inactive  ((t (:foreground "#697375" :background "#303035"))))
  '(window-divider      ((t (                      :background "#353b3c"))))
  '(fringe              ((t (:foreground "#EEDCC1" :background "#2B2B2F"))))
  '(cursor              ((t (:foreground "#2B2B2F" :background "#EEDCC1"))))
  '(visible-mark        ((t (:foreground "#2B2B2F" :background ""))))
  '(error                                     ((t (:foreground "#e84c58"))))
  '(success                                   ((t (:foreground "#47ba99"))))
  '(warning                                   ((t (:foreground "#f3c91f"))))
  '(font-lock-bracket-face                    ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-builtin-face                    ((t (:foreground "#91b9c7"))))
  '(font-lock-comment-delimiter-face          ((t (:inherit font-lock-comment-face))))
  '(font-lock-comment-face                    ((t (:foreground "#697375"))))
  '(font-lock-constant-face                   ((t (:foreground "#4FA8A3"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#BA667D"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#91b9c7"))))
  '(font-lock-keyword-face                    ((t (:foreground "#4EB8CA"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#e84c58"))))
  '(font-lock-number-face                     ((t (:foreground "#EEDCC1"))))
  ;; '(font-lock-operator-face                   ((t (:foreground ""))))
  '(font-lock-preprocessor-face               ((t (:foreground "#ef6787"))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#ef6787"))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#ef6787"))))
  '(font-lock-string-face                     ((t (:foreground "#fbaed2"))))
  '(font-lock-type-face                       ((t (:foreground "#b9c791"))))
  '(font-lock-variable-name-face              ((t (:foreground "#47ba99"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:inherit warning))))
)
