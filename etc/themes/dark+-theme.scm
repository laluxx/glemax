;;; doom-dark+-theme.scm --- ported from equinusocio's VSCode Theme, dark+


;;; Commentary:



;;; Code:

(deftheme 'dark+ "ported from equinusocio's VSCode theme, dark+")

(custom-theme-set-faces 'dark+
  '(default             ((t (:foreground "#d4d4d4" :background "#1e1e1e"))))
  '(mode-line           ((t (:foreground "#f4f4f4" :background "#68217A"))))
  '(mode-line-active    ((t (:foreground "#f4f4f4" :background "#68217A"))))
  '(mode-line-inactive  ((t (:foreground "#339CDB" :background "#1d1d1d"))))
  '(window-divider      ((t (                      :background "#252526"))))
  '(fringe              ((t (:foreground "#4b474c" :background "#1e1e1e"))))
  '(cursor              ((t (:foreground "#1e1e1e" :background "#237AD3"))))
  '(visible-mark        ((t (:foreground "#1e1e1e" :background "#a9a9a9"))))
  '(error                                     ((t (:foreground "#D16969"))))
  '(success                                   ((t (:foreground "#579C4C"))))
  '(warning                                   ((t (:foreground "#D7BA7D"))))
  '(font-lock-bracket-face                    ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-builtin-face                    ((t (:foreground "#C586C0"))))
  '(font-lock-comment-delimiter-face          ((t (:inherit font-lock-comment-face))))
  '(font-lock-comment-face                    ((t (:foreground "#579C4C"))))
  '(font-lock-constant-face                   ((t (:foreground "#339CDB"))))
  '(font-lock-delimiter-face                  ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-doc-face                        ((t (:foreground "#777778"))))
  '(font-lock-doc-markup-face                 ((t (:inherit font-lock-constant-face))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:inherit font-lock-function-name-face))))
  '(font-lock-function-name-face              ((t (:foreground "#D9DAA2"))))
  '(font-lock-keyword-face                    ((t (:foreground "#339CDB"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#85DDFF"))))
  '(font-lock-number-face                     ((t (:foreground "#B5CEA8"))))
  ;; '(font-lock-operator-face                   ((t (:foreground ""))))
  '(font-lock-preprocessor-face               ((t (:foreground "#85DDFF"))))
  '(font-lock-property-name-face              ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  ;; '(font-lock-punctuation-face                ((t (:foreground ""))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#85DDFF"))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#85DDFF"))))
  '(font-lock-string-face                     ((t (:foreground "#DB8E73"))))
  '(font-lock-type-face                       ((t (:foreground "#35CDAF"))))
  '(font-lock-variable-name-face              ((t (:foreground "#85DDFF"))))
  '(font-lock-variable-use-face               ((t (:inherit font-lock-variable-name-face))))
  '(font-lock-warning-face                    ((t (:inherit warning))))

  '(minibuffer-prompt                         ((t (:foreground "#237AD3"))))
  '(region                                    ((t (:background "#113d69"))))
)
