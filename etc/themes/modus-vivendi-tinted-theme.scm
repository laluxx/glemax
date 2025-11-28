;;; modus-vivendi-tinted-theme.scm --- Elegant, highly legible theme with a night sky background


;;; Commentary:

;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  They are also highly customizable and can even
;; be used as the basis for other themes.  Please refer to the official
;; Info manual for further documentation (distributed with the themes,
;; or available at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:

(deftheme 'modus-vivendi-tinted "Elegant, highly legible theme with a night sky background")


(custom-theme-set-faces 'modus-vivendi-tinted
  '(default             ((t (:foreground "#ffffff" :background "#0d0e1c"))))
  '(mode-line           ((t (:foreground "#ffffff" :background "#484d67"))))
  '(mode-line-active    ((t (:foreground "#ffffff" :background "#484d67"))))
  '(mode-line-inactive  ((t (:foreground "#969696" :background "#292d48"))))
  '(window-divider      ((t (                      :background "#61647a"))))
  '(fringe              ((t (:foreground "#ffffff" :background "#1d2235"))))
  '(cursor              ((t (:foreground "#0d0e1c" :background "#ff66ff"))))
  '(visible-mark        ((t (:foreground "#0d0e1c" :background "#555a66"))))
  '(error                                     ((t (:foreground "#ff5f59"))))
  '(success                                   ((t (:foreground "#11c777"))))
  '(warning                                   ((t (:foreground "#d0bc00"))))
  '(font-lock-bracket-face                    ((t (:foreground "#ffffff"))))
  '(font-lock-builtin-face                    ((t (:foreground "#feacd0"))))
  '(font-lock-comment-delimiter-face          ((t (:foreground "#ef8386"))))
  '(font-lock-comment-face                    ((t (:foreground "#ef8386"))))
  '(font-lock-constant-face                   ((t (:foreground "#b6a0ff"))))
  '(font-lock-delimiter-face                  ((t (:foreground "#ffffff"))))
  '(font-lock-doc-face                        ((t (:foreground "#9ac8e0"))))
  '(font-lock-doc-markup-face                 ((t (:foreground "#caa6df"))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:foreground "#d09dc0"))))
  '(font-lock-function-name-face              ((t (:foreground "#f78fe7"))))
  '(font-lock-keyword-face                    ((t (:foreground "#79a8ff"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#ff5f59"))))
  '(font-lock-number-face                     ((t (:foreground "#ffffff"))))
  '(font-lock-operator-face                   ((t (:foreground "#ffffff"))))
  '(font-lock-preprocessor-face               ((t (:foreground "#ff7f86"))))
  '(font-lock-property-name-face              ((t (:foreground "#4ae2f0"))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  '(font-lock-punctuation-face                ((t (:foreground "#ffffff"))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#f78fe7" :inherit bold))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#b6a0ff" :inherit bold))))
  '(font-lock-string-face                     ((t (:foreground "#2fafff"))))
  '(font-lock-type-face                       ((t (:foreground "#11c777" :inherit bold))))
  '(font-lock-variable-name-face              ((t (:foreground "#4ae2f0"))))
  '(font-lock-variable-use-face               ((t (:foreground "#76afbf"))))
  '(font-lock-warning-face                    ((t (:foreground "#d0bc00" :inherit bold)))
))
