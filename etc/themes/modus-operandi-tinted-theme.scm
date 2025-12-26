;;; modus-operandi-tinted-theme.scm --- Elegant, highly legible theme with a light ochre background


;;; Commentary:

;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  They are also highly customizable and can even
;; be used as the basis for other themes.  Please refer to the official
;; Info manual for further documentation (distributed with the themes,
;; or available at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:

(deftheme 'modus-operandi-tinted "Elegant, highly legible theme with a light ochre background")


(custom-theme-set-faces 'modus-operandi-tinted
  '(default             ((t (:foreground "#000000" :background "#fbf7f0"))))
  '(mode-line           ((t (:foreground "#000000" :background "#cab9b2"))))
  '(mode-line-active    ((t (:foreground "#000000" :background "#cab9b2"))))
  '(mode-line-inactive  ((t (:foreground "#585858" :background "#dfd9cf"))))
  '(window-divider      ((t (                      :background "#9f9690"))))
  '(fringe              ((t (:foreground "#000000" :background "#efe9dd"))))
  '(cursor              ((t (:foreground "#fbf7f0" :background "#d00000"))))
  '(visible-mark        ((t (:foreground "#fbf7f0" :background "#c2bcb5"))))
  '(error                                     ((t (:foreground "#a60000"))))
  '(success                                   ((t (:foreground "#006300"))))
  '(warning                                   ((t (:foreground "#6d5000"))))
  '(font-lock-bracket-face                    ((t (:foreground "#000000"))))
  '(font-lock-builtin-face                    ((t (:foreground "#721045"))))
  '(font-lock-comment-delimiter-face          ((t (:foreground "#7f0000"))))
  '(font-lock-comment-face                    ((t (:foreground "#7f0000"))))
  '(font-lock-constant-face                   ((t (:foreground "#531ab6"))))
  '(font-lock-delimiter-face                  ((t (:foreground "#000000"))))
  '(font-lock-doc-face                        ((t (:foreground "#304463"))))
  '(font-lock-doc-markup-face                 ((t (:foreground "#7c318f"))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:foreground "#7b435c"))))
  '(font-lock-function-name-face              ((t (:foreground "#602938"))))
  '(font-lock-keyword-face                    ((t (:foreground "#0031a9"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#a60000"))))
  '(font-lock-number-face                     ((t (:foreground "#000000"))))
  '(font-lock-operator-face                   ((t (:foreground "#000000"))))
  '(font-lock-preprocessor-face               ((t (:foreground "#894000"))))
  '(font-lock-property-name-face              ((t (:foreground "#00603f"))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  '(font-lock-punctuation-face                ((t (:foreground "#000000"))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#8f0075" :weight bold))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#531ab6" :weight bold))))
  '(font-lock-string-face                     ((t (:foreground "#00598b"))))
  '(font-lock-type-face                       ((t (:foreground "#306010" :weight bold))))
  '(font-lock-variable-name-face              ((t (:foreground "#00603f"))))
  '(font-lock-variable-use-face               ((t (:foreground "#2a5045"))))
  '(font-lock-warning-face                    ((t (:foreground "#6d5000" :weight bold)))

  '(minibuffer-prompt                         ((t (:foreground "#00603f"))))
))
