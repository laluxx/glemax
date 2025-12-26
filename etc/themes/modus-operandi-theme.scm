;;; modus-operandi-theme.scm --- Elegant, highly legible theme with a white background


;;; Commentary:

;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  They are also highly customizable and can even
;; be used as the basis for other themes.  Please refer to the official
;; Info manual for further documentation (distributed with the themes,
;; or available at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:

(deftheme 'modus-operandi "Elegant, highly legible theme with a white background")


(custom-theme-set-faces 'modus-operandi
  '(default             ((t (:foreground "#000000" :background "#ffffff"))))
  '(mode-line           ((t (:foreground "#000000" :background "#c8c8c8"))))
  '(mode-line-active    ((t (:foreground "#000000" :background "#c8c8c8"))))
  '(mode-line-inactive  ((t (:foreground "#585858" :background "#e6e6e6"))))
  '(window-divider      ((t (                      :background "#9f9f9f"))))
  '(fringe              ((t (:foreground "#000000" :background "#f2f2f2"))))
  '(cursor              ((t (:foreground "#ffffff" :background "#000000"))))
  '(visible-mark        ((t (:foreground "#ffffff" :background "#bdbdbd"))))
  '(error                                     ((t (:foreground "#a60000"))))
  '(success                                   ((t (:foreground "#005f5f"))))
  '(warning                                   ((t (:foreground "#884900"))))
  '(font-lock-bracket-face                    ((t (:foreground "#000000"))))
  '(font-lock-builtin-face                    ((t (:foreground "#8f0075"))))
  '(font-lock-comment-delimiter-face          ((t (:foreground "#595959"))))
  '(font-lock-comment-face                    ((t (:foreground "#595959"))))
  '(font-lock-constant-face                   ((t (:foreground "#0000b0"))))
  '(font-lock-delimiter-face                  ((t (:foreground "#000000"))))
  '(font-lock-doc-face                        ((t (:foreground "#2a5045"))))
  '(font-lock-doc-markup-face                 ((t (:foreground "#7c318f"))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:foreground "#7b435c"))))
  '(font-lock-function-name-face              ((t (:foreground "#721045"))))
  '(font-lock-keyword-face                    ((t (:foreground "#531ab6"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#a60000"))))
  '(font-lock-number-face                     ((t (:foreground "#000000"))))
  '(font-lock-operator-face                   ((t (:foreground "#000000"))))
  '(font-lock-preprocessor-face               ((t (:foreground "#a0132f"))))
  '(font-lock-property-name-face              ((t (:foreground "#005e8b"))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  '(font-lock-punctuation-face                ((t (:foreground "#000000"))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#721045" :weight bold))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#00663f" :weight bold))))
  '(font-lock-string-face                     ((t (:foreground "#3548cf"))))
  '(font-lock-type-face                       ((t (:foreground "#005f5f" :weight bold))))
  '(font-lock-variable-name-face              ((t (:foreground "#005e8b"))))
  '(font-lock-variable-use-face               ((t (:foreground "#2f3f83"))))
  '(font-lock-warning-face                    ((t (:foreground "#884900" :weight bold)))

  '(minibuffer-prompt                         ((t (:foreground "#005f5f"))))
))
