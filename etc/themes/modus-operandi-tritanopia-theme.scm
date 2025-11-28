;;; modus-operandi-tritanopia-theme.scm --- Tritanopia-optimized theme with a white background


;;; Commentary:

;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  They are also highly customizable and can even
;; be used as the basis for other themes.  Please refer to the official
;; Info manual for further documentation (distributed with the themes,
;; or available at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:

(deftheme 'modus-operandi-tritanopia "Tritanopia-optimized theme with a white background")


(custom-theme-set-faces 'modus-operandi-tritanopia
  '(default             ((t (:foreground "#000000" :background "#ffffff"))))
  '(mode-line           ((t (:foreground "#0f0f0f" :background "#afe0f2"))))
  '(mode-line-active    ((t (:foreground "#0f0f0f" :background "#afe0f2"))))
  '(mode-line-inactive  ((t (:foreground "#585858" :background "#e6e6e6"))))
  '(window-divider      ((t (                      :background "#9f9f9f"))))
  '(fringe              ((t (:foreground "#000000" :background "#f2f2f2"))))
  '(cursor              ((t (:foreground "#ffffff" :background "#d00000"))))
  '(visible-mark        ((t (:foreground "#ffffff" :background "#bdbdbd"))))
  '(error                                     ((t (:foreground "#b21100"))))
  '(success                                   ((t (:foreground "#005e8b"))))
  '(warning                                   ((t (:foreground "#721045"))))
  '(font-lock-bracket-face                    ((t (:foreground "#000000"))))
  '(font-lock-builtin-face                    ((t (:foreground "#721045"))))
  '(font-lock-comment-delimiter-face          ((t (:foreground "#702000"))))
  '(font-lock-comment-face                    ((t (:foreground "#702000"))))
  '(font-lock-constant-face                   ((t (:foreground "#00663f"))))
  '(font-lock-delimiter-face                  ((t (:foreground "#000000"))))
  '(font-lock-doc-face                        ((t (:foreground "#224960"))))
  '(font-lock-doc-markup-face                 ((t (:foreground "#7c318f"))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:foreground "#4a3a8a"))))
  '(font-lock-function-name-face              ((t (:foreground "#3f578f"))))
  '(font-lock-keyword-face                    ((t (:foreground "#a0132f"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#b21100"))))
  '(font-lock-number-face                     ((t (:foreground "#000000"))))
  '(font-lock-operator-face                   ((t (:foreground "#000000"))))
  '(font-lock-preprocessor-face               ((t (:foreground "#b21100"))))
  '(font-lock-property-name-face              ((t (:foreground "#005f5f"))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  '(font-lock-punctuation-face                ((t (:foreground "#000000"))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#721045" :inherit bold))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#a60000" :inherit bold))))
  '(font-lock-string-face                     ((t (:foreground "#005e8b"))))
  '(font-lock-type-face                       ((t (:foreground "#3548cf" :inherit bold))))
  '(font-lock-variable-name-face              ((t (:foreground "#005f5f"))))
  '(font-lock-variable-use-face               ((t (:foreground "#104860"))))
  '(font-lock-warning-face                    ((t (:foreground "#721045" :inherit bold)))
))
