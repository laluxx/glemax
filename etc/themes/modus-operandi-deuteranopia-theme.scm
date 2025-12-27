;;; modus-operandi-deuteranopia-theme.scm --- Deuteranopia-optimized theme with a white background


;;; Commentary:

;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  They are also highly customizable and can even
;; be used as the basis for other themes.  Please refer to the official
;; Info manual for further documentation (distributed with the themes,
;; or available at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:

(deftheme 'modus-operandi-deuteranopia "Deuteranopia-optimized theme with a white background")


(custom-theme-set-faces 'modus-operandi-deuteranopia
  '(default             ((t (:foreground "#000000" :background "#ffffff"))))
  '(mode-line           ((t (:foreground "#0f0f0f" :background "#d0d6ff" :box "#4f4f74"))))
  '(mode-line-active    ((t (:foreground "#0f0f0f" :background "#d0d6ff" :box "#4f4f74"))))
  '(mode-line-inactive  ((t (:foreground "#585858" :background "#e6e6e6" :box "#a3a3a3"))))
  '(window-divider      ((t (                      :background "#9f9f9f"))))
  '(fringe              ((t (:foreground "#000000" :background "#f2f2f2"))))
  '(cursor              ((t (:foreground "#ffffff" :background "#0000ff"))))
  '(visible-mark        ((t (:foreground "#ffffff" :background "#bdbdbd"))))
  '(error                                     ((t (:foreground "#973300"))))
  '(success                                   ((t (:foreground "#0031a9"))))
  '(warning                                   ((t (:foreground "#695500"))))
  '(font-lock-bracket-face                    ((t (:foreground "#000000"))))
  '(font-lock-builtin-face                    ((t (:foreground "#695500"))))
  '(font-lock-comment-delimiter-face          ((t (:foreground "#77492f"))))
  '(font-lock-comment-face                    ((t (:foreground "#77492f"))))
  '(font-lock-constant-face                   ((t (:foreground "#003497"))))
  '(font-lock-delimiter-face                  ((t (:foreground "#000000"))))
  '(font-lock-doc-face                        ((t (:foreground "#2a5045"))))
  '(font-lock-doc-markup-face                 ((t (:foreground "#7c318f"))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:foreground "#70550f"))))
  '(font-lock-function-name-face              ((t (:foreground "#973300"))))
  '(font-lock-keyword-face                    ((t (:foreground "#0000b0"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#973300"))))
  '(font-lock-number-face                     ((t (:foreground "#000000"))))
  '(font-lock-operator-face                   ((t (:foreground "#000000"))))
  '(font-lock-preprocessor-face               ((t (:foreground "#531ab6"))))
  '(font-lock-property-name-face              ((t (:foreground "#005e8b"))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  '(font-lock-punctuation-face                ((t (:foreground "#000000"))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#0000b0" :weight bold))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#77492f" :weight bold))))
  '(font-lock-string-face                     ((t (:foreground "#3548cf"))))
  '(font-lock-type-face                       ((t (:foreground "#005f5f" :weight bold))))
  '(font-lock-variable-name-face              ((t (:foreground "#005e8b"))))
  '(font-lock-variable-use-face               ((t (:foreground "#4a3a8a"))))
  '(font-lock-warning-face                    ((t (:foreground "#695500" :weight bold)))

  '(minibuffer-prompt                         ((t (:foreground "#0031a9"))))
))
