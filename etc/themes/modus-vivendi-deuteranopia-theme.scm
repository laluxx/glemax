;;; modus-vivendi-deuteranopia-theme.scm --- Deuteranopia-optimized theme with a black background


;;; Commentary:

;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  They are also highly customizable and can even
;; be used as the basis for other themes.  Please refer to the official
;; Info manual for further documentation (distributed with the themes,
;; or available at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:

(deftheme 'modus-vivendi-deuteranopia "Deuteranopia-optimized theme with a black background")


(custom-theme-set-faces 'modus-vivendi-deuteranopia
  '(default             ((t (:foreground "#ffffff" :background "#000000"))))
  '(mode-line           ((t (:foreground "#f0f0f0" :background "#2a2a6a" :box "#8080a7"))))
  '(mode-line-active    ((t (:foreground "#f0f0f0" :background "#2a2a6a" :box "#8080a7"))))
  '(mode-line-inactive  ((t (:foreground "#969696" :background "#2d2d2d" :box "#606060"))))
  '(window-divider      ((t (                      :background "#646464"))))
  '(fringe              ((t (:foreground "#ffffff" :background "#1e1e1e"))))
  '(cursor              ((t (:foreground "#ffffff" :background "#efef00"))))
  '(visible-mark        ((t (:foreground "#ffffff" :background "#2fafff"))))
  '(error                                     ((t (:foreground "#ffa00f"))))
  '(success                                   ((t (:foreground "#2fafff"))))
  '(warning                                   ((t (:foreground "#cabf00"))))
  '(font-lock-bracket-face                    ((t (:foreground "#ffffff"))))
  '(font-lock-builtin-face                    ((t (:foreground "#cabf00"))))
  '(font-lock-comment-delimiter-face          ((t (:foreground "#d8af7a"))))
  '(font-lock-comment-face                    ((t (:foreground "#d8af7a"))))
  '(font-lock-constant-face                   ((t (:foreground "#82b0ec"))))
  '(font-lock-delimiter-face                  ((t (:foreground "#ffffff"))))
  '(font-lock-doc-face                        ((t (:foreground "#9ac8e0"))))
  '(font-lock-doc-markup-face                 ((t (:foreground "#caa6df"))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:foreground "#c0965b"))))
  '(font-lock-function-name-face              ((t (:foreground "#ffa00f"))))
  '(font-lock-keyword-face                    ((t (:foreground "#00bcff"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#ffa00f"))))
  '(font-lock-number-face                     ((t (:foreground "#ffffff"))))
  '(font-lock-operator-face                   ((t (:foreground "#ffffff"))))
  '(font-lock-preprocessor-face               ((t (:foreground "#b6a0ff"))))
  '(font-lock-property-name-face              ((t (:foreground "#00d3d0"))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  '(font-lock-punctuation-face                ((t (:foreground "#ffffff"))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#00bcff" :weight bold))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#d8af7a" :weight bold))))
  '(font-lock-string-face                     ((t (:foreground "#79a8ff"))))
  '(font-lock-type-face                       ((t (:foreground "#6ae4b9" :weight bold))))
  '(font-lock-variable-name-face              ((t (:foreground "#00d3d0"))))
  '(font-lock-variable-use-face               ((t (:foreground "#76afbf"))))
  '(font-lock-warning-face                    ((t (:foreground "#cabf00" :weight bold)))

  '(minibuffer-prompt                         ((t (:foreground "#2fafff"))))
  '(region                                    ((t (:foreground "#ffffff" :background "#5a5a5a"))))
))
