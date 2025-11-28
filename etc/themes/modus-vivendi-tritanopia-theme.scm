;;; modus-vivendi-tritanopia-theme.scm --- Tritanopia-optimized theme with a black background


;;; Commentary:

;; The Modus themes conform with the highest standard for
;; color-contrast accessibility between background and foreground
;; values (WCAG AAA).  They are also highly customizable and can even
;; be used as the basis for other themes.  Please refer to the official
;; Info manual for further documentation (distributed with the themes,
;; or available at: <https://protesilaos.com/emacs/modus-themes>).

;;; Code:

(deftheme 'modus-vivendi-tritanopia "Tritanopia-optimized theme with a black background")


(custom-theme-set-faces 'modus-vivendi-tritanopia
  '(default             ((t (:foreground "#ffffff" :background "#000000"))))
  '(mode-line           ((t (:foreground "#f0f0f0" :background "#5f8fb4"))))
  '(mode-line-active    ((t (:foreground "#f0f0f0" :background "#003c52"))))
  '(mode-line-inactive  ((t (:foreground "#969696" :background "#2d2d2d"))))
  '(window-divider      ((t (                      :background "#646464"))))
  '(fringe              ((t (:foreground "#ffffff" :background "#1e1e1e"))))
  '(cursor              ((t (:foreground "#ffffff" :background "#ff5f5f"))))
  '(visible-mark        ((t (:foreground "#ffffff" :background "#9099d9"))))
  '(error                                     ((t (:foreground "#ff6740"))))
  '(success                                   ((t (:foreground "#00d3d0"))))
  '(warning                                   ((t (:foreground "#feacd0"))))
  '(font-lock-bracket-face                    ((t (:foreground "#ffffff"))))
  '(font-lock-builtin-face                    ((t (:foreground "#feacd0"))))
  '(font-lock-comment-delimiter-face          ((t (:foreground "#ff9070"))))
  '(font-lock-comment-face                    ((t (:foreground "#ff9070"))))
  '(font-lock-constant-face                   ((t (:foreground "#88ca9f"))))
  '(font-lock-delimiter-face                  ((t (:foreground "#ffffff"))))
  '(font-lock-doc-face                        ((t (:foreground "#a0d7f2"))))
  '(font-lock-doc-markup-face                 ((t (:foreground "#caa6df"))))
  '(font-lock-escape-face                     ((t (:inherit font-lock-regexp-grouping-backslash))))
  '(font-lock-function-call-face              ((t (:foreground "#9099d9"))))
  '(font-lock-function-name-face              ((t (:foreground "#4ae2ff"))))
  '(font-lock-keyword-face                    ((t (:foreground "#ff7f86"))))
  '(font-lock-misc-punctuation-face           ((t (:inherit font-lock-punctuation-face))))
  '(font-lock-negation-char-face              ((t (:foreground "#ff6740"))))
  '(font-lock-number-face                     ((t (:foreground "#ffffff"))))
  '(font-lock-operator-face                   ((t (:foreground "#ffffff"))))
  '(font-lock-preprocessor-face               ((t (:foreground "#ff6740"))))
  '(font-lock-property-name-face              ((t (:foreground "#6ae4b9"))))
  '(font-lock-property-use-face               ((t (:inherit font-lock-property-name-face))))
  '(font-lock-punctuation-face                ((t (:foreground "#ffffff"))))
  '(font-lock-regexp-face                     ((t (:inherit font-lock-string-face))))
  '(font-lock-regexp-grouping-backslash       ((t (:foreground "#feacd0" :inherit bold))))
  '(font-lock-regexp-grouping-construct       ((t (:foreground "#ff5f59" :inherit bold))))
  '(font-lock-string-face                     ((t (:foreground "#00d3d0"))))
  '(font-lock-type-face                       ((t (:foreground "#79a8ff" :inherit bold))))
  '(font-lock-variable-name-face              ((t (:foreground "#6ae4b9"))))
  '(font-lock-variable-use-face               ((t (:foreground "#76afbf"))))
  '(font-lock-warning-face                    ((t (:foreground "#feacd0" :inherit bold)))
))
