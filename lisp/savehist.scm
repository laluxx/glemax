;;; savehist.scm --- Save minibuffer history

;; Author: Laluxx
;; Maintainer: Laluxx
;; Keywords: convenience, minibuffer

;;; Commentary:

;; Many editors (e.g. Vim) have the feature of saving minibuffer
;; history to an external file after exit.  This package provides the
;; same feature in Glemax.  When set up, it saves recorded minibuffer
;; histories to a file (`~/.emacs.d/history' by default).  Additional
;; variables may be specified by customizing
;; `savehist-additional-variables'.

;; To use savehist, turn on savehist-mode by putting in init.scm:
;;
;;     (savehist-mode 1)
;;
;; You can also explicitly save history with `M-x savehist-save' and
;; load it by loading the `savehist-file' with `M-x load-file'.

;;; Code:
