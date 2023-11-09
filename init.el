;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(defvar config-libs
  '(dd-early-birds
    dd-long-tail
    dd-sanity
    dd-ui
    dd-editing
    dd-navigation
    dd-ide
    dd-org
    dd-tools
    dd-clojure
    dd-misc-lang))

(dolist (lib config-libs)
  (require lib))
