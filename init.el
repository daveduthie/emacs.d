;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

(load (expand-file-name "early-init.el" user-emacs-directory))

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory)))

(require 'dd-early-birds)
(require 'dd-long-tail)
(require 'dd-sanity)
(require 'dd-ui)
(require 'dd-editing)
(require 'dd-navigation)
(require 'dd-ide)
(require 'dd-org)
(require 'dd-tools)
(require 'dd-clojure)
(require 'dd-misc-lang)
(require 'dd-toggles)
(require 'dd-snippets)
(require 'dd-mac)
