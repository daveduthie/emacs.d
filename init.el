;;; init.el --- user-init-file                    -*- lexical-binding: t -*-

(eval-and-compile
  (add-to-list 'load-path (expand-file-name "lisp" (file-name-directory (or load-file-name user-emacs-directory)))))

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
(require 'dd-haskell)
(require 'dd-toggles)
(require 'dd-snippets)
(require 'dd-mac)
(require 'dd-denote)
(require 'dd-llms)
(require 'dd-ligature)

