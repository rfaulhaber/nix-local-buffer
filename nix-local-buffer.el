;;; nix-local-buffer.el --- Buffer-local Nix environment. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ryan Faulhaber
;;
;; Author: Ryan Faulhaber <ryf@sent.as>
;; Maintainer: Ryan Faulhaber <ryf@sent.as>
;; Created: August 08, 2022
;; Modified: August 08, 2022
;; Version: 0.0.1
;; Keywords: tools
;; Homepage: https://github.com/rfaulhaber/nix-local-buffer
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Buffer-local Nix environment.
;;
;;; Code:

;; TODO optionally, run nix search, display list of packages
;; TODO default to nixpkgs flake, but optionally allow for different flake

(defgroup nix-local-buffer nil
  "Buffer-local Nix environments."
  :group 'tools
  :prefix "nix-local-buffer-")

(defconst nix-local-buffer-process-buffer-name "*nix build*")

(defcustom nix-local-buffer-directory-name
  default-directory
  "Directory to place symbolic link for build result. Set to nil to pass the
--no-link flag to nix build."
  :group 'nix-local-buffer
  :type '(directory))

(defcustom nix-local-buffer-default-flake
  "nixpkgs"
  "Default flake to preface all arguments from. Also the default prompt for `nix-local-buffer'."
  :group 'nix-local-buffer
  :type '(string))

;;;###autoload
(defun nix-local-buffer (flake pkgs)
  (interactive
   (list
    (read-from-minibuffer "Flake? (Set to blank to manually specify) " nix-local-buffer-default-flake)
    (read-from-minibuffer "Packages? ")))
  (let* ((pkg-list (split-string pkgs (rx (one-or-more whitespace))))
         (flake-prefix (if (string-blank-p flake)
                           ""
                         (concat (string-trim flake) "#")))
         (installable-list (if (not (string-blank-p flake-prefix))
                               (mapcar (lambda (pkg)
                                         (concat flake-prefix pkg)) pkg-list)
                             pkg-list))
         (out-path-flag (if (null nix-local-buffer-directory-name)
                            '("--no-link")
                          `("--out-link" ,nix-local-buffer-directory-name))))
    (message "%s" installable-list)
    (make-process
     :name "nix build"
     :buffer nix-local-buffer-process-buffer-name
     :command `("nix" "build" "--print-out-paths" ,@out-path-flag)
     :noquery t
     ;; TODO make error buffer
     )
    ))

(provide 'nix-local-buffer)
;;; nix-local-buffer.el ends here
