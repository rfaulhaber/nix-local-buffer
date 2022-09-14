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

(require 'subr-x)

;; TODO optionally, run nix search, display list of packages
;; TODO default to nixpkgs flake, but optionally allow for different flake

(defgroup nix-local-buffer nil
  "Buffer-local Nix environments."
  :group 'tools
  :prefix "nix-local-buffer-")

(defconst nix-local-buffer-process-buffer-name "*nix build*")
(defconst nix-local-buffer-process-error-buffer-name "*nix build error*")

(defcustom nix-local-buffer-directory-name
  "nix-local-buffer"
  "Directory name to pass to --out-link.
Set to nil to pass --no-link to nix build instead."
  :group 'nix-local-buffer
  :type '(directory))

(defcustom nix-local-buffer-default-flake
  "nixpkgs"
  "Default flake to preface all arguments from. Also the default prompt for `nix-local-buffer'."
  :group 'nix-local-buffer
  :type '(string))

(defun nix-local-buffer-get-output-links ()
  (seq-map (lambda (path)
             (concat path "/bin"))
           (seq-filter (lambda (item)
                         (not (string-empty-p item)))
                       (split-string
                        (with-current-buffer nix-local-buffer-process-buffer-name
                          (buffer-string)) "\n"))))

(defun nix-local-buffer-create-new-path (out-paths)
  (let* ((path (parse-colon-path (getenv "PATH"))))
    (concat "PATH=" (string-join (append path out-paths) ":"))))

(defun nix-local-buffer-build-sentinel (process event)
  (message "process: %s event %s" process event)
  (when (string-match-p "finished" event)
    (let* ((output-links (nix-local-buffer-get-output-links))
           (new-path (nix-local-buffer-create-new-path output-links)))
      (setq-local process-environment (append (seq-filter (lambda (item)
                                                            (not (string-match-p (rx bol "PATH=") item))) process-environment)
                                              (list new-path)))
      (setq-local exec-path (append (default-value 'exec-path) output-links))))
  (kill-buffer nix-local-buffer-process-buffer-name))

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
     :command `("nix" "build" "--print-out-paths" ,@out-path-flag ,@installable-list)
     :noquery t
     :sentinel #'nix-local-buffer-build-sentinel
     :stderr (get-buffer-create nix-local-buffer-process-error-buffer-name))))

(provide 'nix-local-buffer)
;;; nix-local-buffer.el ends here
