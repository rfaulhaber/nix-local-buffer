;;; nix-local-buffer.el --- Buffer-local Nix environment. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ryan Faulhaber
;;
;; Author: Ryan Faulhaber <ryf@sent.as>
;; Maintainer: Ryan Faulhaber <ryf@sent.as>
;; Created: September 20, 2022
;; Modified: September 20, 2022
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

(defun nix-local-buffer--get-output-links ()
  "Extracts output links from output buffer."
  (seq-map (lambda (path)
             (concat path "/bin"))
           (seq-filter (lambda (item)
                         (not (string-empty-p item)))
                       (split-string
                        (with-current-buffer nix-local-buffer-process-buffer-name
                          (buffer-string)) "\n"))))

(defun nix-local-buffer--create-new-path (out-paths)
  "Create a new PATH variable based on OUT-PATHS."
  (let* ((path (parse-colon-path (getenv "PATH"))))
    (concat "PATH=" (string-join (append path out-paths) ":"))))

(defun nix-local-buffer--build-sentinel (_process event)
  "Sentinel for nix build process."
  (when (string-match-p "finished" event)
    (let* ((output-links (nix-local-buffer--get-output-links))
           (new-path (nix-local-buffer--create-new-path output-links)))
      (message "output-links %s" output-links)
      (setq-local process-environment (append (seq-filter (lambda (item)
                                                            (not (string-match-p (rx bol "PATH=") item))) process-environment)
                                              (list new-path))
                  exec-path (append exec-path output-links))))
  (kill-buffer nix-local-buffer-process-buffer-name)
  (when (featurep 'quickrun)
    (quickrun--init-command-key-table))
  (message "Packages have been added to the environment!"))

;;;###autoload
(defun nix-local-buffer (flake pkgs)
  "Generate an ephemeral Nix environment from the specified FLAKE#PKGS.
This will modify the local buffer environment and make the executables within
the packages installed available. This tries to be a buffer equivalent of 'nix
shell'."
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
    (make-process
     :name "nix build"
     :buffer nix-local-buffer-process-buffer-name
     :command `("nix" "build" "--print-out-paths" ,@out-path-flag ,@installable-list)
     :noquery t
     :sentinel #'nix-local-buffer--build-sentinel
     :stderr (get-buffer-create nix-local-buffer-process-error-buffer-name))))

(provide 'nix-local-buffer)
;;; nix-local-buffer.el ends here
