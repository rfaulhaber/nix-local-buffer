;;; nix-local-buffer.el --- Buffer-local Nix environment. -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Ryan Faulhaber
;;
;; Author: Ryan Faulhaber <ryf@sent.as>
;; Maintainer: Ryan Faulhaber <ryf@sent.as>
;; Created: September 20, 2022
;; Modified: December 31, 2025
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
  :type 'string)

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
  "Create a new PATH variable based on OUT-PATHS.
OUT-PATHS are prepended to take precedence over existing paths."
  (let* ((path (parse-colon-path (getenv "PATH"))))
    (concat "PATH=" (string-join (append out-paths path) ":"))))

(defun nix-local-buffer--make-sentinel (target-buffer)
  "Create a sentinel that modifies TARGET-BUFFER's environment."
  (lambda (_process event)
    (cond
     ((string-match-p "finished" event)
      (let* ((output-links (nix-local-buffer--get-output-links))
             (new-path (nix-local-buffer--create-new-path output-links)))
        (when (buffer-live-p target-buffer)
          (with-current-buffer target-buffer
            (setq-local process-environment
                        (append (seq-filter (lambda (item)
                                              (not (string-match-p (rx bol "PATH=") item)))
                                            process-environment)
                                (list new-path))
                        exec-path (append output-links exec-path))
            (when (featurep 'quickrun)
              (quickrun--init-command-key-table)))))
      (when (get-buffer nix-local-buffer-process-buffer-name)
        (kill-buffer nix-local-buffer-process-buffer-name))
      (when (get-buffer nix-local-buffer-process-error-buffer-name)
        (kill-buffer nix-local-buffer-process-error-buffer-name))
      (message "nix-local-buffer: Packages added to environment!"))
     ((string-match-p "\\(exited\\|failed\\|signal\\)" event)
      (when (get-buffer nix-local-buffer-process-buffer-name)
        (kill-buffer nix-local-buffer-process-buffer-name))
      (let ((err-buffer (get-buffer nix-local-buffer-process-error-buffer-name)))
        (when err-buffer
          (pop-to-buffer err-buffer)))
      (message "nix-local-buffer: Build failed! See %s for details."
               nix-local-buffer-process-error-buffer-name)))))

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
     :sentinel (nix-local-buffer--make-sentinel (current-buffer))
     :stderr (get-buffer-create nix-local-buffer-process-error-buffer-name))))

(provide 'nix-local-buffer)
;;; nix-local-buffer.el ends here
