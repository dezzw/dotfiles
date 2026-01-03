#!/usr/bin/env -S emacs -x
;; -*- lexical-binding: t -*-

;;; dotfiles.el --- Dotfiles management script using Emacs Lisp

;;; Commentary:
;; Pure Elisp dotfiles management script for Nix-based systems.
;; Replaces shell scripts with native Emacs Lisp implementations.

;;; Code:

;; ============================================================================
;; System Detection
;; ============================================================================

(defconst df-macos-p (eq system-type 'darwin)
  "Non-nil if running on macOS.")

(defconst df-linux-p (memq system-type '(gnu gnu/linux gnu/kfreebsd berkeley-unix))
  "Non-nil if running on Linux.")

;; ============================================================================
;; Utility Functions
;; ============================================================================

(defun df-home-dir ()
  "Return user's home directory path."
  (expand-file-name (concat (if df-macos-p "/Users/" "/home/")
                            (user-login-name))))

(defun df-profiles-dir ()
  "Return nix profiles directory based on platform."
  (expand-file-name ".local/state/nix/profiles/" (df-home-dir)))

(defun df-run-process (program &rest args)
  "Run PROGRAM with ARGS and display output in real-time.
Return non-nil on success, signal error on failure."
  (message "Running: %s %s" program (mapconcat #'identity args " "))
  (let ((exit-code (apply #'call-process program nil t nil args)))
    (unless (zerop exit-code)
      (error "Command failed with exit code %d: %s %s" 
             exit-code program (mapconcat #'identity args " ")))
    t))

(defun df-file-newer-p (file1 file2)
  "Return t if FILE1 is newer than FILE2."
  (time-less-p (file-attribute-modification-time (file-attributes file2))
               (file-attribute-modification-time (file-attributes file1))))

(defun df-get-symlinks (directory)
  "Return list of symbolic links in DIRECTORY (non-recursive)."
  (when (file-directory-p directory)
    (seq-filter #'file-symlink-p
                (directory-files directory t "^[^.]"))))

(defun df-symlink-mtime-alist (symlinks)
  "Return alist of (SYMLINK . MTIME) sorted by modification time (newest first)."
  (sort (mapcar (lambda (link)
                  (cons link (file-attribute-modification-time 
                             (file-attributes link))))
                symlinks)
        (lambda (a b) (time-less-p (cdr b) (cdr a)))))

(defun df-delete-old-profiles (profiles-dir keep-count)
  "Delete old profile symlinks in PROFILES-DIR, keeping KEEP-COUNT newest."
  (when (file-directory-p profiles-dir)
    (message "Cleaning profiles in: %s" profiles-dir)
    (let* ((symlinks (df-get-symlinks profiles-dir))
           (sorted (df-symlink-mtime-alist symlinks))
           (to-delete (nthcdr keep-count sorted)))
      (dolist (entry to-delete)
        (let ((link (car entry)))
          (condition-case err
              (progn
                (delete-file link)
                (message "Deleted: %s" link))
            (error (message "Failed to delete %s: %s" link (error-message-string err)))))))))

;; ============================================================================
;; Task Functions
;; ============================================================================

(defun df-task-update ()
  "Update nix flake."
  (df-run-process "nix" "flake" "update"))

(defun df-task-apply ()
  "Apply system configuration."
  (if df-macos-p
      (df-run-process "sudo" "darwin-rebuild" "switch" "--flake" ".#")
    (let ((process-environment (cons "USER=desmond" process-environment)))
      (df-run-process "home-manager" "switch" "--flake" ".#"))))

(defun df-task-clean ()
  "Clean nix profiles and garbage collect."
  ;; Clean old profile symlinks
  (df-delete-old-profiles (df-profiles-dir) 2)
  
  ;; Delete old generations
  (message "\nDeleting previous generations before the last 7:")
  (if df-macos-p
      (df-run-process "sudo" "nix-env" "--delete-generations" "+7" 
                      "--profile" "/nix/var/nix/profiles/system")
    (df-run-process "nix-env" "--delete-generations" "+7"))
  
  ;; Garbage collect
  (message "\nGarbage collecting older than 7d:")
  (if df-macos-p
      (df-run-process "sudo" "nix-collect-garbage" "--delete-older-than" "7d")
    (df-run-process "nix-collect-garbage" "--delete-older-than" "7d"))
  
  ;; Optimize store
  (message "\nHard linking duplicates:")
  (df-run-process "nix" "store" "optimise"))

(defun df-task-help ()
  "Display available tasks."
  (princ "\
Dotfiles Management Script

Available tasks:
  update  - Update nix flake
  apply   - Apply system configuration  
  clean   - Clean profiles and garbage collect
  help    - Show this help message

Usage:
  ./dotfiles.el <task>
  emacs --script dotfiles.el <task>

Examples:
  ./dotfiles.el update
  ./dotfiles.el apply
  ./dotfiles.el clean
"))

;; ============================================================================
;; Task Registry and Dispatcher
;; ============================================================================

(defconst df-tasks
  '(("update" df-task-update "Update nix flake")
    ("apply"  df-task-apply  "Apply system configuration")
    ("clean"  df-task-clean  "Clean profiles and garbage collect")
    ("help"   df-task-help   "Show help message"))
  "Available tasks: (NAME FUNCTION DESCRIPTION).")

(defun df-run-task (task-name)
  "Run task named TASK-NAME. Return nil if task not found."
  (let ((task (assoc task-name df-tasks)))
    (if task
        (progn
          (funcall (cadr task))
          t)
      nil)))

;; ============================================================================
;; Main Entry Point
;; ============================================================================

(defun df-main ()
  "Main entry point for the script."
  (let ((task-name (car command-line-args-left)))
    (cond
     ((null task-name)
      (df-task-help))
     ((string= task-name "help")
      (df-task-help))
     ((df-run-task task-name)
      (message "\n✓ Task '%s' completed successfully" task-name))
     (t
      (message "✗ Error: Unknown task '%s'\n" task-name)
      (df-task-help)
      (kill-emacs 1)))))

;; Run main when executed as script
(when noninteractive
  (condition-case err
      (df-main)
    (error
     (message "\n✗ Error: %s" (error-message-string err))
     (kill-emacs 1))))

(provide 'dotfiles)
;;; dotfiles.el ends here
