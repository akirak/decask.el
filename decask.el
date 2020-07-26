;;; decask.el --- Recipe helper -*- lexical-binding: t -*-

;; Copyright (C) 2020 Akira Komamura

;; Author: Akira Komamura <akira.komamura@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (dash "2.12") (package-build "0-git"))
;; Keywords: maint lisp
;; URL: https://github.com/akirak/decask.el

;; This file is not part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; decask.el provides a function for copying package recipes
;; from a local copy of MELPA to the local repository for running CI.

;;; Code:

(require 'package-build)
(require 'dash)
(require 'subr-x)

(defgroup decask nil
  "Recipe helper for Emacs packages."
  :group 'maint
  :group 'lisp)

(defcustom decask-recipes-dir
  (bound-and-true-p package-build-recipes-dir)
  "Root directory of your local melpa repository."
  :type 'directory)

(defcustom decask-discover-patterns
  '("*.el")
  "List of patterns used to discover source files."
  :type '(repeat string))

(defcustom decask-recipe-cache-directory
  ".recipes"
  "Directory (relative from the project) to contain recipes."
  :type 'directory)

(defun decask--project-root ()
  "Return the project root."
  (locate-dominating-file default-directory ".git"))

(defmacro decask--require-root (root)
  "Return the ROOT of the project."
  `(or ,root
       (decask--project-root)
       (cond
        (noninteractive default-directory)
        ((called-interactively-p 'any)
         (read-directory-name "Select the root of the project: "))
        (t
         (user-error "Cannot find root")))))

(defun decask--expand-file-specs (root specs)
  "Expand file specs in the project.

ROOT is the project, and SPECS is a spec to select files."
  (-map (pcase-lambda (`(,path . _))
          (expand-file-name path root))
        (ignore-errors
          (package-build-expand-file-specs root specs))))

(defun decask--expand-files-in-recipe (root recipe)
  "Expand file specs in a recipe for a project.

ROOT is the project, and RECIPE is a package recipe."
  (decask--expand-file-specs root
                             (or (plist-get (cdr recipe) :files)
                                 package-build-default-files-spec)))

(defun decask--discover-source-files (&optional root)
  "Find elisp source files in ROOT."
  (let ((root (decask--require-root root)))
    (decask--expand-file-specs root decask-discover-patterns)))

(defun decask--recipes (&optional root)
  "Find existing recipe files in ROOT."
  (let* ((root (decask--require-root root))
         (dir (expand-file-name decask-recipe-cache-directory root)))
    (->> (and (file-directory-p dir)
              (directory-files dir t))
         (-map
          (lambda (file)
            (ignore-errors
              (when (file-exists-p file)
                (with-temp-buffer
                  (insert-file-contents file)
                  (goto-char (point-min))
                  (read))))))
         (delq nil))))

(defun decask--main-file-p (file)
  "Return non-nil if FILE is a main file."
  (with-temp-buffer
    (insert-file-contents file)
    (lm-header "Package-Requires")))

;;;###autoload
(defun decask-discover-packages (&optional root)
  "Discover PACKAGES in ROOT."
  (interactive)
  (unless (and (stringp decask-recipes-dir)
               (file-directory-p decask-recipes-dir))
    (user-error "First set decask-recipes-dir to an existing directory containing recipes"))
  (let* ((root (decask--require-root root))
         (source-files (decask--discover-source-files root))
         (recipes (decask--recipes root))
         (covered-files (->> recipes
                             (-map (lambda (recipe)
                                     (decask--expand-files-in-recipe root recipe)))
                             (apply #'-concat)))
         (uncovered-files (-difference source-files covered-files))
         (new-main-files (-filter #'decask--main-file-p uncovered-files)))
    (dolist (main-file new-main-files)
      (let* ((package-name (file-name-base main-file))
             (dest-dir (expand-file-name decask-recipe-cache-directory root))
             (recipe-file (expand-file-name package-name decask-recipes-dir)))
        (ignore-errors
          (make-directory dest-dir t))
        (unless (file-exists-p recipe-file)
          (with-current-buffer (create-file-buffer recipe-file)
            (let* ((fetcher-spec (decask--fetcher-spec))
                   ;; TODO: Generate files spec
                   (recipe (read--expression
                            (format "Confirm recipe for \"%s\": " package-name)
                            (prin1-to-string `(,(intern package-name)
                                               ,@fetcher-spec)))))
              (princ recipe (current-buffer))
              (setq buffer-file-name recipe-file)
              (setq uncovered-files (cl-set-difference uncovered-files
                                                       (decask--expand-files-in-recipe
                                                        root
                                                        recipe)
                                                       :test #'string-equal))
              (emacs-lisp-mode)
              (save-buffer))))
        (message "Copying %s to %s" package-name dest-dir)
        (copy-file recipe-file (expand-file-name package-name dest-dir))))))

;;;###autoload
(defmacro decask-with-packages (root &rest progn)
  "Evaluate something after recipe generation.

This first discover packages in ROOT using `decask-discover-packages'
and then evaluate PROGN.

If the root is nil, it first looks for one."
  (declare (indent 1))
  `(let* ((root (decask--require-root ,root))
          (default-directory root))
     (decask-discover-packages root)
     ,@progn))

;;;###autoload
(defun decask-discover-packages-and-run (command &optional root)
  "Discover packages in the project and run a command.

COMMAND is a shell command to run, and ROOT is an optional root
of the project."
  (decask-with-packages root
    (compilation-start command t
                       (lambda (_mode-name) "*decask run*"))))

;;;;; Recipe generation helpers

;;;;;; Generate :repo/:url spec

(defcustom decask-user-fetcher 'github
  "Repository hosting service you use for your Emacs Lisp projects."
  :type '(choice (const :tag "github.com" github)
                 (const :tag "gitlab.com" gitlab)
                 (const :tag "Git url" git)))

(defcustom decask-user-name nil
  "Your login name on the repository service."
  :type '(choice null string))

(defcustom decask-use-https-url nil
  "Set origin to HTTPS url."
  :type 'boolean)

(defvar-local decask-fetcher-spec nil
  "Remote repository location set in the buffer.")

(defun decask--fetcher-spec ()
  "Retrieve or set a remote repository location for the recipe."
  (or decask-fetcher-spec
      (-some-> (decask--origin-url)
        (decask--url-to-fetcher-spec))
      (let ((spec (decask--read-fetcher-spec)))
        (if (yes-or-no-p "Set origin of this repository to the spec? ")
            (progn
              (decask--add-remote "origin"
                                  (decask--spec-to-url spec))
              spec)
          (setq decask-fetcher-spec spec)))))

(defun decask--read-fetcher-spec ()
  "Read a remote repository location as fetcher."
  (cl-ecase decask-user-fetcher
    ((github gitlab)
     (let ((fetcher decask-user-fetcher)
           (user decask-user-name))
       (list :fetcher fetcher
             :repo (read-string (format-message "Repository on %s: " fetcher)
                                (when user
                                  (concat user "/"
                                          (file-name-nondirectory
                                           (string-remove-suffix "/" default-directory))))))))
    (git
     (list :fetcher decask-user-fetcher
           :url (read-string "Remote Git URL of the repository: ")))))

(defun decask--break (str needle)
  "Break STR at NEEDLE and return a pair."
  (save-match-data
    (let ((pos (string-match (regexp-quote needle) str)))
      (when pos
        (cons (substring str 0 pos)
              (substring str (+ pos (length needle))))))))

(defun decask--origin-url ()
  "Return the URL of the origin of this repository."
  (-some--> (-find (lambda (s) (string-prefix-p "remote.origin.url=" s))
                   (process-lines "git" "config" "--local" "--list"))
    (decask--break it "=")
    (cdr it)))

(defun decask--url-to-fetcher-spec (git-url)
  "Build a repository location spec for the recipe from GIT-URL."
  (save-match-data
    (cond
     ((string-match (rx bol (or "git@github.com:" "https://github.com/")
                        (group (+? anything)) ".git" eol)
                    git-url)
      `(:fetcher github :repo ,(match-string 1 git-url)))
     ((string-match (rx bol (or "git@gitlab.com:" "https://gitlab.com/")
                        (group (+? anything)) ".git" eol)
                    git-url)
      `(:fetcher gitlab :repo ,(match-string 1 git-url)))
     ;; TODO: Add support for BitBucket and other forges
     (t
      `(:fetcher git :url ,git-url)))))

(defun decask--add-remote (name git-url)
  "Add a remote named NAME to GIT-URL to the repository."
  (let ((result (call-process "git" nil nil nil "remote" "add" name git-url)))
    (unless (= result 0)
      (error "Non-zero exit code while setting remote %s to %s" name git-url))))

(defun decask--spec-to-url (spec)
  "Convert a repository SPEC to URL."
  (cl-ecase (plist-get spec :fetcher)
    (git (plist-get spec :url))
    ((github gitlab) (format (if decask-use-https-url
                                 "https://%s/%s.git"
                               "git@%s:%s.git")
                             (cl-ecase (plist-get spec :fetcher)
                               (github "github.com")
                               (gitlab "gitlab.com"))
                             (plist-get spec :repo)))))

(provide 'decask)
;;; decask.el ends here
