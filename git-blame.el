;;; git-blame.el --- Show and navigate git-blame data. -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;;
;; Author: Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;; URL: https://github.com/petergardfjall/emacs-git-blame
;; Keywords: workspace, project
;; Package-Requires: ((emacs "27.0"))
;; Version: 0.0.2
;; Homepage: https://github.com/petergardfjall/emacs-git-blame
;;
;;
;; This program is free software; you can redistribute it and/or modify
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
;;
;;
;;; Commentary:
;;
;; `git-blame' ... TODO
;;
;;; Code:
;; (require 'hierarchy)
;; (require 'project)
(require 'vc-git)

;; TODO customize faces
;; TODO customize mode map

(defvar git-blame--commit-margin-width 40
  "The width (in characters) that the commit info in the left margin will occupy.")

(defvar git-blame--git-cmd
  "Full system path to a git executable.")

(defun git-blame (&optional rev file lineno)
  (interactive
   (list "HEAD"                               ;; rev TODO: git rev-parse HEAD
         (buffer-file-name (current-buffer))  ;; file
         (line-number-at-pos)))               ;; lineno
  (unless (vc-git-root file)
    (error "File %s is not under git control" file))
  (message "opening git-blame buffer for revision: %s, file: %s, line: %d" rev file lineno)
  (let ((blame-data (git-blame--exec rev file)))
    (git-blame--render rev file blame-data)))

(defun git-blame--render (rev file blame-data)
  ""
  (let* ((repo-path (git-blame--repo-path file))
         (buffer-name (format "git-blame@%s:%s" rev repo-path))
         (curr-line (line-number-at-pos))
         (commit-table (plist-get blame-data :commit-table)))
    (xref-push-marker-stack) ;; Allow moving back by popping xref marker stack.
    (with-current-buffer (get-buffer-create buffer-name)
      ;; Fontify buffer by setting the right major mode for the file name.
      (let ((major-mode-fn (or (assoc-default (buffer-name) auto-mode-alist #'string-match) #'ignore)))
        (funcall major-mode-fn))

      (display-line-numbers-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq-local left-margin-width git-blame--commit-margin-width)
      ;; TODO render: iterate over lines:
      ;; - render "blame-data[lineno]" "<separator>" "<line-content>"
      ;;   - can be multiline
      ;;   - truncate at given width
      ;; - asssociate commit revision with each line
      (dolist (line (plist-get blame-data :lines))
        (let* ((content (plist-get line :content))
               (commit-rev  (plist-get line :commit))
               (commit (gethash commit-rev commit-table))
               (htime (plist-get commit :author-htime))
               (commit-message (plist-get commit :summary))
               (annotation-line
                (truncate-string-to-width
                 (format "%s (%s ago) %s"
                         (truncate-string-to-width commit-rev 7)
                         htime
                         commit-message)
                 git-blame--commit-margin-width 0 ?\s ".." nil))
               (content-line (propertize content 'face 'default)))
          (insert (propertize content-line 'face 'default))
          ;; Display commit data in margin:
          ;; see https://github.com/magit/magit/issues/1381
          (let ((o (make-overlay (line-beginning-position) (line-end-position) nil t)))
            (overlay-put o 'before-string
                         (propertize "o" 'display (list '(margin left-margin)
                                                        (propertize annotation-line 'face 'shadow)))))
          (newline)
          ;; TODO propertize content lines according to content type?
          ;; Can we use auto-mode-alist regexps to determine what mode to set for the new buffer?
          ;; (assoc-default name auto-mode-alist 'string-match)
          ))
      (goto-line curr-line)
      (setq buffer-read-only t)
      (toggle-truncate-lines 1) ;; Don't wrap lines.

      ;; TODO enable git-blame-mode-map for navigation
      (display-buffer-same-window (current-buffer) '()))))

(defun git-blame--exec (rev file)
  "TODO: return a hash table mapping line numbers to commit structs"
  (with-temp-buffer
    (message "repo-path: %s" (git-blame--repo-path file))
    (when-let* ((git-root (vc-git-root file))
                (repo-path (git-blame--repo-path file))
                (gitcmd (executable-find "git")))
                ;; (statuses (make-hash-table :test 'equal))
      (setq-local default-directory git-root)
      (let ((exit-code (call-process gitcmd nil (current-buffer) nil "blame" "--porcelain" rev "--" repo-path)))
        (when (> exit-code 0)
          (error "git-blame gave non-zero exit code: %d" exit-code)))
      (git-blame--parse (current-buffer)))))

(defun git-blame--repo-path (path)
  "Return a path relative to the repository root folder for file PATH."
  (when-let* ((git-root (vc-git-root path)))
    (file-relative-name path git-root)))

;; TODO change parsing to produce two things
;; - commit-table: hash map keyed on commit-rev -> plist (:author, :author-mail, :summary, etc)
;; - lines: array keyed on line number with each entry being a plist (:commit, :content)
;;
;; Use --line-porcelain
;; repeat until EOF
;; - lineindex++
;; - create next commit-object
;; - if not blame header: error
;; - if not commit already in commit-table:
;;   - populate commit-object properties until reaching content-line ('\t')
;; - commit-table[rev] = commit-object
;; - file-lines[lineindex] = line(:commit rev, :content: read-next-line())
(defun git-blame--parse (output-buffer)
  "Parses the output of a git blame call with --porcelain."
  (with-current-buffer output-buffer
    (goto-char (point-min))
    (let* ((file-lines '())
           (commit-table (make-hash-table :test 'equal)))
      (while (not (eobp))
        (when (not (git-blame--porcelain-header-p (git-blame--current-line)))
          (error "Failed to parse git blame output: expected porcelain header"))
        ;; Parse git blame header row for line.
        (let* ((tokens (split-string (git-blame--current-line)))
               (commit-rev       (nth 0 tokens))
               (commit-plist `(:rev ,commit-rev)))
          ;; Commit not encountered before, commit details will follow.
          (when (not (gethash commit-rev commit-table))
            (while (not (git-blame--content-line-p (git-blame--peek-next-line)))
              (let* ((line (git-blame--next-line))
                     (tokens (split-string line)))
                (pcase (nth 0 tokens)
                  ("author"         (setq commit-plist (plist-put commit-plist :author (nth 1 tokens))))
                  ("author-mail"    (setq commit-plist (plist-put commit-plist :author-mail (nth 1 tokens))))
                  ("author-time"
                   (setq commit-plist (plist-put commit-plist :author-time (nth 1 tokens)))
                   (setq commit-plist (plist-put commit-plist :author-htime (git-blame--humanize-time (git-blame--parse-time (nth 1 tokens))))))
                  ("author-tz"      (setq commit-plist (plist-put commit-plist :author-tz (nth 1 tokens))))
                  ("summary"        (setq commit-plist (plist-put commit-plist :summary (string-remove-prefix "summary " line)))))))
            (puthash commit-rev commit-plist commit-table))
          ;; The next (tab-prefixed) line holds the actuan content of the line in the file.
          ;; (setq content-line (git-blame--next-line))
          (let* ((content-line (git-blame--next-line))
                 (file-line `(:commit ,commit-rev :content ,(string-trim-left content-line "\t"))))
            (setq file-lines (append file-lines (list file-line)))))
        (git-blame--next-line))

      ;; Return the gathered content lines and commit table.
      `(:lines ,file-lines :commit-table ,commit-table))))


(defun git-blame--peek-next-line ()
  "Return the next line without moving cursor."
  (save-excursion (git-blame--next-line)))

(defun git-blame--next-line ()
  "TODO"
  (forward-line 1)
  (git-blame--current-line))

(defun git-blame--current-line ()
  "TODO"
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun git-blame--content-line-p (line)
  (string-prefix-p "\t" line))

(defun git-blame--porcelain-header-p (line)
  ;; <commit> <lineno-prior> <lineno-final> [num-group-lines]
  (string-match "[0-9a-f]\\{40\\} [0-9]+ [0-9]+\\( [0-9]+\\)?" line))


(defun git-blame--parse-time (epoch-time-string)
  "Convert a git blame timestamp like '1727681138' to a Lisp timestamp."
  (time-convert (string-to-number epoch-time-string)))


(defun git-blame--humanize-time (time)
  ;; TODO
  (let* ((time-with-unit
          (let* ((seconds-since (time-to-seconds (time-since time))))
            (if (> seconds-since (* 365 24 3600))
                `(,(floor (/ seconds-since (* 365 24 3600))) . "year")
              (if (> seconds-since (* 30 24 3600))
                  `(,(floor (/ seconds-since (* 30 24 3600))) . "month")
                (if (> seconds-since (* 7 24 3600))
                    `(,(floor (/ seconds-since (* 7 24 3600))) . "week")
                  (if (> seconds-since (* 24 3600))
                      `(,(floor (/ seconds-since (* 24 3600))) . "day")
                    (if (> seconds-since 3600)
                        `(,(floor (/ seconds-since 3600)) . "hour")
                      (if (> seconds-since 60)
                          `(,(floor (/ seconds-since 60)) . "minute")
                        `(,seconds-since . "second")))))))))
         (time (round (car time-with-unit)))
         (unit (cdr time-with-unit)))
    (if (> time 1)
        (format "%d %s" time (concat unit "s"))
      (format "%d %s" time unit))))

;; (defcustom projtree-window-width 30
;;   "The width in characters to use for the project tree buffer."
;;   :type 'natnum
;;   :group 'projtree)

;; (defcustom projtree-window-placement 'left
;;   "The placement of the project tree window."
;;   :type '(choice (const :tag "Left-hand side" left)
;;                  (const :tag "Right-hand side" right))
;;   :group 'projtree)

;; (defcustom projtree-buffer-name "*projtree*"
;;   "The name to use for the project tree buffer."
;;   :type 'string
;;   :group 'projtree)

;; (defcustom projtree-show-git-status t
;;   "Whether to show git status for project tree files and folders."
;;   :type 'boolean
;;   :group 'projtree)

;; (defcustom projtree-profiling-enabled t
;;   "Whether to output performance profiling."
;;   :type 'boolean
;;   :group 'projtree)


;; (defface projtree-highlight
;;   '((t :inherit highlight :extend t))
;;   "Face for highlighting the visited file in the project tree."
;;   :group 'projtree)

;; (defface projtree-dir
;;   '((t :inherit font-lock-function-name-face))
;;   "Face for highlighting unmodified folders in the project tree."
;;   :group 'projtree)

;; (defface projtree-file
;;   '((t :inherit default))
;;   "Face for highlighting unmodified files in the project tree."
;;   :group 'projtree)

;; (defface projtree-git-modified
;;   '((t :inherit diff-refine-changed))
;;   "Face for highlighting modified files and folders in the project tree."
;;   :group 'projtree)

;; (defface projtree-git-added
;;   '((t :inherit diff-refine-added))
;;   "Face for highlighting added files in the project tree."
;;   :group 'projtree)

;; (defface projtree-git-renamed
;;   '((t :inherit diff-removed))
;;   "Face for highlighting renamed files in the project tree."
;;   :group 'projtree)

;; (defface projtree-git-ignored
;;   '((t :inherit completions-group-separator))
;;   "Face for highlighting ignored files in the project tree."
;;   :group 'projtree)

;; (defface projtree-git-untracked
;;   '((t :inherit shadow))
;;   "Face for highlighting untracked files in the project tree."
;;   :group 'projtree)

;; (defface projtree-git-conflict
;;   '((t :inherit error))
;;   "Face for highlighting files with unresolved conflicts in the project tree."
;;   :group 'projtree)


;; (defvar projtree-buffer-map
;;   (let ((map (make-sparse-keymap)))
;;     ;; Refreshes the project tree buffer.
;;     (define-key map (kbd "g")    'projtree-open)
;;     (define-key map (kbd "d")    'projtree-dired)
;;     map)
;;   "Keybindings available in the project tree buffer.")


;; (cl-defstruct projtree-set
;;   "A collection of project trees (`projtree' instances).

;; The project trees in the set are keyed on project root path.

;; A global project tree set for the active Emacs session can be
;; retrieved via a call to `projtree-active-set'.  When in
;; `projtree-mode', opening a file in a project (identified by
;; project.el) will add a project tree for that project to the
;; active set."
;;   trees)

;; (defun projtree-set--new ()
;;   "Create an empty `projtree-set'."
;;   (make-projtree-set :trees (make-hash-table :test 'equal)))

;; (defun projtree-set->get-projtree (self project-root)
;;   "Get a project tree from projtree-set SELF with a given PROJECT-ROOT.
;; If the requested `projtree' does not already exist it is created."
;;   (let* ((trees (projtree-set-trees self))
;;          (tree (gethash project-root trees)))
;;     ;; Add a new project tree if one doesn't exist.
;;     (when (not tree)
;;       (puthash project-root (projtree->new project-root) trees))
;;     (gethash project-root trees)))


;; (defvar projtree--active-set nil
;;   "Holds the active `projtree-set' for the current Emacs session.

;; It tracks all visited project trees.  When in `projtree-mode',
;; opening a file in a project (identified by project.el) will add a
;; project tree for that project to the active set.")

;; (defun projtree-active-set ()
;;   "Return the global projtree-set for the current Emacs session."
;;   (when (not projtree--active-set)
;;     (setq projtree--active-set (projtree-set--new)))
;;   projtree--active-set)



;; (cl-defstruct projtree
;;   "Represents a file tree for a particular project.

;; A project tree is rooted at a directory, the project root as
;; determined by `project.el'.  The project root is typically a VCS
;; root directory, but non-VCS projects listed in
;; `project-list-file' are also treated as project roots.

;; `projtree->display' renders a project tree as a browsable file
;; explorer in a buffer using `hierarchy.el'.  The hierarchy can be
;; browsed and directories can be expanded by clicking their nodes.

;; To support moving around the tree and expanding nodes the project
;; tree needs to track the cursor position in the tree as well as
;; which directories and files are expanded in the hierarchy
;; explorer.

;; When in `projtree-mode', visiting a file under the project
;; results in that file being marked as selected in the project tree and
;; highlighed in the project tree buffer."

;;   ;; The root directory of the project tree.
;;   root
;;   ;; The current cursor position in the project tree buffer for this project
;;   ;; tree. This will be set whenever a file is opened or a folder is
;;   ;; opened/folded in the project tree buffer to avoid jumpy cursor behavior on
;;   ;; re-rendering of the tree.
;;   cursor
;;   ;; Tracks which paths of the project tree are expanded (to be displayed).
;;   expanded-paths
;;   ;; Tracks which path in the project tree is currently visited (and therefore
;;   ;; should be highlighted).
;;   selected-path)

;; (defun projtree->new (project-root)
;;   "Create an empty projtree rooted at directory PROJECT-ROOT."
;;   (make-projtree :root (projtree--abspath project-root)
;;                  :cursor nil
;;                  :expanded-paths (make-hash-table :test 'equal)
;;                  :selected-path nil))

;; (defun projtree->root (self)
;;   "Return the root directory of project tree SELF."
;;   (projtree-root self))

;; (defun projtree->cursor (self)
;;   "Return the explorer buffer cursor position for project tree SELF."
;;   (projtree-cursor self))

;; (defun projtree->set-cursor (self point)
;;   "Set the explorer buffer cursor position for project tree SELF to POINT."
;;   (setf (projtree-cursor self) point))

;; (defun projtree->selected-path (self)
;;   "Return currently visited file in project tree SELF."
;;   (projtree-selected-path self))

;; (defun projtree->set-selected-path (self path)
;;   "Set PATH as currently visited file in project tree SELF."
;;   ;; TODO validate that path is below root
;;   (let ((path (projtree--abspath path)))
;;     (setf (projtree-selected-path self) path)))

;; (defun projtree->expanded-p (self path)
;;   "Indicate if path PATH is expanded in project tree SELF."
;;   (let ((path (projtree--abspath path)))
;;     (gethash path (projtree-expanded-paths self))))

;; (defun projtree->expand-path (self path)
;;   "Expand only path PATH (no ancestor directories) in project tree SELF."
;;   (let ((path (projtree--abspath path)))
;;     (puthash path t (projtree-expanded-paths self))))

;; (defun projtree->expand-paths (self paths)
;;   "Expand a list of PATHS in project tree SELF."
;;   (dolist (p paths)
;;     (projtree->expand-path self p)))

;; (defun projtree->expand-to-root (self path)
;;   "Expand path PATH including any ancestor directories in project tree SELF."
;;   (let* ((root (projtree->root self))
;;          (ancestors (projtree--ancestor-paths root path)))
;;     (projtree->expand-paths self ancestors)))

;; (defun projtree->toggle-expand (self path)
;;   "Toggle the expanded state for PATH in project tree SELF.

;; Closing a directory path has the effect of not showing any paths
;; below it."
;;   (let ((path (projtree--abspath path))
;;         (value (not (projtree->expanded-p self path))))
;;     (puthash path value (projtree-expanded-paths self))))

;; (defun projtree->display (self buffer)
;;   "Render project tree SELF in buffer BUFFER.

;; Renders a project tree as a browsable file explorer in a buffer
;; using `hierarchy.el'.  The hierarchy can be browsed and
;; directories can be expanded by clicking their nodes.  Any
;; `selected-path' will be highlighted in the rendered tree."
;;   ;; Change the default-directory of the project tree buffer to make git status
;;   ;; execute in the right context.
;;   (with-current-buffer buffer
;;     (setq-local default-directory (projtree->root self)))
;;   ;; Make sure path to visited file is unfolded in project tree.
;;   ;; TODO buffer displaying selected-path may have been killed
;;   (let ((selected-path (projtree->selected-path self)))
;;     (when selected-path
;;       (projtree->expand-to-root self selected-path)))
;;   ;; Display project tree in project tree buffer.
;;   (projtree->_display-tree self buffer)
;;   ;; Highlight visited file in project tree buffer.
;;   (projtree->_highlight-selected-path self buffer)
;;   ;; Add extra project tree buffer key bindings on top of existing ones.
;;   (with-current-buffer buffer
;;     (use-local-map (make-composed-keymap projtree-buffer-map
;;                                          (current-local-map))))
;;   ;; Move cursor to saved position (if any).
;;   (when (projtree->cursor self)
;;     (with-current-buffer buffer
;;       (set-window-point (get-buffer-window buffer) (projtree->cursor self))))
;;   (with-current-buffer buffer
;;     (setq-local mode-line-format (list "%e" mode-line-front-space "projtree: " (file-name-nondirectory (projtree->root self))))))


;; (autoload 'projtree-git--status "projtree-git")

;; (defun projtree->git-project-p (self)
;;   "Indicate if the project tree SELF is a git project."
;;   (vc-git-root (projtree->root self)))

;; (defun projtree->_git-statuses (self)
;;   "Determine git statuses for the project tree SELF.

;; The status is returned as a hash table where the keys are
;; absolute file paths and values are status codes following the git
;; status porcelain format.  Up-to-date files do not have entries in
;; the resulting hash table.

;; If `projtree-show-git-status' is non-nil or SELF is not a git
;; project nil is returned."
;;   (if (and projtree-show-git-status (projtree->git-project-p self))
;;       (projtree-git--status (projtree->root self))
;;     nil))

;; (defun projtree->_git-status (self git-statuses path)
;;   "Given GIT-STATUSES for project tree SELF, return status for a particular PATH."
;;   (let ((path-status (gethash path git-statuses))
;;         (root-dir (projtree->root self)))
;;     (if path-status
;;         path-status
;;       ;; If no git status is found for path, check to see if any ancestor
;;       ;; directory is ignored/untracked.
;;       (let ((ignored/untracked-ancestor (cl-find-if (lambda (ancestor-path)
;;                                                      (let ((status (gethash ancestor-path git-statuses)))
;;                                                        (or (equal status "!!")
;;                                                            (equal status "??"))))
;;                                                     (butlast (projtree--ancestor-paths root-dir path)))))
;;         (if ignored/untracked-ancestor
;;             (gethash ignored/untracked-ancestor git-statuses)
;;           nil)))))


;; (defun projtree->_display-tree (self buffer)
;;   "Display project tree SELF in the given BUFFER.
;; Overwrites any prior BUFFER content."
;;   (let ((proj-hierarchy (projtree->_build-hierarchy self))
;;         (git-statuses (projtree->_git-statuses self)))
;;     (hierarchy-tabulated-display
;;      proj-hierarchy
;;      (hierarchy-labelfn-indent
;;       (hierarchy-labelfn-button
;;        ;; labelfn
;;        (lambda (path indent)
;;          (projtree->_render-tree-entry self path git-statuses))
;;        ;; actionfn
;;        (lambda (path indent)
;;          ;; Remember cursor position in project tree buffer.
;;          (projtree->set-cursor self (point))
;;          (if (file-directory-p path)
;;              (progn
;;                (projtree->toggle-expand self path)
;;                (projtree->display self buffer))
;;            ;; Open clicked file and switch to that buffer.
;;            (let ((opened-buf (find-file-noselect path)))
;;              (display-buffer-use-some-window opened-buf
;;                                            (list (cons 'inhibit-same-window t)))
;;              (switch-to-buffer opened-buf))))))
;;      buffer)
;;     ;; Change the project tree buffer title from "Item name" to "Project tree".
;;     (with-current-buffer buffer
;;       (setq-local tabulated-list-format (vector '("Project tree" 0 nil)))
;;       (tabulated-list-init-header))))

;; (defun projtree->_render-tree-entry (self path &optional git-statuses)
;;   "Render a PATH in project tree SELF with given GIT-STATUSES.

;; This renders one file entry in the file tree explorer, with git
;; status used to set the appropriate face."
;;   (let* ((filename (file-name-nondirectory path))
;;          (is-dir (file-directory-p path))
;;          (git-status (when git-statuses (projtree->_git-status self git-statuses path)))
;;          (git-face (when git-status (projtree-git--status-face git-status))))
;;     (if is-dir
;;         ;; Directory.
;;         (progn
;;           (let ((expand-symbol (if (projtree->expanded-p self path) "-" "+")))
;;             (insert (propertize expand-symbol 'face 'projtree-dir))
;;             (insert " ")
;;             (insert (propertize filename 'face (or git-face 'projtree-dir)))))
;;       ;; Regular file.
;;       (insert " ")
;;       (insert (propertize filename 'face (or git-face 'projtree-file))))))

;; (defvar projtree--hl-overlay nil)

;; (defun projtree->_highlight-selected-path (self buffer)
;;   "Highlight any currently selected-path in project tree SELF rendered in BUFFER."
;;   (let ((selected-path (projtree->selected-path self)))
;;     ;; First clear any old highlight overlay.
;;     (when projtree--hl-overlay
;;       (delete-overlay projtree--hl-overlay))
;;     ;; Then produce a new highlight overlay for the visited file.
;;     (when selected-path
;;       (projtree--highlight-file selected-path buffer))))

;; (defun projtree--highlight-file (path buffer)
;;   "Highlight a certain PATH in BUFFER."
;;   (with-current-buffer buffer
;;     ;; Note: be defensive (when-let). Visited path may have been deleted.
;;     (when-let ((selected-linum (cl-position path (mapcar #'car tabulated-list-entries) :test #'equal)))
;;       (projtree--highlight-row (+ selected-linum 1) buffer))))

;; (defun projtree--highlight-row (line-number buffer)
;;   "Highlight a certain LINE-NUMBER in BUFFER."
;;   (with-current-buffer buffer
;;     (projtree--goto-line line-number)
;;     (let* ((start (line-beginning-position))
;;            (end (line-end-position))
;;            (hl-overlay (make-overlay start (+ 1 end))))
;;       (overlay-put hl-overlay 'face 'projtree-highlight)
;;       (overlay-put hl-overlay 'before-string (propertize "X" 'display (list 'left-fringe 'right-triangle)))
;;       (setq projtree--hl-overlay hl-overlay)
;;       ;; Move point to highlighted row in in project tree buffer window.
;;       (set-window-point (get-buffer-window (current-buffer)) start))))


;; (defun projtree->_expand-status-symbol (self path)
;;   "Determine the expansion symbol to use for PATH in project tree SELF."
;;   (if (projtree->expanded-p self path)
;;       "-"
;;     "+"))

;; (defun projtree->_children-fn (self)
;;   "Return a function for SELF that lists child files for expanded directories."
;;   (lambda (folder)
;;     (if (and (file-directory-p folder)
;;              (projtree->expanded-p self folder))
;;         ;; Ignore "." and ".." from directory listing.
;;         (cl-remove-if (lambda (file-name)
;;                         (or (string-suffix-p "/." file-name)
;;                             (string-suffix-p "/.." file-name)))
;;                       (directory-files folder t nil nil nil))
;;       nil)))

;; (defun projtree->_build-hierarchy (self)
;;   "Build the `hierarchy.el' tree structure for project tree SELF."
;;   (let* ((root-dir (projtree--abspath (projtree->root self))))
;;     ;; TODO maybe move to construction
;;     (projtree->expand-path self root-dir)
;;     (let ((h (hierarchy-new)))
;;       (hierarchy-add-tree h root-dir nil (projtree->_children-fn self))
;;       h)))


;; (defun projtree--current ()
;;   "Return the project tree rooted at the most recently visited file.
;; Will return nil if the visited file is not in a project structure."
;;   ;; We look at the last file-visiting buffer that was open when determining the
;;   ;; current project tree. We could for example be in the project tree buffer
;;   ;; (for example, pressing 'g') and we then don't want the project tree of that
;;   ;; buffer.
;;   (when-let* ((last-file-buffer (cl-find-if (lambda (buf) (buffer-file-name buf)) (buffer-list)))
;;               (root (projtree--project-root last-file-buffer)))
;;     (projtree-set->get-projtree (projtree-active-set) root)))


;; (defun projtree--project-root (buffer)
;;   "Return the root project directory of BUFFER or nil if none is available."
;;   (when (buffer-live-p buffer)
;;     (with-current-buffer buffer
;;       (let* ((buf-file (buffer-file-name buffer))
;;              ;; Note: for a non-file buffer (like `*scratch*') we consider its
;;              ;; project tree root to be default-directory.
;;              (buffer-dir (file-name-directory (or buf-file default-directory)))
;;              ;; Note: replace `project-find-functions' for the duration of the
;;              ;; call to `project-current' to make it also recognize project roots
;;              ;; in the `project-list-file'.
;;              (project-find-functions (projtree--project-find-functions))
;;              (project (project-current nil buffer-dir)))
;;         (if project
;;             (project-root project)
;;           nil)))))


;; (defun projtree--descendant-p (ancestor child)
;;   "Indicate if CHILD is a descendant of ANCESTOR.

;; For example, '/one/two/three.txt' is a descrendant of '/one'."
;;   (let ((ancestor (projtree--abspath ancestor))
;;         (child (projtree--abspath child)))
;;     (string-prefix-p ancestor child)))


;; (defun projtree--ancestor-paths (project path)
;;   "Return all ancestor directories of PATH up and until PROJECT directory."
;;   (let ((project (projtree--abspath project))
;;         (path (projtree--abspath path)))
;;     (when (not (projtree--descendant-p project path))
;;       (error "Path %s is not a sub-directory of %s" path project))
;;     (if (not (string-equal project path))
;;         (let ((parent (file-name-directory path)))
;;           (append (projtree--ancestor-paths project parent) (list path)))
;;       (list path))))


;; (defun projtree-open ()
;;   "Open a buffer that displays a project tree rooted at the current project.
;; The currently visited project file (if any) is highlighted."
;;   (interactive)
;;   (let ((projtree (projtree--current)))
;;     (when projtree
;;       (projtree->display projtree (projtree--get-projtree-buffer)))))


;; (defun projtree-close ()
;;   "Closes the project tree buffer."
;;   (interactive)
;;   (let ((buf (get-buffer projtree-buffer-name)))
;;     (when buf
;;       (kill-buffer buf))))


;; (defun projtree-dired ()
;;   "Opens a `dired' window for the directory of file at point."
;;   (interactive)
;;   (require 'dired)
;;   (with-current-buffer (projtree--get-projtree-buffer)
;;     (when-let* ((selected (tabulated-list-get-id))
;;                 (parent-dir (file-name-directory selected))
;;                 (dired-buf (dired parent-dir)))
;;       ;; Place dired buffer point at the selected file.
;;       (with-current-buffer dired-buf
;;         (dired-goto-file selected)))))


;; (defun projtree--get-projtree-buffer ()
;;   "Return the buffer used to display the project tree.
;; Creates the buffer if it does not already exist."
;;   (let ((buf (get-buffer-create projtree-buffer-name)))
;;     (display-buffer-in-side-window buf `((side . ,projtree-window-placement) (window-width . ,projtree-window-width) (dedicated . t)))
;;     (let ((win (get-buffer-window buf)))
;;       ;; Make window dedicated to projtree buffer.
;;       (set-window-dedicated-p win t)
;;       ;; Make C-x 1 not close the window.
;;       (set-window-parameter win 'no-delete-other-windows t)
;;       ;; Frame resizing should not affect the size of the project tree window.
;;       (window-preserve-size win t t))
;;     ;; Set the project tree title.
;;     (with-current-buffer buf
;;       (setq-local tabulated-list-format (vector '("Project tree" 0 nil)))
;;       (tabulated-list-init-header))
;;     buf))


;; (defun projtree--rendered-tree-root ()
;;   "Return the root directory of the project tree currently rendered in the project tree buffer."
;;   (buffer-local-value 'default-directory (get-buffer-create projtree-buffer-name)))


;; (defun projtree--abspath (path)
;;   "Return a normalized PATH (absolute and no trailing slash)."
;;   (string-trim-right (expand-file-name path) "/"))


;; (defvar projtree--scheduled-render-timer nil
;;   "Holds a timer for the next scheduled projtree--open-async call.")

;; (defun projtree--open-async ()
;;   "Schedules a `projtree-open' call to render the project tree in its buffer.

;; The rendering is scheduled to happen after a short time of editor
;; idleness.  If a render call is already scheduled it is cancelled
;; and replaced by the current call.

;; Replacing an already scheduled call has the benefit of not
;; freezing up the editor if many files are opened in very quick
;; succession, such as when restoring a desktop via `desktop-read'."
;;   ;; Replace any already scheduled render operation with this one.
;;   (when projtree--scheduled-render-timer
;;     (cancel-timer projtree--scheduled-render-timer))
;;   (setq projtree--scheduled-render-timer
;;         (run-with-idle-timer 0.050 nil #'projtree-open)))


;; (defun projtree--file-visiting-buffer-p (buffer)
;;   "Return t for a BUFFER that is visiting an existing file, nil otherwise."
;;   (let ((buffer-file (buffer-file-name buffer)))
;;     ;; Check both that the buffer visits a file and that the file exists. For a
;;     ;; newly created buffer, the file may not yet exist on the filesystem.
;;     (and buffer-file
;;          (file-exists-p buffer-file))))


;; (defun projtree--render-on-buffer-switch ()
;;   "Render project tree in if current buffer is visiting a project tree file.

;; Intended to be registered as a hook whenever the current buffer changes."
;;   (let ((curr-buf (current-buffer)))
;;     ;; No need to re-render project tree if we're in a buffer not visiting a
;;     ;; file (the mini-buffer, the project tree buffer, etc).
;;     (when (projtree--file-visiting-buffer-p curr-buf)
;;       ;; Note: if the buffer is not visiting a project file we ignore trying to
;;       ;; render the project tree.
;;       (when-let* ((projtree (projtree--current))
;;                   (visited-file (buffer-file-name curr-buf)))
;;          ;; Only re-render the tree if the project root or visited file changed.
;;         (unless
;;             (and
;;              (eq (projtree--rendered-tree-root) (projtree->root projtree))
;;              (eq visited-file (projtree->selected-path projtree)))
;;            ;; Mark path selected in current project tree.
;;           (projtree->set-selected-path projtree visited-file)
;;           ;; We want follow-mode when switching to a file buffer (and cursor has
;;           ;; left project tree).
;;           (projtree--forget-cursor)
;;           (projtree--open-async))))))


;; (defun projtree--forget-cursor ()
;;   "Reset the `*projtree*' buffer cursor of the currently active project tree."
;;   (when-let ((projtree (projtree--current)))
;;     (projtree->set-cursor projtree nil)))


;; (defun projtree--try-project-list (dir)
;;   "Find a project containing DIR from the `project-list-file'."
;;   (catch :root
;;     (dolist (it (project-known-project-roots))
;;       (when (and (string-prefix-p it dir)
;;                  (file-directory-p it))
;;         (throw :root (cons 'transient it))))))


;; (defun projtree--goto-line (line-num)
;;   "Move point to the beginning of line LINE-NUM in the current buffer."
;;   (goto-char (point-min))
;;   (forward-line (1- line-num)))


;; (defun projtree--project-find-functions ()
;;   "Return a projtree replacement for `project-find-functions'.

;; It extends the built-in `project-find-functions' list (whose
;; functions are called to find the project containing a given
;; directory) which by default only finds projects that are
;; version-controlled (through `project-try-vc').  We add a function
;; to also look for project directories that are listed in the
;; `project-list-file'.  When using this with `project-current' a
;; projtree will be correctly rendered also for non-git projects."
;;   (append '(projtree--try-project-list) project-find-functions))


;; (autoload 'projtree-profiling-enable "projtree-profiling")
;; (autoload 'projtree-profiling-disable "projtree-profiling")

;; ;;;###autoload
;; (define-minor-mode projtree-mode
;;   "TODO describe."
;;   :lighter nil ;; Do not display on mode-line.
;;   (if projtree-mode
;;       (progn
;;         (when projtree-profiling-enabled
;;           (projtree-profiling-enable))
;;         (add-hook 'buffer-list-update-hook #'projtree--render-on-buffer-switch)
;;         ;; Create the project tree buffer.
;;         (projtree--get-projtree-buffer)
;;         ;; When we activate the mode we call the switch buffer hook to render
;;         ;; the tree.
;;         (projtree--render-on-buffer-switch))
;;     (remove-hook 'buffer-list-update-hook #'projtree--render-on-buffer-switch)
;;     (projtree-close)
;;     (when projtree-profiling-enabled
;;       (projtree-profiling-disable))))



(provide 'git-blame)

;;; git-blame.el ends here.
