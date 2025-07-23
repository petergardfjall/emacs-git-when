;;; git-when.el --- Show and navigate git blame data. -*- lexical-binding: t -*-
;;
;; Copyright © 2025 Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;;
;; Author: Peter Gardfjäll <peter.gardfjall.work@gmail.com>
;; URL: https://github.com/petergardfjall/emacs-git-when
;; Keywords: workspace, project
;; Package-Requires: ((emacs "27.0"))
;; Version: 0.0.1
;; Homepage: https://github.com/petergardfjall/emacs-git-when
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
;; `git-when' ... TODO
;;
;;; Code:
(require 'vc-git)
(require 'xref)

;; TODO customize faces
;; TODO customize mode map

(defvar git-when-buffer-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<M-down>") #'git-when-at-point)
    (define-key map (kbd "<M-up>")   #'xref-go-back) ;; git-when-pop?
    (define-key map (kbd "d")        #'git-when-display-commit) ;; Show in echo area
    map)
  "Keybindings available in the git blame buffer.")


(defvar git-when--commit-margin-width 40
  "The width (in characters) that the commit info in the left margin will occupy.")

(defvar git-when--git-cmd
  "Full system path to a git executable.")


(defun git-when (&optional rev file lineno)
  (interactive
   (list "HEAD"                               ;; rev TODO: git rev-parse HEAD
         (buffer-file-name (current-buffer))  ;; file
         (line-number-at-pos)))               ;; lineno
  (unless (vc-git-root file)
    (error "File %s is not under git control" file))
  (message "opening git-blame buffer for revision: %s, file: %s, line: %d" rev file lineno)
  (let ((blame-data (git-when--blame-exec rev file)))
    ;; TODO make use of commit :lineno-prior
    (git-when--render rev file blame-data)
    ;; (message "current buffer is: %s" (current-buffer))
    ;; (message "blame-data is bound: %s" (boundp 'blame-data))
    ))

(defun git-when-at-point ()
  "Does a git blame for the commit that created the line at point.
Needs to be run from a git blame buffer."
  (interactive)
  (when (not (git-when-buffer-p (current-buffer)))
    (error "Not visiting a blame buffer"))
  (let* ((lines-table (plist-get blame-data :lines-table))
         (commit-table (plist-get blame-data :commit-table))
         (line (gethash (line-number-at-pos) lines-table))
         (commit (gethash (plist-get line :commit) commit-table))

         (rev      (git-when--commit->rev commit))
         (filename (git-when--commit->filename commit))
         (lineno   (git-when--commit->original-lineno commit)))
    (git-when rev filename lineno)))

(defun git-when-display-commit ()
  "Displays commit details in the echo area for the line at point."
  (interactive)
  (when (not (git-when-buffer-p (current-buffer)))
    (error "Not visiting a blame buffer"))
  (let* ((lines-table (plist-get blame-data :lines-table))
         (commit-table (plist-get blame-data :commit-table))
         (line (gethash (line-number-at-pos) lines-table))
         (commit (gethash (plist-get line :commit) commit-table))

         (rev     (git-when--commit->rev commit))
         (author  (git-when--commit->author commit))
         (time    (format-time-string "%a %b %d %H:%M:%S %Y %z" (git-when--commit->time commit)))
         (summary (git-when--commit->summary commit))
         (message (format "commit %s\nAuthor: %s\nDate: %s\n\n    %s" rev author time summary)))
    (display-message-or-buffer message)))

(defun git-when-buffer-p (buffer)
  "Indicates if the BUFFER is a git blame buffer."
  (with-current-buffer buffer
    (boundp 'blame-data)))


(defun git-when--render (rev file blame-data)
  ""
  (let* ((buffer-name (git-when--buffer-name rev file))
         (curr-line (line-number-at-pos))
         (lines-table (plist-get blame-data :lines-table))
         (commit-table (plist-get blame-data :commit-table)))
    (xref-push-marker-stack) ;; Allow moving back by popping xref marker stack.
    (with-current-buffer (get-buffer-create buffer-name)
      ;; Fontify buffer by setting the right major mode for the file name.
      (let ((major-mode-fn (or (assoc-default (buffer-name) auto-mode-alist #'string-match) #'ignore)))
        (funcall major-mode-fn))

      (display-line-numbers-mode)
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq-local left-margin-width git-when--commit-margin-width)
      ;; TODO render: iterate over lines:
      ;; - render "blame-data[lineno]" "<separator>" "<line-content>"
      ;;   - can be multiline
      ;;   - truncate at given width
      ;; - asssociate commit revision with each line
      (dotimes (i (hash-table-count lines-table))
        (let* ((line (gethash (+ i 1) lines-table))
               (line-content (plist-get line :content))
               (commit-rev  (plist-get line :commit))
               (commit (gethash commit-rev commit-table))
               (htime (git-when--commit->htime commit))
               (summary (git-when--commit->summary commit))
               (annotation-line
                (truncate-string-to-width
                 (format "%s (%s ago) %s"
                         (truncate-string-to-width commit-rev 7)
                         htime
                         summary)
                 git-when--commit-margin-width 0 ?\s ".." nil)))
          (insert (propertize line-content 'face 'default))
          (when (or (eql i 0) (and (> i 0) (not (string-equal (plist-get (gethash i lines-table) :commit) commit-rev))))
            ;; Display commit data in margin:
            ;; see https://github.com/magit/magit/issues/1381
            (let ((o (make-overlay (line-beginning-position) (line-end-position) nil t)))
              (overlay-put o 'before-string
                           (propertize "o" 'display (list '(margin left-margin)
                                                          (propertize annotation-line 'face '(:inherit shadow :overline t)))))))
          (newline)))

      (goto-line curr-line)
      (setq buffer-read-only t)
      (toggle-truncate-lines 1) ;; Don't wrap lines.

      ;; Enable keymap for blame navigation.
      (use-local-map git-when-buffer-keymap)
      (setq-local blame-data blame-data)
      (display-buffer-same-window (current-buffer) '())
      (set-buffer (git-when--buffer-name rev file)))))


(defun git-when--blame-exec (rev file)
  "TODO: return a hash table mapping line numbers to commit structs"
  (with-temp-buffer
    (message "repo-path: %s" (git-when--repo-path file))
    (when-let* ((git-root (vc-git-root file))
                (repo-path (git-when--repo-path file))
                (gitcmd (executable-find "git")))
                ;; (statuses (make-hash-table :test 'equal))
      (setq-local default-directory git-root)
      (let ((exit-code (call-process gitcmd nil (current-buffer) nil "blame" "--porcelain" rev "--" repo-path)))
        (when (> exit-code 0)
          (error "git-blame gave non-zero exit code: %d" exit-code)))
      (git-when--parse-blame (current-buffer)))))

(defun git-when--buffer-name (rev file)
  (format "git-when@%s:%s" rev (git-when--repo-path file)))

(defun git-when--repo-path (path)
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
(defun git-when--parse-blame (output-buffer)
  "Parses the output of a git blame call with --porcelain."
  (with-current-buffer output-buffer
    (goto-char (point-min))
    (let* ((file-lines-table (make-hash-table :test 'equal))
           (commit-table (make-hash-table :test 'equal)))
      (while (not (eobp))
        (when (not (git-when--porcelain-header-p (git-when--current-line)))
          (error "Failed to parse git blame output: expected porcelain header"))
        ;; Parse git blame header row for line.
        (let* ((tokens (split-string (git-when--current-line)))
               (commit-rev    (nth 0 tokens))
               (orig-lineno   (string-to-number (nth 1 tokens)))
               (final-lineno  (string-to-number (nth 2 tokens)))
               (commit (make-git-when--commit :rev commit-rev :original-lineno orig-lineno :final-lineno final-lineno)))
          (message "header tokens: %s" tokens)
          ;; Commit not encountered before, commit details will follow.
          (when (not (gethash commit-rev commit-table))
            (while (not (git-when--content-line-p (git-when--peek-next-line)))
              (let* ((line (git-when--next-line))
                     (tokens (split-string line))
                     (key (nth 0 tokens))
                     (val (nth 1 tokens)))
                (pcase key
                  ("author"         (git-when--commit->set-author commit val))
                  ("author-mail"    (git-when--commit->set-mail commit val))
                  ("author-time"
                   (git-when--commit->set-time commit (git-when--parse-time val))
                   (git-when--commit->set-htime commit (git-when--humanize-time (git-when--parse-time val))))
                  ("author-tz"      (git-when--commit->set-tz commit val))
                  ("summary"        (git-when--commit->set-summary commit (string-remove-prefix "summary " line)))
                  ("filename"       (git-when--commit->set-filename commit (string-remove-prefix "filename " line))))))
            (puthash commit-rev commit commit-table))
          ;; The next (tab-prefixed) line holds the actual content of the line in the file.
          (let* ((content-line (git-when--next-line))
                 (file-line `(:commit ,commit-rev :content ,(string-trim-left content-line "\t"))))
            (puthash final-lineno file-line file-lines-table)))
        (git-when--next-line))
      ;; Return the gathered content lines and commit table.
      `(:lines-table ,file-lines-table :commit-table ,commit-table))))


(defun git-when--peek-next-line ()
  "Return the next line without moving cursor."
  (save-excursion (git-when--next-line)))

(defun git-when--next-line ()
  "TODO"
  (forward-line 1)
  (git-when--current-line))

(defun git-when--current-line ()
  "TODO"
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun git-when--content-line-p (line)
  (string-prefix-p "\t" line))

(defun git-when--porcelain-header-p (line)
  ;; <commit> <lineno-prior> <lineno-final> [num-group-lines]
  (string-match "[0-9a-f]\\{40\\} [0-9]+ [0-9]+\\( [0-9]+\\)?" line))


(defun git-when--parse-time (epoch-time-string)
  "Convert a git blame timestamp like '1727681138' to a Lisp timestamp."
  (time-convert (string-to-number epoch-time-string)))


(defun git-when--humanize-time (time)
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


(cl-defstruct git-when--blame
  ;; A hash table of source file lines keyed on line number (starting at 1).
  ;; Each value is a plist with a `:commit' and a `:content' property.
  lines
  ;; A hash table of commit plists keyed on commit revisions.
  commits)

(defun git-when--blame->new ()
  "Create an empty blame data struct."
  (make-git-when--blame
   :lines (make-hash-table :test 'equal)
   :commits (make-hash-table :test 'equal)))

(defun git-when--blame->x (self)
  "Do x on SELF."
  (git-when--blame-x self))

(cl-defstruct git-when--commit
  "Holds commit details for one line of a source file.
Follows the git blame porcelain format [1].

[1] https://git-scm.com/docs/git-blame#_the_porcelain_format
"

  ;; 40-byte SHA-1 of the commit.
  rev
  ;; The line number of the line in the original file.
  original-lineno
  ;; The line number of the line in the final file.
  final-lineno

  ;; The filename in the commit that the line is attributed to
  filename
  ;; The first line of the commit log message
  summary

  ;; The author name.
  author
  ;; The author email address.
  author-mail
  ;; The commit timestamp.
  author-time
  ;; The humanized commit timestamp. For example, "2 weeks ago".
  author-htime
  ;; The timezone of the commit.
  author-tz)

(defun git-when--commit->rev (self)
  (git-when--commit-rev self))

(defun git-when--commit->set-rev (self rev)
  (setf (git-when--commit-rev self) rev))

(defun git-when--commit->original-lineno (self)
  (git-when--commit-original-lineno self))

(defun git-when--commit->set-original-lineno (self lineno)
  (setf (git-when--commit-original-lineno self) lineno))

(defun git-when--commit->final-lineno (self)
  (git-when--commit-final-lineno self))

(defun git-when--commit->set-final-lineno (self lineno)
  (setf (git-when--commit-final-lineno self) lineno))

(defun git-when--commit->filename (self)
  (git-when--commit-filename self))

(defun git-when--commit->set-filename (self filename)
  (setf (git-when--commit-filename self) filename))

(defun git-when--commit->summary (self)
  (git-when--commit-summary self))

(defun git-when--commit->set-summary (self summary)
  (setf (git-when--commit-summary self) summary))

(defun git-when--commit->author (self)
  (git-when--commit-author self))

(defun git-when--commit->set-author (self author)
  (setf (git-when--commit-author self) author))

(defun git-when--commit->mail (self)
  (git-when--commit-author-mail self))

(defun git-when--commit->set-mail (self email)
  (setf (git-when--commit-author-mail self) email))

(defun git-when--commit->time (self)
  (git-when--commit-author-time self))

(defun git-when--commit->set-time (self time)
  (setf (git-when--commit-author-time self) time))

(defun git-when--commit->htime (self)
  (git-when--commit-author-htime self))

(defun git-when--commit->set-htime (self htime)
  (setf (git-when--commit-author-htime self) htime))

(defun git-when--commit->tz (self)
  (git-when--commit-author-tz self))

(defun git-when--commit->set-tz (self tz)
  (setf (git-when--commit-author-tz self) tz))


(provide 'git-when)

;;; git-when.el ends here.
