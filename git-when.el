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
    (git-when--render rev file blame-data)
    (goto-line lineno)))

(defun git-when-at-point ()
  "Does a git blame for the commit that created the line at point.
Needs to be run from a git blame buffer."
  (interactive)
  (when (not (git-when-buffer-p (current-buffer)))
    (error "Not visiting a blame buffer"))
  (let* ((commit   (git-when--blame->commit blame-data (line-number-at-pos)))
         (rev      (git-when--commit->rev commit))
         (filename (git-when--commit->filename commit))
         (line     (git-when--blame->line blame-data (line-number-at-pos)))
         (orig-lineno   (plist-get line :original-lineno)))
    ;; No-op if buffer's visited-rev is same as target rev.
    (unless (string-equal visited-rev rev)
      (git-when rev filename orig-lineno))))

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
  "Sets the rendered buffer to be the current buffer."
  (let* ((buffer-name (git-when--buffer-name rev file))
         (curr-line (line-number-at-pos)))
    (xref-push-marker-stack) ;; Allow moving back by popping xref marker stack.
    (with-current-buffer (git-when--recreate-buffer buffer-name)
      ;; Fontify buffer by setting the right major mode for the file name.
      (let ((major-mode-fn (or (assoc-default (buffer-name) auto-mode-alist #'string-match) #'ignore)))
        (funcall major-mode-fn))

      (setq buffer-read-only nil)
      (erase-buffer)
      (setq-local left-margin-width git-when--commit-margin-width)
      ;; Render each line with commit data as an overlay in the left margin.
      (dotimes (i (git-when--blame->num-lines blame-data))
        (let* ((lineno (1+ i))
               (line (git-when--blame->line blame-data lineno))
               (commit (git-when--blame->commit blame-data lineno))
               (commit-rev (git-when--commit->rev commit))
               (htime (git-when--commit->htime commit))
               (summary (git-when--commit->summary commit))
               (annotation-line
                (truncate-string-to-width
                 (format "%s (%s ago) %s"
                         (truncate-string-to-width commit-rev 7)
                         htime
                         summary)
                 git-when--commit-margin-width 0 ?\s ".." nil)))
          (insert (plist-get line :content))
          ;; Only show commit details for first line in a chunk of lines
          ;; originating from the same commit.
          (when (or (eql lineno 1)
                    (and (> lineno 1)
                         (not (string-equal
                               (git-when--blame->commit-rev blame-data (1- lineno))
                               commit-rev))))
            ;; Display commit data in margin:
            ;; see https://github.com/magit/magit/issues/1381
            (let ((o (make-overlay (line-beginning-position) (line-end-position) nil t)))
              (overlay-put o 'before-string
                           (propertize "o" 'display (list '(margin left-margin)
                                                          (propertize annotation-line 'face '(:inherit shadow :overline t)))))))
          (newline)))

      (setq buffer-read-only t)
      (toggle-truncate-lines 1) ;; Don't wrap lines.

      ;; Enable keymap for blame navigation.
      (use-local-map git-when-buffer-keymap)
      (setq-local blame-data blame-data)
      (setq-local visited-rev rev)
      (display-buffer-same-window (current-buffer) '()))
    (set-buffer (git-when--buffer-name rev file))))


(defun git-when--blame-exec (rev file)
  "TODO: return a hash table mapping line numbers to commit structs"
  (with-temp-buffer
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
  "Parse the OUTPUT-BUFFER of a git blame call."
  (with-current-buffer output-buffer
    (goto-char (point-min))
    (let* ((blame-data (git-when--blame->new)))
      (while (not (eobp))
        (when (not (git-when--porcelain-header-p (git-when--current-line)))
          (error "Failed to parse git blame output: expected porcelain header"))
        ;; Parse git blame header row for line.
        (let* ((tokens (split-string (git-when--current-line)))
               (commit-rev    (nth 0 tokens))
               (orig-lineno   (string-to-number (nth 1 tokens)))
               (final-lineno  (string-to-number (nth 2 tokens)))
               (commit (make-git-when--commit :rev commit-rev)))
          ;; Commit not encountered before, commit details will follow.
          (when (not (git-when--blame->get-commit blame-data commit-rev))
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
            (git-when--blame->add-commit blame-data commit))
          ;; The next (tab-prefixed) line holds the source code line.
          (let* ((content-line (git-when--next-line))
                 (line (string-trim-left content-line "\t")))
            (git-when--blame->add-line blame-data final-lineno orig-lineno commit-rev line)))
        (git-when--next-line))
      ;; Return the gathered blame data.
      blame-data)))


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

(defun git-when--recreate-buffer (buffer-name)
  "Forcefully create a new buffer named BUFFER-NAME.
If an identically named buffer exists it is killed."
  (when (bufferp (get-buffer buffer-name))
    (kill-buffer buffer-name))
  (get-buffer-create buffer-name))

(cl-defstruct git-when--blame
  ;; A hash table of source file lines keyed on line number (starting at 1).
  ;; Each value is a plist with properties:
  ;; - `:commit'
  ;; - `:original-lineno'
  ;; and a `:content' property.
  lines
  ;; A hash table of commit plists keyed on commit revisions.
  commits)

(defun git-when--blame->new ()
  "Create an empty blame data struct."
  (make-git-when--blame
   :lines (make-hash-table :test 'equal)
   :commits (make-hash-table :test 'equal)))

(defun git-when--blame->num-lines (self)
  "Do x on SELF."
  (hash-table-count (git-when--blame-lines self)))

(defun git-when--blame->add-commit (self commit)
  "Add details about a COMMIT to SELF."
  (let* ((rev (git-when--commit->rev commit)))
    (puthash rev commit (git-when--blame-commits self))))

(defun git-when--blame->get-commit (self commit-rev)
  (gethash commit-rev (git-when--blame-commits self)))

(defun git-when--blame->add-line (self lineno original-lineno commit-rev content)
  "Add a source code line for LINENO to the blame data captured by SELF.
The source code line character are captured in the CONTENT string.
The line was added by commit COMMIT-REV."
  (let* ((line `(:commit ,commit-rev :original-lineno ,original-lineno :content ,content)))
    (puthash lineno line (git-when--blame-lines self))))

(defun git-when--blame->line (self lineno)
  (gethash lineno (git-when--blame-lines self)))

(defun git-when--blame->commit (self lineno)
  (let* ((commit-rev (git-when--blame->commit-rev self lineno)))
    (gethash commit-rev (git-when--blame-commits self))))

(defun git-when--blame->commit-rev (self lineno)
  (let* ((line (git-when--blame->line self lineno)))
    (plist-get line :commit)))



(cl-defstruct git-when--commit
  "Holds commit details for one line of a source file.
Follows the git blame porcelain format [1].

[1] https://git-scm.com/docs/git-blame#_the_porcelain_format
"

  ;; 40-byte SHA-1 of the commit.
  rev

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
