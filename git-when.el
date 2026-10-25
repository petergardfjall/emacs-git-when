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
    ;; Show a blame buffer for commit at point in current blame buffer.
    (define-key map (kbd "<M-down>")  #'git-when-at-point)
    (define-key map (kbd "M-p")       #'git-when-parent-commit-at-point)
    (define-key map (kbd "<M-up>")    #'xref-go-back)
    ;; Show commit at point in current blame buffer.
    (define-key map (kbd "c")         #'git-when-show-commit)
    ;; Show commit message at point in current blame buffer.
    (define-key map (kbd "m")         #'git-when-show-commit-message)
    map)
  "Keybindings available in the git blame buffer.")

(defvar git-when--commit-margin-width 40
  "The width (in characters) that the commit info in the left margin will occupy.")


(defun git-when (&optional rev file lineno)
  "Opens a blame buffer that shows what commit and author last modified each line in FILE.
If no FILE is given, the file visited by the current buffer is assumed.
The REV argument can be supplied to specify the starting point for the
blame if anything other than 'HEAD' is desired.  The cursor will be
placed at line LINENO in the blame buffer."
  (interactive
   (list (git-when--exec-output
          "git" "log" "--format=%H" "--max-count=1"
          (buffer-file-name (current-buffer)))             ;; rev
         (buffer-file-name (current-buffer))               ;; file
         (line-number-at-pos)))                            ;; lineno
  (unless (vc-git-root file)
    (error "File %s is not under git control" file))
  (let ((blamefile (git-when--blame-exec rev file)))
    (git-when--show rev file blamefile)
    (goto-line lineno)))

(defun git-when-at-point ()
  "Does a git blame for the commit that created the line at point.
Needs to be run from a git blame buffer."
  (interactive)
  (when (not (git-when-buffer-p (current-buffer)))
    (error "Not visiting a blame buffer"))
  (let* ((commit   (git-when--file->commit git-when--blamefile (line-number-at-pos)))
         (rev      (git-when--commit->rev commit))
         (filename (git-when--commit->filename commit))
         (absfile  (file-name-concat visited-repo-root filename))
         (line     (git-when--file->line git-when--blamefile (line-number-at-pos)))
         (orig-lineno   (plist-get line :original-lineno)))
    ;; No-op if buffer's visited-rev is same as target rev.
    (unless (string-equal visited-rev rev)
      (git-when rev absfile orig-lineno))))



(defun git-when-parent-commit-at-point ()
  "Does a git blame for the parent commit of the blame buffer's commit at point.
Needs to be run from a git blame buffer."
  (interactive)
  (when (not (git-when-buffer-p (current-buffer)))
    (error "Not visiting a blame buffer"))
  (let* ((commit   (git-when--file->commit git-when--blamefile (line-number-at-pos)))
         (rev      (git-when--commit->rev commit))
         (filename (git-when--commit->filename commit))
         (absfile  (file-name-concat visited-repo-root filename))
         (line     (git-when--file->line git-when--blamefile (line-number-at-pos)))
         (orig-lineno   (plist-get line :original-lineno)))
    (let ((parent-rev (git-when--exec-output "git" "log" "-1" "--format=%H" (format "%s~1" rev) "--" filename)))
      (git-when parent-rev absfile orig-lineno))))

(defun git-when-show-commit ()
  "Display `git show' details for the commit at point.
The details are shown in the echo area if possible, otherwise in a pop-up buffer."
  (interactive)
  (when (not (git-when-buffer-p (current-buffer)))
    (error "Not visiting a blame buffer"))
  (let* ((rev (git-when--file->commit-rev git-when--blamefile (line-number-at-pos)))
         (message (git-when--exec-output "git" "show" "--pretty=medium"  rev)))
    (display-message-or-buffer message)))


(defun git-when-show-commit-message ()
  "Display the commit message for the commit at point.
The message is shown in the echo area if possible, otherwise in a pop-up buffer."
  (interactive)
  (when (not (git-when-buffer-p (current-buffer)))
    (error "Not visiting a blame buffer"))
  (let* ((rev (git-when--file->commit-rev git-when--blamefile (line-number-at-pos)))
         (message (git-when--exec-output "git" "show" "--pretty=medium" "--no-patch" rev)))
    (display-message-or-buffer message)))


(defun git-when-buffer-p (buffer)
  "Indicates if BUFFER is a git blame buffer."
  (local-variable-if-set-p 'git-when--blamefile buffer))


(defun git-when--show (rev file blamefile)
  "Opens a buffer that displays the BLAMEFILE data for commit REV in FILE.
The buffer is named 'git-when@<REV>:<FILE>'"
  (cl-check-type blamefile git-when--file)
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
      (dotimes (i (git-when--file->num-lines blamefile))
        (let* ((lineno (1+ i))
               (line (git-when--file->line blamefile lineno))
               (commit (git-when--file->commit blamefile lineno))
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
                               (git-when--file->commit-rev blamefile (1- lineno))
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
      (setq-local git-when--blamefile blamefile)
      (setq-local visited-rev rev)
      (setq-local visited-repo-root (vc-git-root file))
      (display-buffer-same-window (current-buffer) '()))
    (set-buffer (git-when--buffer-name rev file))))


(defun git-when--blame-exec (rev file)
  "Run git blame against FILE and revision REV.
The git blame output is parsed into a `git-when--file' blame summary."
  (with-temp-buffer
    (when-let* ((git-root (vc-git-root file))
                (repo-path (git-when--repo-path file))
                (gitcmd (executable-find "git")))
      (setq-local default-directory (expand-file-name git-root))
      (let ((exit-code (call-process gitcmd nil (current-buffer) nil "blame" "--porcelain" rev "--" repo-path)))
        (when (> exit-code 0)
          (error "Git blame gave non-zero exit code: %d\n%s" exit-code
                 (buffer-substring-no-properties (point-min) (point-max)))))
      (git-when--parse-blame (current-buffer)))))


(defun git-when--exec-output (command &rest args)
  "Return the output from executing a shell COMMAND with ARGS arguments."
  (with-temp-buffer
    (let* ((fn-params (append (list command nil (current-buffer) nil) args))
           (exit-code (apply #'call-process fn-params)))
        (when (> exit-code 0)
          (error "Command gave non-zero exit code: %d" exit-code))
        (string-trim (buffer-substring (point-min) (point-max))))))


(defun git-when--buffer-name (rev file)
  (format "git-when@%s:%s" rev (git-when--repo-path file)))

(defun git-when--repo-path (path)
  "Return a path relative to the repository root folder for file PATH."
  (when-let* ((git-root (vc-git-root path)))
    (file-relative-name path git-root)))


(defun git-when--parse-blame (output-buffer)
  "Parses a git blame OUTPUT-BUFFER into a `git-when--file' struct."
  (with-current-buffer output-buffer
    (goto-char (point-min))
    (let* ((blamefile (git-when--file->new)))
      (while (not (eobp))
        (when (not (git-when--porcelain-header-p (git-when--current-line)))
          (error "Failed to parse git blame output: expected porcelain header"))
        ;; Parse git blame header row for the next line in file.
        ;; <sha-1> <lineno-original> <lineno-final> [num-group-lines]
        (let* ((tokens (split-string (git-when--current-line)))
               (commit-rev    (nth 0 tokens))
               (orig-lineno   (string-to-number (nth 1 tokens)))
               (final-lineno  (string-to-number (nth 2 tokens)))
               (commit (make-git-when--commit :rev commit-rev)))
          ;; Commit not encountered before, commit details will follow.
          (when (not (git-when--file->get-commit blamefile commit-rev))
            (while (not (git-when--content-line-p (git-when--peek-next-line)))
              (let* ((line (git-when--next-line))
                     (tokens (split-string line))
                     (key (nth 0 tokens))
                     (val (nth 1 tokens)))
                (pcase key
                  ("author"         (git-when--commit->set-author commit val))
                  ("author-mail"    (git-when--commit->set-mail commit val))
                  ;; Note: GitHub blame uses committer-{time,tz} instead of author-{time,tz}.
                  ("committer-time"
                   (git-when--commit->set-time commit (git-when--parse-time val))
                   (git-when--commit->set-htime commit (git-when--humanize-time (git-when--parse-time val))))
                  ("committer-tz"      (git-when--commit->set-tz commit val))
                  ("summary"        (git-when--commit->set-summary commit (string-remove-prefix "summary " line)))
                  ("filename"       (git-when--commit->set-filename commit (string-remove-prefix "filename " line))))))
            (git-when--file->add-commit blamefile commit))
          ;; The next (tab-prefixed) line holds the source code line.
          (let* ((content-line (git-when--next-line))
                 (line (string-trim-left content-line "\t")))
            (git-when--file->add-line blamefile final-lineno orig-lineno commit-rev line)))
        (git-when--next-line))
      ;; Return the gathered blame data.
      blamefile)))


(defun git-when--peek-next-line ()
  "Return the next line in the current buffer without moving point."
  (save-excursion (git-when--next-line)))

(defun git-when--next-line ()
  "Moves point to the next line in the current buffer."
  (forward-line 1)
  (git-when--current-line))

(defun git-when--current-line ()
  "Return the entire line at point in the current buffer."
  (buffer-substring (line-beginning-position) (line-end-position)))

(defun git-when--content-line-p (line)
  "Predicate that checks if LINE represents an (indented) content line."
  (string-prefix-p "\t" line))

(defun git-when--porcelain-header-p (line)
  "Predicate that checks if LINE is a header that precedes a blame line.
Such a line has format:
  <sha-1> <lineno-original> <lineno-final> [num-group-lines]"
  (string-match "[0-9a-f]\\{40\\} [0-9]+ [0-9]+\\( [0-9]+\\)?" line))


(defun git-when--parse-time (epoch-time-string)
  "Convert a git blame EPOCH-TIME-STRING like '1727681138' to a Lisp timestamp."
  (time-convert (string-to-number epoch-time-string)))


(defun git-when--humanize-time (time)
  "Converts a TIME (in seconds since epoch) into a human-readable elapsed time since string like '2 weeks'."
  (let* ((time-with-unit
          (let* ((seconds-since (time-to-seconds (time-since time))))
            (if (> seconds-since (* 365 24 3600))
                `(,(round (/ seconds-since (* 365 24 3600))) . "year")
              (if (> seconds-since (* 30 24 3600))
                  `(,(round (/ seconds-since (* 30 24 3600))) . "month")
                (if (> seconds-since (* 7 24 3600))
                    `(,(round (/ seconds-since (* 7 24 3600))) . "week")
                  (if (> seconds-since (* 24 3600))
                      `(,(round (/ seconds-since (* 24 3600))) . "day")
                    (if (> seconds-since 3600)
                        `(,(round (/ seconds-since 3600)) . "hour")
                      (if (> seconds-since 60)
                          `(,(round (/ seconds-since 60)) . "minute")
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


(cl-defstruct git-when--file
  "Holds parsed git blame output for a file."

  ;; Holds git blame data for a file orgainzed as a lookup table keyed on line
  ;; number (starting at 1). Each value is a plist with properties:
  ;; - `:commit'
  ;; - `:original-lineno'
  ;; - `:content'
  lines
  ;; A hash table of commit plists keyed on commit revisions.
  commits)

(defun git-when--file->new ()
  "Create an empty blame file struct."
  (make-git-when--file
   :lines (make-hash-table :test 'equal)
   :commits (make-hash-table :test 'equal)))

(defun git-when--file->num-lines (self)
  "Returns the number of lines contained in SELF."
  (hash-table-count (git-when--file-lines self)))

(defun git-when--file->add-commit (self commit)
  "Add a COMMIT to SELF.
The COMMIT is of type `git-when--commit'."
  (cl-check-type commit git-when--commit)
  (let* ((rev (git-when--commit->rev commit)))
    (puthash rev commit (git-when--file-commits self))))

(defun git-when--file->get-commit (self rev)
  "Get a commit recorded for at least one line in SELF with a given REV.
The commit is of type `git-when--commit'."
  (gethash rev (git-when--file-commits self)))

(defun git-when--file->add-line (self lineno original-lineno commit-rev content)
  "Add a blame source code line to SELF.
LINENO is the line number of the line in the final file.
ORIGINAL-LINENO is the line number of the line in the original file.
COMMIT-REV is the 40-byte checksum of the revision that the line is attributed to.
CONTENT holds the character content of the string."
  (let* ((line `(:commit ,commit-rev :original-lineno ,original-lineno :content ,content)))
    (puthash lineno line (git-when--file-lines self))))

(defun git-when--file->line (self lineno)
  "Returns a plist representing the blame data for the given LINENO in SELF."
  (gethash lineno (git-when--file-lines self)))

(defun git-when--file->commit (self lineno)
  "Returns the commit found at LINENO in SELF.
The commit is of type `git-when--commit'."
  (let* ((commit-rev (git-when--file->commit-rev self lineno)))
    (gethash commit-rev (git-when--file-commits self))))

(defun git-when--file->commit-rev (self lineno)
  "Returns the checksum of the commit found at LINENO in SELF."
  (let* ((line (git-when--file->line self lineno)))
    (plist-get line :commit)))


(cl-defstruct git-when--commit
  "Holds commit details for one line of a source file.
Follows the git blame porcelain format [1].

[1] https://git-scm.com/docs/git-blame#_the_porcelain_format"

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
  "Returns the full 40 byte checksum commit hash for SELF."
  (git-when--commit-rev self))

(defun git-when--commit->set-rev (self rev)
  "Sets the full 40 byte commit hash for SELF to be REV."
  (setf (git-when--commit-rev self) rev))

(defun git-when--commit->filename (self)
  "Returns the repository filename (relative to repo root) for SELF."
  (git-when--commit-filename self))

(defun git-when--commit->set-filename (self filename)
  "Sets the repository filename (relative to repo root) for SELF to be FILENAME."
  (setf (git-when--commit-filename self) filename))

(defun git-when--commit->summary (self)
  "Returns the first line of commit message for SELF."
  (git-when--commit-summary self))

(defun git-when--commit->set-summary (self summary)
  "Sets the first line of commit message for SELF to be SUMMARY."
  (setf (git-when--commit-summary self) summary))

(defun git-when--commit->author (self)
  "Returns the author (Firstname Lastname) of commit SELF."
  (git-when--commit-author self))

(defun git-when--commit->set-author (self author)
  "Sets the author (Firstname Lastname) of commit SELF to be AUTHOR."
  (setf (git-when--commit-author self) author))

(defun git-when--commit->mail (self)
  "Returns the author email for commit SELF."
  (git-when--commit-author-mail self))

(defun git-when--commit->set-mail (self email)
  "Sets the author email for commit SELF to be EMAIL."
  (setf (git-when--commit-author-mail self) email))

(defun git-when--commit->time (self)
  "Returns the author timestamp (sec since epoch) for commit SELF."
  (git-when--commit-author-time self))

(defun git-when--commit->set-time (self time)
  "Sets the author timestamp (sec since epoch) for commit SELF to be TIME."
  (setf (git-when--commit-author-time self) time))

(defun git-when--commit->htime (self)
  "Returns a human-readable author timestamp (2 years) for commit SELF."
  (git-when--commit-author-htime self))

(defun git-when--commit->set-htime (self htime)
  "Sets the human-readable author timestamp (2 years) for commit SELF to be HTIME."
  (setf (git-when--commit-author-htime self) htime))

(defun git-when--commit->tz (self)
  "Returns an author timezone (+0200) for commit SELF."
  (git-when--commit-author-tz self))

(defun git-when--commit->set-tz (self tz)
  "Sets the author timezone (+0200) for commit SELF to be TZ."
  (setf (git-when--commit-author-tz self) tz))


(provide 'git-when)

;;; git-when.el ends here.
