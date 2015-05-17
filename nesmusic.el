;; -*- coding: utf-8; lexical-binding: t -*-

;;;; Emacs minor-mode for composing NES music

(require 'cl)

(define-minor-mode nesmusic-mode
  "\\<nesmusic-mode-map>\
Compose music for the Nintendo Entertainment System

\\[nesmusic-audition] - Audition pattern"
  nil
  " NESmusic"
  '(("\C-c\C-a" . nesmusic-audition)))

(defun nesmusic-slime-search-buffer-package ()
  (interactive)
  (let ((case-fold-search t)
        (regexp "^(\\(cl:\\|nesmus:\\)?define-song\\>[ 	']*\"\\([[:alnum:]\|[:upper:]\|[:space:]]*\\)"))
    (save-excursion
      (if (or (re-search-backward regexp nil t)
              (re-search-forward regexp nil t))
          (format "%s (song)" (match-string-no-properties 2))
        (slime-search-buffer-package)))))

(setq slime-find-buffer-package-function
      'nesmusic-slime-search-buffer-package)

(defcustom nesmusic-player-path nil
  "Path to NSF player"
  :type 'string
  :group 'nesmusic)

(defun nesmusic-ensure-player-path-set ()
  (interactive)
  (unless nesmusic-player-path
    (setq nesmusic-player-path
          (expand-file-name (read-file-name "NSF Player: " nil nil t "festalon" 'file-executable-p)))))

;; (message "Directory %s" (slime-eval `(swank:default-directory)))

(defun nesmusic-audition (&optional repeat-count)
  (interactive "P")
  (nesmusic-ensure-player-path-set)
  (unless (and (integerp repeat-count) (> repeat-count 0))
    (setq repeat-count 1))
  (message "%s" repeat-count)

  (slime-connection)
  (let* ((region (slime-region-for-defun-at-point))
         (start-offset (first region))
         (string (apply 'buffer-substring-no-properties region)))
    (slime-flash-region (first region) (second region))
    (apply 'run-hook-with-args 'slime-before-compile-functions region)

    (let* ((line (save-excursion
                   (goto-char start-offset)
                   (list (line-number-at-pos) (1+ (current-column)))))
           (position `((:position ,start-offset) (:line ,@line))))
      (slime-eval-async
          `(swank:compile-string-for-emacs
            ,string
            ,(buffer-name)
            ',position
            ,(if (buffer-file-name) (slime-to-lisp-filename (buffer-file-name)))
            ',slime-compilation-policy)
        (lambda (result)
          (hacked-slime-compilation-finished result repeat-count))))))

(defun hacked-slime-compilation-finished (result repeat-count)
  (with-struct (slime-compilation-result. notes duration successp
                                          loadp faslfile) result
    (setf slime-last-compilation-result result)
    (slime-show-note-counts notes duration (cond ((not loadp) successp)
                                                 (t (and faslfile successp))))
    (when slime-highlight-compiler-notes
      (slime-highlight-notes notes))
    (run-hook-with-args 'slime-compilation-finished-hook notes)
    (cond
     ((and loadp faslfile
           (or successp
               (slime-load-failed-fasl-p)))
      (slime-eval-async `(swank:load-file ,faslfile)
        (lambda (_)
          (nesmusic-audition-last-pattern repeat-count))))
     (successp
      (nesmusic-audition-last-pattern repeat-count)))))

(defun nesmusic-audition-last-pattern (repeat-count)
  (slime-eval-async `(nesmus::play-audition
                      ,repeat-count
                      ,nesmusic-player-path)))

(provide 'nesmusic)
