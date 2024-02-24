;;; ob-tts.el --- org-babel functions for tts evaluation

;; Copyright (C) 

;; Author: zbelial
;; Maintainer: zbelial
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/zbelial/tts.el
;; Keywords: TTS Emacs

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:


;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'tts)

(define-derived-mode tts-mode text-mode "TTS")

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("tts" . "tts"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:tts '())

;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:tts' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:tts (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-tts nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "%s=%S"
                (car pair) (org-babel-tts-var-to-tts (cdr pair))))
      vars "\n")
     "\n" body "\n")))

;; This is the main function which is called to evaluate a code
;; block.
;;
;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp depending on the value of the
;; :results header argument
;; - output means that the output to STDOUT will be captured and
;;   returned
;; - value means that the value of the last statement in the
;;   source code block will be returned
;;
;; The most common first step in this function is the expansion of the
;; PARAMS argument using `org-babel-process-params'.
;;
;; Please feel free to not implement options which aren't appropriate
;; for your language (e.g. not all languages support interactive
;; "session" evaluation).  Also you are free to define any new header
;; arguments which you feel may be useful -- all header arguments
;; specified by the user will be available in the PARAMS variable.
(defun org-babel-execute:tts (body params)
  "Execute a block of tts code with org-babel.
This function is called by `org-babel-execute-src-block'"
  (message "executing tts source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the session if the value of the session keyword is not the
         ;; string `none'
         ;; (session (unless (string= value "none")
         ;;            (org-babel-tts-initiate-session
         ;;             (cdr (assq :session processed-params)))))
         ;; variables assigned for use in the block
         (vars (org-babel--get-vars processed-params))
         (result-params (assq :result-params processed-params))
         ;; either OUTPUT or VALUE which should behave as described above
         (result-type (assq :result-type processed-params))
         (rate (assoc-default :rate processed-params))
         (voice (assoc-default :voice processed-params))
         (pitch (assoc-default :pitch processed-params))
         (volume (assoc-default :volume processed-params))
         ;; expand the body with `org-babel-expand-body:tts'
         (full-body (org-babel-expand-body:tts
                     body params processed-params))
         result)
    ;; actually execute the source-code block either in a session or
    ;; possibly by dropping it to a temporary file and evaluating the
    ;; file.
    (message "params: %s" params)
    (message "rate: %s" rate)
    (setq result (tts full-body
                      :voice (if voice (format "%s" voice))
                      :rate (if rate (format "%s" rate))
                      :pitch (if pitch (format "%s" pitch))
                      :volume (if volume (format "%s" volume))))
    (when (and result
               (file-exists-p (tts--result-media-file result)))
      (format "[[elisp:(tts--play-audio \"%s\")][Play]]" (tts--result-media-file result)))))

;; This function should be used to assign any variables in params in
;; the context of the session environment.
(defun org-babel-prep-session:tts (session params)
  "Prepare SESSION according to the header arguments specified in PARAMS."
  )

(defun org-babel-tts-var-to-tts (var)
  "Convert an elisp var into a string of tts source code
specifying a var of the same value."
  (format "%S" var))

(defun org-babel-tts-table-or-string (results)
  "If the results look like a table, then convert them into an
Emacs-lisp table, otherwise return the results as a string."
  )

(defun org-babel-tts-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION then create.
Return the initialized session."
  (unless (string= session "none")
    ))

(provide 'ob-tts)
;;; ob-tts.el ends here
