;;; tts.el --- Text to speech for Emacs -*- lexical-binding: t; -*-

;; Author: zbelial
;; Maintainer: zbelial
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Homepage: https://github.com/zbelial/tts.el
;; Keywords: TTS Emacs


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Code
(require 'seq)

(eval-when-compile
  (require 'cl-macs))

(defgroup tts nil
  "Text to speech for Emacs."
  :group 'tools)

(defcustom tts-temp-dir nil
  "A temporary dir to store downloaded files.
If nil, will use `make-temp-file' to create one."
  :type 'string
  :group 'tts)

(defcustom tts-player-function #'tts--ffplay
  "Function to play audio files.
This function should accept an argument, the path of an audio file."
  :type 'function
  :group 'tts)

(defcustom tts-edge-tts-default-voice "en-US-AriaNeural"
  "Default voice name for edge.
Use 'edge-tts --list-voices' to list all available voices."
  :type 'string
  :group 'tts)

(defcustom tts-edge-tts-default-rate nil
  "Default rate for edge."
  :type 'string
  :group 'tts)

(defcustom tts-edge-tts-default-volume nil
  "Default volume for edge."
  :type 'string
  :group 'tts)

(defcustom tts-edge-tts-default-pitch nil
  "Default pitch for edge."
  :type 'string
  :group 'tts)

(defcustom tts-play-audio-automatically nil
  "If t, play the audio file after it's generated."
  :type 'boolean
  :group 'tts)

(defcustom tts-default-backend 'edge-tts
  "Which backend to use, only edge-tts is supported now."
  :type 'symbol
  :group 'tts)

(cl-defstruct tts--key
  text
  backend
  )

(defun tts--key-test-fn (k1 k2)
  (and (string-equal (tts--key-text k1)
                     (tts--key-text k2))
       (string-equal (tts--key-backend k1)
                     (tts--key-backend k2))))

(defun tts--key-hash-fn (k)
  (let ((text (tts--key-text k))
        (backend (tts--key-backend k))
        (hash 0))
    (seq-do (lambda (c)
              (setq hash (+ (* 31 hash) c))
              (setq hash (% hash (max-char))))
            (concat text backend))
    hash))

(define-hash-table-test 'tts--cache-hash-equal 'tts--key-test-fn 'tts--key-hash-fn)

(defvar tts--cache (make-hash-table :test #'tts--cache-hash-equal)
  "Cache tts result. Key is a `tts--key', value is a `tts--result'."
  )

(cl-defstruct tts--result
  "TTS result."
  backend ;; 
  text ;; 
  volume
  rate
  pitch
  voice
  media-file ;;
  subtitles-file
  )

(defvar tts--play-process nil)

(defun tts--play-audio (path)
  "Play the audio generated."
  (tts--kill-play-process)
  (tts--ffplay path))

(defun tts--kill-play-process ()
  "Kill the play process."
  (when (and tts--play-process
             (process-live-p tts--play-process))
    (kill-process tts--play-process)
    (setq tts--play-process nil)))

(defun tts--ffplay (path)
  "Use ffplay to play the audio file PATH ."
  (if (executable-find "ffplay")
      (if (file-exists-p path)
          (start-process "tts--ffplay" nil "ffplay" "-nodisp" "-autoexit" path)
        (message "File %s not existing."))
    (message "Player ffplay not available.")))

(defun tts--ensure-temp-dir ()
  (let ((temp-dir tts-temp-dir))
    (ignore-errors
      (if (null tts-temp-dir)
          (setq temp-dir (make-temp-file "tts--" t))
        (when (not (file-exists-p tts-temp-dir))
          (make-directory tts-temp-dir t))))
    temp-dir))

(defvar tts--edge-voice tts-edge-tts-default-voice)
(defvar tts--edge-pitch tts-edge-tts-default-pitch)
(defvar tts--edge-rate tts-edge-tts-default-rate)
(defvar tts--edge-volume tts-edge-tts-default-volume)

(defvar tts--edge-tts-proc nil)
(defun tts--stop-process (proc)
  (when (and proc
             (processp proc)
             (process-live-p proc))
    (kill-process proc)))

(cl-defun tts--by-edge-tts (text &key volume rate pitch voice)
  "Text to speech via edge-tts."
  (let ((voice (or voice tts--edge-voice "en-US-JennyNeural"))
        (pitch (or pitch tts--edge-pitch))
        (volume (or volume tts--edge-volume))
        (rate (or rate tts--edge-rate))
        (text (string-trim (or text "")))
        (backend "edge-tts")
        (temp-dir)
        proc
        args
        key result
        text-md5
        media-file
        subtitles-file)
    (unless (executable-find "edge-tts")
      (message "edge-tts is not available.")
      (cl-return-from tts--by-edge-tts nil))
    
    (when (string-empty-p text)
      (message "Text is empty.")
      (cl-return-from tts--by-edge-tts nil))

    (setq key (make-tts--key :text text
                             :backend backend))
    (setq result (gethash key tts--cache))
    (when (and result
               (file-exists-p (tts--result-media-file result)))
      (message "Text has already been transformed.")
      (cl-return-from tts--by-edge-tts result))

    (setq temp-dir (tts--ensure-temp-dir))
    (unless (and temp-dir
                 (file-exists-p temp-dir))
      (message "Unable to create a temp dir.")
      (cl-return-from tts--by-edge-tts nil))

    (setq text-md5 (md5 text))
    (setq media-file (expand-file-name (concat text-md5 ".mp3") temp-dir)
          subtitles-file (expand-file-name (concat text-md5 ".vtt") temp-dir))

    (when volume
      (push (format "--volume=%s" volume) args))
    (when rate
      (push (format "--rate=%s" rate) args))
    (when pitch
      (push (format "--pitch=%s" pitch) args))
    
    (if args
        (setq result (apply #'call-process "edge-tts" nil t nil
                            "-t" (concat "\"" text "\"")
                            "--write-media" media-file
                            "--write-subtitles" subtitles-file
                            "-v" voice
                            args))
      (setq result (call-process "edge-tts" nil t nil
                                 "-t" (concat "\"" text "\"")
                                 "--write-media" media-file
                                 "--write-subtitles" subtitles-file
                                 "-v" voice
                                 )))
    (when (and (= result 0)
               (file-exists-p media-file))
      (setq result (make-tts--result :text text
                                     :backend backend
                                     :media-file media-file
                                     :subtitles-file subtitles-file
                                     :volume volume
                                     :rate rate
                                     :pitch pitch
                                     :voice voice))
      (puthash key result tts--cache))
    result))

(defun tts (text &optional volume rate pitch voice backend)
  "Text to speech."
  (let (result)
    (setq result (tts--by-edge-tts text
                                   :volume volume
                                   :rate rate
                                   :pitch pitch
                                   :voice voice))))

(defun tts-clear-cache ()
  "Clear tts cache."
  (interactive)
  (clrhash tts--cache))

(provide 'tts)
