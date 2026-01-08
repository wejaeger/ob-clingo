;;; ob-clingo.el --- org-babel functions for clingo potassco asp evaluation

;; Copyright (C) 2025 Werner Jäger

;; Author: Werner Jäger
;; Keywords: emacs, literate programming, org-mode, clingo, answer-set-programming, org-babel
;; URL: https://github.com/wejaeger/ob-clingo
;; Version: 0.01

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

;; Org-Babel support for evaluating potassco clingo  asp
;;
;; Supported header arguments:
;; :n - optional number of answer sets to generate; 0 tells it to generate all answer sets, defaults to 1
;; :options - optional command line options, list available options by invoking `clingo --help'
;; :instance - optional file name containing a problem instance

;; Example:
;;
;; #+begin_src clingo
;;
;; parent(vader, luke).
;; child(C, P) :- parent(P, C).
;; #show child/2.
;;
;; #+end_src
;;
;;; Requirements:
;;;   clingo  | https://github.com/potassco/clingo/releases
;;;
;;; Code:
(require 'ob)
(require 'ob-ref)
(require 'ob-eval)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("clingo" . "lp"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:clingo
  '((:results . "output") (:exports . "both"))
  "Default arguments for evaluatiing a clingo source block.")


;; This function expands the body of a source code block by doing things like
;; prepending argument definitions to the body, it should be called by the
;; `org-babel-execute:clingo' function below. Variables get concatenated in
;; the `mapconcat' form, therefore to change the formatting you can edit the
;; `format' form.
(defun org-babel-expand-body:clingo (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (require 'inf-clingo nil t)
  (let ((vars (org-babel--get-vars (or processed-params (org-babel-process-params params)))))
    (concat
     (mapconcat ;; define any variables
      (lambda (pair)
        (format "#const %s = %s."
                (car pair) (org-babel-clingo-var-to-clingo (cdr pair))))
      vars "\n")
     "\n" body "\n")))

;; This function will evaluate the body of the source code and
;; return the results as emacs-lisp
;; Here the output to STDOUT will be captured and returned
(defun org-babel-execute:clingo (body params)
  "Execute a block of Clingo code with org-babel.
   This function is called by `org-babel-execute-src-block'"
  (message "executing Clingo source code block")
  (let* ((processed-params (org-babel-process-params params))
         ;; set the -n parameter
         (models (cdr (assoc :n processed-params)))

	 ;; set the options parameter
	 (options (cdr (assoc :options processed-params)))

	 ;; set instance parameter
	 (instance (cdr (assoc :instance processed-params)))

         ;; variables assigned for use in the block
         (vars (org-babel--get-vars processed-params))
         (result-params (assq :result-params processed-params))

         ;; result is always OUTPUT
         ;(result-type "output")

         ;; expand the body with `org-babel-expand-body:clingo'
         (full-body (org-babel-expand-body:clingo body params processed-params))

         (temp-file (org-babel-temp-file "clingo-"))
         (clingo (executable-find "clingo"))
         (cmd (concat (shell-quote-argument (expand-file-name clingo))
                      (when models (concat " -n " (int-to-string models)))
		      (when options (concat " " options))
		      (when instance (concat " " (org-babel-process-file-name instance)))
                      " " (org-babel-process-file-name temp-file)
	      )
	 )
        )

        ;; body: uses the bound variables
        (unless (file-executable-p clingo)
          (error "Cannot find or execute `clingo', please check it is installed and in PATH"))

        ;; execute the source-code block by dropping it to a temporary file
        ;; and evaluating the file.
        ;;
        ;; ref. https://github.com/arnm/ob-mermaid
        (with-temp-file temp-file (insert full-body))
        (message "%s" cmd)
        (org-babel-my-eval cmd "")
  )
)

(defun org-babel-clingo-var-to-clingo (var)
  "Convert an elisp var into a string of clingo source code
   specifying a var of the same value."
  (format "%S" var))

;; same as `org-babel-my-eva', except only negative return code is considered
;; to be an error.
(defun org-babel-my-eval (command query)
  "Run COMMAND on QUERY.
Return standard output produced by COMMAND.  If COMMAND exits
with a negative code or produces error output, show it with
`org-babel-eval-error-notify'.

Writes QUERY into a temp-buffer that is processed with
`org-babel--shell-command-on-region'."
  (let ((error-buffer (get-buffer-create " *Org-Babel Error*")) exit-code)
    (with-current-buffer error-buffer (erase-buffer))
    (with-temp-buffer
      ;; Ensure trailing newline.  It is required for cmdproxy.exe.
      (insert query "\n")
      (setq exit-code (org-babel--shell-command-on-region command error-buffer))
      (let ((stderr (with-current-buffer error-buffer (buffer-string))))
        (if (or (not (numberp exit-code))
                (< exit-code 0)
                (not (string-empty-p stderr)))
            (progn
              (org-babel-eval-error-notify exit-code stderr)
              (save-excursion
                (when (get-buffer org-babel-error-buffer-name)
                  (with-current-buffer org-babel-error-buffer-name
                    (unless (derived-mode-p 'compilation-mode)
                      (compilation-mode))
                    ;; Compilation-mode enforces read-only, but
                    ;; Babel expects the buffer modifiable.
                    (setq buffer-read-only nil))))
              ;; Return output, if any.
              (buffer-string))
          (buffer-string)
	)
      )
    )
  )
)


(provide 'ob-clingo)
;;; ob-clingo.el ends here
