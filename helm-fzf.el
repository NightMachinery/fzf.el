;;; fzf.el/helm-fzf.el -*- lexical-binding: t; -*-
;;;
;; @forked from https://raw.githubusercontent.com/ibmandura/helm-fzf/master/helm-fzf.el
;;;
;; * @todo0 [[https://github.com/emacs-helm/helm/discussions/2450][Make `helm-source-async` split the output by `\0` instead of `\n` · Discussion #2450 · emacs-helm/helm · GitHub]]
;;;
(require 'helm)
(require 'helm-files)
(require 's)
(require 'dash)

(defun night/helm-clipboard-action (dummy)
  "@ref https://kitchingroup.cheme.cmu.edu/blog/2015/02/01/Handling-multiple-selections-in-helm/"
  (cl-loop for cand in (helm-marked-candidates)
        do
        (night/insert-from-clipboard cand)))

(defvar night/helm-fzf-source
  (helm-build-async-source "fzf"
    :candidates-process 'night/helm-fzf--do-candidate-process
    ;; :filter-one-by-one 'identity
    :requires-pattern 0 ;; Requiring at least a query with X characters
    :action #'night/helm-clipboard-action
    :multiline t
    :candidate-number-limit 9999))

(defun night/helm-fzf--do-candidate-process ()
  (let* ((cmd-args
          (-concat night/fzf-cmd (list (getenv "CLIPBOARD_RECORD_FILE")) night/fzf-cmd-args (list "-f" helm-pattern
                                                                                                  "--tac" "--tiebreak=index")))
         (proc (apply 'start-file-process "helm-fzf" helm-buffer cmd-args)))
    (prog1 proc
      (set-process-sentinel
       (get-buffer-process helm-buffer)
       ;; #'night/helm-fzf-mode--sentinel
       #'(lambda (process event)
           (helm-process-deferred-sentinel-hook
            process event (helm-default-directory)))
       ))))

(defun night/helm-clipboard ()
  (interactive)
  (let* ((default-directory "/")
        (helm-process-output-split-string-re " ")
        (night/fzf-cmd-args
         (-concat night/fzf-cmd-args (list "--exact" "--print0" "--read0"))))
    (helm :sources '(night/helm-fzf-source)
          :resume 'noresume
          :buffer "*helm-clipboard*")))

;;;
(defvar helm-process-output-split-string-re "\n")
(defun helm-output-filter--process-source (process output-string source limit)
  (cl-dolist (candidate
              (helm-transform-candidates
               (helm-output-filter--collect-candidates
                (split-string output-string helm-process-output-split-string-re)
                (assq 'incomplete-line source))
               source t))
    (setq candidate
          (helm--maybe-process-filter-one-by-one-candidate candidate source))
    (if (assq 'multiline source)
        (let ((start (point)))
          (helm-insert-candidate-separator)
          (helm-insert-match candidate 'insert-before-markers
                             (1+ (cdr (assq 'item-count source)))
                             source)
          (put-text-property start (point) 'helm-multiline t))
      (helm-insert-match candidate 'insert-before-markers
                         (1+ (cdr (assq 'item-count source)))
                         source))
    (cl-incf (cdr (assq 'item-count source)))
    (when (>= (assoc-default 'item-count source) limit)
      (helm-kill-async-process process)
      (helm-log-run-hook 'helm-async-outer-limit-hook)
      (cl-return))))
;;;
(provide 'night/helm-fzf)
