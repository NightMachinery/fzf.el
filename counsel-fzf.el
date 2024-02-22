;;; gitmodules/fzf.el/counsel-fzf.el -*- lexical-binding: t; -*-

(defvar counsel-recent-files-history nil
  "History for `counsel-recent-files-history'.")
(defvar counsel-fzf-history nil
  "General history for counsel fzf commands that have not used a more specific history.")
(defvar night/fzf-cmd (list (concat (file-name-directory load-file-name) "/fzf_in2.dash")))
(defvar night/fzf-cmd-args '())
(defvar night/counsel--fzf-entries nil)
;;;
(defun night/helper-counsel-fzf-entries (str)
  (let ((default-directory "/") ; TRAMP: The point is default-directory. If it is local, your command runs locally
        (entries night/counsel--fzf-entries))
    (cond
     ((equal entries "MAGIC_CLIPBOARD_READ")
      ;; (setq ivy--old-re (ivy--regex-fuzzy str)) ; too slow
      (setq ivy--old-re "")
      (counsel--async-command
       (-concat night/fzf-cmd (list (getenv "CLIPBOARD_RECORD_FILE")) night/fzf-cmd-args (list "-f" str "--read0" "--print0" "--tac" "--tiebreak=index")))
      )
     (t
      (setq ivy--old-re (ivy--regex-fuzzy str))
      (let ((night/counsel--stdin (mapconcat (lambda (x) x) entries "\n")))
        (f-write-text night/counsel--stdin 'utf-8 "/tmp/nightFzf.txt")
                                        ; @bug https://emacs.stackexchange.com/questions/63507/how-to-run-commands-locally-even-when-on-tramp
                                        ; The problem is that nightFzf.txt is created locally but the command runs on the remote server.
        (counsel--async-command
         (-concat night/fzf-cmd (list "/tmp/nightFzf.txt") night/fzf-cmd-args (list "-f" str))
         )))))
  nil)

(defun night/counsel-fzf-with-entries (entries &optional action prompt history)
  (interactive)
  (setq night/counsel--fzf-entries entries)
  (let (
        ;; (ivy-truncate-lines nil
        ;;                     ;; feel free to cancel this patch, I am not sure if it is a good idea
        ;;                     )
        )
    (ivy-read (or prompt "")
              #'night/helper-counsel-fzf-entries
              :initial-input ""
              ;; :re-builder #'ivy--regex-fuzzy
              :dynamic-collection t
              :history (or history 'counsel-fzf-history)
              :unwind #'counsel-delete-process
              :action (or action #'counsel-fzf-action)
              :caller 'counsel-fzf)))
;;;
(defun night/dir-list (dir &rest args)
  (interactive)
  (if (f-exists-p dir)
      (apply #'directory-files-recursively dir args)
    '()
    )
  )
;;;
(setq vfiles '())
(defun night/vfiles-init ()
  (interactive)
  (when (and (stringp (getenv "NIGHTDIR")))
    (setq vfiles (let
                     ;; `ec $textglob | sd -s '|' '\\|'`
                     ((re "\\.\\(txt\\|md\\|org\\|m\\|cpp\\|h\\|c\\|pl\\|applescript\\|as\\|osa\\|nu\\|nush\\|el\\|ss\\|scm\\|lisp\\|rkt\\|py\\|jl\\|scala\\|sc\\|kt\\|kotlin\\|java\\|clj\\|cljs\\|rkt\\|js\\|rs\\|zsh\\|dash\\|bash\\|sh\\|ml\\|php\\|lua\\|glsl\\|frag\\|go\\|ini\\|json\\|cson\\|toml\\|conf\\|plist\\|xml\\)$"))
                   (mapcar #'abbreviate-file-name ;; terrible @perf, but idk how to improve it. So for now, we can just call this manually every once in a while.
                           (-concat
                            (night/dir-list (getenv "NIGHTDIR") re)
                            (night/dir-list (getenv "DOOMDIR") re)
                            (night/dir-list (getenv "nightNotes") re)
                            (night/dir-list (concat (getenv "codedir") "/nodejs") re)
                            (night/dir-list (concat (getenv "codedir") "/julia") re)
                            (night/dir-list (concat (getenv "codedir") "/lua") re)
                            (night/dir-list (concat (getenv "codedir") "/python") re)
                            (night/dir-list (concat (getenv "codedir") "/uni") re)
                            (night/dir-list (concat (getenv "codedir") "/Clj") re)
                            (night/dir-list (concat (getenv "codedir") "/rust") re)
                            (night/dir-list (concat (getenv "codedir") "/golang") re)))))
    (nconc recentf-list vfiles)         ;; adds vfiles to the end of recentf-list
    (recentf-cleanup)
    (recentf-save-list)))

(defun night/fzf-recentf ()
  (interactive)
  (let ((ivy-truncate-lines nil))
    (night/counsel-fzf-with-entries
     ;; recentf-list
     (if vfiles
         (-concat recentf-list vfiles)
       (progn
         ;; (z bello)
         recentf-list))
     ;; vfiles
     (lambda (f) (progn
                   ;; (message "DBG: %s" f )
                   (find-file-existing f)))
     counsel-recent-files-history)))
;;;
;; @solvedBug https://github.com/abo-abo/swiper/issues/2830 previous ivy-read dynamic collection pollutes new calls to ivy-read : use `:unwind #'counsel-delete-process`
(defun night/fzf-M-x (&optional initial-input)
  "Ivy version of `execute-extended-command'.
Optional INITIAL-INPUT is the initial input in the minibuffer.
This function integrates with either the `amx' or `smex' package
when available, in that order of precedence."
  (interactive)
  ;; When `counsel-M-x' returns, `last-command' would be set to
  ;; `counsel-M-x' because :action hasn't been invoked yet.
  ;; Instead, preserve the old value of `this-command'.
  (setq this-command last-command)
  (setq real-this-command real-last-command)
  (let ((externs (counsel--M-x-externs)))
    (setq night/counsel--fzf-entries (or externs obarray))
    (ivy-read (counsel--M-x-prompt)
              ;; (or externs obarray)
              #'night/helper-counsel-fzf-entries
              :predicate (if externs
                             (lambda (x)
                               (not (get (intern x) 'no-counsel-M-x)))
                           (lambda (sym)
                             (and (commandp sym)
                                  (not (get sym 'byte-obsolete-info))
                                  (not (get sym 'no-counsel-M-x)))))
              :require-match t
              :history 'counsel-M-x-history
              :action #'counsel-M-x-action
              ;; :re-builder #'ivy--regex-fuzzy
              :dynamic-collection t
              :keymap counsel-describe-map
              :initial-input initial-input
              :unwind #'counsel-delete-process
              :caller 'counsel-M-x)))

;;;
(defvar counsel-clipboard-history nil
  "History for `night/counsel-clipboard'.")
(defun night/counsel-clipboard ()
  "Interactively paste. Multiple selections are, of course, possible (see ivy-mark). Use C-o to see other options including copying the selection."
  (interactive)
  (let ((night/fzf-cmd-args (-concat night/fzf-cmd-args (list "--exact")))
        (night/counsel--fzf-entries "MAGIC_CLIPBOARD_READ"))
    (ivy-read "Clipboard: "
              #'night/helper-counsel-fzf-entries
              :require-match t
              :history 'counsel-clipboard-history
              :action #'night/insert-from-clipboard
              :multi-action #'night/insert-multiple
              :dynamic-collection t
              :unwind #'counsel-delete-process
              ;; :caller 'counsel-register
              :caller 'night/counsel-clipboard)
    ))

(after! (counsel)
  (add-to-list 'counsel-async-split-string-re-alist '(night/counsel-clipboard . "\x00"))
  (add-to-list 'ivy-re-builders-alist '(night/counsel-clipboard . ivy--regex-plus))
  )

(defun night/evil-region-delete-on-visual-paste ()
  ;; See evil-visual-paste
  (when (evil-visual-state-p)
    ;; (z fsay hello)
    (evil-delete evil-visual-beginning
                 evil-visual-end
                 (evil-visual-type)
                 (unless evil-kill-on-visual-paste ?_))))

(defun night/insert-from-clipboard (input)
  (let ((items (if (listp input)
                   input
                 (list input)))
        (res ""))

    (night/evil-region-delete-on-visual-paste)

    (dolist (item items)
      (let ((parts (split-string item "" t)))
        (setq res (concat res (car parts)))))
    (insert-for-yank res)
    (kill-new res)
    (redraw-display) ;; good for emojis
    ))
(defun night/insert-multiple (items)
  (night/insert-from-clipboard
   items
   ;; (mapconcat (lambda (x) x) items "\n")
   )
  )
;; (map! :leader "zp" #'night/counsel-clipboard)
(with-eval-after-load 'night/helm-fzf
  (map!
   :nvig "C-p"
   ;; #'night/helm-clipboard
   ;; `night/helm-clipboard' was used before switching to consult.
   ;;
   ;; #'night/counsel-clipboard
   ;; #'helm-show-kill-ring       ;; does not capture OS copies
;;; @alt
   #'consult-yank-from-kill-ring

   :map evil-ex-completion-map
   :nvig "C-p"
   ;; #'night/helm-clipboard
   #'consult-yank-from-kill-ring
   ))
;; (map! :nvig "C-v" #'night/counsel-clipboard)

;;;
(provide 'counsel-fzf)
