(Co)mingling of Codeium for Emacs with Overlays

A fork of [codeium.el](https://github.com/Exafunction/codeium.el "codeium.el").  Please review that site it for install, usage, etc. If you think there is a bug with this implementation, feel free to send here.

This version replaces the Completion At Point Functions (CAPF) for Codeium served completions, with using overlays to present the completions.  This allows normal CAPF to be used for other completion-at-point routines from Corfu, Company, etc. while still seeing the inline completions with this mode enabled.

This also has [codeium.el PR #124](https://github.com/Exafunction/codeium.el/pull/124) included.

The default key bindings are:

  * `M-n`   comingle-next-completion
  * `M-p`   comingle-previous-completion
  * `M-<return>` comingle-accept-completion

Different from original, you must enable `comingle-mode` to start getting completions.  And still need to `comingle-init` prior, in order to get login, etc.

This is my init.el settings for this package that takes care of doing init work, as well as automatically starting `comingle-mode` in `prog-mode-hook` if enabled through `my/comingle-is-enabled` variable 

```emacs-lisp
(defvar my/comingle-is-enabled t)

(use-package comingle
  :after (pinentry) ;; for api-key in auth-source
  ;;:ensure t
  ;;:straight '(:type git :host github :repo "jeff-phil/comingle.el")
  ;;:vc (:fetcher github :repo "jeff-phil/comingle.el" :rev "main")
  :load-path "~/devel/emacs/comingle.el"
  :commands (my/comingle-toggle)
  :bind
  (("H-C" . 'my/comingle-toggle)
   ("C-c p C" . comingle-chat-open)) ;;  new chat option from PR
  :hook (prog-mode . (lambda ()
                       (when (bound-and-true-p my/comingle-is-enabled)
                         (comingle-mode))))
  :init
  (when (bound-and-true-p my/comingle-is-enabled)
    ;; optionally set a timer, which might speed up things as the
    ;; comingle local language server takes ~0.2s to start up
    (add-hook 'emacs-startup-hook
              (lambda ()
                (run-with-timer
                 0.2
                 nil
                 (lambda ()
                   (my/comingle-toggle))))))
  :config
  (defun my/comingle-toggle (&optional state)
    "Toggle comingle's enabled state."
    (interactive)
    (let ((current-state (or state comingle-state)))
      (cond
       ;; First condition: Is comingle currently running?
       ((and current-state (comingle-state-proc current-state))
        ;; -> Then, disable it. No `progn` needed.
        (comingle-reset)
        (message "Comingle disabled")
        (setq my/comingle-is-enabled nil))

       ;; if you don't want to use customize to save the api-key
       (t (setopt comingle/metadata/api_key
                  `,(auth-source-pass-get 'secret "ai/api_key@codeium.com"))
          ;; Reset only if it was already in a valid (but not running) state
          (when current-state
            (comingle-reset))
          (comingle-init)
          (message "Comingle enabled")
          (setq my/comingle-is-enabled t)))))

  (add-to-list 'comingle-language-alist '(emacs-lisp-mode . 60))

  (setq use-dialog-box nil) ;; do not use popup boxes

  ;; get comingle status in the modeline
  (setq comingle-mode-line-enable
        (lambda (api)
          (when (bound-and-true-p my/comingle-is-enabled)
            (not (memq api '(CancelRequest Heartbeat AcceptCompletion))))))
  (add-to-list 'mode-line-format '(:eval (car-safe comingle-mode-line)) t)
  ;; alternatively for a more extensive mode-line
  ;; (add-to-list 'mode-line-format '(-50 "" comingle-mode-line) t)

  ;; use M-x comingle-diagnose to see apis/fields that would be sent
  ;; to the local language server
  (setq comingle-api-enabled
        (lambda (api)
          (memq api '(GetCompletions GetProcesses GetHeartbeat CancelRequest GetAuthToken
                                     RegisterUser auth-redirect AcceptCompletion))))

  ;; You can overwrite all the comingle configs!
  ;; for example, we recommend limiting the string sent to comingle for better perf
  (defun my/comingle/document/text ()
    (buffer-substring-no-properties
     (max (- (point) 3000) (point-min))
     (min (+ (point) 1000) (point-max))))
  ;; if you change the text, you should also change the cursor_offset
  ;; warning: this is measured by UTF-8 encoded bytes
  (defun my/comingle/document/cursor_offset ()
    (comingle-utf8-byte-length
     (buffer-substring-no-properties (max (- (point) 3000) (point-min)) (point))))
  (setq comingle/document/text 'my/comingle/document/text)
  (setq comingle/document/cursor_offset 'my/comingle/document/cursor_offset))
```
