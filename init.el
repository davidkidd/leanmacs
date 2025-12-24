;;; init.el --- Lean fallback init (Emacs 30+) -*- lexical-binding: t; -*-
;;; Commentary:
;; A fast, lean alternative init using only built-ins.
;;
;; Features:
;; - Requires only Emacs 30+, no other dependencies
;; - Minor optional customisations (theme, font etc) 
;;; Code:

;; ---------------------------------------------------------------------
;; Optional settings (edit these)
;; ---------------------------------------------------------------------
(defconst my/lean-custom-dir-relative "custom"
  "Relative dir under `user-emacs-directory` for themes/custom files.
If nil, custom dir, custom-file, and theme are all disabled.")

(defconst my/lean-custom-file-name "custom.el"
  "Filename within the custom dir to use as `custom-file`.
If nil, custom-file loading is disabled (but custom dir/theme may still be used).")

(defconst my/lean-theme 'novarange
  "Theme symbol to load from `custom-theme-load-path`.
If nil, theme loading is disabled.") 

(defconst my/lean-fonts '("Ioskeley Mono" "FiraCode Nerd Font")
  "Fonts to try in order. First available is used. Nil disables font setting.")

(defconst my/lean-font-height 100
  "Default face height to apply if a font is found. 100 is roughly 10pt-ish.")

(defconst my/lean-strip-some-colours t
  "If non-nil, override certain faces to avoid clashing with themes.
This is opinionated and may partially override theme colours.")

(defconst my/lean-scratch-base
";; Shortcuts
;;
;; M-x or C-\\   Run command
;; C-|          Menu
;; C-g          Cancel anything
;; 
\n"
  "Base text shown in *scratch* for init-lean.")

;; Capture the file path *while this file is being loaded/eval'd*.
(defvar my/lean-init-file
  (or load-file-name buffer-file-name)
  "Absolute path of this init-lean file (captured at load/eval time).")

;; ---------------------------------------------------------------------
;; Keybinds (declared early, applied later)
;; ---------------------------------------------------------------------

(defconst my/lean-keybinds
  '(
    ;; Core
    ("C-\\"     . execute-extended-command)
    ("C-c C-a"  . mark-whole-buffer)
    ("C-c a"    . mark-whole-buffer)
    ("C-M-l"    . duplicate-dwim)
    ("C-M-h"    . my/mark-defun)

    ("M-0"      . fixup-whitespace)

    ("M-]"      . forward-paragraph)
    ("M-["      . backward-paragraph)

    ("M-o"      . other-window)
    ("C-c o"    . delete-other-windows)
    ("C-c 0"    . delete-window)

    ("C-c ]"    . next-buffer)
    ("C-c ["    . previous-buffer)
    ("C-c SPC"  . my/lean-switch-buffer-or-recent)
    ("C-c b"    . switch-to-buffer)

    ;; Word motion
    ("C-."      . forward-word)
    ("C-,"      . backward-word)

    ;; Kill buffer+window
    ("C-x K"    . kill-buffer-and-window)

    ;; Lean menu
    ("C-|"      . my/lean-menu)
    )
  "Global keybindings for init-lean.")

;; Some modes will take over incredibly useful keybinds.
;; Identify the useful keybinds here.
(defconst my/lean-key-override-blockers
  '("C-." "C-,")
  "Keys that should never be overridden by minor modes.")

;; And then list the culprits. The keybind police will use both
;; lists to lazily ensure the keybinds are not overridden.
;; To avoid surprises, supply mode name and the mode name map.
(defconst my/lean-key-override-culprits
  '((flyspell . flyspell-mode-map))
  "Alist of (FEATURE . KEYMAP-SYMBOL) to strip keys from after FEATURE loads.")

;; ---------------------------------------------------------------------
;; User customisation is over. Here be dragons.
;; ---------------------------------------------------------------------

(defmacro my/safely (label &rest body)
  "Run BODY; on error, report LABEL and continue."
  (declare (indent 1))
  `(condition-case err
       (progn ,@body)
     (error
      (message "[init-lean] %s: %s" ,label (error-message-string err))
      nil)))

(defun my/lean-msg (fmt &rest args)
  "Log to *Messages* only."
  (apply #'message (concat "[init-lean] " fmt) args))

(defun my/lean-open-init ()
  "Open this init-lean file."
  (interactive)
  (if (and (stringp my/lean-init-file) (file-exists-p my/lean-init-file))
      (find-file my/lean-init-file)
    (my/lean-msg "Can't locate init file (my/lean-init-file=%S)" my/lean-init-file)))

(defun my/lean-append-scratch (line)
  "Append LINE (a string) to *scratch*."
  (with-current-buffer (get-buffer-create "*scratch*")
    (let ((inhibit-read-only t))
      (when (= (buffer-size) 0)
        (insert my/lean-scratch-base))
      (save-excursion
        (goto-char (point-max))
        (insert line)
        (unless (string-suffix-p "\n" line)
          (insert "\n"))))))

(defun my/lean-open-messages ()
  "Open the *Messages* buffer."
  (interactive)
  (pop-to-buffer (messages-buffer)))

(defun my/lean-block-keys-in-keymap (keymap)
  "Remove `my/lean-key-override-blockers` bindings from KEYMAP."
  (when (keymapp keymap)
    (dolist (key my/lean-key-override-blockers)
      (define-key keymap (kbd key) nil))))

(my/safely "key override blockers"
  (dolist (pair my/lean-key-override-culprits)
    (let ((feature (car pair))
          (map-sym  (cdr pair)))
      (with-eval-after-load feature
        (when (boundp map-sym)
          (my/lean-block-keys-in-keymap (symbol-value map-sym)))))))

;; ---------------------------------------------------------------------
;; Basics (do this early so scratch is stable)
;; ---------------------------------------------------------------------

(my/safely "basic vars"
  (setq inhibit-startup-message t
        inhibit-startup-screen t
        ring-bell-function #'ignore
        delete-by-moving-to-trash t
        initial-scratch-message my/lean-scratch-base)
  (fset 'yes-or-no-p #'y-or-n-p)
  (setq-default cursor-type 'bar))

(my/safely "minor modes"
  (electric-pair-mode 1)
  (delete-selection-mode 1))

(defun duplicate-dwim ()
  "Duplicate current line, or active region if any."
  (interactive)
  (if (use-region-p)
      (let ((text (buffer-substring (region-beginning) (region-end))))
        (goto-char (region-end))
        (newline)
        (insert text))
    (save-excursion
      (let ((line (thing-at-point 'line t)))
        (end-of-line)
        (newline)
        (insert line)))))


;; ---------------------------------------------------------------------
;; UI chrome
;; ---------------------------------------------------------------------

(my/safely "disable chrome"
  (dolist (mode '(scroll-bar-mode tool-bar-mode menu-bar-mode))
    (when (fboundp mode)
      (funcall mode -1))))

;; ---------------------------------------------------------------------
;; Transient: launcher menu
;; ---------------------------------------------------------------------

(my/safely "require transient"
  (require 'transient))

;; Forward declare (defined later).
(declare-function my/lean-switch-buffer-or-recent "init-lean")

(transient-define-prefix my/lean-menu ()
  "Lean launcher."
  [["Open"
    ("f" "File"            find-file)
    ("d" "Directory"       dired)
    ("p" "Project"         project-switch-project)
    ("r" "Recent/buffer"   my/lean-switch-buffer-or-recent)
    ("m" "Messages"        my/lean-open-messages)
    ("i" "Init"  my/lean-open-init)]
   ["Window"
    ("v" "Split vertical"        split-window-right)
    ("h" "Split horizontal"      split-window-below)
    ("w" "Cycle windows"         other-window)
    ("0" "Close window"          delete-window)
    ("1" "Close other windows"   delete-other-windows)
    ("k" "Kill buffer"           kill-this-buffer)
    ("K" "Kill buffer + close"   kill-buffer-and-window)]
   ["Search"
    ("s" "Quick (i-search)"      isearch-forward)
    ("o" "Results (occur)"       occur)
    ("g" "Project (grep)"        project-find-regexp)]]
  [["Exit"
    ("q" "Close menu" transient-quit-one)
    ("QR" "Restart Emacs" restart-emacs)
    ("QQ" "Quit Emacs" save-buffers-kill-terminal)]])

(my/safely "lean transient keybind"
  (global-set-key (kbd "C-|") #'my/lean-menu))

;; ---------------------------------------------------------------------
;; Completion and Which-Key
;; ---------------------------------------------------------------------

(my/safely "completion"
  (fido-vertical-mode 1)
  (setq completion-styles '(flex basic)))

(my/safely "which-key"
  (require 'which-key)
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.05)
  (which-key-mode 1))

;; ---------------------------------------------------------------------
;; Searching
;; ---------------------------------------------------------------------

(my/safely "search defaults"
  (setq search-whitespace-regexp ".*"     ;; whitespace = “match anything”
        case-fold-search t
        isearch-lazy-highlight t
        lazy-highlight-cleanup t
        search-default-mode #'char-fold-to-regexp
        isearch-allow-scroll t))

;; ---------------------------------------------------------------------
;; Custom dir + custom-file + theme
;; ---------------------------------------------------------------------

(defvar my/lean-custom-had-error nil
  "Non-nil if any non-nil custom setting failed to load.")

(defconst my/custom-dir
  (and my/lean-custom-dir-relative
       (expand-file-name my/lean-custom-dir-relative user-emacs-directory))
  "Resolved custom directory, or nil if disabled.")

(defconst my/custom-file
  (and my/custom-dir my/lean-custom-file-name
       (expand-file-name my/lean-custom-file-name my/custom-dir))
  "Resolved custom file path, or nil if disabled.")

;; Informational messages when knobs are nil
(when (null my/lean-custom-dir-relative)
  (my/lean-msg "Custom dir: disabled (custom-file/theme will not be attempted)"))
(when (and my/lean-custom-dir-relative (null my/lean-custom-file-name))
  (my/lean-msg "Custom file: disabled"))
(when (and my/lean-custom-dir-relative (null my/lean-theme))
  (my/lean-msg "Theme: disabled"))

;; Custom dir is the root capability
(when my/custom-dir
  (if (file-directory-p my/custom-dir)
      (progn
        ;; Optional, but handy if you drop helper .el files in there.
        (add-to-list 'load-path my/custom-dir)
        (add-to-list 'custom-theme-load-path my/custom-dir))
    (setq my/lean-custom-had-error t)
    (my/lean-msg "Custom dir not found: %s" my/custom-dir)))

;; custom-file (only if custom dir exists on disk and knob is non-nil)
(when (and my/custom-dir (file-directory-p my/custom-dir) my/lean-custom-file-name)
  (setq custom-file my/custom-file)
  (if (and (stringp my/custom-file) (file-exists-p my/custom-file))
      (load custom-file nil 'nomessage)
    (setq my/lean-custom-had-error t)
    (my/lean-msg "Custom file not found: %s" my/custom-file)))

;; Theme (only if custom dir exists on disk and knob is non-nil)
(when (and my/custom-dir (file-directory-p my/custom-dir) my/lean-theme)
  (if (member my/lean-theme (custom-available-themes))
      (my/safely "load theme"
        (load-theme my/lean-theme t))
    (setq my/lean-custom-had-error t)
    (my/lean-msg "Theme not found or unavailable: %S" my/lean-theme)))

;; Scratch: append one generic notice if something (non-nil) failed.
(when my/lean-custom-had-error
  (my/lean-append-scratch ";; Problem loading custom data, check *Messages* buffer"))

;; ---------------------------------------------------------------------
;; Faces / completion buffer readability
;; ---------------------------------------------------------------------

(defun my/lean-apply-opinionated-faces ()
  "Apply opinionated face tweaks when enabled."
  (when my/lean-strip-some-colours
    (my/safely "apply opinionated faces"
      ;; *Completions* buffer
      (set-face-attribute 'completions-common-part nil
                          :foreground 'unspecified
                          :inherit 'default
                          :weight 'bold))))

(my/lean-apply-opinionated-faces)

;; ---------------------------------------------------------------------
;; Fonts (optional)
;; ---------------------------------------------------------------------

(defun my/font-setter (desired-fonts &optional height)
  "Set the first available font from DESIRED-FONTS for this session."
  (my/safely "font-setter"
    (when (and desired-fonts (listp desired-fonts))
      (let ((chosen nil))
        (dolist (name desired-fonts)
          (when (and (not chosen)
                     (stringp name)
                     (find-font (font-spec :name name)))
            (setq chosen name)))
        (when chosen
          (set-face-attribute 'default nil :font chosen :height (or height 100)))))))

(when my/lean-fonts
  (my/font-setter my/lean-fonts my/lean-font-height))


;; ---------------------------------------------------------------------
;; recentf + “buffers or recent files” switcher
;; ---------------------------------------------------------------------

(my/safely "recentf"
  (require 'recentf)
  (recentf-mode 1)
  (setq recentf-max-saved-items 200))

(my/safely "require seq"
  (require 'seq))

(defun my/lean-switch-buffer-or-recent ()
  "Switch to a buffer or open a recent file (built-in only)."
  (interactive)
  (my/safely "switch buffer/recent"
    (let* ((buffers (mapcar #'buffer-name (buffer-list)))
           (files (seq-filter #'file-exists-p recentf-list))
           (candidates
            (append
             (mapcar (lambda (b) (concat "[B] " b)) buffers)
             (mapcar (lambda (f) (concat "[F] " f)) files)))
           (choice (completing-read "Buffer or file: " candidates nil t)))
      (when (and (stringp choice) (not (string-empty-p choice)))
        (cond
         ((string-prefix-p "[B] " choice)
          (switch-to-buffer (substring choice 4)))
         ((string-prefix-p "[F] " choice)
          (find-file (substring choice 4))))))))

;; ---------------------------------------------------------------------
;; Keybind helpers
;; ---------------------------------------------------------------------

(defun my/mark-defun ()
  "mark-defun alternative using end/beginning-of-defun."
  (interactive)
  (end-of-defun)
  (push-mark (point) t t)
  (beginning-of-defun))

(defun my/lean-apply-keybinds ()
  "Apply `my/lean-keybinds` safely."
  (my/safely "apply keybinds"
    (dolist (pair my/lean-keybinds)
      (let ((key (kbd (car pair)))
            (fn  (cdr pair)))
        (when (fboundp fn)
          (global-set-key key fn))))))

;; Project prefix map (built-in)
(my/safely "project prefix map"
  (global-set-key (kbd "C-c p") project-prefix-map))

;; Finally, apply our keybinds now that everything is defined
(my/lean-apply-keybinds)

(my/lean-msg "Loaded successfully.")
(provide 'init-lean)

;;; init-lean.el ends here
