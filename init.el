;;; init-lean.el --- Lean fallback init (Emacs 30+) -*- lexical-binding: t; -*-
;;; Commentary:
;; A fast, lean alternative init using only built-ins.
;;; Code:

;; ---------------------------------------------------------------------
;; Options:
;; - Reasonable, opinionated defaults
;; - User-specific overrides are best placed in the custom file so this
;;   init can be updated from the repository with minimal merge conflicts
;;   (e.g. (setq my/lean-font-height-override 132))
;; ---------------------------------------------------------------------

(defvar my/lean-custom-dir-relative "custom"
  "Relative directory under `user-emacs-directory`.

If non-nil and the directory exists:
- it is used to locate the custom file specified by
  `my/lean-custom-file-name`
- it is added to `custom-theme-load-path`, allowing themes placed
  there to be loaded via `my/lean-theme`

If nil, custom dir and custom-file handling are disabled.")

(defvar my/lean-custom-file-name "custom.el"
  "Filename within the custom directory to use as `custom-file`.

If nil, custom-file loading is disabled (the custom directory and
theme loading may still be used).")

(defvar my/lean-theme 'tango-dark
  "Theme symbol to load via `load-theme`.

The theme may be built-in or located in any directory listed in
`custom-theme-load-path`. If nil, theme loading is disabled.")

(defvar my/lean-fonts
  '("DejaVu Sans Mono"
    "Liberation Mono"
    "Consolas"
    "Menlo"
    "Monaco"
    "FiraCode Nerd Font"
    "Fira Code"
    "JetBrains Mono")
  "Fonts to try in order. First available is used.")

(defvar my/lean-font-height-override nil
  "Optional global font height override (1/10 pt).
If nil, the implicit default of 100 is used.")

(defvar my/lean-strip-some-colours t
  "If non-nil, override certain faces to avoid clashing with themes.
This is opinionated and may partially override theme colours.")

(defvar my/lean-scratch-base
";; Shortcuts
;;
;; M-x or C-\\   Run command
;; C-|          Menu
;; C-g          Cancel anything
;;
\n"
  "Base text shown in *scratch* for init-lean.")

(defvar my/lean-keybinds
  '(
    ;; Core
    ("C-\\"     . execute-extended-command)
    ("C-c C-a"  . mark-whole-buffer)
    ("C-c a"    . mark-whole-buffer)
    ("C-M-l"    . duplicate-dwim)
    ("C-M-h"    . my/mark-defun)

    ("M-0"      . fixup-whitespace)

;;    ("M-]"      . forward-paragraph)
;;    ("M-["      . backward-paragraph)

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

(defvar my/lean-key-override-blockers
  '("C-." "C-,")
  "Keys that should never be overridden by minor modes.")

(defvar my/lean-key-override-culprits
  '((flyspell . flyspell-mode-map))
  "Alist of (FEATURE . KEYMAP-SYMBOL) to strip keys from after FEATURE loads.")

;; ---------------------------------------------------------------------
;; User customisation is over. Here be dragons.
;; ---------------------------------------------------------------------

(defvar my/lean-init-file
  (or load-file-name buffer-file-name)
  "Absolute path of this init file (captured at load/eval time).")

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

(defun my/lean-open-messages ()
  "Open the *Messages* buffer."
  (interactive)
  (pop-to-buffer (messages-buffer)))

(defun my/lean-append-scratch (text)
  "Append TEXT (a string) to *scratch*."
  (with-current-buffer (get-buffer-create "*scratch*")
    (let ((inhibit-read-only t))
      (when (= (buffer-size) 0)
        (insert my/lean-scratch-base))
      (save-excursion
        (goto-char (point-max))
        (insert text)
        (unless (string-suffix-p "\n" text)
          (insert "\n"))))))

(defvar my/lean-scratch-warned nil
  "Non-nil once we've appended the custom failure banner to *scratch*.")

(defun my/lean-scratch-warn-once (text)
  "Append TEXT to *scratch* once per session."
  (unless my/lean-scratch-warned
    (setq my/lean-scratch-warned t)
    (my/lean-append-scratch text)))

;; ---------------------------------------------------------------------
;; Custom dir + custom-file + theme (LOAD CUSTOM EARLY, SAFELY)
;;
;; Rule:
;; - location of custom dir/file comes from variables above
;; - values (my/lean-theme, my/lean-fonts, my/lean-scratch-base, etc.)
;;   may be overridden by custom.el before we use them elsewhere.
;; ---------------------------------------------------------------------

(defvar my/lean-custom-had-error nil
  "Non-nil if custom dir/file/theme had an error (missing/broken/unavailable).")

(defvar my/custom-dir nil
  "Resolved custom directory, or nil if disabled.")

(defvar my/custom-file nil
  "Resolved custom file path, or nil if disabled.")

(defun my/lean-disable-custom (why &optional err)
  "Disable custom dir/custom-file handling and report WHY. If ERR non-nil, log it too."
  (setq my/lean-custom-dir-relative nil
        my/lean-custom-file-name nil
        my/custom-dir nil
        my/custom-file nil
        my/lean-custom-had-error t)
  (if err
      (my/lean-msg "%s: %s" why (error-message-string err))
    (my/lean-msg "%s" why))
  (my/lean-scratch-warn-once
   ";; Custom file failed to load. Running with defaults.\n;; See *Messages* for details."))

;; Resolve paths from knobs
(setq my/custom-dir
      (and my/lean-custom-dir-relative
           (expand-file-name my/lean-custom-dir-relative user-emacs-directory)))

(setq my/custom-file
      (and my/custom-dir my/lean-custom-file-name
           (expand-file-name my/lean-custom-file-name my/custom-dir)))

;; Custom dir is the root capability: if enabled, it must exist.
(when my/custom-dir
  (if (file-directory-p my/custom-dir)
      (progn
        (add-to-list 'load-path my/custom-dir)
        (add-to-list 'custom-theme-load-path my/custom-dir))
    (my/lean-disable-custom (format "Custom dir not found: %s" my/custom-dir))))

;; Load custom-file early so it can override values before we use them.
(when (and my/custom-dir my/lean-custom-file-name)
  (setq custom-file my/custom-file)
  (cond
   ((not (and (stringp my/custom-file) (file-exists-p my/custom-file)))
    ;; Missing custom file is not a hard error, but we treat it as “custom disabled”.
    (my/lean-disable-custom (format "Custom file not found: %s" my/custom-file)))
   (t
    (condition-case err
        (load custom-file nil 'nomessage)
      (error
       (my/lean-disable-custom (format "Error loading custom file: %s" my/custom-file) err))))))

;; Informational messages AFTER custom-file, so they reflect final values.
(when (null my/lean-custom-dir-relative)
  (my/lean-msg "Custom dir: disabled"))
(when (and my/lean-custom-dir-relative (null my/lean-custom-file-name))
  (my/lean-msg "Custom file: disabled"))
(when (and my/lean-custom-dir-relative (null my/lean-theme))
  (my/lean-msg "Theme: disabled"))

;; If the custom dir exists, it can also serve as a theme dir, but theme
;; loading should not depend on it.
(when (and my/custom-dir (file-directory-p my/custom-dir))
  (add-to-list 'custom-theme-load-path my/custom-dir))

;; Theme (independent of custom-file/custom-dir)
(when my/lean-theme
  (my/safely "load theme"
    (load-theme my/lean-theme t)))

;; ---------------------------------------------------------------------
;; Keybind override blockers (flyspell etc.)
;; ---------------------------------------------------------------------

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
;; Basics (custom.el already loaded, so overridden values apply)
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

(declare-function my/lean-switch-buffer-or-recent "init-lean")

(transient-define-prefix my/lean-menu ()
  "Lean launcher."
  [["Open"
    ("f" "File"            find-file)
    ("d" "Directory"       dired)
    ("p" "Project"         project-switch-project)
    ("r" "Recent/buffer"   my/lean-switch-buffer-or-recent)
    ("m" "Messages"        my/lean-open-messages)
    ("i" "Init"            my/lean-open-init)]
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
  (setq search-whitespace-regexp ".*"
        case-fold-search t
        isearch-lazy-highlight t
        lazy-highlight-cleanup t
        search-default-mode #'char-fold-to-regexp
        isearch-allow-scroll t))

;; ---------------------------------------------------------------------
;; Faces / completion buffer readability
;; ---------------------------------------------------------------------

(defun my/lean-apply-opinionated-faces ()
  "Apply opinionated face tweaks when enabled."
  (when my/lean-strip-some-colours
    (my/safely "apply opinionated faces"
      (set-face-attribute 'completions-common-part nil
                          :foreground 'unspecified
                          :inherit 'default
                          :weight 'bold))))

(my/lean-apply-opinionated-faces)

;; ---------------------------------------------------------------------
;; Fonts (optional) (custom.el already loaded, so overrides work)
;; ---------------------------------------------------------------------

(defun my/lean-font-height ()
  "Return the configured font height for this session."
  (or (and (integerp my/lean-font-height-override)
           my/lean-font-height-override)
      100))

(defun my/font-setter (desired-fonts)
  "Set the first available font from DESIRED-FONTS for this session and log it."
  (my/safely "font-setter"
    (when (and desired-fonts (listp desired-fonts))
      (let ((chosen nil)
            (height (my/lean-font-height)))
        (dolist (name desired-fonts)
          (when (and (not chosen)
                     (stringp name)
                     (find-font (font-spec :name name)))
            (setq chosen name)))
        (if chosen
            (progn
              (set-face-attribute 'default nil :font chosen :height height)
              (my/lean-msg "Font selected: %s (height %d)" chosen height))
          (my/lean-msg "No preferred fonts found; leaving default font unchanged"))))))

(when my/lean-fonts
  (my/font-setter my/lean-fonts))

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

(my/safely "project prefix map"
  (global-set-key (kbd "C-c p") project-prefix-map))

(my/lean-apply-keybinds)

(my/lean-msg "Loaded successfully.")

;; ---------------------------------------------------------------------
;; TTY clipboard / mouse (Ghostty, etc.)
;; ---------------------------------------------------------------------

(defun my/lean--tty-setup ()
  "Robust TTY setup for xterm-style terminals (clipboard + mouse)."
  (unless (display-graphic-p)
    ;; 1) Force a known-good TERM. Don't try to be clever.
    (setenv "TERM" "xterm-256color")

    ;; 2) Tell Emacs up-front which xterm capabilities to assume.
    ;;    This must happen BEFORE terminal init / xterm setup.
    (setq xterm-extra-capabilities '(getSelection setSelection modifyOtherKeys))

    ;; 3) Ensure terminal init runs with the TERM we just forced.
    (tty-run-terminal-initialization (selected-frame) (getenv "TERM"))

    ;; 4) Mouse support
    (require 'xt-mouse)
    (xterm-mouse-mode 1)
    (mouse-wheel-mode 1)
    (setq mouse-wheel-scroll-amount '(3 ((shift) . 6)))

    ;; 5) Disable GUI-only pixel scrolling if it somehow got enabled
    (when (bound-and-true-p pixel-scroll-precision-mode)
      (pixel-scroll-precision-mode -1))))

;; Run at the correct lifecycle point for TTY frames:
(add-hook 'tty-setup-hook #'my/lean--tty-setup)

;; Also run once immediately (covers some startup orders):
(my/lean--tty-setup)


(provide 'init-lean)



;;; init-lean.el ends here
