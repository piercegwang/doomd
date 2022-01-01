;; [[file:config.org::*Simple Settings][Simple Settings:1]]
(setq-default
 delete-by-moving-to-trash t                      ; Delete files to trash
 window-combination-resize t                      ; take new window space from all other windows (not just current)
 x-stretch-cursor t)                              ; Stretch cursor to the glyph width

(setq undo-limit 80000000                         ; Raise undo-limit to 80Mb
      evil-want-fine-undo t                       ; By default while in insert all changes are one big blob. Be more granular
      auto-save-default t                         ; Nobody likes to loose work, I certainly don't
      truncate-string-ellipsis "…"                ; Unicode ellispis are nicer than "...", and also save /precious/ space
      password-cache-expiry nil                   ; I can trust my computers ... can't I?
      scroll-preserve-screen-position nil     ; Don't have `point' jump around
      scroll-margin 2)                            ; It's nice to maintain a little margin

(display-time-mode 1)                             ; Enable time in the mode-line

(unless (string-match-p "^Power N/A" (battery))   ; On laptops...
  (display-battery-mode 1))                       ; it's nice to know how much power you have

(global-subword-mode 1)                           ; Iterate through CamelCase words
;; Simple Settings:1 ends here

;; [[file:config.org::*Auto-customisations][Auto-customisations:1]]
(setq-default custom-file (expand-file-name ".custom.el" doom-private-dir))
(when (file-exists-p custom-file)
  (load custom-file))
;; Auto-customisations:1 ends here

;; [[file:config.org::*Windows][Windows:1]]
(setq evil-vsplit-window-right t
      evil-split-window-below t)
;; Windows:1 ends here

;; [[file:config.org::*Windows][Windows:2]]
(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-buffer))
;; Windows:2 ends here

;; [[file:config.org::*Windows][Windows:3]]
(setq +ivy-buffer-preview t)
;; Windows:3 ends here

;; [[file:config.org::*Windows][Windows:4]]
(map! :map evil-window-map
      "SPC" #'rotate-layout
      ;; Navigation
      "<left>"     #'evil-window-left
      "<down>"     #'evil-window-down
      "<up>"       #'evil-window-up
      "<right>"    #'evil-window-right
      ;; Swapping windows
      "C-<left>"       #'+evil/window-move-left
      "C-<down>"       #'+evil/window-move-down
      "C-<up>"         #'+evil/window-move-up
      "C-<right>"      #'+evil/window-move-right)
;; Windows:4 ends here

;; [[file:config.org::*Allow Local Variables][Allow Local Variables:1]]
(setq-default enable-local-variables t)
;; Allow Local Variables:1 ends here

;; [[file:config.org::*Bookmarks][Bookmarks:1]]
  (global-set-key (kbd "C-x r D") 'bookmark-delete)
;; Bookmarks:1 ends here

;; [[file:config.org::*=tab-bar-mode=][=tab-bar-mode=:1]]
  ;; (tab-bar-mode 1)
  (setq tab-bar-show nil)
;; =tab-bar-mode=:1 ends here

;; [[file:config.org::*Tab Switching Keybinds][Tab Switching Keybinds:1]]
  (global-set-key (kbd "s-{") (lambda () (interactive) (tab-next -1)))
  (global-set-key (kbd "s-}") (lambda () (interactive) (tab-next 1)))
;; Tab Switching Keybinds:1 ends here

;; [[file:config.org::*append-to-list][append-to-list:1]]
  (defun append-to-list (list-var elements)
    "Append ELEMENTS to the end of LIST-VAR.

  The return value is the new value of LIST-VAR."
    (unless (consp elements)
      (error "ELEMENTS must be a list"))
    (let ((list (symbol-value list-var)))
      (if list
          (setcdr (last list) elements)
        (set list-var elements)))
    (symbol-value list-var))
;; append-to-list:1 ends here

;; [[file:config.org::*Increment/Decrement Numbers][Increment/Decrement Numbers:1]]
  ;;; Increment Numbers
  (defun increment-number-at-point ()
    "Increments numbers at cursor"
    (interactive)
    (skip-chars-backward "0-9")
    (or (looking-at "[0-9]+")
        (error "No number at point"))
    (replace-match (number-to-string (1+ (string-to-number (match-string 0))))))

  ;;; Decrement Numbers
  (defun decrement-number-at-point ()
    "Decrements numbers at cursor"
    (interactive)
    (skip-chars-backward "0-9")
    (or (looking-at "[0-9]+")
        (error "No number at point"))
    (replace-match (number-to-string (1- (string-to-number (match-string 0))))))

  ;;; Binding
  (global-set-key (kbd "C-; C-=") 'increment-number-at-point)
  (global-set-key (kbd "C-; C--") 'decrement-number-at-point)
;; Increment/Decrement Numbers:1 ends here

;; [[file:config.org::*Insert Directory (obsolete?)][Insert Directory (obsolete?):1]]
  (defun insertdirectory ()
    "Insert current directory for macro use"
    (interactive)
    (insert default-directory))
;; Insert Directory (obsolete?):1 ends here

;; [[file:config.org::*Insert Org-mode Image][Insert Org-mode Image:1]]
  (defun insert-org-image (&optional swindow)
    "Prompt user for name of file, append time and date string, then use the Mac OSX `screencapture` feature to take a photo and place it in the relative ./figures directory."
    (interactive "P")
    (unless (not (memq system-type '(gnu gnu/linux darwin)))
           (let* ((outdir "figures")
                  (givenname (read-string "Enter File Name: "))
                  (namefile (concat (format-time-string "%Y%m%d_%H%M%S") (if (not (string= givenname "")) (concat "_" givenname) "") ".jpeg"))
                  (program (cond ((if (memq system-type '(gnu gnu/linux)) "gnome-screenshot" nil))
                                 ((if (eq system-type 'darwin) "screencapture" ""))))
                  (argument (cond ((string= program "screencapture") (if swindow "-w" "-i"))
                                  ((string= program "gnome-screenshot") (if swindow "-w" "-a"))))
                  (outfile (concat outdir "/" namefile)))
             (unless (file-directory-p outdir)
               (make-directory outdir t))
             (if (memq system-type '(gnu gnu/linux))
                 (setq outfile (concat "--file=" outfile)))
             (message "Program: %s\nArgument: %s\nOutfile: %s" program argument outfile)
             (call-process program nil nil nil argument outfile)
             (message namefile)
             (insert (concat (concat "[[file:./figures/" (file-name-nondirectory outfile)) "]]")))))
;; Insert Org-mode Image:1 ends here

;; [[file:config.org::*Keyboard Coding System][Keyboard Coding System:1]]
  (set-keyboard-coding-system nil)
;; Keyboard Coding System:1 ends here

;; [[file:config.org::*Dired Open File][Dired Open File:1]]
  (defun pgw/dired-open-file ()
    "In dired, open the file named on this line using the default application in the system."
    (interactive)
    (let ((file (dired-get-filename nil t)) ; Full path
          (filename (dired-get-filename t t))) ; File name for display
      (message "Opening %s..." filename)
      (cond ((memq window-system '(mac ns))
             (call-process "open" nil 0 nil file))
            ((memq window-system '(x))
             (call-process "xdg-open" nil 0 nil file)))
      (message "Opening %s done" filename)))
;; Dired Open File:1 ends here

;; [[file:config.org::*Copy MLA org-file][Copy MLA org-file:1]]
  (defun pgw/copy-mla-file ()
    "Copy MLA_OrgFile.org to current directory for use in school essays."
    (interactive)
    (copy-file "~/Dropbox/org/templates/school/MLA_OrgFile.org" default-directory)
    )
;; Copy MLA org-file:1 ends here

;; [[file:config.org::*Lookup in Dictionary (Apple)][Lookup in Dictionary (Apple):1]]
  (when (eq system-type 'darwin)
    (defun pgw/lookup-dictionary ()
      "Function to open a dictionary searching the highlighted word
  No spaces are allowed in the input of this function"
      (interactive)
      (let ((word (read-from-minibuffer "Word query: ")))
        (call-process "open" nil nil nil (concat "dict://" word)))
      )
    (global-set-key (kbd "M-#") 'pgw/lookup-dictionary)
    )
;; Lookup in Dictionary (Apple):1 ends here

;; [[file:config.org::*Test network (internet-up-p)][Test network (internet-up-p):1]]
  (defun internet-up-p (&optional host)
    (= 0 (call-process "ping" nil nil nil "-c" "1" "-W" "1"
                       (if host host "1.1.1.1"))))
;; Test network (internet-up-p):1 ends here

;; [[file:config.org::*Get org link][Get org link:1]]
  (defun pgw/org-get-link-at-point ()
    "Get the link from an org heading"
    (interactive)
    (let* ((context (org-element-context))
           (link (if (eq (car context) 'link)
                     (org-element-property :path context)
                   nil)))
      (if link (kill-new (concat (org-element-property :type context) ":" link)))))

  (global-set-key (kbd "C-c s-l") 'pgw/org-get-link-at-point)
;; Get org link:1 ends here

;; [[file:config.org::*Make-shell][Make-shell:1]]
  (defun make-shell (name)
    "Create a shell buffer named NAME."
    (interactive "sName: ")
    (setq name (concat "$" name))
    (eshell 4)
    (rename-buffer name))
;; Make-shell:1 ends here

;; [[file:config.org::*Concat with new lines][Concat with new lines:1]]
(defun concatnl (&rest SEQS)
  "Concatenate strings with new lines"
  (let ((return ""))
    (dolist (element SEQS return)
      (setq return (concat return "\n" element)))
    (substring return 1 nil)))
;; Concat with new lines:1 ends here

;; [[file:config.org::*Modifier Keys][Modifier Keys:1]]
  (when (eq system-type 'darwin)
    (with-no-warnings
      (setq mac-option-modifier 'meta)
      (setq mac-control-modifier 'control)
      (setq ns-function-modifier 'hyper)))

  (when (eq system-type 'gnu/linux)
    (with-no-warnings (setq x-super-keysym 'hyper)))
;; Modifier Keys:1 ends here

;; [[file:config.org::*Visuals][Visuals:1]]
  ;(load-theme 'tango-dark t)
  ;; (load-theme 'monokai)
  ;; ;
  ;; Frame
  (add-to-list 'default-frame-alist '(height . 46))
  (add-to-list 'default-frame-alist '(width . 146))

  ;;; Visual line mode (for text wrapping)
  (global-set-key (kbd "C-x v v") 'visual-line-mode)

  ;; Darkroom-tentative-mode
  (global-set-key (kbd "C-x v d") 'darkroom-tentative-mode)

  ;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
  ;; (setq ns-use-proxy-icon nil)
  ;; (setq frame-title-format nil)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)

  (setq visual-line-fringe-indicators '(left-curly-arrow hollow-square)) ;; '(left-curly-arrow right-curly-arrow) for both left and right
  ;; Testing freetonik's fringe indicator alist
  (setq-default fringe-indicator-alist '((truncation left-arrow right-arrow)
   (continuation nil right-arrow)
   (overlay-arrow . right-triangle)
   (up . up-arrow)
   (down . down-arrow)
   (top top-left-angle top-right-angle)
   (bottom bottom-left-angle bottom-right-angle top-right-angle top-left-angle)
   (top-bottom left-bracket right-bracket top-right-angle top-left-angle)
   (empty-line . empty-line)
   (unknown . question-mark)))
;; Visuals:1 ends here

;; [[file:config.org::*All the Icons][All the Icons:1]]
  (use-package! all-the-icons)
;; All the Icons:1 ends here

;; [[file:config.org::*Doom Theme][Doom Theme:1]]
  (use-package! doom-themes
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

    ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
    ;; may have their own settings.
    (load-theme 'doom-snazzy t)
    ;; (load-theme 'modus-operandi)

    ;; Enable flashing mode-line on errors
    (doom-themes-visual-bell-config)

    ;; Enable custom neotree theme (all-the-icons must be installed!)
    ;; (doom-themes-neotree-config)
    ;; or for treemacs users
    (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
    (doom-themes-treemacs-config)

    ;; Doom themes fontifies #hashtags and @at-tags by default.
    ;; To disable this:
    (setq doom-org-special-tags nil)

    ;; Corrects (and improves) org-mode's native fontification.
    (doom-themes-org-config)
    )
;; Doom Theme:1 ends here

;; [[file:config.org::*Frame Resize Pixelwise][Frame Resize Pixelwise:1]]
  (setq frame-resize-pixelwise t)
;; Frame Resize Pixelwise:1 ends here

;; [[file:config.org::*Line Numbers][Line Numbers:1]]
  ;; (global-visual-line-mode t)
  (setq display-line-numbers-type 'visual)
  ;; (setq-default display-line-numbers 'visual)
  (set-default 'truncate-lines t)
;; Line Numbers:1 ends here

;; [[file:config.org::*Window Management][Window Management:1]]
  (use-package! rotate)
;; Window Management:1 ends here

;; [[file:config.org::*Treemacs][Treemacs:1]]
  (use-package! treemacs)
  (use-package! treemacs-evil)
  (use-package! treemacs-magit)
;; Treemacs:1 ends here

;; [[file:config.org::*ztree (Tool for diffing and merging directories)][ztree (Tool for diffing and merging directories):1]]
  (use-package! ztree)
;; ztree (Tool for diffing and merging directories):1 ends here

;; [[file:config.org::*Doom fonts][Doom fonts:1]]
(setq doom-font (font-spec :family "Ubuntu Mono" :size 14 :height 1.0)
      doom-big-font (font-spec :family "Ubuntu Mono" :size 26)
      doom-variable-pitch-font (font-spec :family "Open Sans" :size 12)
      doom-unicode-font (font-spec :family "Ubuntu Mono")
      doom-serif-font (font-spec :family "Ubuntu"))
;; Doom fonts:1 ends here

;; [[file:config.org::*Mixed Pitch][Mixed Pitch:1]]
  (use-package! mixed-pitch
    :config
    ;; (set-face-attribute 'variable-pitch :height 160)
    (dolist (face '(line-number line-number-current-line org-list-dt org-link)) (add-to-list 'mixed-pitch-fixed-pitch-faces face))
    ;; (add-hook! 'text-mode-hook 'mixed-pitch-mode)
    (map! :leader
          :n "t m" 'mixed-pitch-mode)
    (set-face-attribute 'variable-pitch nil :height 0.8))
;; Mixed Pitch:1 ends here

;; [[file:config.org::*GPG][GPG:1]]
  ;; (require 'epa-file)
  (epa-file-enable)
  (setf epa-pinentry-mode 'loopback)
;; GPG:1 ends here

;; [[file:config.org::*Passwords][Passwords:1]]
  (load-file "~/.passwords.el")
;; Passwords:1 ends here

;; [[file:config.org::*Other Configuration][Other Configuration:1]]
  (size-indication-mode 1)
  (line-number-mode -1)
;; Other Configuration:1 ends here

;; [[file:config.org::#org][Org-mode:1]]
(after! org
  (map! :leader
        :map org-mode-map
        :n "m b t t" 'org-table-toggle-column-width
        :nv "m b y" 'org-table-copy-region
        :nv "m b p" 'org-table-paste-rectangle
        :nv "m b d y" 'org-table-cut-region)
  (setq org-directory "~/Dropbox/org"
        org-default-notes-file (concat org-directory "/inbox.org")
        org-use-property-inheritance t
        org-log-done 'time
        org-list-allow-alphabetical t
        org-export-in-background nil
        org-catch-invisible-edits 'smart
        org-export-with-sub-superscripts '{}
        org-babel-default-header-args
        '((:session . "none")
          (:results . "replace")
          (:exports . "code")
          (:cache . "no")
          (:noweb . "no")
          (:hlines . "no")
          (:tangle . "no")
          (:comments . "link")))
  (use-package! org-roam
    :hook (after-init . org-roam-mode)
    :config
    (map! :leader
          (:prefix-map ("r" . "Roam")
           :desc "Roam" "l" #'org-roam
           :desc "Roam Find File" "f" #'org-roam-find-file
           :desc "Roam Graph" "g" #'org-roam-graph
           :desc "Roam Capture" "c" #'org-roam-capture
           :desc "Roam Refresh" "!" #'pgw/org-roam-refresh
           :desc "Roam Insert" "i" #'org-roam-insert
           (:prefix ("d" . "Dailies")
            :desc "Capture Today" "." #'org-roam-dailies-capture-today
            :desc "Capture Yesterday" "h" #'org-roam-dailies-capture-yesterday
            :desc "Capture Tomorrow" "l" #'org-roam-dailies-capture-tomorrow
            :desc "Capture Date" "d" #'org-roam-dailies-capture-date
            :desc "Find Date" "/" #'org-roam-dailies-find-date
            :desc "Find Next Note" "L" #'org-roam-dailies-find-next-note
            :desc "Find Prev Note" "H" #'org-roam-dailies-find-previous-note)))
    (setq org-roam-directory "~/Dropbox/org-roam/"
          org-roam-db-location "~/Dropbox/org-roam/org-roam.db"
          org-roam-dailies-directory "daily/"
          org-roam-db-update-method 'immediate ;; could change later if it gets slow
          org-roam-tag-sources '(prop vanilla)
          org-roam-encrypt-files nil
          org-roam-graph-viewer "/Applications/Firefox.app/Contents/MacOS/firefox-bin"
          org-roam-dailies-capture-templates
          '(("j" "Journal" entry
             #'org-roam-capture--get-point
             "* %? :journal:\n:PROPERTIES:\n:LOGGED: %U\n:END:"
             :file-name "daily/daily-%<%Y-%m-%d>"
             :head "#+title: [%<%Y-%m-%d %a>]\n\n")
            ("s" "Sermon" plain
             #'org-roam-capture--get-point
             "* %? :sermon:faith:\n:PROPERTIES:\n:CATEGORY: faith\n:PASSAGE: \n:END:"
             :file-name "daily/daily-%<%Y-%m-%d>"
             :head "#+roam_tags: \n#+category: \n#+title: [%<%Y-%m-%d %a>]\n\n")
            ("c" "Conducting Lesson" plain
             #'org-roam-capture--get-point
             "* %? :conducting:"
             :file-name "daily/daily-%<%Y-%m-%d>"
             :head "#+title: [%<%Y-%m-%d>]\n\n")
            ("v" "Violin" entry
             #'org-roam-capture--get-point
             "* %?\n:PROPERTIES:\n:CATEGORY: %^{Category}\n:END:"
             :file-name "daily/daily-%<Y-%m-%d>"
             :head "#+title: [%<%Y-%m-%d>]\n\n"
             :olp ("Violin")))
          org-roam-capture-templates
          '(("d" "Default" plain
             #'org-roam-capture--get-point
             "%?"
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+title: ${title}\n#+roam_tags:\n#+category: \n"
             :unnarrowed t)
            ("t" "Temporary" plain
             (function org-roam-capture--get-point)
             :file-name "temporary/%<%Y%m%d%H%M%S>-${slug}"
             :head "#+title: ${title}\n#+author: %(concat user-full-name)\n#+email: %(concat user-mail-address)\n#+created: %(format-time-string \"[%Y-%m-%d %H:%M]\")\n#+roam_tags:\n\n%?")
            ("e" "Entry" entry
             #'org-roam-capture--get-point
             "* %?\n%U\n"
             :file-name "%<%Y%m%d%H%M%S>-${slug}"
             :head "#+roam_tags: \n#+title: ${title}\n#+category: \n"
             :unnarrowed t)))
    (org-roam-mode 1)
    (defun pgw/org-roam-refresh ()
      (interactive)
      (org-roam-db-build-cache :force)
      (org-roam-buffer--update-maybe :redisplay)))
    (setq org-todo-keywords
          '((sequence "NEXT(n)" "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)" "DELEGATED(g)")))
  (setq org-tag-persistent-alist '(("noexport" . ?N))
        org-complete-tags-always-offer-all-agenda-tags nil)
  (setq org-log-done 'time) ; Log when task marked as done
  (setq pgw/refile-targets (file-expand-wildcards "~/Dropbox/org/*.org"))
  (setq org-refile-targets '((nil :maxlevel . 9)
                             (org-agenda-files :maxlevel . 9)
                             (pgw/refile-targets :maxlevel . 9)))
  (setq org-refile-use-outline-path 'file)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes 'confirm)
  ;; org-agenda-auto-exclude-function
  ;; (defun pgw/org-my-auto-exclude-function (tag)
  ;;   (if
  ;;       (string= tag "officehours")
  ;;       (concat "-" tag)))
  ;; (setq org-agenda-auto-exclude-function 'pgw/org-my-auto-exclude-function)
  
  ;(setq org-agenda-overriding-columns-format "%28ITEM %TODO %SCHEDULED %DEADLINE %TAGS")
  
  ;; Re-align tags when window shape changes
  (add-hook! 'org-agenda-mode-hook
            (lambda () (add-hook! 'window-configuration-change-hook 'org-agenda-align-tags nil t)))
  
  ;(add-hook! 'org-agenda-finalize-hook
  ;   'org-agenda-align-tags)
  
  (setq org-deadline-warning-days 7)
  
  (add-hook! 'org-agenda-finalize-hook
            (lambda ()
              (display-line-numbers-mode -1)
              ))
  
  ;; Org entries
  (setq org-agenda-max-entries nil)
  
  
  (after! org
    (map! :map evil-org-agenda-mode-map "SPC m l" #'org-agenda-log-mode))
    (setq org-agenda-custom-commands
          '(("c" . "Columbia")
            ("cf" . "Columbia Friend Schedules")
            ("cfe" "Ellie's Schedule" agenda ""
             ((org-agenda-span 7)
              (org-agenda-files
               (file-expand-wildcards "~/Dropbox/org/notes/columbia/2021_fall/calendar/2021_fall_ellie.org"))))
            ("cfk" "Kaeon's Schedule" agenda ""
             ((org-agenda-span 7)
              (org-agenda-files
               (file-expand-wildcards "~/Dropbox/org/notes/columbia/2021_fall/calendar/2021_fall_kaeon.org"))))
            ("l" "Logging View" agenda ""
             ((org-agenda-span 1)
              (org-agenda-files
               (file-expand-wildcards "~/Dropbox/org/*.org"))))
            ("A" "General Agenda" agenda ""
             ((org-agenda-span 1)
              (org-agenda-sorting-strategy
               '((agenda habit-down time-up deadline-up)))))
            ("D" "College Deadlines" tags-todo "+collegeapps")
            ("Q" . "Custom queries")
            ("Qa" "Query all (Archive included)" search ""
             ((org-agenda-files (append (file-expand-wildcards (concat org-directory "/*.org"))
                                        (file-expand-wildcards (concat org-directory "/*.org_archive"))))))
            ("Ql" "Query Links" search ""
             ((org-agenda-files (list (concat org-directory "/links.org")
                                      (concat org-directory "/links.org_archive")))))))
  (setq org-agenda-files (append (file-expand-wildcards "~/Dropbox/org/*.org")
                                 (file-expand-wildcards "~/Dropbox/org/*.org.gpg")
                                 (file-expand-wildcards "~/Dropbox/org/calendars/*.org")))
  
  (defun pgw/org-agenda-reload-files ()
    (interactive)
    (setq org-agenda-files (append (file-expand-wildcards "~/Dropbox/org/*.org")
                                   (file-expand-wildcards "~/Dropbox/org/*.org.gpg")
                                   (file-expand-wildcards "~/Dropbox/org/calendars/*.org"))))
    (setq org-agenda-time-grid '((daily today require-timed)
                                 (600 800 1000 1200 1400 1600 1800 2000 2200)
                                 "......" "----------------"))
  (defun pgw/year-month ()
    "Custom function to return date in format: YYYY-MM"
    (format-time-string "%Y-%m"))
  
  (defun pgw/U ()
    "Custom function to return date in org inactive timestamp format"
    (format-time-string "[%Y-%m-%d %a]"))
  
  (defun pgw/add-12 ()
    "Custom function return active org timestamp with exactly 24 hour difference"
    (format-time-string "%Y-%m-%d %a %H:%M" (time-add (current-time) 85500)))
  
  (defun pgw/headline_date ()
    "Function to find the date as headline for Violin capture template"
    (goto-char (point-min))
    (let ((searchresults (search-forward (format-time-string "[%Y-%m-%d %a]") nil t)))
      (if searchresults
          'searchresults
        (error "Not found! Use Vc to create today's practice first."))))
  (setq org-capture-templates
        (doct '(("Inboxes" :keys "i"
                 :file "~/Dropbox/org/inbox.org"
                 :type entry
                 :template ("* %?")
                 :children (("Flexible Entry" :keys "i")
                            ("Todo" :keys "t"
                             :template ("* TODO %?"))
                            ("Notes Entry" :keys "n"
                             :file "~/Dropbox/org/notes.org"
                             :template ("* %?"
                                        "%U"))
                            ("Link" :keys "l"
                             :file "~/Dropbox/org/links.org"
                             :headline "!Inbox"
                             :prepend t
                             :template ("* [[%?%:link][%:description]]"
                                        "%U"))))
                ("Finances" :keys "f"
                 :file "~/Dropbox/org/finances.org.gpg"
                 :children (("Credit Card Transaction" :keys "c"
                             :headline "Nordstrom Card"
                             :type table-line
                             :table-line-pos "III-1"
                             :template ("| | %? | |"))))
                ("Events" :keys "e"
                 :type entry
                 :children (("Emacs Entry (Not Synced)" :keys "f"
                             :file "~/Dropbox/org/events.org")
                            ("Emacs Calendar" :keys "e"
                             :file "~/Dropbox/org/calendars/cal_emacs.org"
                             :template ("* %^{Title of event}"
                                        "SCHEDULED: %^{Scheduled time + duration}T"
                                        ":PROPERTIES:"
                                        ":calendar-id: ihfv2u5n9uf5ksj5484vbe7mj4@group.calendar.google.com"
                                        ":END:"
                                        ":org-gcal:%?"
                                        ":END:"))
                            ("Emacs Calendar" :keys "g"
                             :file "~/Dropbox/org/calendars/cal_gmail.org"
                             :template ("* %^{Title of event}"
                                        "SCHEDULED: %^{Scheduled time + duration}T"
                                        ":PROPERTIES:"
                                        ":calendar-id: pierce.g.wang@gmail.com"
                                        ":END:"
                                        ":org-gcal:%?"
                                        ":END:"))))
                ("Stuff and Things" :keys "s"
                 :file "~/Dropbox/org/notes/stuff_and_things/organizing_temp.org"
                 :children (("Database Entry" :keys "i"
                             :type entry
                             :template ("* DECIDE %?"
                             "%U"))
                            ("Packing for College" :keys "p"
                             :type entry
                             :file "~/Dropbox/org-roam/temporary/20210805114431-packing_for_college.org"
                             :contexts (:in-file "20210805114431-packing_for_college.org")
                             :template ("* DONE Item"
                                        "%^{TYPE}p"
                                        "%^{QUANTITY}p"
                                        "%^{COLOR}p"
                                        "%^{FIT}p"
                                        "%^{NOTES}p")
                             :children (("Shirts" :keys "s"
                                         :headline "Shirts")
                                        ("Pants" :keys "p"
                                         :headline "Pants")
                                        ("Other" :keys "o"
                                         :headline "Other")))
                            ("Violin Repertoire" :keys "m"
                             :type entry
                             :id "e48fe999-3716-425b-8445-fce296c7635a"
                             :contexts (:in-file "repertoire.org")
                             :template ("* - %?"
                                        "%^{COMPOSER}p"
                                        "%^{ARRANGEMENT}p")))))))
  
    ;; Set to the name of the file where new notes will be stored
    (setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/index.org")
    ;; Set to <your Dropbox root directory>/MobileOrg.
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
    (use-package! org-crypt
      :config
      (org-crypt-use-before-save-magic)
      (setq org-tags-exclude-from-inheritance (quote ("crypt")))
  
      (setq org-crypt-key "3C44F187958295E4")
      ;; GPG key to use for encryption
      ;; Either the Key ID or set to nil to use symmetric encryption.
  
      (setq auto-save-default nil)
      ;; Auto-saving does not cooperate with org-crypt.el: so you need
      ;; to turn it off if you plan to use org-crypt.el quite often.
      ;; Otherwise, you'll get an (annoying) message each time you
      ;; start Org.
  
      ;; To turn it off only locally, you can insert this:
      ;;
      ;; # -*- buffer-auto-save-file-name: nil; -*-
      :bind (("C-c s-d e" . org-encrypt-entry)
             ("C-c s-d d" . org-decrypt-entry))
      )
    (with-eval-after-load 'org
      (org-babel-do-load-languages 'org-babel-load-languages
                                   '((python . t)
                                     )))
    ;;; org-drill
    (use-package! org-drill)
    (require 'ox-latex)
    (use-package! cdlatex
      :after org
      :config
      (add-hook! 'org-mode-hook #'org-cdlatex-mode)
      (add-to-list 'org-tab-first-hook 'org-try-cdlatex-tab)
      )
    (setq org-format-latex-options
          ;; '(:foreground "#000000" :background default ;; light theme
          '(:foreground "#d6d6d4" :background default ;; dark theme
                        :scale 1.1
                        :html-foreground "Black" :html-background "Transparent"
                        :html-scale 1.0
                        :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))
    (let ((dvipng--plist (alist-get 'dvipng org-preview-latex-process-alist)))
      (plist-put dvipng--plist :use-xcolor t)
      (plist-put dvipng--plist :image-converter '("dvipng -D %D -T tight -o %O %f")))
    (global-set-key (kbd "C-c C-x C-l") 'org-toggle-latex-fragment)
    (use-package! org-superstar
      :config
      (setq org-superstar-prettify-item-bullets t)
      :hook (org-mode . org-superstar-mode))
  
    (require 'ox-publish)
    (setq org-publish-project-alist
          '(("pages-notes"
             :base-directory "~/Dropbox/org_publish/"
             :base-extension "org"
             :publishing-directory "~/Documents/Projects/Github/github_pages/"
             :recursive t
             :publishing-function org-html-publish-to-html
             :headline-levels 4             ; Just the default for this project.
             ;; :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/style.css\"/>"
             :auto-preamble t
             )
            ("pages-static"
             :base-directory "~/Dropbox/org_publish/"
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|jpeg"
             :publishing-directory "~/Documents/Projects/Github/github_pages/"
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("pages" :components ("pages-notes" "pages-static"))
            ))
    (setq org-html-validation-link nil)
    (use-package! org-noter
      :after org
      :ensure t
      :config
      (setq org-noter-default-notes-file-names '("notes.org")
            org-noter-notes-search-path '("~/Dropbox/org/notes"))
      )
  (use-package! org-gcal
    :bind (("C-c s-g p" . org-gcal-post-at-point)
           ("C-c s-g s" . org-gcal-sync)
           ("C-c s-g f" . org-gcal-fetch)
           ("C-c s-g d" . org-gcal-delete-at-point)
           ("C-c s-g b s" . org-gcal-sync-buffer)
           ("C-c s-g b f" . org-gcal-sync-buffer))
    :config
    (setq org-gcal-client-id pgw/org-gcal-client-id
          org-gcal-client-secret pgw/org-gcal-client-secret
          org-gcal-file-alist pgw/org-gcal-file-alist
          org-gcal-local-timezone "America/New_York"
          org-gcal-notify-p nil)
    (setq org-gcal-remove-api-cancelled-events t))
    (setq org-reveal-root "file:///Users/piercewang/Documents/projects/revealjs/reveal.js-4.1.0")
    (use-package! darkroom)
  )
;; Org-mode:1 ends here

;; [[file:config.org::*org-gcal: Calendar Integration][org-gcal: Calendar Integration:1]]
(use-package! org-gcal
  :bind (("C-c s-g p" . org-gcal-post-at-point)
         ("C-c s-g s" . org-gcal-sync)
         ("C-c s-g f" . org-gcal-fetch)
         ("C-c s-g d" . org-gcal-delete-at-point)
         ("C-c s-g b s" . org-gcal-sync-buffer)
         ("C-c s-g b f" . org-gcal-sync-buffer))
  :config
  (setq org-gcal-client-id pgw/org-gcal-client-id
        org-gcal-client-secret pgw/org-gcal-client-secret
        org-gcal-file-alist pgw/org-gcal-file-alist
        org-gcal-local-timezone "America/New_York"
        org-gcal-notify-p nil)
  (setq org-gcal-remove-api-cancelled-events t))
;; org-gcal: Calendar Integration:1 ends here

;; [[file:config.org::*Company mode][Company mode:1]]
(setq company-idle-delay nil) ;; original 0.2
;; Company mode:1 ends here

;; [[file:config.org::*LaTeX][LaTeX:1]]
  (setq TeX-engine 'xetex)
  (setq latex-run-command "xetex")
;; LaTeX:1 ends here

;; [[file:config.org::*AUCTEX][AUCTEX:1]]
  (use-package! tex
    :ensure auctex
    :defer t
    :config
    (setq TeX-auto-save t))
;; AUCTEX:1 ends here

;; [[file:config.org::*Classes - Adding Academic XeTeX Times New Roman Class][Classes - Adding Academic XeTeX Times New Roman Class:1]]
(after! ox-latex
  (add-to-list 'org-latex-classes
               '("Times"
                 "\\documentclass[12pt]{article}
\\usepackage{fontspec}
\\setmainfont{Times New Roman}
\\usepackage{hyperref}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))
;; Classes - Adding Academic XeTeX Times New Roman Class:1 ends here

;; [[file:config.org::*Macro Query][Macro Query:1]]
  (defun my-macro-query (arg)
    "Prompt for input using minibuffer during kbd macro execution.
  With prefix argument, allows you to select what prompt string to use.
  If the input is non-empty, it is inserted at point."
    (interactive "P")
    (let* ((query (lambda () (kbd-macro-query t)))
           (prompt (if arg (read-from-minibuffer "PROMPT: ") "Input: "))
           (input (unwind-protect
                      (progn
                        (add-hook! 'minibuffer-setup-hook query)
                        (read-from-minibuffer prompt))
                    (remove-hook 'minibuffer-setup-hook query))))
      (unless (string= "" input) (insert input))))
  (global-set-key "\C-xQ" 'my-macro-query)
;; Macro Query:1 ends here

;; [[file:config.org::*Magit][Magit:1]]
  (use-package! magit
    :config
    (global-set-key (kbd "C-x g") 'magit-status))
;; Magit:1 ends here

;; [[file:config.org::*Backups][Backups:1]]
  (setq backup-directory-alist '(("." . "~/org/backup"))
    backup-by-copying t    ; Don't delink hardlinks
    version-control t      ; Use version numbers on backups
    delete-old-versions t  ; Automatically delete excess backups
    kept-new-versions 20   ; how many of the newest versions to keep
    kept-old-versions 5    ; and how many of the old
    )
;; Backups:1 ends here

;; [[file:config.org::*Daemon][Daemon:1]]
  ;;(if 'server-process
  ;;    (server-start))
  (load "server")
  (unless (server-running-p) (server-start))
;; Daemon:1 ends here

;; [[file:config.org::*Revert Mode][Revert Mode:1]]
  (global-auto-revert-mode 1)
;; Revert Mode:1 ends here

;; [[file:config.org::*Calendar][Calendar:1]]
(setq calendar-latitude 37.759995)
(setq calendar-longitude -122.427046)
(setq calendar-location-name "San Francisco, CA")
;; Calendar:1 ends here

;; [[file:config.org::*Date Style][Date Style:1]]
(calendar-set-date-style 'iso)
;; Date Style:1 ends here

;; [[file:config.org::*Artist Mode][Artist Mode:1]]
(add-hook! 'artist-mode-hook
          (lambda ()
            (display-line-numbers-mode -1)
            (evil-emacs-state)
            (local-set-key (kbd "<f1>") 'artist-select-op-poly-line)
            (local-set-key (kbd "<f2>") 'artist-select-op-pen-line)
            (local-set-key (kbd "<f3>") 'artist-select-op-line)
            (local-set-key (kbd "<f4>") 'artist-select-op-square)
            (local-set-key (kbd "<f5>") 'artist-select-op-ellipse))
          )
;; Artist Mode:1 ends here

;; [[file:config.org::*Image Mode][Image Mode:1]]
  (add-hook! 'image-mode-hook
            (lambda ()
              (display-line-numbers-mode -1)
              (evil-emacs-state))
            )
;; Image Mode:1 ends here

;; [[file:config.org::*Flyspell mode][Flyspell mode:1]]
  (defun pgw/turn-on-flyspell-hook ()
    (if (or (string-match "^/Users/piercewang/Google Drive/OHS/" (if (eq buffer-file-name nil) "" buffer-file-name))
            (string-match "^/Users/piercewang/Dropbox/org/notes/college/" (if (eq buffer-file-name nil) "" buffer-file-name)))
        (flyspell-mode 1)))

  (add-hook! 'org-mode-hook 'turn-on-flyspell)
;; Flyspell mode:1 ends here

;; [[file:config.org::*Calc][Calc:1]]
(evil-set-initial-state 'calc-mode 'emacs)
;; Calc:1 ends here

;; [[file:config.org::*Tetris][Tetris:1]]
(use-package! tetris
  :bind (:map tetris-mode-map
         ("z" . tetris-rotate-prev)
         ("x" . tetris-rotate-next)
         ("k" . tetris-move-bottom)
         ("h" . tetris-move-left)
         ("j" . tetris-move-down)
         ("l" . tetris-move-right)))
;; Tetris:1 ends here

;; [[file:config.org::*2048][2048:1]]
(use-package! 2048-game
  :bind (:map 2048-mode-map
              ("h" . 2048-left)
              ("j" . 2048-down)
              ("k" . 2048-up)
              ("l" . 2048-right)))
;; 2048:1 ends here

;; [[file:config.org::*ERC][ERC:1]]
  (setq erc-log-channels-directory "~/logs/")
  (setq erc-save-buffer-on-part t)
  (global-set-key (kbd "H-M-e") (lambda () (interactive) (erc :server "irc.freenode.net" :port 6667 :nick "tesrodome" :password passwords_ERC)))
;; ERC:1 ends here

;; [[file:config.org::*Keybinds][Keybinds:1]]
  ;;; replace-regexp
  (global-set-key (kbd "C-M-$") 'replace-regexp)

  ;;; Open .emacs.d
  (global-set-key (kbd "H-C-M-e") (lambda () (interactive) (dired "~/.emacs.d/")))

  ;;; Regular find-file
  (global-set-key (kbd "H-C-x o") (lambda () (interactive) (switch-to-buffer "*Org Agenda*")))


  ;;; Close window
  (global-set-key (kbd "s-0") 'delete-window)
;; Keybinds:1 ends here

;; [[file:config.org::*Keybinds][Keybinds:2]]
  (global-set-key (kbd "<f8>") 'insert-org-image)
;; Keybinds:2 ends here

;; [[file:config.org::*which-key][which-key:1]]
(use-package! which-key
  :config
  (which-key-mode)
  (setq which-key-popup-type 'side-window)
  (setq which-key-side-window-location 'bottom)
  (setq which-key-idle-delay 2.5))
;; which-key:1 ends here

;; [[file:config.org::*Line Moving][Line Moving:1]]
(map! :n "j" 'next-line
      :n "k" 'previous-line)
;; Line Moving:1 ends here

;; [[file:config.org::*Hydra for Resizing Windows][Hydra for Resizing Windows:1]]
  (defhydra hydra-windowmanage (global-map "H-c ^")
    "Hydra for window management."
    ("=" enlarge-window "+Vertical")
    ("-" (enlarge-window -1) "-Vertical")
    ("]" enlarge-window-horizontally "+Horizontal")
    ("[" shrink-window-horizontally "-Horizontal")
    ("q" nil "Quit"))

  (global-set-key (kbd "C-c C-6") 'hydra-windowmanage/body)
;; Hydra for Resizing Windows:1 ends here

;; [[file:config.org::*User Configuration][User Configuration:1]]
(setq user-full-name "Pierce Wang"
      user-mail-address "pierce.g.wang@gmail.com")
;; User Configuration:1 ends here

;; [[file:config.org::*Dired][Dired:1]]
  (setq delete-by-moving-to-trash t)
  (setq trash-directory "~/.Trash")
  (setq insert-directory-program "/usr/local/bin/gls"
        dired-use-ls-dired t)
;; Dired:1 ends here

;; [[file:config.org::*Dired][Dired:2]]
  (setq dired-dwim-target t)
;; Dired:2 ends here

;; [[file:config.org::*Dired][Dired:3]]
  (define-key dired-mode-map (kbd "P") nil)
;; Dired:3 ends here

;; [[file:config.org::*Dired][Dired:4]]
  (define-key dired-mode-map (kbd "O") 'pgw/dired-open-file)
;; Dired:4 ends here

;; [[file:config.org::*Dired][Dired:5]]
  (define-key dired-mode-map (kbd "Y") 'dired-do-symlink)
;; Dired:5 ends here

;; [[file:config.org::*Human readable format for ls switches (=-h=)][Human readable format for ls switches (=-h=):1]]
  (setq dired-listing-switches "-alh")
  (setq dired-actual-switches "-alh")
;; Human readable format for ls switches (=-h=):1 ends here

;; [[file:config.org::*browse-url-firefox-program][browse-url-firefox-program:1]]
  (setq browse-url-firefox-program "/Applications/Firefox.app/Contents/MacOS/firefox-bin")
;; browse-url-firefox-program:1 ends here

;; [[file:config.org::*Generate Class Calendar][Generate Class Calendar:1]]
(defun pgw/date-block (absolute y1 m1 d1 y2 m2 d2)
  "Block date entry. An adapted version of the `diary-block'
function from the diary-lib."
  (let ((date1 (calendar-absolute-from-gregorian
                (diary-make-date y1 m1 d1)))
        (date2 (calendar-absolute-from-gregorian
                (diary-make-date y2 m2 d2)))
        (d absolute))
    (and (<= date1 d) (<= d date2))))

(defun pgw/date-date (absolute year month day)
  "Check for equality of date"
  (equal absolute (calendar-absolute-from-gregorian (diary-make-date year month day))))

(defun pgw/check-ohs-class (absolute classname semesters days times fallstart fallend springstart springend noclasses)
  "Returns a list with formatted strings: (classname curdate
headline). These can then be used to create the headline. The curdate
is in the form of a list"
  (let* ((dayname (calendar-day-of-week (calendar-gregorian-from-absolute absolute)))
         (curdate (calendar-gregorian-from-absolute absolute))
         (time (nth (- (length days) (length (memq dayname days))) times)))
    (when (and (memq dayname days) ;; Account for MLK Monday on Friday
               (or (if (memq 1 semesters) (pgw/date-block absolute (nth 0 fallstart) (nth 1 fallstart) (nth 2 fallstart)
                                                         (nth 0 fallend) (nth 1 fallend) (nth 2 fallend)))
                   (if (memq 2 semesters) (pgw/date-block absolute (nth 0 springstart) (nth 1 springstart) (nth 2 springstart)
                                                          (nth 0 springend) (nth 1 springend) (nth 2 springend)))))
      (when (not (memq 't
                           (mapcar (lambda (noclass) (if (> (length noclass) 3)
                                                          (pgw/date-block absolute (nth 0 noclass) (nth 1 noclass) (nth 2 noclass) (nth 3 noclass) (nth 4 noclass) (nth 5 noclass))
                                                        (pgw/date-date absolute (nth 0 noclass) (nth 1 noclass) (nth 2 noclass))))
                                noclasses)))
            (list classname curdate time)))))

(defun pgw/create-entry (classname semesters days times &optional desc custom-dates)
  "Creates headlines for class schedule.
CLASSNAME: a string with the class name (to appear on agenda)

SEMESTERS: a list of integers. e.g. for both just a first semester:
'(1) or for both semesters '(1 2)

DAYS: the days of the class. Normally it will be M/W or T/Th but in
order to have flexibility, the function takes an input of another list
of integers representing days of the week. Monday starts on 1 and
Sunday is 0

TIMES: a cons list containing a list of the times which should be
the same length as the list of days

optional DESC: string containing a description for the event

This function uses the variable `pgw/schoolyear-dates' for the value of holidays
unless custom-dates is specified"

  (let* ((current (calendar-absolute-from-gregorian (diary-make-date 2021 9 9)))
         (desc (if desc (setq desc (format "\n%s\n" desc)) (setq desc "")))
         (schoolyear-dates (if custom-dates custom-dates (setq schoolyear-dates pgw/schoolyear-dates)))
         (fallstart (gethash "fallstart" schoolyear-dates))
         (fallend (gethash "fallend" schoolyear-dates))
         (springstart (gethash "springstart" schoolyear-dates))
         (springend (gethash "springend" schoolyear-dates))
         (noclasses (gethash "noclasses" schoolyear-dates)))
    (goto-char (point-max))
    (insert (format "\n* %s" classname))
    (while (pgw/date-block current (nth 0 fallstart) (nth 1 fallstart) (nth 2 fallstart)
                           (nth 0 springend) (nth 1 springend) (nth 2 springend)) ; Make sure we're within starting and ending dates of school
      (let ((info (pgw/check-ohs-class current classname semesters days times fallstart fallend springstart springend noclasses)))
        (when info
          (let* ((headline (nth 0 info))
                 (days-of-week '("Sun" "Mon" "Tue" "Wed" "Thu" "Fri" "Sat"))
                 (fulldate (nth 1 info))
                 (year (nth 2 fulldate))
                 (month (nth 0 fulldate))
                 (day (nth 1 fulldate))
                 (dayofweek (nth (calendar-day-of-week fulldate) days-of-week))
                 (time (nth 2 info)))
            (goto-char (point-max))
            ;; (insert (format "\n** %s\n:PROPERTIES:\n:TIMEZONE: UTC\n:END:\n<%d-%02d-%02d %s %s>\n%s"
            ;;                 headline year month day dayofweek time desc)))))
            (insert (format "\n** %s\n<%d-%02d-%02d %s %s>\n%s"
                            headline year month day dayofweek time desc)))))
      (setq current (+ current 1)))))

;; (setq pgw/schoolyear-dates
;;       #s(hash-table
;;          size 5
;;          test equal
;;          data ("fallstart" (2021 9 9)
;;                "fallend" (2021 12 13)
;;                "springstart" (2022 1 18)
;;                "springend" (2022 5 2)
;;                "noclasses" ((2021 9 6) ;; Labor Day
;;                             (2021 11 1) ;; No Classes
;;                             (2021 11 2) ;; Election Day, University Holiday
;;                             (2021 11 24 2021 11 26) ;; No Classes
;;                             (2021 11 25) ;; Thanksgiving, University Holiday
;;                             (2022 1 17)            ;; Martin Luther King Jr. Day, University Holiday
;;                             (2022 3 14 2022 3 18))  ;; Spring Break
;;                             )))

(setq pgw/schoolyear-dates
      #s(hash-table
         size 5
         test equal
         data ("fallstart" (2021 08 30)
               "fallend" (2021 12 17)
               "springstart" (2022 1 18)
               "springend" (2022 5 13)
               "noclasses" ((2021 9 6) ;; Labor Day
                            (2021 11 1) ;; No Classes
                            (2021 11 2) ;; Election Day, University Holiday
                            (2021 11 24 2021 11 26) ;; No Classes
                            (2021 11 25) ;; Thanksgiving, University Holiday
                            (2022 1 17)  ;; Martin Luther King Jr. Day, University Holiday
                            (2022 3 14 2022 3 18)  ;; Spring Break
                            (2022 5 3 2022 5 13)) ; Reading and Exam Days
                            )))

(setq pgw/juilliard-schoolyear-dates
      #s(hash-table
         size 5
         test equal
         data ("fallstart" (2021 08 30)
               "fallend" (2021 12 17)
               "springstart" (2022 1 10)
               "springend" (2022 5 13)
               "noclasses" ((2021 9 6) ;; Labor Day
                            (2021 11 1) ;; No Classes
                            (2021 11 2) ;; Election Day, University Holiday
                            (2021 11 24 2021 11 28) ;; No Classes
                            (2021 11 25) ;; Thanksgiving, University Holiday
                            (2022 1 17)  ;; Martin Luther King Jr. Day, University Holiday
                            (2022 2 26 2022 3 13)) ;; Midterm Recess
                            ;; (2022 5 3 2022 5 6)) ;; Jury week
                            )))
;; Generate Class Calendar:1 ends here

;; [[file:config.org::*Sync gcal Bash Script][Sync gcal Bash Script:1]]
  (defun pgw/sync-canvas-cal ()
    (interactive)
    (start-process-shell-command "Running ~/QScripts/canvcal_org.sh" nil "bash ~/QScripts/canvcal_org.sh"))

  (global-set-key (kbd "C-c s-g o") 'pgw/sync-canvas-cal)
;; Sync gcal Bash Script:1 ends here

;; [[file:config.org::*Python][Python:1]]
(use-package! lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))  ; or lsp-deferred
;; Python:1 ends here
