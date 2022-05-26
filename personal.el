;; [[file:config.org::*Doom fonts][Doom fonts:1]]
(setq doom-font (font-spec :family "Ubuntu Mono" :size 14 :height 1.0)
      doom-big-font (font-spec :family "Ubuntu Mono" :size 26)
      doom-variable-pitch-font (font-spec :family "Open Sans" :size 12)
      doom-unicode-font (font-spec :family "Ubuntu Mono")
      doom-serif-font (font-spec :family "Ubuntu"))
;; Doom fonts:1 ends here

;; [[file:config.org::*Passwords][Passwords:1]]
  (load-file "~/.passwords.el")
;; Passwords:1 ends here

;; [[file:config.org::*Org-mode][Org-mode:2]]
(after! org
    ;; Set to the name of the file where new notes will be stored
    (setq org-mobile-inbox-for-pull "~/Dropbox/Apps/MobileOrg/index.org")
    ;; Set to <your Dropbox root directory>/MobileOrg.
    (setq org-mobile-directory "~/Dropbox/Apps/MobileOrg")
  (after! org-crypt
    (setq org-crypt-key "pierce.g.wang@gmail.com")
    ;; GPG key to use for encryption
    ;; Either the Key ID or set to nil to use symmetric encryption.
    )
    (require 'ox-publish)
    (setq org-publish-project-alist
          '(("pages-notes"
             :base-directory "~/Dropbox/org_publish/"
             :base-extension "org"
             :publishing-directory "~/Documents/github/github_pages/"
             :recursive t
             :publishing-function org-html-publish-to-html
             :headline-levels 4             ; Just the default for this project.
             ;; :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/style.css\"/>"
             :auto-preamble t
             )
            ("pages-static"
             :base-directory "~/Dropbox/org_publish/"
             :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|jpeg\\|txt\\|json"
             :publishing-directory "~/Documents/github/github_pages/"
             :recursive t
             :publishing-function org-publish-attachment
             )
            ("pages" :components ("pages-notes" "pages-static"))
            ))
  (use-package! org-gcal
    :config
    (map! :leader
          (:prefix-map ("d" . "Gcal Commands")
           :desc "Post to gcal" "p" #'org-gcal-post-at-point
           :desc "Sync with gcal" "s" #'org-gcal-sync
           :desc "Fetch from gcal" "f" #'org-gcal-fetch
           :desc "Delete at point" "d" #'org-gcal-delete-at-point
           :desc "Sync current buffer" "b s" #'org-gcal-sync-buffer
           :desc "Fetch current buffer" "b f" #'org-gcal-fetch-buffer))
    (setq org-gcal-client-id pgw/org-gcal-client-id
          org-gcal-client-secret pgw/org-gcal-client-secret
          org-gcal-file-alist pgw/org-gcal-file-alist
          org-gcal-local-timezone "America/New_York"
          org-gcal-notify-p nil
          org-gcal-up-days 60)
    (setq org-gcal-remove-api-cancelled-events t))
    (setq org-reveal-root "file:///Users/piercewang/Documents/projects/revealjs/reveal.js-4.1.0")
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
                                        ":PROPERTIES:"
                                        ":calendar-id: ihfv2u5n9uf5ksj5484vbe7mj4@group.calendar.google.com"
                                        ":END:"
                                        ":org-gcal:"
                                        "%^{Scheduled time + duration}T%?"
                                        ":END:"))
                            ("Emacs Calendar" :keys "g"
                             :file "~/Dropbox/org/calendars/cal_gmail.org"
                             :template ("* %^{Title of event}"
                                        ":PROPERTIES:"
                                        ":calendar-id: pierce.g.wang@gmail.com"
                                        ":END:"
                                        ":org-gcal:"
                                        "%^{Scheduled time + duration}T%?"
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
                                        "%^{ARRANGEMENT}p"
                                        "%^{COMPOSED}p")))))))
  
  (after! org-roam
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
             :unnarrowed t))))
  (setq pgw/refile-targets (file-expand-wildcards "~/Dropbox/org/*.org"))
  )
;; Org-mode:2 ends here

;; [[file:config.org::*exec-path-from-shell][exec-path-from-shell:1]]
(when IS-MAC
  (use-package! exec-path-from-shell
    :config
    (setq exec-path-from-shell-shell-name "/bin/zsh"))
  (exec-path-from-shell-initialize))
;; exec-path-from-shell:1 ends here

;; [[file:config.org::*Shell][Shell:1]]
(setq shell-file-name "/bin/zsh")
;; Shell:1 ends here

;; [[file:config.org::*Flyspell mode][Flyspell mode:1]]
  (defun pgw/turn-on-flyspell-hook ()
    (if (or (string-match "^/Users/piercewang/Dropbox/org/notes/college/" (if (eq buffer-file-name nil) "" buffer-file-name)))
        (flyspell-mode 1)))

  (add-hook! 'org-mode-hook 'turn-on-flyspell)
;; Flyspell mode:1 ends here

;; [[file:config.org::*User Configuration][User Configuration:1]]
(setq user-full-name "Pierce Wang"
      user-mail-address "pierce.g.wang@gmail.com")
;; User Configuration:1 ends here

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
  (start-process-shell-command "Running syncgcal.sh" nil "bash ~/Documents/github/org_canvas_parser/syncgcal.sh"))

(map! :leader "d o" #'pgw/sync-canvas-cal)
;; Sync gcal Bash Script:1 ends here

;; [[file:config.org::*End of personal.el][End of personal.el:1]]
(provide '.personal)
;; End of personal.el:1 ends here
