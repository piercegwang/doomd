(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modules '(ol-bibtex org-habit))
 '(safe-local-variable-values
   '((projectile-project-package-cmd nil)
     (projectile-project-compilation-cmd "cd bin && cmake .. && make")
     (org-roam-dailies-capture-templates
      ("c" "Class" plain #'org-roam-capture--get-point "%?" :file-name "daily/class-%<%Y%m%d%H%M%S>" :head "#+setupfile: ~/.doom.d/org-mode/templates/general/note.org
#+setupfile: ~/.doom.d/org-mode/templates/school/name_email.org
#+setupfile: ~/.doom.d/org-mode/templates/school/writing_setup.org
#+setupfile: ~/.doom.d/org-mode/templates/general/web.org
#+roam_tags: discussion
#+options: toc:nil
#+title: " :unnarrowed t)
      ("o" "Office Hours" plain #'org-roam-capture--get-point "%?" :file-name "daily/oh-%<%Y%m%d%H%M%S>" :head "#+setupfile: ~/.doom.d/org-mode/templates/general/note.org
#+setupfile: ~/.doom.d/org-mode/templates/school/name_email.org
#+setupfile: ~/.doom.d/org-mode/templates/school/writing_setup.org
#+setupfile: ~/.doom.d/org-mode/templates/general/web.org
#+roam_tags: office_hours
#+options: toc:nil
#+title: " :unnarrowed t))
     (org-roam-capture-templates
      ("r" "Readings" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+setupfile: ~/.doom.d/org-mode/templates/general/note.org
#+setupfile: ~/.doom.d/org-mode/templates/school/name_email.org
#+setupfile: ~/.doom.d/org-mode/templates/school/writing_setup.org
#+setupfile: ~/.doom.d/org-mode/templates/general/web.org
#+roam_tags: readings
#+options: toc:nil
#+title: ${title}
" :unnarrowed t)
      ("t" "Topic" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+roam_tags: topic
#+title: ${title}
" :unnarrowed t))
     (org-roam-db-location . "/Users/piercewang/Documents/OCRA1/notes/org-roam.db")
     (org-roam-directory . "/Users/piercewang/Documents/OCRA1/notes/")
     (org-roam-db-location . "/Users/piercewang/Dropbox/org/notes/hacking/notes/course_org-roam.db")
     (org-roam-directory . "/Users/piercewang/Dropbox/org/notes/hacking/course_notes")
     (org-roam-dailies-capture-templates
      ("d" "Daily Doodle" plain #'org-roam-capture--get-point "* %?" :file-name "daily/%<%Y-%m-%d>" :head "#+SETUPFILE  ~/.doom.d/org-mode/templates/personal/name_email.org
#+SETUPFILE: ~/.doom.d/org-mode/templates/general/note.org
#+SETUPFILE: ~/.doom.d/org-mode/templates/general/web_drawing.org
#+OPTIONS: toc:nil
#+roam_tags:
#+title: %u" :unnarrowed t))
     (org-roam-capture-templates
      ("D" "Drawing" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+SETUPFILE  ~/.emacs.d/org-mode/templates/personal/name_email.org
#+SETUPFILE: ~/.doom.d/org-mode/templates/general/note.org
#+SETUPFILE: ~/.doom.d/org-mode/templates/general/web_drawing.org
#+OPTIONS: toc:nil
#+roam_tags:
#+title: ${title}
%U
" :unnarrowed t))
     (org-roam-db-location . "/Users/piercewang/Dropbox/org/notes/drawing/org-roam.db")
     (org-roam-directory . "/Users/piercewang/Dropbox/org/notes/drawing")
     (org-roam-encrypt-files)
     (org-roam-db-update-method . immediate)
     (org-roam-dailies-capture-templates
      ("d" "Daily Doodle" plain #'org-roam-capture--get-point "* %?" :file-name "daily/%<%Y-%m-%d>" :head "#+SETUPFILE  ~/.emacs.d/org-mode/templates/personal/name_email.org
#+SETUPFILE: ~/.emacs.d/org-mode/templates/general/note.org
#+SETUPFILE: ~/.emacs.d/org-mode/templates/general/web_drawing.org
#+OPTIONS: toc:nil
#+roam_tags:
#+title: %u" :unnarrowed t))
     (org-roam-capture-templates
      ("D" "Drawing" plain #'org-roam-capture--get-point "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head "#+SETUPFILE  ~/.emacs.d/org-mode/templates/personal/name_email.org
#+SETUPFILE: ~/.emacs.d/org-mode/templates/general/note.org
#+SETUPFILE: ~/.emacs.d/org-mode/templates/general/web_drawing.org
#+OPTIONS: toc:nil
#+roam_tags:
#+title: ${title}
%U
" :unnarrowed t))
     (org-roam-db-location . "~/Dropbox/org/notes/drawing/org-roam.db")
     (org-roam-directory . "~/Dropbox/org/notes/drawing"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'list-timers 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)
