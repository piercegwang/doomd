(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-modules '(ol-bibtex org-habit))
 '(safe-local-variable-values
   '((org-roam-db-location . "/Users/piercewang/Dropbox/org/notes/hacking/notes/course_org-roam.db")
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
