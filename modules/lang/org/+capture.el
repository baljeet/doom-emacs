;;; lang/org/+capture.el -*- lexical-binding: t; -*-

;; Sets up two `org-capture' workflows that I like:
;;
;; 1. The traditional way: invoking `org-capture' directly (or through a
;;    command, like :org).
;;
;; 2. Through a org-capture popup frame that is invoked from outside Emacs (the
;;    script is in ~/.emacs.d/bin). This lets me open an org-capture box
;;    anywhere I can call org-capture (whether or not Emacs is open/running),
;;    like, say, from qutebrowser, vimperator, dmenu or a global keybinding.

(defvar +org-default-todo-file "todo.org"
  "TODO")

(defvar +org-default-notes-file "notes.org"
  "NOTE")

(defvar my/org-meeting-template "** Meeting about %^{something}
SCHEDULED: %<%Y-%m-%d %H:%M>
*Attendees:*
- [X] Baljeet
- [ ] %?
*Agenda:*
-
-
*Notes:*
" "Meeting Template")
 (defvar my/org-basic-task-template "* TODO %^{Task}
:PROPERTIES:
:Effort: %^{effort|1:00|0:05|0:15|0:30|2:00|4:00}
:END:
Captured %<%Y-%m-%d %H:%M>
%?

%i
" "Basic task data")

(defvar org-capture-templates
  `(("t" "Tasks" entry
     (file+headline "~/org/organizer.org" "Inbox")
     ,my/org-basic-task-template)
    ("T" "Quick task" entry
     (file+headline "~/org/organizer.org" "Inbox")
     "* TODO %^{Task}\nSCHEDULED: %t\n"
     :immediate-finish t)
    ("i" "Interrupting task" entry
     (file+headline "~/org/organizer.org" "Inbox")
     "* STARTED %^{Task}"
     :clock-in :clock-resume)
    ("e" "Emacs idea" entry
     (file+headline "~/code/emacs-notes/tasks.org" "Emacs")
     "* TODO %^{Task}"
     :immediate-finish t)
    ("E" "Energy" table-line
     (file+headline "~/org/organizer.org" "Track energy")
     "| %U | %^{Energy 5-awesome 3-fuzzy 1-zzz} | %^{Note} |"
     :immediate-finish t
     )
    ("b" "Business task" entry
     (file+headline "~/org/business.org" "Tasks")
     ,my/org-basic-task-template)
    ("p" "People task" entry
     (file+headline "~/org/people.org" "Tasks")
     ,my/org-basic-task-template)
    ("j" "Journal entry" plain
     (file+datetree "~/org/journal.org")
     "%K - %a\n%i\n%?\n"
     :unnarrowed t)
    ("J" "Journal entry with date" plain
     (file+datetree+prompt "~/org/journal.org")
     "%K - %a\n%i\n%?\n"
     :unnarrowed t)
    ("s" "Journal entry with date, scheduled" entry
     (file+datetree+prompt "~/org/journal.org")
     "* \n%K - %a\n%t\t%i\n%?\n"
     :unnarrowed t)
    ("c" "Protocol Link" entry (file+headline "~/org/notes.org" "Inbox")
     "* [[%:link][%:description]] \n\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n%?\n\nCaptured: %U")
    ("db" "Done - Business" entry
     (file+headline "~/org/business.org" "Tasks")
     "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
    ("dp" "Done - People" entry
     (file+headline "~/org/people.org" "Tasks")
     "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
    ("dt" "Done - Task" entry
     (file+headline "~/org/organizer.org" "Inbox")
     "* DONE %^{Task}\nSCHEDULED: %^t\n%?")
    ("q" "Quick note" item
     (file+headline "~/org/organizer.org" "Quick notes"))
    ("B" "Book" entry
     (file+datetree "~/org/books.org" "Inbox")
     "* %^{Title}  %^g
  %i
  *Author(s):* %^{Author} \\\\
  *ISBN:* %^{ISBN}

  %?

  *Review on:* %^t \\
  %a
  %U"
     :clock-in :clock-resume)
    ("C" "Contact" entry (file "~/org/contacts.org")
     "* %(org-contacts-template-name)
  :PROPERTIES:
  :EMAIL: %(my/org-contacts-template-email)
  :END:")
    ("n" "Daily note" table-line (file+olp "~/org/organizer.org" "Inbox")
     "| %u | %^{Note} |"
     :immediate-finish t)
    ("r" "Notes" entry
     (file+datetree "~/org/organizer.org")
     "* %?\n\n%i\n%U\n"
     ))

  )


(after! org
  (defvaralias 'org-default-notes-file '+org-default-notes-file)

  (setq org-default-notes-file (expand-file-name +org-default-notes-file +org-dir))

  (add-hook 'org-capture-after-finalize-hook #'+org-capture|cleanup-frame)

  (when (featurep! :feature evil)
    (add-hook 'org-capture-mode-hook #'evil-insert-state))

  (when (featurep! :ui doom-dashboard)
    (add-hook '+doom-dashboard-inhibit-functions #'+org-capture-frame-p)))
