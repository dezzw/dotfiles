(with-eval-after-load "org"
  
  (setq planner-path "~/Documents/Org/Planner/")

  (defun dw/update-planner-files ()

    (setq planner-files (directory-files planner-path))
    (setq planner-files (cdr planner-files))
    (setq planner-files (cdr planner-files))
    (setq planner-files (cdr planner-files))
    
    (let (value)
      (while planner-files
	(setq value (cons (concat planner-path (car planner-files)) value))
	(setq planner-files (cdr planner-files))
	)
      value))

  (setq org-agenda-files (dw/update-planner-files))


  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  ;; Custom TODO states and Agendas
  (setq org-todo-keywords
	'((sequence "TODO(t)" "NEXT(n)" "TBA(b)" "|" "DONE(d!)")
	  ))

  (setq org-tag-alist
	'((:startgroup)
	  ;; Put mutually exclusive tags here
	  (:endgroup)
	  ("review" . ?r)
	  ("assignment" . ?a)
	  ("test" . ?t)
	  ("quiz" . ?q)
	  ("final" . ?f)
	  ("pratice" . ?p)
	  ("emacs" . ?e)
	  ("note" . ?n)
	  ("idea" . ?i)))


  (use-package org-super-agenda
    :hook (org-agenda-mode . org-super-agenda-mode)
    :init
    (setq org-agenda-skip-scheduled-if-done t
          org-agenda-skip-deadline-if-done t
          org-agenda-compact-blocks t
	  org-agenda-start-with-log-mode t
          org-agenda-start-day "+0d"
	  org-agenda-include-diary t
	  org-agenda-time-leading-zero t
	  org-agenda-span 1)
  
    
    (setq org-agenda-custom-commands
	  '(("D" "Dashboard"
             ((agenda "" ((org-agenda-span 'day)

			  (org-super-agenda-groups
			   '((:name "Today"
                                    :time-grid t
                                    :date today
                                    :scheduled today
                                    :order 1)))))
              (alltodo "" ((org-agenda-overriding-header "")
			   (org-super-agenda-groups
                            '((:name "Next to do"
                                     :todo "NEXT"
                                     :order 1)
                              (:name "Important"
                                     :priority "A"
                                     :order 6)
                              (:name "Due Today"
                                     :deadline today
                                     :order 2)
                              (:name "Due Soon"
                                     :deadline future
                                     :order 8)
                              (:name "Overdue"
                                     :deadline past
                                     :order 7)
                              (:name "Assignments"
                                     :tag "assignment"
                                     :order 10)
			      (:name "Tests/Quiz"
				     :tag ("test" "quiz")
				     :order 10)
			      (:name "Final Exam"
				     :tag "final"
				     :order  9)
                              (:name "Projects"
                                     :tag "Project"
                                     :order 14)
                              (:name "Emacs"
                                     :tag "Emacs"
                                     :order 13)
                              (:name "To read"
                                     :tag "Read"
                                     :order 30)
                              (:name "trivial"
                                     :priority<= "C"
                                     :tag ("Trivial" "Unimportant")
                                     :todo ("SOMEDAY" )
                                     :order 90)
                              ))))))
	    ("A" "Assignments"
	     ((agenda "" ((org-agenda-span 'day)
			  (org-super-agenda-groups
			   '((:name "Today"
				    :time-grid t
				    :and (:tag "assignment" :deadline today)
				    )
			     (:discard (:anything t))))))
	      (tags "assignment" ((org-agenda-overriding-header "")
				  (org-super-agenda-groups
				   '((:name "Due Today"
					    :and (:tag "assignment" :deadline today)
					    )
				     (:name "Next to do"
					    :and (:todo "NEXT" :tag "assignment")
					    :order 2)
				     (:name "Due Soon"
					    :and (:tag "assignment" :deadline future)
					    :order 3)
				     (:name "Overdue"
					    :and (:tag "assignment" :deadline past)
					    :order 99)
				     (:discard (:anything t))))))))
	    ("T" "Tests/Quiz"
	     ((agenda "" ((org-agenda-span 'day)
			  (org-agenda-include-deadlines nil)
			  (org-super-agenda-groups
			   '((:name "Today"
				    :and (:scheduled today :tag "test")
				    :time-grid t)
			     (:discard (:anything t))))))
	      (tags "\\(?:final\\|quiz\\|test\\)" ((org-agenda-overriding-header "")
						   (org-super-agenda-groups
						    '((:name "Tests"
							     :and (:tag "test" :scheduled future)
							     )
						      (:name "Quiz"
							     :and (:tag "quiz" :scheduled future)
							     )
						      (:name "Final Exam"
							     :and (:tag "final" :scheduled future)
							     )
						      (:discard (:anything t))))))))
	    )
	  )
    )
  

  ;; Refiling
  (setq org-refile-targets
	'(("~/Documents/Org/Planner/Archive.org" :maxlevel . 1)))

  ;; Save Org buffers after refiling!
  (advice-add 'org-refile :after 'org-save-all-org-buffers)

  ;; Capture Templates
  (defun dw/read-file-as-string (path)
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string)))

  (setq org-capture-templates
	`(("t" "Tasks / Projects")
	  ("tt" "Task" entry (file+olp "~/Documents/Org/Planner/Tasks.org" "Inbox")
           "* TODO %?\n  %U\n  %a\n  %i" :empty-lines 1)))

  ;; Habit Tracking
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit)
  (setq org-habit-graph-column 60)
  )


(provide 'init-org-agenda)
