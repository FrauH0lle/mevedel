;;; prebuilt-install.el --- Install the prebuilt module without a compiler -*- lexical-binding: t -*-
;; MEVEDEL_LAB_PREBUILT_URL replaces the GitHub release URL (format with the
;; source hash and architecture).  Local builds fail, as without a compiler.
(defvar request-lab-prebuilt-result nil)
(with-eval-after-load 'mevedel-view-native
  (setq mevedel-view-native--release-url (getenv "MEVEDEL_LAB_PREBUILT_URL"))
  (advice-add 'mevedel-view-native--build :override
              (lambda () (error "Native animation needs cc and pkg-config"))))
(advice-add 'request-lab-start :before
            (lambda (&rest _)
              (unless request-lab-prebuilt-result
                (setq request-lab-prebuilt-result
                      (condition-case err
                          (progn (mevedel-view-native-install)
                                 mevedel-view-native--load-state)
                        (error (error-message-string err)))))))
(defun request-lab-diagnostic-result ()
  "Return the install outcome and the module's loaded path."
  (list :install request-lab-prebuilt-result
        :module (seq-find (lambda (f) (string-match-p "prebuilt-" f))
                          (mapcar #'car load-history))))
(provide 'prebuilt-install)
