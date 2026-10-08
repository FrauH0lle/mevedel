;;; no-native.el --- Defaults without the native presenter -*- lexical-binding: t -*-
;; As for a user without a C compiler: the build fails and text animates.
(advice-add 'request-lab-start :after
            (lambda (&rest _) (customize-set-variable 'mevedel-view-native-enabled nil)))
(provide 'no-native)
