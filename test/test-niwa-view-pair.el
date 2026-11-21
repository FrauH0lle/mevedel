;;; test-niwa-view-pair.el -- Docked pair repair decisions -*- lexical-binding: t -*-

;;; Commentary:

;; Test layout decisions without relying on redisplay.

;;; Code:

(require 'helpers
         (file-name-concat
          (file-name-directory (or buffer-file-name load-file-name byte-compile-current-file))
          "helpers"))

(require 'niwa-view-pair)

(niwa-deftest niwa-view-pair-plan ()
  ,test
  (test)

  :doc "Keep a restored pair, dock another view, and remove foreign/orphan docks."
  (should
   (equal (niwa-view-pair-plan '((left draft dock) (right draft foreign))
                                 '((dock . draft) (foreign . other) (stray . draft)))
          '(:keep ((left . dock)) :add (right) :remove (foreign stray))))

  :doc "A complete layout leaves no additions or removals."
  (should
   (equal (niwa-view-pair-plan '((left draft one) (right draft two))
                                 '((one . draft) (two . draft)))
          '(:keep ((left . one) (right . two)) :add nil :remove nil))))

(provide 'test-niwa-view-pair)
;;; test-niwa-view-pair.el ends here
