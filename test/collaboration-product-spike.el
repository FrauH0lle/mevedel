;;; collaboration-product-spike.el --- product loopback collaboration probe -*- lexical-binding: t; -*-

;;; Commentary:

;; Starts the repository collaboration module against a real GNU ELPA
;; `web-server' listener.  The companion Node client exercises HTTP routes,
;; Origin/guest admission, authentication, UTF-8 snapshot framing, ping/pong,
;; the read-only boundary, the non-reading guest queue bound, authenticated
;; incomplete-frame timeout, and final ended teardown.

;;; Code:

(require 'mevedel-collaboration-projection)
(require 'mevedel-collaboration-transport)
(require 'mevedel-collaboration)
(require 'mevedel-chat)
(require 'mevedel-structs)

(defvar mevedel-collaboration-product-spike--buffer nil)
(defvar mevedel-collaboration-product-spike--state-file
  (getenv "MEVEDEL_SPIKE_STATE"))
(defvar mevedel-collaboration-product-spike--stop-file
  (getenv "MEVEDEL_SPIKE_STOP"))
(defvar mevedel-collaboration-product-spike--burst-file
  (getenv "MEVEDEL_SPIKE_BURST"))
(defvar mevedel-collaboration-product-spike--lifecycle-file
  (getenv "MEVEDEL_SPIKE_LIFECYCLE"))
(defvar mevedel-collaboration-product-spike--settle-file
  (getenv "MEVEDEL_SPIKE_SETTLE"))
(defvar mevedel-collaboration-product-spike--mutation-file
  (getenv "MEVEDEL_SPIKE_MUTATION"))
(defvar mevedel-collaboration-product-spike--mutation-state-file
  (getenv "MEVEDEL_SPIKE_MUTATION_STATE"))
(defvar mevedel-collaboration-product-spike--burst-sent nil)
(defvar mevedel-collaboration-product-spike--lifecycle-done nil)
(defvar mevedel-collaboration-product-spike--lifecycle-tool-pending nil)
(defvar mevedel-collaboration-product-spike--mutation-baseline nil)
(defvar mevedel-collaboration-product-spike--mutation-checked nil)

(defun mevedel-collaboration-product-spike--stop ()
  "Stop the product spike room and release its temporary buffer."
  (mevedel-collaboration--stop-internal 'product-spike-exit)
  (when (buffer-live-p mevedel-collaboration-product-spike--buffer)
    (kill-buffer mevedel-collaboration-product-spike--buffer))
  (setq mevedel-collaboration-product-spike--buffer nil))

(defun mevedel-collaboration-product-spike--settle-tool ()
  "Settle the product tool after the client observes its running state."
  (when (and mevedel-collaboration-product-spike--lifecycle-tool-pending
             mevedel-collaboration-product-spike--settle-file
             (file-exists-p mevedel-collaboration-product-spike--settle-file))
    (setq mevedel-collaboration-product-spike--lifecycle-tool-pending nil)
    (with-current-buffer mevedel-collaboration-product-spike--buffer
      (mevedel-collaboration--post-tool
       '(:id "product-tool" :name "Bash" :args (:command "true")
         :result "tool settled"))
      (mevedel-collaboration--publish mevedel-collaboration--room)
      (when-let ((guest (plist-get mevedel-collaboration--room :guest)))
        (mevedel-collaboration--guest-send
         guest
         '(("type" . "record") ("version" . 1)
           ("record" . (("id" . "tool-product-tool")
                        ("kind" . "tool") ("revision" . 2)
                        ("name" . "Bash") ("status" . "completed")
                        ("summary" . "Bash") ("result" . "tool settled")
                        ("truncated" . nil)))))))
    (setq mevedel-collaboration-product-spike--mutation-baseline
          (with-current-buffer mevedel-collaboration-product-spike--buffer
            (buffer-string)))))

(defun mevedel-collaboration-product-spike--check-mutation ()
  "Record whether an inbound mutation changed the authoritative buffer."
  (when (and (not mevedel-collaboration-product-spike--mutation-checked)
             mevedel-collaboration-product-spike--mutation-file
             (file-exists-p mevedel-collaboration-product-spike--mutation-file))
    (setq mevedel-collaboration-product-spike--mutation-checked t)
    (let ((same (equal mevedel-collaboration-product-spike--mutation-baseline
                       (with-current-buffer
                           mevedel-collaboration-product-spike--buffer
                         (buffer-string)))))
      (when mevedel-collaboration-product-spike--mutation-state-file
        (with-temp-file mevedel-collaboration-product-spike--mutation-state-file
          (insert (if same "unchanged\n" "changed\n")))))))

(add-hook 'kill-emacs-hook
          #'mevedel-collaboration-product-spike--stop)

(setq mevedel-collaboration-product-spike--buffer
      (generate-new-buffer " *mevedel-collaboration-product-spike*"))
(with-current-buffer mevedel-collaboration-product-spike--buffer
  (insert "Grüße from the loopback product spike"))

(let* ((link (mevedel-collaboration--start
              (mevedel-session--create :name "product-spike")
              mevedel-collaboration-product-spike--buffer))
       (room mevedel-collaboration--room)
       (server (plist-get room :server))
       (process (mevedel-collaboration--web-server-slot server 'process))
       (port (process-contact process :service)))
  (ignore link)
  (with-temp-file mevedel-collaboration-product-spike--state-file
    (insert (format "started:127.0.0.1:%s:%s:%s\n"
                    port
                    (plist-get room :room-id)
                    (plist-get room :token))))
  (message "collaboration product spike listening on 127.0.0.1:%s" port)
  (while mevedel-collaboration--room
    (mevedel-collaboration-product-spike--settle-tool)
    (mevedel-collaboration-product-spike--check-mutation)
    (when (and (not mevedel-collaboration-product-spike--lifecycle-done)
               mevedel-collaboration-product-spike--lifecycle-file
               (file-exists-p mevedel-collaboration-product-spike--lifecycle-file))
      (setq mevedel-collaboration-product-spike--lifecycle-done t)
      (with-current-buffer mevedel-collaboration-product-spike--buffer
        ;; Exercise the same accepted local-turn seam used by generated
        ;; prompts; its failure-isolated observer publishes the room before
        ;; the subsequent tool lifecycle begins.
        (mevedel--insert-local-user-turn
         "Ordinary prompt from product lifecycle")
        (mevedel-collaboration--pre-tool
         '(:id "product-tool" :name "Bash" :args (:command "true")))
        (setq mevedel-collaboration-product-spike--lifecycle-tool-pending t)))
    (when (and (not mevedel-collaboration-product-spike--burst-sent)
               mevedel-collaboration-product-spike--burst-file
               (file-exists-p mevedel-collaboration-product-spike--burst-file))
      (setq mevedel-collaboration-product-spike--burst-sent t)
      (when-let ((guest (plist-get mevedel-collaboration--room :guest)))
        (let ((payload (make-string 900000 ?x)))
          (dotimes (_ 3)
            (mevedel-collaboration--guest-send
             guest `(("type" . "burst") ("payload" . ,payload)))))))
    (when (and mevedel-collaboration-product-spike--stop-file
               (file-exists-p mevedel-collaboration-product-spike--stop-file))
      (mevedel-collaboration--stop-internal 'product-spike-stop))
    (accept-process-output nil 0.2)))

(provide 'collaboration-product-spike)
;;; collaboration-product-spike.el ends here
