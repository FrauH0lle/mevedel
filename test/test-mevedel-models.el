;;; test-mevedel-models.el --- Tests for model tier resolution -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

(require 'gptel)
(require 'gptel-openai)
(require 'mevedel-models)
(require 'helpers
         (file-name-concat
          (file-name-directory
           (or buffer-file-name
               load-file-name
               byte-compile-current-file))
          "helpers"))


;;
;;; Provider and tier resolution

(defmacro mevedel-models-test--with-backends (&rest body)
  "Run BODY with an isolated gptel backend registry."
  (declare (indent 0) (debug t))
  `(let ((gptel--known-backends nil))
     (gptel-make-openai "Fast" :key "test" :models '(fast-model))
     (gptel-make-openai "Balanced" :key "test" :models '(balanced-model))
     (gptel-make-openai "Ollama" :key "test" :models '(llama3.1:8b))
     ,@body))

(mevedel-deftest mevedel-model-resolve-provider ()
  ,test
  (test)

  :doc "resolves gptel transient-style BACKEND:MODEL strings"
  (mevedel-models-test--with-backends
    (let ((provider (mevedel-model-resolve-provider "Fast:fast-model")))
      (should (equal "Fast" (gptel-backend-name (plist-get provider :backend))))
      (should (eq 'fast-model (plist-get provider :model)))))

  :doc "splits at the first colon so provider strings support colon models"
  (mevedel-models-test--with-backends
    (let ((provider (mevedel-model-resolve-provider "Ollama:llama3.1:8b")))
      (should (equal "Ollama" (gptel-backend-name (plist-get provider :backend))))
      (should (eq 'llama3.1:8b (plist-get provider :model)))))

  :doc "resolves structured (BACKEND . MODEL) specs"
  (mevedel-models-test--with-backends
    (let ((provider (mevedel-model-resolve-provider '("Balanced" . balanced-model))))
      (should (equal "Balanced"
                     (gptel-backend-name (plist-get provider :backend))))
      (should (eq 'balanced-model (plist-get provider :model))))))

(mevedel-deftest mevedel-model-resolve-tier ()
  ,test
  (test)

  :doc "resolves configured tiers to provider plists"
  (mevedel-models-test--with-backends
    (let ((mevedel-model-tiers
           '((fast . "Fast:fast-model")
             (balanced . "Balanced:balanced-model")
             (strong . nil))))
      (let ((provider (mevedel-model-resolve-tier 'fast)))
        (should (equal "Fast" (gptel-backend-name
                               (plist-get provider :backend))))
        (should (eq 'fast-model (plist-get provider :model))))))

  :doc "unconfigured fast and strong fall back to balanced"
  (mevedel-models-test--with-backends
    (let ((mevedel-model-tiers
           '((fast . nil)
             (balanced . "Balanced:balanced-model")
             (strong . nil))))
      (dolist (tier '(fast strong))
        (let ((provider (mevedel-model-resolve-tier tier)))
          (should (equal "Balanced"
                         (gptel-backend-name (plist-get provider :backend))))
          (should (eq 'balanced-model (plist-get provider :model)))))))

  :doc "unconfigured balanced inherits"
  (let ((mevedel-model-tiers
         '((fast . nil) (balanced . nil) (strong . nil))))
    (should (null (mevedel-model-resolve-tier 'balanced)))))

(mevedel-deftest mevedel-model-parse-selector ()
  ,test
  (test)

  :doc "parses tier strings as tier selectors"
  (should (equal '(:tier fast)
                 (mevedel-model-parse-selector "fast")))

  :doc "parses BACKEND:MODEL strings as concrete provider selectors"
  (mevedel-models-test--with-backends
    (let ((selector (mevedel-model-parse-selector "Fast:fast-model")))
      (should (equal "Fast" (gptel-backend-name
                             (plist-get selector :backend))))
      (should (eq 'fast-model (plist-get selector :model))))))

(mevedel-deftest mevedel-model-apply-provider-to-info ()
  ,test
  (test)

  :doc "applies backend and model to FSM info and patches plist data"
  (mevedel-models-test--with-backends
    (let* ((provider (mevedel-model-resolve-provider "Fast:fast-model"))
           (info (mevedel-model-apply-provider-to-info
                  '(:data (:model "old" :messages []))
                  provider)))
      (should (equal "Fast" (gptel-backend-name (plist-get info :backend))))
      (should (eq 'fast-model (plist-get info :model)))
      (should (equal "fast-model"
                     (plist-get (plist-get info :data) :model))))))

(provide 'test-mevedel-models)
;;; test-mevedel-models.el ends here
