;;; packages.el --- shintaro4-vue Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq shintaro4-vue-packages
      '(
        company
        company-tern
        evil-matchit
        flycheck
        js-doc
        smartparens
        tern
        web-mode
        ))

(defun shintaro4-vue/post-init-company ()
  (spacemacs|add-company-hook shintaro4-vue-mode))

(defun shintaro4-vue/post-init-company-tern ()
  (push 'company-tern company-backends-shintaro4-vue-mode))

(defun shintaro4-vue/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'shintaro4-vue-mode
               '((evilmi-simple-get-tag evilmi-simple-jump)
                 (evilmi-javascript-get-tag evilmi-javascript-jump)
                 (evilmi-html-get-tag evilmi-html-jump)))))

(defun shintaro4-vue/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint javascript-standard))
      (flycheck-add-mode checker 'shintaro4-vue-mode)))
  (add-hook 'shintaro4-vue-mode-hook #'shintaro4//vue-use-eslint-from-node-modules)
  (spacemacs/add-flycheck-hook 'shintaro4-vue-mode))

(defun shintaro4-vue/post-init-js-doc ()
  (add-hook 'shintaro4-vue-mode-hook 'spacemacs/js-doc-require)
  (spacemacs/js-doc-set-key-bindings 'shintaro4-vue-mode))

(defun shintaro4-vue/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'shintaro4-vue-mode-hook #'smartparens-strict-mode)
    (add-hook 'shintaro4-vue-mode-hook #'smartparens-mode)))

(defun shintaro4-vue/post-init-tern ()
  (add-hook 'shintaro4-vue-mode-hook 'tern-mode)
  (spacemacs//set-tern-key-bindings 'shintaro4-vue-mode))

(defun shintaro4-vue/post-init-web-mode ()
  (define-derived-mode shintaro4-vue-mode web-mode "shintaro4-vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . shintaro4-vue-mode)))
