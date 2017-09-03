;;; packages.el --- Vue Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq vue-packages
      '(
        company
        css-mode
        evil-matchit
        flycheck
        js-doc
        sass-mode
        scss-mode
        smartparens
        tagedit
        web-mode
        ))

(defun vue/post-init-company ()
  (spacemacs|add-company-hook css-mode)
  (spacemacs|add-company-hook vue-mode))

(defun vue/init-css-mode ()
  (use-package css-mode
    :defer t
    :init
    (progn
      (push 'company-css company-backends-css-mode)

      ;; Mark `css-indent-offset' as safe-local variable
      (put 'css-indent-offset 'safe-local-variable #'integerp)

      ;; Explicitly run prog-mode hooks since css-mode does not derive from
      ;; prog-mode major-mode in Emacs 24 and below.
      (when (version< emacs-version "25")
        (add-hook 'css-mode-hook 'spacemacs/run-prog-mode-hooks))

      (defun css-expand-statement ()
        "Expand CSS block"
        (interactive)
        (save-excursion
          (end-of-line)
          (search-backward "{")
          (forward-char 1)
          (while (or (eobp) (not (looking-at "}")))
          (let ((beg (point)))
            (newline)
            (search-forward ";")
            (indent-region beg (point))
            ))
          (newline)))

      (defun css-contract-statement ()
        "Contract CSS block"
        (interactive)
        (end-of-line)
        (search-backward "{")
        (while (not (looking-at "}"))
          (join-line -1)))

      (spacemacs/set-leader-keys-for-major-mode 'css-mode
        "zc" 'css-contract-statement
        "zo" 'css-expand-statement))))

(defun vue/post-init-evil-matchit ()
  (with-eval-after-load 'evil-matchit
    (plist-put evilmi-plugins 'vue-mode
               '((evilmi-simple-get-tag evilmi-simple-jump)
                 (evilmi-javascript-get-tag evilmi-javascript-jump)
                 (evilmi-html-get-tag evilmi-html-jump)))))

(defun vue/post-init-flycheck ()
  (with-eval-after-load 'flycheck
    (dolist (checker '(javascript-eslint javascript-standard))
      (flycheck-add-mode checker 'vue-mode)))
  (add-hook 'vue-mode-hook #'spacemacs//vue-use-eslint-from-node-modules)
  (spacemacs/add-flycheck-hook 'vue-mode))

(defun vue/init-less-css-mode ()
  (use-package less-css-mode
    :defer t
    :mode ("\\.less\\'" . less-css-mode)))

(defun vue/post-init-js-doc ()
  (add-hook 'vue-mode-hook 'spacemacs/js-doc-require)
  (spacemacs/js-doc-set-key-bindings 'vue-mode))

(defun vue/init-sass-mode ()
  (use-package sass-mode
    :defer t
    :mode ("\\.sass\\'" . sass-mode)))

(defun vue/init-scss-mode ()
  (use-package scss-mode
    :defer t
    :mode ("\\.scss\\'" . scss-mode)))

(defun vue/post-init-smartparens ()
  (if dotspacemacs-smartparens-strict-mode
      (add-hook 'vue-mode-hook #'smartparens-strict-mode)
    (add-hook 'vue-mode-hook #'smartparens-mode)))

(defun vue/init-tagedit ()
  (use-package tagedit
    :defer t
    :config
    (progn
      (tagedit-add-experimental-features)
      (add-hook 'vue-mode-hook (lambda () (tagedit-mode 1)))
      (spacemacs|diminish tagedit-mode " Ⓣ" " T"))))

(defun vue/init-web-mode ()
  (use-package web-mode
    :defer t
    :mode
    ("\\.vue\\'" . web-mode)))

(defun vue/post-init-web-mode ()
  (define-derived-mode vue-mode web-mode "vue")
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . vue-mode)))
