;;; packages.el --- shintaro4-html Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Andrea Moretti <axyzxp@gmail.com>
;; URL: https://github.com/axyz
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq shintaro4-html-packages
  '(
    company
    (company-web :toggle (configuration-layer/package-usedp 'company))
    css-mode
    evil-matchit
    flycheck
    (helm-css-scss :toggle (configuration-layer/package-usedp 'helm))
    less-css-mode
    sass-mode
    scss-mode
    smartparens
    tagedit
    web-mode
    ))

(defun shintaro4-html/post-init-company ()
  (spacemacs|add-company-hook css-mode))

;;TODO: whenever company-web makes a backend for haml-mode it should be added here. -- @robbyoconnor
(defun shintaro4-html/init-company-web ()
  (use-package company-web
    :defer t
    :init
    (progn
      (spacemacs|add-company-hook web-mode))))

(defun shintaro4-html/init-css-mode ()
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

(defun shintaro4-html/post-init-evil-matchit ()
  (add-hook 'web-mode-hook 'turn-on-evil-matchit-mode))

(defun shintaro4-html/post-init-flycheck ()
  (dolist (mode '(less-mode
                  sass-mode
                  scss-mode
                  web-mode))
    (spacemacs/add-flycheck-hook mode)))

(defun shintaro4-html/init-helm-css-scss ()
  (use-package helm-css-scss
    :defer t
    :init
    (dolist (mode '(css-mode scss-mode))
      (spacemacs/set-leader-keys-for-major-mode mode "gh" 'helm-css-scss))))

(defun shintaro4-html/init-less-css-mode ()
  (use-package less-css-mode
    :defer t
    :mode ("\\.less\\'" . less-css-mode)))

(defun shintaro4-html/init-sass-mode ()
  (use-package sass-mode
    :defer t
    :mode ("\\.sass\\'" . sass-mode)))

(defun shintaro4-html/init-scss-mode ()
  (use-package scss-mode
    :defer t
    :mode ("\\.scss\\'" . scss-mode)))

(defun shintaro4-html/post-init-smartparens ()
  (spacemacs/add-to-hooks
   (if dotspacemacs-smartparens-strict-mode
       'smartparens-strict-mode
     'smartparens-mode)
   '(css-mode-hook scss-mode-hook sass-mode-hook less-css-mode-hook))

  (add-hook 'web-mode-hook 'spacemacs/toggle-smartparens-off))

(defun shintaro4-html/init-tagedit ()
  (use-package tagedit
    :defer t
    :config
    (progn
      (tagedit-add-experimental-features)
      (add-hook 'shintaro4-html-mode-hook (lambda () (tagedit-mode 1)))
      (spacemacs|diminish tagedit-mode " Ⓣ" " T"))))

(defun shintaro4-html/init-web-mode ()
  (use-package web-mode
    :defer t
    :init
    (progn
      (push '(company-web-html company-css) company-backends-web-mode))

    :mode
    (("\\.phtml\\'"      . web-mode)
     ("\\.tpl\\.php\\'"  . web-mode)
     ("\\.twig\\'"       . web-mode)
     ("\\.html\\'"       . web-mode)
     ("\\.htm\\'"        . web-mode)
     ("\\.[gj]sp\\'"     . web-mode)
     ("\\.as[cp]x?\\'"   . web-mode)
     ("\\.eex\\'"        . web-mode)
     ("\\.erb\\'"        . web-mode)
     ("\\.mustache\\'"   . web-mode)
     ("\\.handlebars\\'" . web-mode)
     ("\\.hbs\\'"        . web-mode)
     ("\\.eco\\'"        . web-mode)
     ("\\.ejs\\'"        . web-mode)
     ("\\.djhtml\\'"     . web-mode))))
