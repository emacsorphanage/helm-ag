(package "helm-ag"
         "0.64"
         "The silver searcher with helm interface")

(website-url "https://github.com/syohex/emacs-helm-ag")
(keywords "")

(package-file "helm-ag.el")

(script "test" "echo \"Error: no test specified\" && exit 1")

(source 'gnu)
(source 'melpa)

(depends-on "emacs" "27.1")
(depends-on "helm")

(development
 (depends-on "ert")
 (depends-on "helm-projectile"))

(setq network-security-level 'low)  ; see https://github.com/jcs090218/setup-emacs-windows/issues/156#issuecomment-932956432
