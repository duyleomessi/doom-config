(package! mu4e-alert)

(package! dotenv-mode)
(package! leetcode)
(package! ob-restclient)
;; FIX: jq-set-var not found
(package! restclient-jq :recipe
  (:host github
   :repo "pashky/restclient.el"
   :files ("restclient-jq.el")))
;; (package! ob-jq)
(package! ob-deno)
(package! deno-fmt)

(package! instant-rename-tag
  :recipe (:host github
           :repo "manateelazycat/instant-rename-tag"))
(package! js-doc)
(package! js-react-redux-yasnippets)
(package! jest)
(package! phpactor)
(package! prettier-js)
(package! ob-typescript)
(package! web-beautify)
;; (package! ts-comint)
(package! dash-at-point
  :recipe (:host github
           :repo "waymondo/dash-at-point"))

(package! crux)
(package! deft)
(package! autoinsert)
(package! ranger)
(package! sudo-edit)

(package! popper
  :recipe (:host github :repo "waymondo/popper") :disable t)

(package! selectric-mode
  :pin "1840de71f7414b7cd6ce425747c8e26a413233aa")

(package! osx-lib)
(package! emacs-everywhere
  :recipe (:host github
           :repo "tecosaur/emacs-everywhere"))
(package! systemd
  :pin "b6ae63a236605b1c5e1069f7d3afe06ae32a7bae")
(package! counsel-osx-app)
(package! prodigy)

(package! counsel-tramp)
(package! net-utils)
(package! engine-mode)
;; (package! server)
(package! verb)
(package! httprepl)

(package! counsel-org-clock)
(package! doct
  :recipe (:host github :repo "progfolio/doct"))
;; hightlight latex export results
(package! engrave-faces
  :recipe (:host github :repo "tecosaur/engrave-faces"))

(package! org-appear)
(package! org-chef)
(package! org-fancy-priorities)
(package! org-fragtog)
(package! graphviz-dot-mode)
(package! org-pandoc-import :recipe
  (:host github
   :repo "tecosaur/org-pandoc-import"
   :files ("*.el" "filters" "preprocessors")))

(package! org-super-agenda)
(package! ox-gfm)
(package! org-ol-tree
  :recipe (:host github :repo "Townk/org-ol-tree"))
(package! org-sort-tasks
  :recipe (:host github :repo "felipelalli/org-sort-tasks"))
(package! org-preview-html :disable t)
(package! org-download)
(package! org-roam)
;; (package! mathpix.el
;;   :recipe ((:host github :repo "jethrokuan/mathpix.el")))
(package! lsp-latex)
(package! valign
  :recipe (:host github :repo "casouri/valign")
  :pin "be82f6048118cbc81e6e029be1965f933612d871")

(package! bm)
(package! imenu-list)
(package! git-gutter)
(package! projector)
(package! yasnippet-snippets)

(package! anzu)
(package! deadgrep)
(package! color-rg :recipe (:host github :repo "manateelazycat/color-rg"))
(package! visual-regexp)
(package! visual-regexp-steriods
  :recipe (:host github :repo "benma/visual-regexp-steroids.el"))
(package! youdao-dictionary)

(package! cycle-quotes)
(package! delsel)
(package! hungry-delete)
(package! org-bullets)
(package! move-text)
(package! pangu-spacing)
(package! pandoc-mode)
(package! parrot)
(package! string-inflection)
(package! maple-iedit
  :recipe (:host github
           :repo "honmaple/emacs-maple-iedit"))
(package! vmd-mode)
(package! vimrc-mode)

(package! mixed-pitch)
(package! spacemacs-theme)
(package! visual-fill-column)

;; (when (package! eaf :recipe (:host github
;;                              :repo "manateelazycat/emacs-application-framework"
;;                              :files ("*.el" "*.py" "app" "core")
;;                              :build (:not compile)))

;;   (package! ctable :recipe (:host github :repo "kiwanami/emacs-ctable"))
;;   (package! deferred :recipe (:host github :repo "kiwanami/emacs-deferred"))
;;   (package! epc :recipe (:host github :repo "kiwanami/emacs-epc")))

(disable-packages! bookmark tide eldoc grip-mode)
