;;; org-page.el --- 基于org-mode的静态站点生成器

;;; 注释:

;; See documentation at https://github.com/kelvinh/org-page

;; Org-page is a static site generator based on org mode.

;; Org-page provides following features:

;; 1) org sources and html files managed by git
;; 2) incremental publication (according to =git diff= command)
;; 3) category support
;; 4) tags support (auto generated)
;; 5) RSS support (auto generated)
;; 6) search engine support (auto generated)
;; 7) a beautiful theme
;; 8) theme customization support
;; 9) commenting (implemented using disqus)
;; 10) site visiting tracking (implemented using google analytics)
;; 11) index/about page support (auto generated if no default provided)
;; 12) site preview
;; 13) highly customizable

;;; 代码:

(require 'ox)
(require 'ht)
(require 'op-util)
(require 'op-vars)
(require 'op-git)
(require 'op-enhance)
(require 'op-export)
(require 'simple-httpd)
;;org-page版本
(defconst org-page-version "0.5")
;;
(defun op/do-publication (&optional force-all
                                    base-git-commit pub-base-dir
                                    auto-commit auto-push)
  "org-page入口函数,整个工作流程:
1) 验证配置
2) 读取仓库`op/repository-directory'中分支`op/repository-org-branch'中变动文件.变动文件的定义为:
   1. 如果FORCE-ALL 是 non-nil, 所有文件被发布
   2. 如果FORCE-ALL 是 nil, 文件是否变动依据 BASE-GIT-COMMIT 判断
   3. 如果BASE-GIT-COMMIT 是 nil 或者 没有设定, 文件是否变动依据上一次提交判定
3)导出org文件为html.如果 PUB-BASE-DIR 指定,使用指定的目录保存生成的html文件,否则使用repository `op/repository-directory' 中的 branch`op/repository-html-branch' 
4) 如果PUB-BASE-DIR 是 nil, 而且 AUTO-COMMIT 是 non-nil, 那么保存在`op/repository-html-branch'的变动,会自动提交.注意,这种操作不建议,手动提交更安全
5) 如果 PUB-BASE-DIR 是 nil, AUTO-COMMIT 是 non-nil, 而且 AUTO-PUSH 是 non-nil, branch `op/repository-html-branch'会被push到远程仓库"
  (interactive
   (let* ((f (y-or-n-p "Publish all org files? "))
          (b (unless f (read-string "Base git commit: " "HEAD~1")))
          (p (when (y-or-n-p
                    "Publish to a directory? (to original repo if not) ")
               (read-directory-name "html生成目录: ")))
          (a (when (not p)
               (y-or-n-p "自动提交到本地仓库? ")))
          (u (when (and a (not p))
               (y-or-n-p "自动提交到远程仓库? "))))
     (list f b p a u)))
  (op/verify-configuration)
  (setq op/item-cache nil)
  (let* ((orig-branch (op/git-branch-name op/repository-directory))
         (to-repo (not (stringp pub-base-dir)))
         (store-dir (if to-repo "~/.op-tmp/" pub-base-dir)) ; TODO customization
         (store-dir-abs (file-name-as-directory (expand-file-name store-dir)))
         changed-files all-files remote-repos)
    (op/git-change-branch op/repository-directory op/repository-org-branch)
    (op/prepare-theme store-dir)
    (setq all-files
          (cl-remove-if
           #'(lambda (file)
               (let ((root-dir (file-name-as-directory
                                (expand-file-name op/repository-directory))))
                 (member t
                         (mapcar
                          #'(lambda (cat)
                              (string-prefix-p
                               cat
                               (file-relative-name file root-dir)))
                          op/category-ignore-list))))
           (op/git-all-files op/repository-directory)))
    (setq changed-files (if force-all
                            `(:update ,all-files :delete nil)
                          (op/git-files-changed op/repository-directory
                                                (or base-git-commit "HEAD~1"))))
    (op/publish-changes all-files changed-files store-dir-abs)
    (when to-repo
      (op/git-change-branch op/repository-directory op/repository-html-branch)
      (copy-directory store-dir op/repository-directory t t t)
      (delete-directory store-dir t))
    (when (and to-repo auto-commit)
      (op/git-commit-changes op/repository-directory "Update published html \
files, committed by org-page.")
      (when auto-push
        (setq remote-repos (op/git-remote-name op/repository-directory))
        (if (not remote-repos)
            (message "No valid remote repository found.")
          (let (repo)
            (if (> (length remote-repos) 1)
                (setq repo (read-string
                            (format "Which repo to push %s: "
                                    (prin1-to-string remote-repos))
                            (car remote-repos)))
              (setq repo (car remote-repos)))
            (if (not (member repo remote-repos))
                (message "Invalid remote repository '%s'." repo)
              (op/git-push-remote op/repository-directory
                                  repo
                                  op/repository-html-branch)))))
      (op/git-change-branch op/repository-directory orig-branch))
    (if to-repo
        (message "Publication finished: on branch '%s' of repository '%s'."
                 op/repository-html-branch op/repository-directory)
      (message "Publication finished, output directory: %s." pub-base-dir))))

(defun op/new-repository (repo-dir)
  "Generate a new git repository in directory REPO-DIR, which can be
perfectly manipulated by org-page."
  (interactive
   (list (read-directory-name
          "Specify a directory to become the repository: " nil nil nil)))
  (op/git-init-repo repo-dir)
  (op/generate-readme repo-dir)
  (op/git-commit-changes repo-dir "initial commit")
  (op/git-new-branch repo-dir op/repository-org-branch)
  (op/generate-index repo-dir)
  (op/git-commit-changes repo-dir "add source index.org")
  (op/generate-about repo-dir)
  (op/git-commit-changes repo-dir "add source about.org")
  (mkdir (expand-file-name "blog/" repo-dir) t))

(defun op/verify-configuration ()
  "确保所需要的配置文件已经正确配置, 包括:
`op/repository-directory': <required>
`op/site-domain': <required>
`op/personal-disqus-shortname': <optional>
`op/personal-duoshuo-shortname': <optional>
`op/export-backend': [optional](default 'html)
`op/repository-org-branch': [optional] (推荐定制)
`op/repository-html-branch': [optional] (推荐定制)
`op/site-main-title': [optional] (推荐定制)
`op/site-sub-title': [optional] (推荐定制)
`op/personal-github-link': [optional] (but customization recommended)
`op/personal-google-analytics-id': [optional] (but customization recommended)
`op/theme': [optional]
`op/highlight-render': [optional](default 'js)"
  (unless (and op/repository-directory
               (file-directory-p op/repository-directory))
    (error "Directory `%s' is not properly configured."
           (symbol-name 'op/repository-directory)))
  (unless (file-directory-p (op/get-theme-dir))
    (error "Org-page cannot detect theme directory `%s' automatically, please \
help configure it manually, usually it should be <org-page directory>/themes/."
           (symbol-name 'op/theme)))
  (unless op/site-domain
    (error "Site domain `%s' is not properly configured."
           (symbol-name 'op/site-domain)))

  (setq op/repository-directory (expand-file-name op/repository-directory))
  (unless (or (string-prefix-p "http://" op/site-domain)
              (string-prefix-p "https://" op/site-domain))
    (setq op/site-domain (concat "http://" op/site-domain)))
  (unless op/theme
    (setq op/theme 'mdo))
  (unless op/highlight-render
    (setq op/highlight-render 'js)))

(defun op/generate-readme (save-dir)
  "为`op/new-repository'生成README. SAVE-DIR是保存生成的README的目录"
  (string-to-file
   (concat
    (format "Personal site of %s, managed by emacs, org mode, git and org-page."
            (or user-full-name "[Author]"))
    "\n\n"
    "This git repository is generated by org-page \"op/new-repository\" \
function, it is only used for demonstrating how the git branches and directory \
structure are organized by org-page.")
   (expand-file-name "README" save-dir)))

(defun op/generate-index (save-dir)
  "Generate index.org for `op/new-repository'. SAVE-DIR is the directory where
to save generated index.org."
  (string-to-file
   (concat "#+TITLE: Index" "\n\n"
           (format "This is the home page of %s."
                   (or user-full-name "[Author]")))
   (expand-file-name "index.org" save-dir)))

(defun op/generate-about (save-dir)
  "为 op/new-repository 生成about.org .SAVE-DIR 为保存生成的about.org的目录."
  (string-to-file
   (concat "#+TITLE: About" "\n\n"
           (format "* About %s" (or user-full-name "[Author]")) "\n\n"
           "  This file is automatically generated by org-page.")
   (expand-file-name "about.org" save-dir)))

(defun op/insert-options-template (&optional title uri
                                             keywords tags description)
  "在当前buffer中插入模板，为导出准备.

TITLE: 文章标题
URI: 文章uri, 格式通常为: /2013/12/27/the-post-title,
可以使用下面的参数:
    %y: 当天所在的年
    %m: 当天所在的月 
    %d: 当天所在的日期
KEYWORDS: post关键字,供搜引擎使用
TAGS: post的tag,使用逗号或空格分割
DESCRIPTION: post的描述,在RSS订阅中显示

注意：函数不会验证输入的参数，用户需要自己确保参数的有效性"
  (interactive
   (let* ((i (read-string "Title: "))
          (u (read-string "URI(%y, %m and %d can be used to represent year, \
month and day): " (unless (string= i "")
                    (format-spec "/blog/%y/%m/%d/%t"
                                 `((?y . "%y")
                                   (?m . "%m")
                                   (?d . "%d")
                                   (?t . ,(encode-string-to-url i)))))))
          (k (read-string "Keywords(separated by comma and space [, ]): "))
          (a (read-string "Tags(separated by comma and space [, ]): "))
          (d (read-string "Description: ")))
     (list i u k a d)))
  (if (not (bolp)) (newline))
  (insert (format
           "#+TITLE:       %s
#+AUTHOR:      %s
#+EMAIL:       %s
#+DATE:        %s
#+URI:         %s
#+KEYWORDS:    %s
#+TAGS:        %s
#+LANGUAGE:    %s
#+OPTIONS:     H:%d num:%s toc:%s \\n:%s ::%s |:%s ^:%s -:%s f:%s *:%s <:%s
#+DESCRIPTION: %s
"
           (if (string= title "") (buffer-name) title)
           (user-full-name)
           user-mail-address
           (format-time-string (substring (car org-time-stamp-formats) 1 -1))
           (if (string= uri "") "<TODO: insert your uri here>" uri)
           (if (string= keywords "")
               "<TODO: insert your keywords here>"
             keywords)
           (if (string= tags "") "<TODO: insert your tags here>" tags)
           org-export-default-language
           org-export-headline-levels
           nil ;; org-export-with-section-numbers
           nil ;; org-export-with-toc
           org-export-preserve-breaks
           ;; org-export-html-expand
           org-export-with-fixed-width
           org-export-with-tables
           nil ;; org-export-with-sub-superscripts
           nil ;; org-export-with-special-strings
           org-export-with-footnotes
           org-export-with-emphasize
           org-export-with-timestamps
           (if (string= description "")
               "<TODO: insert your description here>"
             description))))

(defun op/new-post (&optional category filename)
  "发布一个新的post.

CATEGORY: post的分类
FILENAME: post的文件名

注意：函数不会验证分类和名字，用户需要自己确保这两个参数的有效性
"
  (interactive
   (let* ((c (read-string "Category: " "blog"))
          (f (read-string "filename: " "new-post.org")))
     (list c f)))
  (if (string= category "")
      (setq category "blog"))
  (if (string= filename "")
      (setq filename "new-post.org"))
  (unless (string-suffix-p ".org" filename)
    (setq filename (concat filename ".org")))
  (let* ((dir (concat (file-name-as-directory op/repository-directory)
                      (file-name-as-directory category)))
         (path (concat dir filename)))
    (op/git-change-branch op/repository-directory op/repository-org-branch)
    (if (file-exists-p path)
        (error "Post `%s' already exists." path))
    (unless (file-directory-p dir)
      (mkdir dir t))
    (switch-to-buffer (find-file path))
    (if (called-interactively-p 'any)
        (call-interactively 'op/insert-options-template)
      (op/insert-options-template "<Insert Your Title Here>"
                                  "/%y/%m/%d/%t/"
                                  "add, keywords, here"
                                  "add, tags, here"
                                  "add description here"))
    (save-buffer)))

(defun op/do-publication-and-preview-site (path)
  "Do publication in PATH and preview the site in browser with simple-httpd.
When invoked without prefix argument then PATH defaults to
`op/site-preview-directory'."
  (interactive
   (if current-prefix-arg
       (list (read-directory-name "Path: "))
       (list op/site-preview-directory)))
  (op/do-publication t nil path)
  (httpd-serve-directory path)
  (browse-url (format "http://%s:%d" system-name httpd-port)))


(provide 'org-page)

;;; org-page.el ends here
