;; #+SETUPFILE: org-head.org
;; #+STARTUP: indent
;; #+AUTHOR: SunDawning ([[https://github.com/SunDawning][Github]]/[[https://steemit.com/@SunDawning][Steemit]])
;; #+EMAIL: dpmeichen@gmail.com
;; #+TITLE: 异步的Emacs的用户配置文件

;; ［创建时间］：<2019-01-12 Sat 17:39:06 UTC+08:00>

;; ［更新时间］：<2020-11-13 Fri 20:41:01 UTC+08:00>

;; 就像没有任何用户配置文件一样打开Emacs，当符合预定的触发条件时，如果没有相应的程序，会尝试异步下载、安装、配置，不能打断当前的任何操作，最好的情况是再次触发该条件时，新的程序已经正常工作，最坏的情况是再次触发该条件时，Emacs还和往常一样，此外，无他。

;; 在
;; [[file:emacs-user-init-file.org]]
;; 基础上重构。




;; ** BOF


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*BOF][BOF:1]]
(defvar earned::*gc-cons-threshold* t)

(when earned::*gc-cons-threshold*
  (setf gc-cons-threshold (* 256 1024 1024)))

;; BOF:1 ends here
;; ** BENCHMARK

;; 按理来说，应该属于＂MODULES＂，但只能放在文件前面的地方，确保之后的过程都能被记录，只是无法记录自身。


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*BENCHMARK][BENCHMARK:1]]
(defun earned:emacs-init-times ()
  "查看加载user-init-file过程里各部分所耗费的时间"
  (interactive)
  (let ((times 'earned::*emacs-init-times*)
        (time 'earned::*emacs-init-time*))
    (when (and (boundp times)
               (symbol-value times))
      (with-current-buffer
          (get-buffer-create
           (format "emacs-init-time: %.2fs"
                   (float-time
                    (if (boundp time) (symbol-value time)
                      (time-subtract after-init-time before-init-time)))))
        (tabulated-list-mode)
        (setf tabulated-list-format
              (vector
               (list "Startˢ" 6
                     (lambda (entry1 entry2)
                       (> (string-to-number (elt (nth 1 entry1) 0))
                          (string-to-number (elt (nth 1 entry2) 0)))))
               (list "Timeᵐˢ" 6
                     (lambda (entry1 entry2)
                       (> (string-to-number (elt (nth 1 entry1) 1))
                          (string-to-number (elt (nth 1 entry2) 1)))))
               (list "Symbol" 26 t))
              tabulated-list-sort-key (cons "Startˢ" t)
              tabulated-list-entries
              (eval
               `(lambda ()
                  (let ((last nil))
                    (cl-loop
                     for (symbol second) in (reverse (symbol-value (quote ,times)))
                     with order = 0
                     do (cl-incf order)
                     collect
                     (let ((duration (if last (- second last) 0))
                           (truncate-second
                            (lambda (second)
                              (format (format "%%.%sf"
                                              (- 6 1 (length (number-to-string (truncate second)))))
                                      second))))
                       (setf last second)
                       (list order (vector (funcall truncate-second second)
                                           (funcall truncate-second (* 1000 duration))
                                           (symbol-name symbol)))))))))
        (tabulated-list-init-header)
        (when (fboundp 'tablist-minor-mode)
          (tablist-minor-mode))
        (tabulated-list-revert)
        (switch-to-buffer (current-buffer))
        (goto-char (point-min))))))

(defun earned::emacs-init-times (symbol)
  (let ((times 'earned::*emacs-init-times*))
    (unless (boundp times)
      (setf (symbol-value times) nil))
    (when (and (boundp times)
               (not (alist-get symbol (symbol-value times)))
               (not (featurep 'earned)))
      (add-to-list times (list symbol (float-time (time-subtract (current-time) before-init-time)))))))

(defun earned::defun (&rest r)
  (when (functionp (function earned::emacs-init-times))
    (let ((name (nth 0 r)))
      (when (string-match "^earned:" (symbol-name name))
        (earned::emacs-init-times name)))))

(when (functionp (function earned::defun))
  (advice-add (quote defun) :after (function earned::defun)))

;; BENCHMARK:1 ends here
;; ** UTILITY

;; 也应该属于＂MODULES＂，只是需要放在文件前面的地方，不如独立分割出来。


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*UTILITY][UTILITY:1]]
(defun earned::run-with-timer (secs repeat function &rest args)
  (when (and (fboundp function)
             earned::*run-with-timer*
             (numberp secs))
    (unless (memq function
                  (mapcar (function timer--function)
                          timer-list))
      (apply (function run-with-timer)
             secs repeat function args))))
(defvar earned::*run-with-timer* t)

(defun earned::global-set-key (keys function)
  (when (commandp function)
    (global-set-key (kbd keys) function)))

(defun earned::define-key (keymap keys function)
  (let ((key (kbd keys)))
    (unless (eq (key-binding key)
                function)
      (when (and (commandp function)
                 (keymapp keymap))
        (define-key keymap key function)))))

(defun earned::add-hook (hook function)
  (when (functionp function)
    (add-hook hook function)))

(defun earned::advice-add (symbol where function)
  (when (and (functionp symbol)
             (functionp function)
             (keywordp where))
    (advice-add symbol where function)))

(defun earned::applies (lists)
  (unless (fboundp 'cl-destructuring-bind)
    (require 'cl-macs))
  (dolist (list lists)
    (cl-destructuring-bind (function i)
        list
      (when (functionp function)
        (dolist (arguments i)
          (when (listp arguments)
            (apply function arguments)))))))

(defun earned::funcall (sequence)
  (mapc (lambda (object)
          (when (functionp object)
            (funcall object)))
        sequence))

(defun earned::executable-find (command)
  (unless (fboundp 'cl-typecase)
    (require 'cl-macs))
  (when (fboundp 'cl-typecase)
    (cl-typecase command
      (null t)
      (string (executable-find command))
      (symbol (earned::executable-find (symbol-name command)))
      (list (unless (functionp (function cl-every))
              (require 'cl-extra))
            (when (functionp (function cl-every))
              (cl-every (function earned::executable-find)
                        command))))))

(defun earned::system-type ()
  (when (functionp (function earned::executable-find))
    (cond ((eq system-type (quote windows-nt)) system-type)
          ((earned::executable-find (quote (termux-setup-storage termux-reload-settings apt)))
           'termux)
          ((and (earned::executable-find (quote (sudo apt uname)))
                (string-match "kali" (shell-command-to-string "uname -a")))
           'kali)
          (t system-type))))

(defun earned::ln (target directory)
  "target是软链所指向的源文件或源文件夹，Specifies the path (relative or absolute) that the new link refers to.
directory是所创建的软链文件夹的名字，Creates a directory symbolic link.  Default is a file symbolic link."
  (when (and (stringp target) (stringp directory)
             (file-directory-p target))
    (when (and (file-directory-p directory)
               ;; ".", ".."
               (= (length (directory-files directory))
                  2))
      (delete-directory directory))
    (unless (file-directory-p directory)
      (let ((target (expand-file-name target))
            (directory (expand-file-name directory)))
        (if (eq system-type (quote windows-nt))
            ;; fix mklink: You do not have sufficient privilege to perform this operation.
            (let ((default-directory temporary-file-directory))
              (let ((temp-file (convert-standard-filename "earned::ln")))
                (with-temp-file temp-file
                  (insert
                   (prin1-to-string
                    `(progn
                       (shell-command-to-string
                        (format "mklink /D %S %S" ,directory ,target))
                       (delete-file ,temp-file)
                       (save-buffers-kill-terminal)))))
                (shell-command-to-string
                 (format "mshta vbscript:CreateObject(\"Shell.Application\").ShellExecute(\"%s\",\"--load %s\",\"\",\"runas\",1)(window.close)"
                         (executable-find "runemacs")
                         temp-file))))
          (when (executable-find "ln")
            (shell-command-to-string
             (format "ln -s %S %S" target directory))))))))

(defun earned::onlinep (host port)
  (let ((name "background-ping")
        (service port))
    (or (cl-some
         (eval
          `(lambda (process)
             (let ((connection
                    (process-contact process t)))
               (and (eq (process-type process)
                        (quote network))
                    (string-match ,name (process-name process))
                    (eq (process-status process)
                        (quote open))
                    (equal (plist-get connection :host)
                           ,host)
                    (equal (plist-get connection :service)
                           ,service)))))
         (process-list))
        (condition-case var
            (make-network-process
             :name name
             :host host
             :service service)
          (error nil)))))

(defun earned::send-string-to-shell (string)
  "并行，如果当前终端处于繁忙状态，便启用新的终端。"
  (unless (and (boundp (quote server-name))
               (stringp server-name)
               (equal server-name "background")
               (string-match "sudo" string)
               (string-match "sudo: no tty present and no askpass program specified" (shell-command-to-string "sudo sudo")))
    (unless (fboundp 'cl-labels)
      (require 'cl-macs))
    (when (fboundp 'cl-labels)
      (cl-labels
          ((shell-busy-p
            (buffer)
            "有没有办法判断 shell buffer 中是否有命令或脚本正在运行 - Emacs-general - Emacs China: https://emacs-china.org/t/shell-buffer/8897"
            (with-current-buffer buffer
              (and (boundp (quote comint-last-prompt)) comint-last-prompt
                   (not (string-match comint-prompt-regexp (buffer-substring-no-properties (car comint-last-prompt) (cdr comint-last-prompt))))))))
        (save-window-excursion
          (let* ((shell (shell))
                 (renamed-shell (when (shell-busy-p shell) (rename-uniquely) (current-buffer)))
                 (buffer
                  (or (when renamed-shell (if (shell-busy-p renamed-shell) (shell) renamed-shell))
                      shell)))
            (process-send-string (get-buffer-process buffer) (concat string "\n"))))))))

;; UTILITY:1 ends here
;; ** MODULES

;; 每当添加一个新功能时，在org-mode里添加模块，编辑代码块，导出到用户配置文件，每当移除一个功能时，编辑代码块让其不再导出到用户配置文件，正是因为每段程序都是各自独立的，在别处使用该程序前都判断该程序是否存在，每一次的增删查改都像第一次的增删查改一样，此外，无他。




;; *** find-file


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*find-file][find-file:1]]
(defun earned:find-file ()
  "`find-file'"
  (interactive)
  (when (fboundp 'earned::ivy)
    (earned::ivy))
  (execute-extended-command current-prefix-arg "find-file"))

;; find-file:1 ends here
;; *** after-init-hook


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*after-init-hook][after-init-hook:1]]
(defun earned:after-init-hook ()
  "Manually run `after-init-hook'"
  (interactive)
  (when (functionp 'earned::after-init-hook)
    (earned::after-init-hook)))

(defun earned::after-init-hook ()
  (when (functionp (function earned::funcall))
    (earned::funcall
     (quote (earned:setf
             earned::coding-system
             earned::exec-path
             earned:applies
             earned::server))))
  (when (functionp 'earned:emacs-init-time)
    (earned:emacs-init-time)))

(defun earned::coding-system ()
  "设置统一的编码系统"
  (let ((coding-system (quote utf-8)))
    (setf default-buffer-file-coding-system coding-system
          selection-coding-system coding-system)
    (prefer-coding-system coding-system)
    (when (eq system-type (quote windows-nt))
      (let ((coding-system (quote utf-16-le)))
        (set-next-selection-coding-system coding-system)
        (set-selection-coding-system coding-system))
      (unless (cl-member 'cp65001 coding-system-list)
        (define-coding-system-alias 'cp65001 coding-system))))
  (when (functionp (function w32-list-locales))
    (setf system-time-locale "ENU")))

(defun earned::exec-path ()
  (dolist (path
           (list
            (file-name-directory (executable-find (car command-line-args)))))
    (add-to-list (quote exec-path)
                 path
                 :test (function file-equal-p))))

(defun earned:setf ()
  "设置全局变量的值"
  (interactive)
  (setf earned::*package-refresh-contents* nil
        custom-file (expand-file-name "custom.el" user-emacs-directory)
        package-archives
        '(("melpa-cn" . "http://elpa.emacs-china.org/melpa/")
          ("org-cn"   . "http://elpa.emacs-china.org/org/")
          ("gnu-cn"   . "http://elpa.emacs-china.org/gnu/"))
        ring-bell-function 'ignore
        (symbol-function 'yes-or-no-p) (symbol-function 'y-or-n-p)
        enable-recursive-minibuffers t
        initial-buffer-choice t
        debug-on-error nil))

(defun earned:applies ()
  "批量添加按键绑定、hook等"
  (interactive)
  (when (functionp 'earned::applies)
    (earned::applies
     `((earned::global-set-key (("C-x C-f" earned:find-file)
                                ("C-x C-w" earned:write-file)
                                ("C-x C-b" ibuffer)
                                ("M-x"     earned:M-x)
                                ("C-c ;"   avy-goto-char)))
       (earned::add-hook       ((emacs-lisp-mode-hook earned::emacs-lisp-mode-hook)
                                (ibuffer-hook         earned::ibuffer-hook)))))))

;; after-init-hook:1 ends here
;; *** org


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*org][org:1]]
(defun earned::org-mode ()
  (unless (zerop org-edit-src-content-indentation)
    (setf truncate-lines nil
          indent-tabs-mode nil
          org-src-fontify-natively nil
          org-edit-src-content-indentation 0
          org-babel-default-header-args
          '((:session . "none")
            (:results . "silent")
            (:exports . "code")
            (:padline . "no")
            (:cache . "no")
            (:noweb . "no")
            (:hlines . "no")
            (:tangle . "no")
            (:comments . "both")
            (:mkdirp . "t"))))
  (when (functionp 'earned::define-key)
    (earned::define-key org-mode-map "C-c ;" (function avy-goto-char))
    (earned::define-key org-mode-map "C-y" (function earned:yank))
    (let ((key (kbd "C-c /")))
      (unless (eq (key-binding key)
                  'toggle-input-method)
        (earned::define-key org-mode-map key (function 五笔)))))
  (when (functionp 'earned::no-indent-tab-whitespace)
    (earned::no-indent-tab-whitespace))
  (when (functionp 'earned::minor-mode)
    (mapcar (function earned::minor-mode)
            (quote
             (undo-tree
              font-lock
              pangu-spacing))))
  (when (functionp 'earned::yasnippet-minor-mode)
    (earned::yasnippet-minor-mode))
  (when (functionp 'earned::goer)
    (earned::goer)))

(when (functionp (function earned::org-mode))
  (with-eval-after-load (quote org)
    (earned::org-mode)
    (when (functionp (function earned::add-hook))
      (earned::add-hook (quote org-mode-hook)
                        (function earned::org-mode)))))

(with-eval-after-load (quote org-src)
  (when (functionp (function earned::define-key))
    (earned::define-key org-src-mode-map "C-x C-s" (function goer-org-edit-src-save)))
  (add-to-list (quote org-src-lang-modes)
               (cons "html" (quote mhtml))))

;; org:1 ends here
;; *** elisp-mode


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*elisp-mode][elisp-mode:1]]
(defun earned::emacs-lisp-mode-hook ()
  (when (functionp 'earned::no-indent-tab-whitespace)
    (earned::no-indent-tab-whitespace))
  (when (functionp (function earned::lisp-minor-modes))
    (earned::lisp-minor-modes))
  (when (functionp 'earned::define-key)
    (earned::define-key emacs-lisp-mode-map "C-c C-s" (function earned:complete-form))))

(defun earned::lisp-minor-modes ()
  (when (functionp 'earned::minor-mode)
    (mapcar (function earned::minor-mode)
            (quote
             (show-paren
              column-number line-number
              pangu-spacing
              font-lock
              rainbow-delimiters
              company aggressive-indent undo-tree symbol-overlay paredit))))
  (when (functionp 'earned::yasnippet-minor-mode)
    (earned::yasnippet-minor-mode)))

;; elisp-mode:1 ends here
;; *** no indent/tab/whitespace


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*no indent/tab/whitespace][no indent/tab/whitespace:1]]
(defun earned::no-indent-tab-whitespace ()
  (setf indent-tabs-mode nil)
  (unless buffer-read-only ; Buffer is read-only
    (untabify (point-min) (point-max)))
  (ignore-errors ; Text is read-only
    (whitespace-cleanup-region (point-min) (point-max))))

;; no indent/tab/whitespace:1 ends here
;; *** ivy


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*ivy][ivy:1]]
(defun earned:M-x ()
  "`execute-extended-command'"
  (interactive)
  (when (functionp 'earned::ivy)
    (earned::ivy))
  (if (functionp 'counsel-M-x)
      (counsel-M-x)
    (execute-extended-command current-prefix-arg)))

(defun earned::ivy ()
  (when (functionp 'earned::package-install)
    (earned::package-install 'counsel)
    (earned::package-install 'smex))
  (when (functionp 'earned::require)
    (earned::require 'ivy))
  (when (featurep 'ivy)
    (unless ivy-use-virtual-buffers
      (ivy-mode)
      (setq-default ivy-use-virtual-buffers t
                    ivy-count-format ""
                    projectile-completion-system 'ivy)
      (define-key ivy-minibuffer-map (kbd "RET") (function ivy-alt-done)))))

;; ivy:1 ends here
;; *** write-file


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*write-file][write-file:1]]
(defun earned:write-file ()
  "`write-file'"
  (interactive)
  (when (functionp 'earned::ivy)
    (earned::ivy))
  (execute-extended-command current-prefix-arg "write-file"))

(defun earned::write-region (oldfun &rest rest)
  "`write-region',rest∈{(START END FILENAME &optional APPEND VISIT LOCKNAME
MUSTBENEW)}"
  (setf (nth 2 rest)
        (convert-standard-filename (nth 2 rest)))
  (apply oldfun rest))

(when (functionp (function earned::advice-add))
  (earned::advice-add (quote write-region) :around (function earned::write-region)))

;; write-file:1 ends here
;; *** package


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*package][package:1]]
(defun earned::package-install (package)
  (when (and (functionp (function earned::package-installed-p))
             (not (earned::package-installed-p package))
             (functionp (function earned::onlinep))
             (earned::onlinep "bing.com" "80"))
    (when (functionp (function earned::package-installing-message))
      (earned::package-installing-message package))
    (if (and (functionp (function background-eval))
             (fboundp 'earned::package-install-asynchronously))
        (earned::package-install-asynchronously package)
      (when (fboundp 'earned::package-install-synchronously)
        (earned::package-install-synchronously package)))))

(defun earned::package-install-asynchronously (package)
  (when (fboundp 'earned::package-installed-p)
    (unless (earned::package-installed-p package)
      (when (and (fboundp 'background-eval)
                 (not (and (boundp (quote server-name))
                           (stringp server-name)
                           (equal server-name "background"))))
        (when (functionp 'earned::server)
          (earned::server))
        (background-eval
         `(when (functionp (function earned::package-install-synchronously))
            (earned::package-install-synchronously (quote ,package)))
         :callback
         `(let ((package (quote ,package)))
            (when (and (functionp (function earned::package-installed-p))
                       (not (earned::package-installed-p package))
                       (functionp (function earned::package-install-synchronously)))
              (earned::package-install-synchronously package))
            (when (functionp (function earned::package-installed-message))
              (earned::package-installed-message package))))))))

(defun earned::package-install-synchronously (package)
  (when (fboundp 'earned::package-installed-p)
    (unless (earned::package-installed-p package)
      (let ((refreshp 'earned::*package-refresh-contents*)
            (emacs-lisp-mode-hook nil))
        (unless (boundp refreshp)
          (setf (symbol-value refreshp)
                nil))
        (when (boundp refreshp)
          (unless (symbol-value refreshp)
            (package-refresh-contents)
            (setf (symbol-value refreshp)
                  t))
          (when (and (symbol-value refreshp)
                     (member package
                             (mapcar (function car)
                                     package-archive-contents)))
            (package-install package t)
            (when (functionp (function earned::package-installed-message))
              (earned::package-installed-message package))))))))

(defun earned::package-installed-p (package)
  (unless (functionp (function package-installed-p))
    (require 'package))
  (when (functionp (function package-installed-p))
    (when (and (boundp 'package--initialized)
               (not package--initialized))
      (package-initialize))
    (or (package-installed-p package)
        (progn
          (package-initialize)
          (package-installed-p package)))))

(defun earned::require (feature)
  (unless (featurep feature)
    (when (and (fboundp 'earned::package-installed-p)
               (earned::package-installed-p feature))
      (require feature))))

(defun earned::package-install-and-require (package)
  (when (functionp 'earned::package-install)
    (earned::package-install package))
  (when (functionp 'earned::require)
    (earned::require package)))

(defun earned::minor-mode (minor-mode-prefix)
  (let ((function (intern (concat "earned::" (symbol-name minor-mode-prefix))))
        (minor-mode (intern (concat (symbol-name minor-mode-prefix) "-mode"))))
    (cond ((and (functionp minor-mode)
                (boundp minor-mode))
           (funcall minor-mode))
          ((functionp function)
           (funcall function))
          (t (when (functionp 'earned::package-install-and-require)
               (earned::package-install-and-require minor-mode-prefix))
             (when (featurep minor-mode-prefix)
               (earned::minor-mode minor-mode-prefix))))))

(defun earned::autoload (symbol feature)
  (when symbol
    (if (listp symbol)
        (dolist (i symbol)
          (earned::autoload i feature))
      (unless (functionp symbol)
        (let ((dependency (intern (concat "earned::" (symbol-name feature)))))
          (when (functionp dependency)
            (eval `(defun ,symbol (&rest rest)
                     (interactive)
                     (message (format "Loading dependency %s..." (quote ,dependency)))
                     (,dependency)
                     (when (featurep (quote ,feature))
                       (if (and (null rest)
                                (commandp (function ,symbol)))
                           (execute-extended-command current-prefix-arg ,(symbol-name symbol))
                         (apply (function ,symbol)
                                rest)))))))))))

(defun earned::package-installing-message (package)
  (let ((installedp 'earned::*package-install*))
    (unless (boundp installedp)
      (setf (symbol-value installedp)
            nil))
    (unless (fboundp 'cl-pushnew)
      (require 'cl-lib))
    (when (and (boundp installedp)
               (functionp (function earned::package-installed-p))
               (not (earned::package-installed-p package))
               (not (member package (symbol-value installedp)))
               (fboundp 'cl-pushnew))
      (cl-pushnew package (symbol-value installedp))
      (when (functionp (function earned::message))
        (earned::message (format "Installing %s" package))))))

(defun earned::package-installed-message (package)
  (when (and (functionp (function earned::package-installed-p))
             (earned::package-installed-p package)
             (functionp (function earned::message)))
    (earned::message (format "Installed %s" package))))

(defun earned::message (string)
  (if (functionp (function notification-default))
      (notification-default "Hi" string)
    (message string)))

;; package:1 ends here
;; *** org project


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*org project][org project:1]]
(defun earned::load-org-project-and-require (feature org-file org-project)
  (when (functionp (function earned::load-org-project))
    (earned::load-org-project org-file org-project)
    (unless (featurep feature)
      (require feature nil t))))

(defun earned::load-org-project (org-file org-project)
  (when (functionp (function earned::tangle-org-project))
    (earned::tangle-org-project org-file org-project))
  (when (file-exists-p org-project)
    (load org-project t t)))

(defun earned::org-babel-tangle-file (org-file)
  (when (file-exists-p org-file)
    (unless (functionp (function org-babel-tangle-file))
      (require 'org nil t))
    (when (functionp (function org-babel-tangle-file))
      (let ((org-babel-default-header-args
             '((:session . "none")
               (:results . "silent")
               (:exports . "code")
               (:padline . "no")
               (:cache . "no")
               (:noweb . "no")
               (:hlines . "no")
               (:tangle . "no")
               (:comments . "both")
               (:mkdirp . "t")))
            (org-mode-hook nil)
            (org-url-hexify-p nil)
            (emacs-lisp-mode-hook nil))
        (org-babel-tangle-file org-file)))))

(defun earned::tangle-org-project (org-file org-project)
  (when (file-exists-p org-file)
    (let ((tangledp 'earned:*tangle-org-project*)
          (org-project (convert-standard-filename org-project)))
      (unless (boundp tangledp)
        (setf (symbol-value tangledp)
              nil))
      (unless (functionp (function cl-assoc))
        (require 'cl-seq))
      (unless (fboundp 'cl-pushnew)
        (require 'cl-lib))
      (when (and (boundp tangledp)
                 (functionp (function cl-assoc))
                 (fboundp 'cl-pushnew))
        (let ((item (cl-assoc org-file (symbol-value tangledp)
                              :test (function equal))))
          (unless (and item
                       (<= (time-to-seconds (time-subtract (current-time)
                                                           (cdr item)))
                           (* 5 60)))
            (cl-pushnew (cons org-file (current-time))
                        (symbol-value tangledp)
                        :test (function equal))
            (when (and (or (not (file-exists-p org-project))
                           (> (time-to-seconds
                               (time-subtract (nth 5 (file-attributes org-file))
                                              (nth 5 (file-attributes org-project))))
                              (* 5 60)))
                       (functionp (function earned::org-babel-tangle-file)))
              (earned::org-babel-tangle-file org-file))))))))

(defun earned:tangle-org-project ()
  "手动导出＂emacs-init-asynchronously.org＂文件"
  (interactive)
  (when (functionp (function earned::org-babel-tangle-file))
    (earned::org-babel-tangle-file "~/literate-programming/emacs-init-asynchronously.org")))

;; org project:1 ends here
;; *** complete-form


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*complete-form][complete-form:1]]
(defun earned:complete-form ()
  "- 可以使用三个＂C-u＂前缀来直接补全所有的参数列表，带括号和所有＂&optional＂之类的那种，可用于改写已有的函数。
- 也可以使用两个＂C-u＂前缀来补全不带括号的参数列表，同样带有＂&optional＂
- 原有的一个＂C-u＂前缀保持原有去除＂&optional＂而补全参数列表的功能
- 最后，如果＂arg＂为空，同样保持忽略所有的可选参数的功能。"
  (interactive)
  (when (and (fboundp 'earned::complete-argument)
             (fboundp 'earned::prefix-arg-to-complete-type))
    (unless (fboundp 'cl-loop)
      (require 'cl-macs))
    (when (fboundp 'cl-loop)
      (cl-loop
       (unless (earned::complete-argument (earned::prefix-arg-to-complete-type current-prefix-arg))
         (cl-return))))))

(defun earned::complete-argument (complete-type)
  "complete-type∈{`earned::prefix-arg-to-complete-type'}"
  (unless (functionp (function cl-second))
    (require 'cl-lib))
  (unless (fboundp 'cl-labels)
    (require 'cl-macs))
  (unless (functionp (function cl-remove-if))
    (require 'cl-seq))
  (unless (functionp (function cl-subseq))
    (require 'cl-extra))
  (when (and (functionp (function cl-second))
             (fboundp 'cl-labels)
             (functionp (function cl-remove-if))
             (functionp (function cl-subseq)))
    (let* ((fnsym (elisp--fnsym-in-current-sexp))
           (symbol (car fnsym))
           (position (cl-second fnsym)))
      (cl-labels
          ((append-current
            (position args)
            (let ((current (nth (1- position) args)))
              (when current (insert (downcase (prin1-to-string current)))
                    t)))
           (end-of-place
            () (unless (memq (char-before) '(40 32)) (beginning-of-sexp))
            (forward-sexp))
           (append-whitespace
            () (if (eq (char-after) 32)
                   (forward-char)
                 (insert " ")))
           (append-end
            (args)
            (let ((position (cl-second (elisp--fnsym-in-current-sexp))))
              (when (append-current position args)
                (unless (= position (length args)) (insert " "))
                t)))
           (args
            (complete-type optional-parameter-position position optional-parameters full)
            (cond
             ((eq complete-type :full-with-optional-and-bracket)
              (list full))
             ((eq complete-type :full-with-optional)
              full)
             ((or (eq complete-type :full-without-optional)
                  (not optional-parameter-position)
                  (and (< (1- position) optional-parameter-position)
                       (eq (char-before) 32) (memq (char-after) '(32 10)))
                  (>= (1- position) optional-parameter-position))
              (cl-remove-if (lambda (item) (memq item optional-parameters)) full))
             ((eq complete-type :without-optional)
              (cl-subseq full 0 optional-parameter-position)))))
        (when (and symbol (fboundp symbol))
          (let* ((current-sexp (save-excursion (backward-up-list nil t) (sexp-at-point)))
                 (optional-parameters '(&optional &rest &body &key &aux &allow-other-keys))
                 (full (car (read-from-string (replace-regexp-in-string "\\.\\{3\\}" "" (elisp-get-fnsym-args-string symbol nil "")))))
                 (optional-parameter-position (cl-position-if (lambda (item) (memq item optional-parameters)) full))
                 (args (args complete-type optional-parameter-position position optional-parameters full))
                 (args-length (length args))
                 (current-sexp-length (length current-sexp)))
            (cond
             ((or (not args) (not (listp args))) ; (defun [Arg list not available until function definition is loaded.])
              nil)
             ((= args-length (1- current-sexp-length))
              nil)
             ((and (eq (char-before) 32) (memq (char-after) '(32 10)) (< position args-length))
              (append-current position args))
             ((= (1+ position) current-sexp-length)
              (end-of-place)
              (append-whitespace)
              (append-end args))
             ((= position current-sexp-length)
              (append-end args)))))))))

(defun earned::prefix-arg-to-complete-type (arg)
  "arg∈{(64),(16),(4),nil,3,2,1,0}"
  (unless (functionp (function cl-some))
    (require 'cl-extra))
  (unless (functionp (function cl-find))
    (require 'cl-seq))
  (or (when (and (functionp (function cl-some))
                 (functionp (function cl-find)))
        (cl-some
         (lambda (list)
           (apply (lambda (base type)
                    (when (cl-find arg (list base (list (expt 4 base)))
                                   :test 'equal)
                      type))
                  list))
         '((3 :full-with-optional-and-bracket)
           (2 :full-with-optional)
           (1 :full-without-optional)
           (0 :without-optional))))
      :without-optional))

;; complete-form:1 ends here
;; *** ibuffer-vc


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*ibuffer-vc][ibuffer-vc:1]]
(defun earned::ibuffer-hook ()
  (when (functionp 'earned::package-install-and-require)
    (earned::package-install-and-require 'ibuffer-vc))
  (font-lock-mode)
  (when (and (featurep 'ibuffer-vc)
             (eq major-mode 'ibuffer-mode))
    (ibuffer-vc-set-filter-groups-by-vc-root)
    (unless (eq ibuffer-sorting-mode 'filename/process)
      (ibuffer-do-sort-by-filename/process))))

;; ibuffer-vc:1 ends here
;; *** background


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*background][background:1]]
(let ((org-file "~/literate-programming/emacs-lisp-background-task.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::background ()
        (when (functionp 'earned::load-org-project-and-require)
          (earned::load-org-project-and-require 'background ,org-file "~/.emacs.d/site-lisp/emacs-lisp-background-task/background-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (background-debug
     background-debug-view-log-file
     background-debug-kill-all-background-processes-and-emacs-daemon
     background-debug-server-running-p
     background-debug-with-gnome-terminal
     background-debug-org-eval-src-block
     background-debug-org-tangle-and-load
     background-eval
     background-decode
     background-encode))
   (quote background)))

;; background:1 ends here
;; *** yasnippet


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*yasnippet][yasnippet:1]]
(let ((org-file "~/literate-programming/yasnippet.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::yasnippet-minor-mode ()
        (when (functionp (function earned::yasnippet))
          (earned::yasnippet))
        (when (featurep 'yasnippet)
          (when (functionp 'earned::tangle-org-project)
            (earned::tangle-org-project ,org-file "~/.emacs.d/snippets/org-mode/<head-插入-org-mode-head.snippet"))
          (advice-add 'message :override 'ignore)
          (yas-reload-all)
          (advice-remove 'message 'ignore)
          (yas-minor-mode))))))

(defun earned::yasnippet ()
  (when (functionp (function earned::package-install-and-require))
    (earned::package-install-and-require 'yasnippet)))

;; yasnippet:1 ends here
;; *** goer


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*goer][goer:1]]
(let ((org-file "~/literate-programming/org-mode-user.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::goer ()
        (when (functionp 'earned::load-org-project-and-require)
          (earned::load-org-project-and-require 'goer ,org-file "~/.emacs.d/site-lisp/org-mode-user/goer-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (goer-jump-to-tangle
     goer-jump-to-org
     goer-org-mode
     goer-org-to-html-and-open
     goer-tangle-and-load-tangled-file
     goer-background-tangle-and-load-tangled-file
     goer-babel-tangle-file
     goer-background-babel-tangle-file
     goer-find-definitions
     goer-view-todos-in-directory
     goer-view-create-times-in-directory
     goer-view-orgs-in-directory
     goer-org-edit-src-save
     goer-normal-mode
     goer-find-file
     goer-outline
     goer-copy-the-top-level-symbolic-expression
     goer-execute-the-top-level-symbolic-expression
     goer-org-scratch))
   (quote goer)))

(when (functionp (function earned::global-set-key))
  (earned::global-set-key "M-." (function goer-find-definitions)))

(when (functionp (function earned::advice-add))
  (eval-after-load 'goer
    `(earned::advice-add 'normal-mode :override 'goer-normal-mode)))

;; goer:1 ends here
;; *** avy


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*avy][avy:1]]
(defun earned::avy ()
  (when (functionp (function earned::package-install-and-require))
    (earned::package-install-and-require 'avy)))

(when (functionp (function earned::autoload))
  (earned::autoload 'avy-goto-char 'avy))

;; avy:1 ends here
;; *** server


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*server][server:1]]
(unless (eq system-type 'windows-nt)
  (defun earned::server ()
    (unless (fboundp 'server-running-p)
      (require 'server))
    (when (fboundp 'server-running-p)
      (unless (server-running-p server-name)
        (server-start)))))

;; server:1 ends here
;; *** switch-buffer


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*switch-buffer][switch-buffer:1]]
(defun earned:switch-to-buffer ()
  "`switch-to-buffer'"
  (interactive)
  (when (functionp 'earned::ivy)
    (earned::ivy))
  (if (functionp 'ivy-switch-buffer)
      (ivy-switch-buffer)
    (execute-extended-command current-prefix-arg "switch-to-buffer"))
  (when (boundp (quote earned::*switch-to-buffer-hook*))
    (run-hook-with-args (quote earned::*switch-to-buffer-hook*))))

(when (functionp (function earned::global-set-key))
  (earned::global-set-key "C-x b" (function earned:switch-to-buffer)))

;; switch-buffer:1 ends here
;; *** git


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*git][git:1]]
(unless (executable-find "git")
  (when (eq system-type (quote windows-nt))
    (let ((directory (expand-file-name "c:/Program Files/Git/bin/")))
      (when (file-exists-p directory)
        (add-to-list (quote exec-path)
                     directory)))))

;; git:1 ends here
;; *** magit


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*magit][magit:1]]
(defun earned::magit ()
  (when (and (executable-find "git")
             (not (version< emacs-version "25.1"))
             (functionp (function earned::package-install-and-require)))
    (earned::package-install-and-require 'magit)))

(defun earned::magit-log-mode-hook ()
  (setf truncate-lines nil
        magit-buffer-margin '(t age 30 t 10)))

(when (functionp (function earned::add-hook))
  (earned::add-hook (quote magit-diff-mode-hook)
                    (function font-lock-mode))
  (earned::add-hook (quote magit-log-mode-hook)
                    (function earned::magit-log-mode-hook)))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (magit-diff-buffer-file
     magit-status
     magit-log-all
     magit-log-buffer-file
     magit-file-checkout
     magit-stage-file
     magit-push-current))
   (quote magit)))

;; magit:1 ends here
;; *** orgit


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*orgit][orgit:1]]
(let ((org-file "~/literate-programming/orgit.org"))
  (when (and (executable-find "git")
             (file-exists-p org-file))
    (eval
     `(defun earned::orgit ()
        (when (and (executable-find "git")
                   (functionp (function earned::load-org-project-and-require)))
          (earned::load-org-project-and-require 'orgit ,org-file "~/.emacs.d/site-lisp/orgit/orgit-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (orgit-background-add-commit-current-buffer-file
     orgit-background-add-commit-current-buffer-file-and-org-project
     orgit-add-commit-current-buffer-file-and-org-project
     orgit-add-commit-current-buffer-file
     orgit-push-current-org-project
     orgit-push
     orgit-jump-to-org-project-directory
     orgit-copy-line
     orgit-reset-current-project
     orgit-reset
     orgit-org-projects))
   (quote orgit)))

;; orgit:1 ends here
;; *** 五笔


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*五笔][五笔:1]]
(let ((org-file "~/literate-programming/五笔.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::五笔 ()
        (when (functionp (function earned::load-org-project-and-require))
          (earned::load-org-project-and-require '五笔 ,org-file "~/.emacs.d/site-lisp/五笔/五笔输入法用户配置.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (五笔
     五笔:添加新词组
     五笔:添加当前页面
     五笔:后台添加当前页面
     五笔:修复词库
     五笔:查询编码
     五笔:美化词库文件))
   (quote 五笔)))

(when (functionp (function earned::global-set-key))
  (earned::global-set-key "C-c /" (function 五笔)))

;; 五笔:1 ends here
;; *** close-emacs


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*close-emacs][close-emacs:1]]
(defun earned:close-emacs-and-logoff-logout-the-computer (&optional arg)
  (interactive "P")
  (if arg
      (run-with-timer
       (string-to-number
        (read-string "Close Emacs After Seconds: "))
       nil
       (lambda ()
         (when (functionp (function earned:close-emacs-and-logoff-logout-the-computer))
           (earned:close-emacs-and-logoff-logout-the-computer))))
    (when (functionp (function earned::close-emacs))
      (earned::close-emacs
       (lambda ()
         (when (functionp (function earned::shutdown))
           (earned::shutdown (quote logoff))))))))

(defun earned:close-emacs-and-shut-down-poweroff-the-computer (&optional arg)
  "Restart emacs from within emacs - Emacs Stack Exchange: https://emacs.stackexchange.com/questions/5428/restart-emacs-from-within-emacs"
  (interactive "P")
  (if arg
      (run-with-timer
       (string-to-number
        (read-string "Close Emacs After Seconds: "))
       nil
       (lambda ()
         (when (functionp (function earned:close-emacs-and-shut-down-poweroff-the-computer))
           (earned:close-emacs-and-shut-down-poweroff-the-computer))))
    (when (functionp (function earned::close-emacs))
      (earned::close-emacs
       (lambda ()
         (when (functionp (function earned::load-org-project))
           (earned::tangle-org-project "~/literate-programming/emacs-init-asynchronously.org" "~/.emacs.d/init.el"))
         (when (functionp (function earned::git-push))
           (earned::git-push "~/literate-programming/"))
         (when (functionp (function earned::shutdown))
           (earned::shutdown 'poweroff)))))))

(defun earned:close-emacs-and-restart-reboot-computer (&optional arg)
  (interactive "P")
  (if arg
      (run-with-timer
       (string-to-number
        (read-string "Close Emacs After Seconds: "))
       nil
       (lambda ()
         (when (functionp (function earned:close-emacs-and-restart-reboot-computer))
           (earned:close-emacs-and-restart-reboot-computer))))
    (when (functionp (function earned::close-emacs))
      (earned::close-emacs
       (lambda ()
         (when (functionp (function earned::shutdown))
           (earned::shutdown 'reboot)))))))

(defun earned::close-emacs (function)
  (let ((kill-emacs-hook
         (append kill-emacs-hook
                 (list function)))
        (message '("
         .----.
      _.'__    `.
  .--(#)(##)---/#\\
.' @          /###\\
:         ,   #####
 `-..__.-' _.-\\###/
       `;_:    `\"'
     .'\"\"\"\"\"`.
    /,  GOD  ,\\
   // Bless U. \\\\
   `-._______.-'
   ___`. | .'___
  (______|______)" "
 　 ﾍ^ヽ､　 /⌒､　_,_
　 |  　 ￣7　 (⌒r⌒7/
　 レ　　 　＼_/￣＼_｣
＿/　　　　　　　　 {
_ﾌ　●　　　　　　　ゝ
_人　　　ο　　● 　 ナ
　 `ト､＿　　　　　メ
　　　 /　 ￣ ーィﾞ
　　 〈ﾟ･｡｡｡･ﾟ 　丶}" "
       ○ ＿＿＿＿
       ‖  GOD  |
       ‖ Bless |
       ‖   U   |
       ‖￣￣￣￣
  ∧__∧ ‖
 \(`･ω･ ‖
 \ つ ０
  し--Ｊ" "
 　 \\　  /
┏━━━━\\ /━━━━┓
┃┏━━━━━━━━━┓┃
┃┃   GOD   ┃┃
┃┃ Bless U ┃┃
┃┗━━━━━━━━━┛┃
┗━━━∪━━━∪━━━┛" "
{\____/}
\(✿◕‿◕)
/つᴳᴼᴰBₗₑₛₛᵁ")))
    (switch-to-buffer "*close-emacs*")
    (erase-buffer)
    (insert (nth (random (length message)) message))
    (kill-emacs)))

;; close-emacs:1 ends here
;; *** beautify


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*beautify][beautify:1]]
(defun earned:beautify ()
  "美化：更换颜色、设置模式栏、调整字体、隐藏菜单工具滚动栏。

目的是将一些占用大量时间的功能通过按键或者手动触发，但在触发之前不能影响使用Emacs，其本身可以占用超长时间。

因此，这里可以容纳不只有美化Emacs的功能，譬如：

- 在Linux下手动开启WiFI等服务
- 在Windows下手动关闭其他无用的程序"
  (interactive)
  (when (functionp (function earned::funcall))
    (earned::funcall (quote
                      (earned::beautify-color
                       earned::beautify-mode-line
                       earned::beautify-font
                       earned::beautify-window
                       earned::beautify-others)))))

(when (functionp (function earned::global-set-key))
  (earned::global-set-key "<f5>" (function earned:beautify)))

(defun earned::beautify-color ()
  (global-font-lock-mode)
  (unless custom-enabled-themes
    (dolist (theme custom-enabled-themes)
      (disable-theme theme))
    (load-theme (quote manoj-dark))))

(defun earned::beautify-mode-line ()
  (let ((format "(%Y-%m-%d %a %H:%M)"))
    (unless (boundp 'display-time-format) (require 'time))
    (when (boundp 'display-time-format)
      (unless (equal display-time-format format)
        (setf display-time-format format)
        (display-time-mode)
        (display-battery-mode)
        (custom-set-variables
         '(mode-line-format
           '("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position
             (vc-mode vc-mode)
             "  " mode-line-misc-info mode-line-end-spaces)))))))

(defun earned::beautify-window ()
  (if (display-graphic-p)
      (when menu-bar-mode (menu-bar-mode 0))
    (when (= (frame-parameter (car (frame-list))
                              (quote menu-bar-lines))
             1)
      (menu-bar-mode 0)))
  (when (and (fboundp 'tool-bar-mode) tool-bar-mode)
    (tool-bar-mode 0))
  (when (and (display-graphic-p) scroll-bar-mode)
    (scroll-bar-mode 0)
    (unless (when fringe-mode (zerop fringe-mode))
      (fringe-mode 5)))
  (dotimes (i 3)
    (toggle-frame-fullscreen))
  ;; Emacs 透明窗口 - Emacs-general - Emacs China: https://emacs-china.org/t/topic/2405/16
  (let ((value 70)
        (frame nil)
        (parameter (quote alpha)))
    (unless (and (frame-parameter frame parameter)
                 (= (frame-parameter frame parameter)
                    value))
      (set-frame-parameter nil parameter value))))

(defun earned::beautify-font ()
  (when (display-graphic-p)
    (when (and (not (functionp (function default-text-scale-increment)))
               (functionp (function earned::package-install-and-require)))
      (earned::package-install-and-require (quote default-text-scale)))
    (when (functionp (function default-text-scale-increment))
      (let ((size 130)
            (height (face-attribute 'default :height)))
        (when (>= (abs (- size height)) 10)
          (default-text-scale-increment (- size height)))))
    (or (when (functionp (function earned::beautify-windows-nt-font))
          (earned::beautify-windows-nt-font))
        (when (functionp (function earned::beautify-gnu-linux-font))
          (earned::beautify-gnu-linux-font)))))

(when (eq system-type 'windows-nt)
  (eval
   `(defun earned::beautify-windows-nt-font ()
      (let ((family "Lucida Console"))
        (unless (functionp (function cl-member))
          (require 'cl-seq))
        (when (and (eq system-type 'windows-nt)
                   (functionp (function cl-member))
                   (cl-member "Lucida Console"
                              (font-family-list)
                              :test (function string-equal)))
          (let ((font (format "-outline-Lucida Console-normal-normal-normal-mono-%d-*-*-*-c-*-iso10646-1"
                              (line-pixel-height))))
            (unless (string-equal (frame-parameter nil 'font)
                                  font)
              (set-frame-font font)
              family)))))))

(defun earned::font-family-p (family)
  (unless (functionp (function cl-member))
    (require 'cl-seq))
  (when (functionp (function cl-member))
    (car (cl-member family (font-family-list)
                    :test (function string-equal)))))

(when (eq system-type 'gnu/linux)
  (eval
   `(defun earned::beautify-gnu-linux-font ()
      "Emacs中英文等宽字体设置 - peng_gy - 博客园: http://www.cnblogs.com/penggy/p/7475831.html"
      (unless (functionp (function cl-some))
        (require (quote cl-extra)))
      (let* ((families (quote ("WenQuanYiZenHeiMono" "WenQuanYi Zen Hei Mono")))
             (family
              (when (and (functionp (function earned::font-family-p))
                         (functionp (function cl-some)))
                (cl-some (function earned::font-family-p) families))))
        (when (and (functionp (function earned::start-process-shell-command))
                   (not family))
          (when (and (not (file-exists-p "~/.fonts/WenQuanYiZenHeiMono.ttf"))
                     (executable-find "wget"))
            (earned::start-process-shell-command "background-wget-font" "wget -c https://github.com/l04m33/dot-files/raw/master/.stumpwm.d/fonts/WenQuanYiZenHeiMono.ttf -P ~/.fonts"))
          (when (and (file-exists-p "~/.fonts/WenQuanYiZenHeiMono.ttf")
                     (executable-find "fc-cache"))
            (earned::start-process-shell-command nil "fc-cache ~/.fonts")))
        (set-face-attribute 'default nil :font
                            (format "%s:pixelsize=%d" family 30))
        (let ((family
               (when (and (functionp (function earned::font-family-p))
                          (functionp (function cl-some)))
                 (cl-some (function earned::font-family-p) families))))
          (when family
            (dolist (charset '(kana han symbol cjk-misc bopomofo))
              (set-fontset-font (frame-parameter nil 'font) charset
                                (font-spec :family family :size 22)))
            family))))))

(defun earned::beautify-others ()
  (when (functionp (earned::auto-windows-startup))
    (earned::auto-windows-startup))
  (when (functionp (function earned:redmibook-air-13))
    (earned:redmibook-air-13)))

;; beautify:1 ends here
;; *** shell-command


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*shell-command][shell-command:1]]
(defun earned::start-process-shell-command (name command)
  (unless (functionp (function cl-find-if-not))
    (require 'cl-seq))
  (when (and (functionp (function cl-find-if-not))
             (not (cl-find-if
                   (lambda (item)
                     (cl-find command item :test (function equal)))
                   (mapcar (function process-command)
                           (process-list)))))
    (start-process-shell-command
     (or name "background")
     (when name (generate-new-buffer-name name))
     command)))

;; shell-command:1 ends here
;; *** inferior-emacs-lisp-mode


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*inferior-emacs-lisp-mode][inferior-emacs-lisp-mode:1]]
(defun earned::inferior-emacs-lisp-mode-hook ()
  (when (functionp (function earned::lisp-minor-modes))
    (earned::lisp-minor-modes))
  (when (functionp 'earned::define-key)
    (earned::define-key inferior-emacs-lisp-mode-map "C-c C-s" (function earned:complete-form))))

(when (functionp (function earned::add-hook))
  (earned::add-hook (quote ielm-mode-hook)
                    (function earned::inferior-emacs-lisp-mode-hook)))

;; inferior-emacs-lisp-mode:1 ends here
;; *** lisp-interaction-mode


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*lisp-interaction-mode][lisp-interaction-mode:1]]
(defun earned::lisp-interaction-mode-hook ()
  (when (functionp (function earned::lisp-minor-modes))
    (earned::lisp-minor-modes))
  (when (functionp 'earned::define-key)
    (earned::define-key lisp-interaction-mode-map "C-c C-s" (function earned:complete-form))))

(when (functionp (function earned::add-hook))
  (earned::add-hook (quote lisp-interaction-mode-hook)
                    (function earned::lisp-interaction-mode-hook)))

;; lisp-interaction-mode:1 ends here
;; *** *scratch*


;; [[file:~/literate-programming/emacs-init-asynchronously.org::**scratch*][*scratch*:1]]
(defun earned::scratch-buffer-hook ()
  (when (and (equal (buffer-name) "*scratch*")
             (eq major-mode 'lisp-interaction-mode)
             (functionp (function earned::lisp-interaction-mode-hook)))
    (earned::lisp-interaction-mode-hook)))

(setf initial-major-mode
      (lambda ()
        (delay-mode-hooks (lisp-interaction-mode))))

(when (functionp (function earned::add-hook))
  (earned::add-hook (quote earned::*switch-to-buffer-hook*)
                    (function earned::scratch-buffer-hook)))


;; *scratch*:1 ends here
;; *** emacs-init-time


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*emacs-init-time][emacs-init-time:1]]
(defun earned:emacs-init-time ()
  "类似于`emacs-init-time'，计算从加载init.el开始到能正常使用Emacs之间的时间。"
  (interactive)
  (let ((time 'earned::*emacs-init-time*))
    (message
     (format "%.3f seconds"
             (float-time
              (if (boundp time)
                  (symbol-value time)
                (setf (symbol-value time)
                      (time-subtract (current-time)
                                     before-init-time))))))))

;; emacs-init-time:1 ends here
;; *** try user-init-file

;; #+NAME: try-user-init-file

;; [[file:~/literate-programming/emacs-init-asynchronously.org::try-user-init-file][try-user-init-file]]
(defun earned::try-user-init-file (select collection)
  (when (and (executable-find "emacs") (display-graphic-p))
    (let ((tmp temporary-file-directory)
          (start-process-command ; start-process
           (lambda (name command &optional sentinel)
             (unless (functionp (function cl-find-if-not)) (require 'cl-seq))
             (when (and (functionp (function cl-find-if-not)) (functionp (function make-process))
                        (not (cl-find-if (lambda (item) (cl-find command item :test (function equal))) (mapcar (function process-command) (process-list)))))
               (make-process :name name :command (cons shell-file-name (list shell-command-switch command)) :sentinel sentinel)))))
      (when (and (file-exists-p tmp)
                 select (symbolp select) collection (listp collection) (listp (car collection)))
        (let* ((from (alist-get select collection))
               (emacs-d (concat (symbol-name select) ".emacs.d"))
               (to (file-name-as-directory (expand-file-name emacs-d tmp)))
               (init (expand-file-name "init.el" to)))
          ;; download or copy init.el
          (if (let ((cl-x (url-generic-parse-url from))) (and (url-type cl-x) (url-host cl-x)))
              (when (and (not (file-exists-p init)) (executable-find "git") (not (file-exists-p to)))
                (message "Cloning to %s" to)
                (funcall
                 start-process-command (concat "background-clone-" emacs-d)
                 (format "%S clone --depth=1 %s %s"  (executable-find "git") from to)
                 (eval
                  `(lambda (&rest rest)
                     ;; Spacemacs needs downloading melpa repository.
                     (when (eq (quote ,select) 'spacemacs)
                       (let ((melpa (expand-file-name ".cache/quelpa/melpa" ,to))
                             (start-process-command (function ,start-process-command)))
                         (when (and (not (file-exists-p melpa)) (functionp start-process-command)
                                    (executable-find "git"))
                           (funcall start-process-command "background-clone-melpa" (concat (executable-find "git") " clone --depth=1 https://github.com/melpa/melpa.git " melpa)))))
                     ;; Start Emacs or notify
                     (if (functionp (function earned::try-user-init-file))
                         (earned::try-user-init-file (quote ,select) (quote ,collection))
                       (message ,(format "Cloned to %s" to)))))))
            (unless (file-exists-p to) (make-directory to))
            (copy-file (expand-file-name "init.el" from) init t))
          (when (file-exists-p init)
            ;; copy local .emacs.d/elpa directory
            (let ((elpa (file-name-as-directory (expand-file-name "elpa" user-emacs-directory))))
              (when (and (not (file-exists-p (expand-file-name "elpa" to))) (file-exists-p elpa))
                (copy-directory elpa to t t)))
            ;; exwm
            (when (functionp (function earned::exwm)) (earned::exwm))
            ;; emacs -Q
            (funcall
             start-process-command (concat "background-" emacs-d)
             (format
              "%S -Q -D --load %S" (executable-find "emacs")
              (let ((temporary-file (expand-file-name (concat "try-" emacs-d) temporary-file-directory)))
                (when (file-exists-p temporary-file) (delete-file temporary-file))
                (with-temp-file temporary-file
                  (pp
                   `(progn
                      (setf user-emacs-directory ,to user-init-file ,init default-directory ,tmp
                            server-name ,emacs-d)
                      (unless (member 'cp65001 coding-system-list)
                        (define-coding-system-alias 'cp65001 'utf-8))
                      (load user-init-file)
                      (unless (eq system-type 'windows-nt)
                        (unless (functionp (function server-running-p)) (require (quote server)))
                        (when (and (functionp (function server-running-p)) (not (server-running-p))) (server-start)))
                      (run-hook-with-args 'after-init-hook))
                   (current-buffer)))
                temporary-file)))))))))

(defun earned:try-user-init-file ()
  "从零开始试用`user-init-file'，用于学习与调试。"
  (interactive)
  (let ((collection
         `((purcell                   . "https://github.com/purcell/emacs.d.git")
           (spacemacs                 . "https://gitee.com/mirrors/spacemacs.git")
           (centaur-emacs             . "https://github.com/seagle0128/.emacs.d.git")
           (seagle0128                . "https://github.com/seagle0128/.emacs.d.git")
           (sundawning                . "https://github.com/SunDawning/.emacs.d.git")
           (,(intern user-login-name) . ,user-emacs-directory))))
    (when (functionp (function earned::try-user-init-file))
      (earned::try-user-init-file
       (intern (completing-read "emacs.d: "
                                (mapcar (function car) collection)
                                nil t))
       collection))))

;; try-user-init-file ends here
;; *** exwm


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*exwm][exwm:1]]
(let ((org-file "~/literate-programming/exwm.org"))
  (when (and (executable-find "xinit")
             (eq system-type 'gnu/linux)
             (display-graphic-p)
             (file-exists-p org-file))
    (eval
     `(defun earned::exwm (&rest rest)
        (when (and (not (functionp (function exwm-init)))
                   (functionp (function earned::package-install-and-require)))
          (earned::package-install-and-require 'exwm))
        (when (functionp (function earned::tangle-org-project))
          (earned::tangle-org-project ,org-file "~/.xinitrc"))
        (when (and (functionp (function exwm-init))
                   (or (not (boundp 'exwm-input-simulation-keys))
                       (null exwm-input-simulation-keys)))
          (setf exwm-input-simulation-keys
                '(([?\C-b] . [left])
                  ([?\C-f] . [right])
                  ([?\C-p] . [up])
                  ([?\C-n] . [down])
                  ([?\C-a] . [home])
                  ([?\C-e] . [end])
                  ([?\M-v] . [prior])
                  ([?\C-v] . [next])
                  ([?\C-d] . [delete])
                  ([?\C-k] . [S-end delete])))
          (exwm-init))
        (when (functionp (function earned::ewe))
          (earned::ewe))))))

(when (functionp (function earned::exwm))
  (eval
   `(progn
      (defun earned:exwm ()
        "启用EXWM"
        (interactive)
        (earned::exwm))
      (defun earned::exwm-update-class-hook ()
        (when (fboundp 'exwm-workspace-rename-buffer)
          (exwm-workspace-rename-buffer exwm-class-name))))))

(when (functionp (function earned::add-hook))
  (earned::add-hook (quote exwm-update-class-hook)
                    (function earned::exwm-update-class-hook)))

(when (functionp (function earned::advice-add))
  (dolist (symbol (quote
                   (browse-url
                    browse-url-generic
                    background-debug-with-gnome-terminal)))
    (earned::advice-add symbol :before (function earned::exwm))))

;; exwm:1 ends here
;; *** brightness


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*brightness][brightness:1]]
(defun earned::show-brightness-guidance (brightness)
  "参照`quail-show-guidance'，在调整亮度之后显示上一步的操作"
  (when (minibufferp)
    (unless (functionp (function quail-minibuffer-message))
      (require (quote quail)))
    (when (functionp (function quail-minibuffer-message))
      (quail-minibuffer-message
       (format "\n上一个操作：%s" brightness)))))

(let ((brightness-file "/sys/class/backlight/acpi_video0/brightness"))
  (when (file-exists-p brightness-file)
    (eval
     `(defun earned::brightness (brightness)
        (let ((brightness-file ,brightness-file))
          (when (and (file-exists-p brightness-file)
                     (not (zerop (length (format "%s" brightness)))))
            (let ((current
                   (string-to-number
                    (with-temp-buffer
                      (insert-file-contents brightness-file)
                      (buffer-string)))))
              (unless (fboundp 'cl-case)
                (require 'cl-macs))
              (when (fboundp 'cl-case)
                (cl-case brightness
                  (+ (earned::brightness (1+ current)))
                  (- (earned::brightness (1- current)))
                  (t (let* ((new (string-to-number (format "%s" brightness)))
                            (brightness  (cond ((< new 0) 0)
                                               ((> new 10) 10)
                                               (t new)))
                            (string
                             (if (executable-find "sudo")
                                 (format "echo %d > brightness && sudo cp brightness %s && rm brightness" brightness brightness-file)
                               (format "su -c 'echo %d > %s'" brightness brightness-file))))
                       (when (functionp (function earned::send-string-to-shell))
                         (earned::send-string-to-shell string))
                       (when (functionp (function earned::show-brightness-guidance))
                         (earned::show-brightness-guidance brightness)))))))))))))

(unless (functionp (function earned::brightness))
  (let ((nircmd "nircmd.exe"))
    (unless (executable-find nircmd)
      (let ((default-directory
              (expand-file-name
               "nircmd"
               (expand-file-name "Downloads" (getenv "userprofile")))))
        (when (file-exists-p (expand-file-name nircmd default-directory))
          (add-to-list (quote exec-path) default-directory))))
    (when (executable-find nircmd)
      (eval
       `(defun earned::brightness (brightness)
          (shell-command-to-string
           (format "%S %s" (executable-find ,nircmd)
                   (if (memq brightness (quote (- +)))
                       (format "changebrightness %s" (if (eq brightness (quote -)) -10 10))
                     (format "setbrightness %s" (* 10 (string-to-number (format "%s" brightness)))))))
          (when (functionp (function earned::show-brightness-guidance))
            (earned::show-brightness-guidance brightness)))))))

(when (functionp (function earned::brightness))
  (eval
   `(defun earned:brightness ()
      "像`exwm-workspace-switch'一样，可以用按键来控制亮度。"
      (interactive)
      (when (functionp (function earned::brightness))
        (earned::brightness
         (read-from-minibuffer
          "输入亮度［0，10］：" nil
          (let ((map (make-sparse-keymap))
                (configs
                 `(,@(mapcar (lambda (key) (list key (lambda () (interactive) (earned::brightness '+))))
                             '("+" "<right>" "<up>" "b" "u" "k"))
                   ,@(mapcar (lambda (number)
                               (list (format "%s" number)
                                     `(lambda () (interactive) (earned::brightness ,number))))
                             (number-sequence 0 9))
                   ("m" (lambda () (interactive) (earned::brightness 10)))
                   ,@(mapcar (lambda (key) (list key (lambda () (interactive) (earned::brightness '-))))
                             '("-" "<left>" "<down>" "d" "j"))
                   ,@(mapcar (lambda (key) (list key (function abort-recursive-edit)))
                             '("C-g" "C-]"))
                   ,@(mapcar (lambda (key) (list key (function exit-minibuffer)))
                             '("C-j" "<return>" "<SPC>")))))
            (dolist (config configs)
              (apply (lambda (keys def) (define-key map (kbd keys) def))
                     config))
            map)
          nil nil 5))))))

;; brightness:1 ends here
;; *** yank


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*yank][yank:1]]
(defun earned:yank ()
  "粘贴"
  (interactive)
  (when (use-region-p)
    (delete-region
     (region-beginning)
     (region-end)))
  (if (eq major-mode (quote org-mode))
      (org-yank)
    (yank)))

(when (functionp (function earned:yank))
  (earned::global-set-key "C-y" (function earned:yank)))

;; yank:1 ends here
;; *** 小米加密兔


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*小米加密兔][小米加密兔:1]]
(let ((org-file "~/literate-programming/el-jiamitu.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::jiamitu ()
        (when (functionp (function earned::cookie))
          (earned::cookie))
        (when (and (functionp (function earned::load-org-project-and-require))
                   (featurep 'el-cookie-encrypt))
          (earned::load-org-project-and-require 'jiamitu ,org-file "~/.emacs.d/site-lisp/el-jiamitu/jiamitu-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (jiamitu
     jiamitu-carrot
     jiamitu-background-carrot))
   (quote jiamitu)))

(when (functionp (function earned::run-with-timer))
  (earned::run-with-timer (max (random 3600) 300)
                          (* 24 60 60)
                          (function jiamitu-background-carrot)))

;; 小米加密兔:1 ends here
;; *** cookie


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*cookie][cookie:1]]
(let ((org-file "~/literate-programming/cookie.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::cookie ()
        (when (functionp (function earned::load-org-project-and-require))
          (earned::load-org-project-and-require 'el-cookie-encrypt ,org-file "~/.emacs.d/site-lisp/el-cookie/el-cookie-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (el-cookie-encrypt-select
     el-cookie-encrypt-database
     el-cookie-encrypt-insert
     el-cookie-encrypt-reset))
   (quote cookie)))

;; cookie:1 ends here
;; *** openssl-cipher


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*openssl-cipher][openssl-cipher:1]]
(defun earned::openssl-cipher ()
  (when (executable-find "openssl")
    (when (functionp (function earned::add-remote-git-repository))
      (earned::add-remote-git-repository "https://github.com/mhayashi1120/Emacs-openssl-cipher.git"))
    (unless (featurep 'openssl-cipher)
      (require 'openssl-cipher nil t))))

;; openssl-cipher:1 ends here
;; *** remote git repository


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*remote git repository][remote git repository:1]]
(defun earned::add-remote-git-repository (url)
  (when (executable-find "git")
    (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory))
          (directory (file-name-base url)))
      (unless (file-exists-p default-directory)
        (make-directory default-directory))
      (when (and (functionp (function earned::start-process-shell-command))
                 (not (file-exists-p directory)))
        (earned::start-process-shell-command
         "background-git-clone"
         (concat "git clone --depth=1 " url)))
      (when (file-exists-p directory)
        (add-to-list 'load-path (expand-file-name directory))))))

;; remote git repository:1 ends here
;; *** smali-mode


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*smali-mode][smali-mode:1]]
(defun earned::smali-mode ()
  (when (functionp (function earned::add-remote-git-repository))
    (earned::add-remote-git-repository "https://github.com/strazzere/Emacs-Smali.git"))
  (unless (featurep 'smali-mode)
    (require 'smali-mode nil t)))

;; smali-mode:1 ends here
;; *** fund-assistant


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*fund-assistant][fund-assistant:1]]
(let ((org-file "~/literate-programming/emacs-lisp-fund-assistant.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::fund-assistant ()
        (when (functionp (function earned::funcall))
          (earned::funcall (function (earned::background earned::notification earned::ewe))))
        (when (and (functionp (function earned::load-org-project-and-require))
                   (featurep 'background)
                   (featurep 'notification)
                   (featurep 'ewe))
          (earned::load-org-project-and-require 'fund-assistant ,org-file "~/.emacs.d/site-lisp/emacs-lisp-fund-assistant/fund-assistant-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (fund-assistant
     fund-assistant-open-url
     fund-assistant-background))
   (quote fund-assistant)))

(when (functionp (function earned::run-with-timer))
  (earned::run-with-timer (max (random 3600) 300)
                          (* 24 60 60)
                          (function fund-assistant-background)))

;; fund-assistant:1 ends here
;; *** notification


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*notification][notification:1]]
(let ((org-file "~/literate-programming/emacs-lisp-notification.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::notification ()
        (when (functionp (function earned::load-org-project-and-require))
          (earned::load-org-project-and-require (quote notification) ,org-file "~/.emacs.d/site-lisp/emacs-lisp-notification/notification-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (notification
     notification-default
     notification-api-alarm-clock
     notification-api-alarm-stop
     notification-api-alarm-resume
     notification-msg-exe
     notification-termux-notification
     notification-notify-send
     notification-org-freedesktop-notifications
     notification-message
     notification-alarm-clock
     notification-restore-alarm-clock
     notification-ping))
   (quote notification)))

(when (functionp (function earned::run-with-timer))
  (earned::run-with-timer (max (random 600) 300)
                          nil (function notification-restore-alarm-clock)))

;; notification:1 ends here
;; *** ewe


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*ewe][ewe:1]]
(let ((org-file "~/literate-programming/eww-user.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::ewe ()
        (when (functionp (function earned::tesla))
          (earned::tesla))
        (when (and (functionp (function earned::load-org-project-and-require))
                   (featurep 'tesla))
          (earned::load-org-project-and-require 'ewe ,org-file "~/.emacs.d/site-lisp/eww-user/ewe-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (ewe-copy-page-title-and-url
     ewe-eww-search-at-point
     ewe-ace-link
     ewe-quit-eww
     ewe-site-search
     ewe-websites
     ewe-preview-eww-content-in-a-specific-mode
     ewe-eww-handle-link
     ewe-http-request-parse-headers
     ewe-http-request))
   (quote ewe)))

;; ewe:1 ends here
;; *** eww


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*eww][eww:1]]
(defun earned::eww-mode-hook ()
  (when (functionp (function earned::ewe))
    (earned::ewe))
  (when (functionp (function earned::gloss))
    (earned::gloss))
  (when (functionp (function earned::assess))
    (earned::assess)))

(with-eval-after-load 'eww
  (when (functionp (function earned::ewe))
    (earned::ewe)))

(when (functionp (function earned::add-hook))
  (earned::add-hook (quote eww-mode-hook)
                    (function earned::eww-mode-hook)))

;; eww:1 ends here
;; *** tesla


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*tesla][tesla:1]]
(let ((org-file "~/literate-programming/locate-the-first-useful-information-on-eww.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::tesla ()
        (when (functionp (function earned::funcall))
          (earned::funcall (function (earned::see earned::enlive))))
        (when (and (functionp (function earned::load-org-project-and-require))
                   (featurep 'see)
                   (featurep 'enlive))
          (earned::load-org-project-and-require 'tesla ,org-file "~/.emacs.d/site-lisp/locate-the-first-useful-information/tesla-user.el"))))))

;; tesla:1 ends here
;; *** see


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*see][see:1]]
(let ((org-file "~/literate-programming/emacs-text-classifier.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::see ()
        (when (functionp (function earned::load-org-project-and-require))
          (earned::load-org-project-and-require 'see ,org-file "~/.emacs.d/site-lisp/emacs-text-classifier/see-user.el"))))))

;; see:1 ends here
;; *** enlive


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*enlive][enlive:1]]
(defun earned::enlive ()
  (when (functionp (function earned::package-install-and-require))
    (earned::package-install-and-require (quote enlive))))

;; enlive:1 ends here
;; *** feeds


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*feeds][feeds:1]]
(let ((org-file "~/literate-programming/elfeed-user.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::feeds ()
        (when (functionp (function earned::funcall))
          (earned::funcall (function (earned::ace-link earned::elfeed))))
        (when (and (functionp (function earned::load-org-project-and-require))
                   (featurep 'ace-link)
                   (featurep 'elfeed))
          (earned::load-org-project-and-require 'feeds ,org-file "~/.emacs.d/site-lisp/elfeed-user/feeds-user.el"))))))

(when (functionp (function earned::add-hook))
  (earned::add-hook (quote elfeed-search-mode-hook)
                    (function earned::feeds)))

(with-eval-after-load 'elfeed
  (when (functionp (function earned::feeds))
    (earned::feeds)))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (feeds-background-update
     feeds-elfeed-search-print-entry
     feeds-elfeed-search-mode-hook))
   (quote feeds)))

;; feeds:1 ends here
;; *** ace-link


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*ace-link][ace-link:1]]
(defun earned::ace-link ()
  (when (functionp (function earned::package-install-and-require))
    (earned::package-install-and-require 'ace-link)))

;; ace-link:1 ends here
;; *** elfeed


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*elfeed][elfeed:1]]
(defun earned::elfeed ()
  (when (functionp (function earned::package-install-and-require))
    (earned::package-install-and-require 'elfeed)))

(when (functionp (function earned::autoload))
  (earned::autoload (quote elfeed)
                    (quote elfeed)))

;; elfeed:1 ends here
;; *** gloss


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*gloss][gloss:1]]
(let ((org-file "~/literate-programming/diigo-client-on-emacs-lisp.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::gloss ()
        (when (functionp (function earned::funcall))
          (earned::funcall (function (earned::elisp-format earned::yasnippet earned::ace-link earned::cookie))))
        (when (and (functionp (function earned::load-org-project-and-require))
                   (featurep 'elisp-format)
                   (featurep 'yasnippet)
                   (featurep 'ace-link)
                   (featurep 'el-cookie-encrypt))
          (earned::load-org-project-and-require 'gloss ,org-file "~/.emacs.d/site-lisp/diigo-client-on-emacs-lisp/gloss-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (gloss-save-current-page-to-bookmark
     gloss-recent-bookmarks
     gloss-search-bookmarks
     gloss-edit-bookmark
     gloss-ace-link
     gloss-remove-eww-after-render-hook
     gloss-add-eww-after-render-hook
     gloss-search-eww-current-url
     gloss-save-bookmark
     gloss-save-bookmark-with-hook))
   (quote gloss)))

;; gloss:1 ends here
;; *** elisp-format


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*elisp-format][elisp-format:1]]
(defun earned::elisp-format ()
  (when (functionp (function earned::package-install-and-require))
    (earned::package-install-and-require 'elisp-format)))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (elisp-format-buffer elisp-format-region))
   (quote elisp-format)))

;; elisp-format:1 ends here
;; *** nodejs


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*nodejs][nodejs:1]]
(let ((nodejs (expand-file-name "nodejs" (getenv "ProgramFiles"))))
  (when (file-exists-p nodejs)
    (let ((node (expand-file-name "node.exe" nodejs)))
      (when (file-exists-p node)
        (let ((path (getenv "path")))
          (unless (string-match "node" path)
            ;; setx PATH "%PATH%;C:\Program Files\nodejs"
            (shell-command
             (replace-regexp-in-string "/" "\\\\" (format "setx PATH %S" (concat "%PATH%;" nodejs))))))
        (let ((node-path (getenv "node_path"))
              (modules (expand-file-name "node_modules" nodejs)))
          ;; setx NODE_PATH "C:\Program Files\nodejs\node_modules"
          (unless node-path
            (shell-command
             (replace-regexp-in-string "/" "\\\\" (format "setx NODE_PATH %S" modules)))))
        (add-to-list (quote exec-path) nodejs)))))

(let ((org-file "~/literate-programming/emacs-lisp-nodejs.org"))
  (when (and (file-exists-p org-file)
             (or (executable-find "node")
                 (executable-find "nodejs")))
    (eval
     `(defun earned::nodejs ()
        (when (functionp (function earned::load-org-project-and-require))
          (earned::load-org-project-and-require 'nodejs ,org-file "~/.emacs.d/site-lisp/emacs-lisp-nodejs/nodejs-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote (nodejs-eval nodejs-module-exists-p nodejs-nodejs-exists-p nodejs-check-modules))
   (quote nodejs)))

;; nodejs:1 ends here
;; *** steem


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*steem][steem:1]]
(when (functionp (function earned::nodejs))
  (eval
   `(defun earned::steem ()
      (when (functionp (function earned::nodejs))
        (earned::nodejs))
      (when (boundp '*nodejs-modules*)
        (let ((list
               (alist-get
                (when (functionp (function earned::system-type))
                  (earned::system-type))
                (when (functionp (function earned::steem-shell-commands))
                  (earned::steem-shell-commands)))))
          (when (and list
                     (functionp (function earned::send-string-to-shell)))
            (let* ((ln (alist-get 'ln list))
                   (*nodejs-modules* (cl-second ln)))
              (let ((nodejs-list (alist-get 'nodejs list)))
                (when nodejs-list
                  (apply (lambda (nodejs command)
                           (unless (nodejs-nodejs-exists-p nodejs)
                             (earned::send-string-to-shell command)))
                         nodejs-list)))
              (when (executable-find "npm")
                (dolist (module (quote (steem md5)))
                  (unless (nodejs-module-exists-p (symbol-name module))
                    (earned::send-string-to-shell
                     (alist-get module list)))))
              (when (functionp (function earned::ln))
                (apply (function earned::ln)
                       ln)))))))))

(defun earned::steem-shell-commands ()
  `((termux (nodejs "node" "apt install -y nodejs")
            (steem . "npm -g install steem")
            (md5 . "npm -g install md5")
            (ln "/data/data/com.termux/files/usr/lib/node_modules/" "~/literate-programming/node_modules"))
    (kali (nodejs "node" "sudo apt install -y nodejs npm")
          (steem . "sudo npm -g install steem")
          (md5 . "sudo npm -g install md5")
          (ln "/usr/local/lib/node_modules/" "~/literate-programming/node_modules"))
    (windows-nt
     ,(cons (quote steem) (format "%S -g install steem" (executable-find "npm")))
     ,(cons (quote md5) (format "%S -g install md5" (executable-find "npm")))
     ,(list (quote ln)
            "~/npm/node_modules"
            "~/literate-programming/node_modules"))))

;; steem:1 ends here
;; *** steemit


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*steemit][steemit:1]]
(let ((org-file "~/literate-programming/steemit.org"))
  (when (and (file-exists-p org-file)
             (functionp (function earned::steem)))
    (eval
     `(defun earned::steemit ()
        (when (functionp (function earned::funcall))
          (earned::funcall (function (earned::steem earned::elisp-format earned::yasnippet earned::assess earned::cookie))))
        (when (and (functionp (function earned::load-org-project-and-require))
                   (featurep 'nodejs)
                   (featurep 'elisp-format)
                   (featurep 'yasnippet)
                   (featurep 'assess)
                   (featurep 'el-cookie-encrypt))
          (earned::load-org-project-and-require 'steemit ,org-file "~/.emacs.d/site-lisp/steemit/steemit-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (steemit-mode
     steemit-edit-save
     steemit-edit-abort
     steemit-edit-add-file-comment
     steemit-edit-add-blog
     steemit-edit-comment-options
     steemit-edit-add-blog-and-comment-options
     steemit-edit-one-click-sharing
     steemit-edit-delete-comment
     steemit-homepage
     steemit-open-online-submit-page
     steemit-format-gloss-save-bookmark-cache-and-copy
     steemit-process
     steemit-add-file-comment
     steemit-gloss-save-bookmark-hook
     steemit-add-blog
     steemit-read-posting-key))
   (quote steemit)))

(when (functionp (function earned::add-hook))
  (earned::add-hook (quote *gloss-save-bookmark-hook*)
                    (function steemit-gloss-save-bookmark-hook)))

;; steemit:1 ends here
;; *** assess


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*assess][assess:1]]
(let ((org-file "~/literate-programming/emacs-lisp-google-translate-user.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::assess ()
        (when (functionp (function earned::package-install-and-require))
          (earned::package-install-and-require (quote popup))
          (earned::package-install (quote google-translate)))
        (when (functionp (function earned::load-org-project-and-require))
          (earned::load-org-project-and-require 'assess ,org-file "~/.emacs.d/site-lisp/emacs-lisp-google-translate-user/assess-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (assess-replace-the-currently-selected-chinese-name-to-english-lisp-name
     assess-translate-to-chinese-at-point
     assess-translate-to-english-at-point
     assess-translate-current-line-to-chinese
     assess-request))
   (quote assess)))

;; assess:1 ends here
;; *** gitconfig


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*gitconfig][gitconfig:1]]
(let ((org-file "~/literate-programming/gitconfig.org"))
  (when (and (file-exists-p org-file)
             (executable-find "git")
             (not (file-exists-p
                   (if (eq system-type 'windows-nt)
                       (expand-file-name "../../.gitconfig" "~/")
                     (expand-file-name "~/.gitconfig")))))
    (eval
     `(defun earned::gitconfig ()
        (when (functionp (function earned::load-org-project))
          (earned::load-org-project ,org-file "~/.emacs.d/site-lisp/gitconfig.el"))))))

(when (functionp (function earned::run-with-timer))
  (earned::run-with-timer (max (random 300) 60)
                          nil
                          (function earned::gitconfig)))

;; gitconfig:1 ends here
;; *** sir


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*sir][sir:1]]
(let ((org-file "~/literate-programming/slime.org"))
  (when (and (file-exists-p org-file)
             (executable-find "sbcl"))
    (eval
     `(defun earned::sir ()
        (when (functionp (function earned::package-install-and-require))
          (earned::package-install-and-require (quote slime)))
        (when (and (functionp (function earned::load-org-project-and-require))
                   (featurep (quote slime)))
          (earned::load-org-project-and-require (quote sir) ,org-file "~/.emacs.d/site-lisp/slime-user/sir.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (sir
     sir-eval-last-expression
     sir-eval-src-block
     sir-trident-eval-src-block
     sir-skewer-eval-css-src-block
     sir-trident-eval-last-expression-in-src-block
     sir-trident-eval-sexp
     sir-trident-eval-last-expression
     sir-trident-eval-defun
     sir-trident-eval-region
     sir-trident-eval-buffer))
   (quote sir)))

;; sir:1 ends here
;; *** slice


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*slice][slice:1]]
(let ((org-file "~/literate-programming/emacs-lisp-process-user.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::slice ()
        (when (functionp (function earned::load-org-project-and-require))
          (earned::load-org-project-and-require (quote slice) ,org-file "~/.emacs.d/site-lisp/emacs-lisp-process-user/slice-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (slice-mark
     slice-unmark
     slice-delete
     slice-switch-to-buffer
     slice-stop-all-stuck-processes
     slice-stop-all-stuck-external-programs
     slice-timer-resume
     slice-timer-stop))
   (quote slice)))

;; slice:1 ends here
;; *** timestamp


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*timestamp][timestamp:1]]
(defun earned:insert-time-stamp-timestamp ()
  "日期时间格式输出为：<2018-04-06 Fri 09:12:41 UTC+08:00>

\"<%Y-%m-%d %a %H:%M:%S UTC%:z>\"

- 如果当前光标处于时间戳，那么更新时间戳里的时间"
  (interactive)
  (when (and (fboundp 'org-at-timestamp-p)
             (org-at-timestamp-p))
    (replace-match ""))
  (insert
   (format-time-string "<%Y-%m-%d %a %H:%M:%S UTC%:z>")))

;; timestamp:1 ends here
;; *** printscreen


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*printscreen][printscreen:1]]
(defun earned:screenshot-printscreen ()
  "xrandr --output LVDS-1 -s 3440x2880 --panning 3440x2880
sleep 1 ; xwd -root -out full-screen.xwd
convert full-screen.xwd full-screen.png
xrandr -s 1366x768
rm full-screen.xwd
eog full-screen.png"
  (interactive)
  (let* ((default-directory temporary-file-directory)
         (sh "screenshot.sh")
         (filespec (format-time-string "%Y-%m-%d-%a-%H-%M-%S.png")))
    (when (earned::executable-find (quote (xrandr sleep xwd convert rm sh)))
      (with-temp-file sh
        (cl-destructuring-bind (screen connected &rest rest)
            (split-string (shell-command-to-string "xrandr")
                          "\n")
          (insert "\nsleep 1 ; xwd -root -out full-screen.xwd")
          (insert
           (format "\nconvert full-screen.xwd %s" filespec))
          (insert "\nrm full-screen.xwd")
          (when (executable-find "eog")
            (insert
             (format "\neog %s" filespec)))
          (insert (format "\nrm %s" sh))))
      (start-process-shell-command "screenshot" nil
                                   (format "sh %s" sh)))))

(when (functionp (function earned::global-set-key))
  (earned::global-set-key "<print>" (function earned:screenshot-printscreen)))

;; printscreen:1 ends here
;; *** reverse region


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*reverse region][reverse region:1]]
(defun earned:reverse-region ()
  "2019 => 9102"
  (interactive)
  (cl-destructuring-bind (start end)
      (if (region-active-p)
          (list (region-beginning)
                (region-end))
        (list (point-min)
              (point-max)))
    (let ((reverse (reverse (buffer-substring-no-properties start end))))
      (delete-region start end)
      (insert reverse))))

;; reverse region:1 ends here
;; *** duplicate characters


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*duplicate characters][duplicate characters:1]]
(defun earned:duplicate-characters ()
  "找出重复的字符"
  (interactive)
  (let ((string
         (if (region-active-p)
             (buffer-substring-no-properties
              (region-beginning)
              (region-end))
           (read-string "Content: ")))
        (uniques '())
        (duplicates '()))
    (dolist (item (split-string string "" t))
      (if (cl-member item uniques :test 'equal)
          (push item duplicates)
        (push item uniques)))
    (let ((filtered
           (cl-remove-if
            (lambda (item)
              (cl-member
               (string-to-char item)
               (list 10 65292 12290 34)))
            (cl-remove-duplicates duplicates :test 'equal))))
      (if (< (length filtered)
             100)
          (message "%s" filtered)
        (switch-to-buffer-other-window
         (get-buffer-create "duplicate-characters"))
        (erase-buffer)
        (insert
         (format "%s" filtered))))))

;; duplicate characters:1 ends here
;; *** switch-window


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*switch-window][switch-window:1]]
(defun earned::switch-window ()
  (when (functionp (function earned::package-install-and-require))
    (earned::package-install-and-require (quote switch-window)))
  (when (functionp (function switch-window))
    (setf switch-window-shortcut-style 'alphabet)))

(defun earned:switch-window ()
  "`other-window'"
  (interactive)
  (when (functionp (function earned::switch-window))
    (earned::switch-window))
  (if (featurep (quote switch-window))
      (switch-window)
    (execute-extended-command current-prefix-arg "other-window")))

(when (functionp (function earned::global-set-key))
  (earned::global-set-key "C-x o" (function earned:switch-window)))

;; switch-window:1 ends here
;; *** help


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*help][help:1]]
(defun earned::symbol-names (package)
  (all-completions (format "%s" package) obarray))

(defun earned::functions (package)
  (when (functionp (function earned::symbol-names))
    (cl-remove-if-not (function functionp)
                      (mapcar (function intern)
                              (earned::symbol-names package)))))

(defun earned::commands (package)
  (when (functionp (function earned::symbol-names))
    (cl-remove-if-not (function commandp)
                      (mapcar (function intern)
                              (earned::symbol-names package)))))

(defun earned::help (feature)
  (when (and feature
             (or (stringp feature)
                 (symbolp feature)))
    (let ((feature (intern (format "%s" feature))))
      (when (and (functionp (function earned::commands))
                 (featurep feature))
        (let ((buffer (format "*%s Help*" (upcase (symbol-name feature))))
              (form
               (mapcar
                (lambda (command)
                  (let ((key-description
                         (key-description (car (where-is-internal command overriding-local-map))))
                        (documentation
                         (ignore-errors (car (split-string (documentation command) "\n" t)))))
                    (remove
                     (quote nil)
                     (list command
                           (unless (equal key-description "") key-description)
                           documentation))))
                (earned::commands feature))))
          (when form
            (when (get-buffer buffer)
              (kill-buffer buffer))
            (with-current-buffer (get-buffer-create buffer)
              (delay-mode-hooks (emacs-lisp-mode))
              (font-lock-mode)
              (pp (cl-sort form (function string<)
                           :key (lambda (list) (downcase (symbol-name (car list)))))
                  (current-buffer))
              (goto-char (point-min)))
            (switch-to-buffer buffer)))))))

(defun earned:help ()
  "show help in `features'"
  (interactive)
  (when (functionp (function earned::help))
    (earned::help
     (if (not current-prefix-arg)
         'earned
       (funcall
        (if (functionp (function ivy-completing-read))
            (function ivy-completing-read)
          (function completing-read))
        "Specify a feature: " features nil t)))))

;; help:1 ends here
;; *** emacs -Q

;; 在＂emacs -Q --load ~/.emacs.d/init.el＂的情况下，Emacs的启动速度可以得到进一步的提升，不过在＂earned::after-init-hook＂里的功能就不能正常使用。


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*emacs -Q][emacs -Q:1]]
(defun earned::when-emacs-Q-load-user-init-file ()
  "emacs -Q --load ~/.emacs.d/init.el"
  (let ((string (mapconcat (function identity) command-line-args " ")))
    (when (and (string-match "--load \\([^ ]*\\)" string)
               (functionp (function earned::after-init-hook))
               (not (featurep (quote earned))))
      (let* ((file (match-string 1 string))
             (unquoted-file (car (read-from-string file)))
             (true-file
              (if (stringp unquoted-file)
                  unquoted-file
                file)))
        (when (file-exists-p true-file)
          (setf user-init-file true-file)))
      (earned::after-init-hook))))

;; emacs -Q:1 ends here


;; 在不同的桌面和操作系统下，要想让＂emacs -Q＂功能生效，也需要进行一些设置：

;; - EXWM下修改＂~/.xinitrc＂
;;   #+BEGIN_EXAMPLE
;;   exec dbus-launch --exit-with-session emacs -Q --load ~/.emacs.d/init.el
;;   #+END_EXAMPLE
;; - Windows10下修改桌面的快捷方式＂C:\Users\simi\AppData\Roaming\Microsoft\Windows\Start Menu\Programs\Gnu Emacs＂
;;   #+BEGIN_EXAMPLE
;;   "c:\Users\simi\Downloads\emacs-26.1-x86_64\bin\runemacs.exe" -Q --load "c:\Users\simi\AppData\Roaming\.emacs.d\init.el"
;;   #+END_EXAMPLE


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*emacs -Q][emacs -Q:1]]
(defun earned::emacs-Q-command (&optional escapep command eval)
  "`escapep'用于在＂Windows＂下启动程序的路径需要将＂/＂转换为＂\\＂时，也可以不转换，保留原样的数据。
`command'如果强制设为＂emacs＂，以便与＂-nw＂一起使用，来在非GUI界面里使用，默认是生成能启动GUI界面的字符串。
`eval'是在＂--load＂之前使用的，与＂--eval＂进行拼接的字符串。"
  (let ((init (expand-file-name "~/.emacs.d/init.el")))
    (when (file-exists-p init)
      (let ((args (mapconcat (function identity) (remove nil (list "-Q" "-D" (when eval (concat "--eval" " " (format "%S" eval))) "--load" (format "%S" init))) " "))
            (command (when command (executable-find command))))
        (cond
         ((eq system-type (quote windows-nt))
          (let ((runemacs (or command (executable-find "runemacs"))))
            (when runemacs
              (let ((command (format "%S %s" runemacs args)))
                (if escapep
                    (replace-regexp-in-string "/" (regexp-quote "\\") command)
                  command)))))
         ((and (eq system-type (intern "gnu/linux"))
               (executable-find "exec"))
          (let ((emacs (or command (executable-find "emacs"))))
            (when emacs
              (format "exec dbus-launch --exit-with-session %s"
                      (format "%S %s" emacs args)))))
         (t
          (let ((emacs (or command (executable-find "emacs"))))
            (when emacs
              (format "%S %s" emacs args)))))))))

(defun earned:copy-emacs-Q-command ()
  "复制Emacs -Q命令，便于手动修改启动Emacs的命令行参数`command-line-args'，被`command-line'函数使用。
{-Q,--quick}: (setf init-file-user nil site-run-file nil inhibit-x-resources t)
{-d,--basic-display}: (setf no-blinking-cursor t emacs-basic-display t)"
  (interactive)
  (let ((string (earned::emacs-Q-command t)))
    (when string
      (kill-new string)
      (message string))))

;; emacs -Q:1 ends here
;; *** git pull branch


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*git pull branch][git pull branch:1]]
(let ((directory "~/literate-programming/"))
  (when (and (executable-find "git")
             (file-exists-p directory))
    (eval
     `(defun earned:git-pull-branch ()
        (interactive)
        (when (functionp (function earned::start-process-shell-command))
          (let ((default-directory ,directory))
            (earned::start-process-shell-command nil "git pull")))))))

(when (functionp (function earned::run-with-timer))
  (earned::run-with-timer (max (random 600) 300)
                          nil
                          (function earned:git-pull-branch)))

;; git pull branch:1 ends here
;; *** git push


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*git push][git push:1]]
(let ((git (executable-find "git")))
  (when git
    (eval
     `(defun earned::git-push (directory)
        (let ((default-directory directory))
          (when (and (file-exists-p default-directory)
                     ,git)
            (dotimes (i 3)
              (when (string-match
                     "## master...origin/master \\[ahead [0-9]*\\]\n"
                     (shell-command-to-string
                      "git status --branch master --short"))
                (shell-command-to-string "git push")))))))))

;; git push:1 ends here
;; *** shutdown

;; #+BEGIN_EXAMPLE
;; Usage: shutdown.exe [/i | /l | /s | /r | /g | /a | /p | /h | /e] [/f]
;;     [/m \\computer][/t xxx][/d [p|u:]xx:yy [/c "comment"]]

;;     No args    Display help. This is the same as typing /?.
;;     /?         Display help. This is the same as not typing any options.
;;     /i         Display the graphical user interface (GUI).
;;                This must be the first option.
;;     /l         Log off. This cannot be used with /m or /d options.
;;     /s         Shutdown the computer.
;;     /r         Shutdown and restart the computer.
;;     /g         Shutdown and restart the computer. After the system is
;;                rebooted, restart any registered applications.
;;     /a         Abort a system shutdown.
;;                This can only be used during the time-out period.
;;     /p         Turn off the local computer with no time-out or warning.
;;                Can be used with /d and /f options.
;;     /h         Hibernate the local computer.
;;                Can be used with the /f option.
;;     /e         Document the reason for an unexpected shutdown of a computer.
;;     /m \\computer Specify the target computer.
;;     /t xxx     Set the time-out period before shutdown to xxx seconds.
;;                The valid range is 0-315360000 (10 years), with a default of 30.
;;                If the timeout period is greater than 0, the /f parameter is
;;                implied.
;;     /c "comment" Comment on the reason for the restart or shutdown.
;;                Maximum of 512 characters allowed.
;;     /f         Force running applications to close without forewarning users.
;;                The /f parameter is implied when a value greater than 0 is
;;                specified for the /t parameter.
;;     /d [p|u:]xx:yy  Provide the reason for the restart or shutdown.
;;                p indicates that the restart or shutdown is planned.
;;                u indicates that the reason is user defined.
;;                If neither p nor u is specified the restart or shutdown is
;;                unplanned.
;;                xx is the major reason number (positive integer less than 256).
;;                yy is the minor reason number (positive integer less than 65536).

;; Reasons on this computer:
;; (E = Expected U = Unexpected P = planned, C = customer defined)
;; Type    Major   Minor   Title

;;  U      0       0       Other (Unplanned)
;; E       0       0       Other (Unplanned)
;; E P     0       0       Other (Planned)
;;  U      0       5       Other Failure: System Unresponsive
;; E       1       1       Hardware: Maintenance (Unplanned)
;; E P     1       1       Hardware: Maintenance (Planned)
;; E       1       2       Hardware: Installation (Unplanned)
;; E P     1       2       Hardware: Installation (Planned)
;; E       2       2       Operating System: Recovery (Planned)
;; E P     2       2       Operating System: Recovery (Planned)
;;   P     2       3       Operating System: Upgrade (Planned)
;; E       2       4       Operating System: Reconfiguration (Unplanned)
;; E P     2       4       Operating System: Reconfiguration (Planned)
;;   P     2       16      Operating System: Service pack (Planned)
;;         2       17      Operating System: Hot fix (Unplanned)
;;   P     2       17      Operating System: Hot fix (Planned)
;;         2       18      Operating System: Security fix (Unplanned)
;;   P     2       18      Operating System: Security fix (Planned)
;; E       4       1       Application: Maintenance (Unplanned)
;; E P     4       1       Application: Maintenance (Planned)
;; E P     4       2       Application: Installation (Planned)
;; E       4       5       Application: Unresponsive
;; E       4       6       Application: Unstable
;;  U      5       15      System Failure: Stop error
;;  U      5       19      Security issue
;; E       5       19      Security issue
;; E P     5       19      Security issue
;; E       5       20      Loss of network connectivity (Unplanned)
;;  U      6       11      Power Failure: Cord Unplugged
;;  U      6       12      Power Failure: Environment
;;   P     7       0       Legacy API shutdown
;; #+END_EXAMPLE


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*shutdown][shutdown:1]]
(defun earned::shutdown (type)
  (unless (fboundp (quote cl-case))
    (require (quote cl-macs)))
  (when (fboundp (quote cl-case))
    (cl-case type
      (reboot
       (cond ((executable-find "/sbin/shutdown")
              (call-process "sh" nil nil nil "-c" "/sbin/shutdown --reboot 0"))
             ((executable-find "shutdown.exe")
              (shell-command
               (format
                "%s -r -f -t 0"
                (executable-find "shutdown.exe"))))))
      (logoff
       (cond ((executable-find "shutdown.exe")
              (shell-command
               (format
                "%s -l -f"
                (executable-find "shutdown.exe"))))))
      (poweroff
       (cond ((executable-find "/sbin/shutdown")
              (call-process "sh" nil nil nil "-c" "/sbin/shutdown --poweroff 0"))
             ((executable-find "shutdown.exe")
              (shell-command
               (format
                "%s -s -f -t 0"
                (executable-find "shutdown.exe")))))))))

;; shutdown:1 ends here
;; *** ~/.bash_profile


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*~/.bash_profile][~/.bash_profile:1]]
(let ((org-file "~/literate-programming/emacs-lisp-bash-profile.org"))
  (when (and (file-exists-p org-file)
             (file-exists-p "/etc/skel/.profile")
             (eq system-type (quote gnu/linux))
             (executable-find "bash"))
    (eval
     `(defun earned::bash-profile ()
        (when (functionp (function earned::load-org-project-and-require))
          (earned::load-org-project-and-require (quote bash-profile) ,org-file "~/.emacs.d/site-lisp/emacs-lisp-bash-profile/bash-profile-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload (quote bash-profile) (quote bash-profile)))

;; ~/.bash_profile:1 ends here
;; *** replace pulseaudio with alsa


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*replace pulseaudio with alsa][replace pulseaudio with alsa:1]]
(defun earned:replace-pulseaudio-with-alsa ()
  (interactive)
  (when (and (not (executable-find "alsamixer"))
             (executable-find "pulseaudio")
             (executable-find "sudo")
             (executable-find "apt")
             (functionp (function earned::send-string-to-shell)))
    (earned::send-string-to-shell
     "sudo apt purge -y pulseaudio && sudo apt install -y alsa-utils && sudo alsactl init")))

;; replace pulseaudio with alsa:1 ends here
;; *** browse-url


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*browse-url][browse-url:1]]
(defun earned:browse-url-incognito ()
  "在隐私模式里打开链接"
  (interactive)
  (when (functionp (function earned::browse-url-incognito))
    (let ((copied (ignore-errors (current-kill 0 t))))
      (earned::browse-url-incognito
       (cond ((and (functionp (function eww-suggested-uris))
                   (eww-suggested-uris))
              (car (eww-suggested-uris)))
             ((url-type (url-generic-parse-url copied))
              copied)
             (t
              (read-string "URL: ")))))))

(unless (eq system-type (quote windows-nt))
  (eval
   `(defun earned::browse-url-incognito (url)
      (let* ((browse-url-chromium-arguments (quote ("--incognito")))
             (browse-url-generic-args browse-url-chromium-arguments)
             (browse-url-chrome-arguments browse-url-chromium-arguments))
        (browse-url url)))))

;; browse-url:1 ends here
;; *** shell-mode


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*shell-mode][shell-mode:1]]
(when (functionp (function earned::add-hook))
  (earned::add-hook (quote shell-mode-hook) (function font-lock-mode)))

;; shell-mode:1 ends here
;; *** dired-mode


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*dired-mode][dired-mode:1]]
(when (functionp (function earned::add-hook))
  (earned::add-hook (quote dired-mode-hook) (function font-lock-mode)))

;; dired-mode:1 ends here
;; **** 运行桌面里的外部程序


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*运行桌面里的外部程序][运行桌面里的外部程序:1]]
(when (eq system-type (quote windows-nt))
  (eval
   `(defun earned::start-lnk (lnk &optional another-shell-command)
      "windows - Run a shortcut with a batch file - Stack Overflow: https://stackoverflow.com/questions/38318719/run-a-shortcut-with-a-batch-file

start /?
Starts a separate window to run a specified program or command.

START [\"title\"] [/D path] [/I] [/MIN] [/MAX] [/SEPARATE | /SHARED]
      [/LOW | /NORMAL | /HIGH | /REALTIME | /ABOVENORMAL | /BELOWNORMAL]
      [/AFFINITY <hex affinity>] [/WAIT] [/B] [command/program]
      [parameters]

    \"title\"     Title to display in window title bar.
    path        Starting directory.
    B           Start application without creating a new window. The
                application has ^C handling ignored. Unless the application
                enables ^C processing, ^Break is the only way to interrupt
                the application.
    I           The new environment will be the original environment passed
                to the cmd.exe and not the current environment.
    MIN         Start window minimized.
    MAX         Start window maximized.
    SEPARATE    Start 16-bit Windows program in separate memory space.
    SHARED      Start 16-bit Windows program in shared memory space.
    LOW         Start application in the IDLE priority class.
    NORMAL      Start application in the NORMAL priority class.
    HIGH        Start application in the HIGH priority class.
    REALTIME    Start application in the REALTIME priority class.
    ABOVENORMAL Start application in the ABOVENORMAL priority class.
    BELOWNORMAL Start application in the BELOWNORMAL priority class.
    AFFINITY    The new application will have the specified processor
                affinity mask, expressed as a hexadecimal number.
    WAIT        Start application and wait for it to terminate.
    command/program
                If it is an internal cmd command or a batch file then
                the command processor is run with the /K switch to cmd.exe.
                This means that the window will remain after the command
                has been run.

                If it is not an internal cmd command or batch file then
                it is a program and will run as either a windowed application
                or a console application.

    parameters  These are the parameters passed to the command/program.

NOTE: The SEPARATE and SHARED options are not supported on 64-bit platforms.

If Command Extensions are enabled, external command invocation
through the command line or the START command changes as follows:

non-executable files may be invoked through their file association just
    by typing the name of the file as a command.  (e.g.  WORD.DOC would
    launch the application associated with the .DOC file extension).
    See the ASSOC and FTYPE commands for how to create these
    associations from within a command script.

When executing an application that is a 32-bit GUI application, CMD.EXE
    does not wait for the application to terminate before returning to
    the command prompt.  This new behavior does NOT occur if executing
    within a command script.

When executing a command line whose first token is the string \"CMD \"
    without an extension or path qualifier, then \"CMD\" is replaced with
    the value of the COMSPEC variable.  This prevents picking up CMD.EXE
    from the current directory.

When executing a command line whose first token does NOT contain an
    extension, then CMD.EXE uses the value of the PATHEXT
    environment variable to determine which extensions to look for
    and in what order.  The default value for the PATHEXT variable
    is:

        .COM;.EXE;.BAT;.CMD

    Notice the syntax is the same as the PATH variable, with
    semicolons separating the different elements.

When searching for an executable, if there is no match on any extension,
then looks to see if the name matches a directory name.  If it does, the
START command launches the Explorer on that path.  If done from the
command line, it is the equivalent to doing a CD /D to that path.
"
      (when (stringp lnk)
        (let ((coding-system-for-write locale-coding-system)
              ;; A command is running in the default buffer.  Use a new buffer? (y or n)
              (async-shell-command-buffer (or another-shell-command async-shell-command-buffer)))
          (save-window-excursion
            (async-shell-command (format "start %S %S" "" lnk)))))))
  (eval
   `(defun earned::windows-external-programs ()
      (cl-concatenate
       (quote list)
       ;; exe
       (quote
        (("Performance Monitor" "perfmon.msc")
         ("Window Task Manager" "taskmgr.exe")
         ("Windows Mobility Center" "mblctr.exe")
         ("Resource Monitor" "resmon.exe")))
       ;; lnk/appref-ms/bat
       (mapcar (lambda (lnk)
                 (list (format "%s (%s)" (file-name-base lnk)
                               (propertize (file-name-base (directory-file-name (file-name-directory lnk))) (quote face) (quote font-lock-keyword-face)))
                       lnk))
               (apply
                (function cl-concatenate) (quote list)
                (mapcar (lambda (directory)
                          (directory-files directory t
                                           "\\.\\(lnk\\|appref-ms\\|bat\\)"
                                           t))
                        (list "~/Microsoft/Windows/Start Menu/Programs/"
                              (expand-file-name "Desktop" (getenv "userprofile")))))))))
  (eval
   `(defun earned:start-program ()
      "启动Windows开始菜单、桌面程序，启动可执行程序。"
      (interactive)
      (when (and (functionp (function earned::start-lnk))
                 (functionp (function earned::windows-external-programs)))
        (let ((options (earned::windows-external-programs)))
          (earned::start-lnk
           (cl-second
            (cl-assoc
             (let ((prompt "Specify a program: "))
               (if (functionp (function earned::completing-read))
                   (earned::completing-read 'earned::*start-program* prompt (mapcar (function car) options))
                 (completing-read prompt options)))
             options :test (function equal))))))))
  (eval
   `(defun earned:start-program-maximize ()
      "以最大化的方式异步启动外部程序"
      (interactive)
      (async-shell-command
       (format "start %S /max %S" "" (read-shell-command "Shell command: "))))))

(when (functionp (function earned::start-lnk))
  (eval
   `(defun earned:start-lnk-at-point ()
      "手动打开当前位置的文件或程序"
      (interactive)
      (earned::start-lnk (thing-at-point (quote filename))))))

;; 运行桌面里的外部程序:1 ends here
;; **** 控制面板里的程序


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*控制面板里的程序][控制面板里的程序:1]]
(eval
 `(defun earned:control-panel ()
    "Accessing the Control Panel via the Commandline - TechNet Articles - United States (English) - TechNet Wiki: https://social.technet.microsoft.com/wiki/contents/articles/4486.accessing-the-control-panel-via-the-commandline.aspx"
    (interactive)
    (let* ((collection
            (quote
             (("Personalization Font size and DPI"            . "DpiScaling")
              ("Personalization Screen resolution"            . "control desk.cpl,Settings,@Settings")
              ("Personalization Display settings"             . "control desk.cpl,Settings,@Settings")
              ("Personalization Themes"                       . "control desk.cpl,Themes,@Themes")
              ("Personalization Screensaver"                  . "control desk.cpl,screensaver,@screensaver")
              ("Personalization Multi-monitor"                . "control desk.cpl,Monitor,@Monitor")
              ("Personalization Color Scheme"                 . "control /name Microsoft.Personalization /page pageColorization")
              ("Personalization Desktop background"           . "control /name Microsoft.Personalization /page pageWallpaper")
              ("System Performance"                           . "SystemPropertiesPerformance")
              ("System Remote access"                         . "SystemPropertiesRemote")
              ("System Computer name"                         . "SystemPropertiesComputerName")
              ("System protection"                            . "SystemPropertiesProtection")
              ("System Advanced system properties"            . "SystemPropertiesAdvanced")
              ("Programs and Features Add or remove programs" . "control /name Microsoft.ProgramsAndFeatures")
              ("Programs and Features Windows features"       . "OptionalFeatures")
              ("Regional and Language Options Keyboard"       . "control /name Microsoft.RegionalAndLanguageOptions /page /p:\"keyboard\"")
              ("Regional and Language Options Location"       . "control /name Microsoft.RegionalAndLanguageOptions /page /p:\"location\"")
              ("Regional and Language Options Administrative" . "control /name Microsoft.RegionalAndLanguageOptions /page /p:\"administrative\"")
              ("Folder Options Folder searching"              . "rundll32 shell32.dll,Options_RunDLL 2")
              ("Folder Options File associations"             . "control /name Microsoft.DefaultPrograms /page pageFileAssoc")
              ("Folder Options View"                          . "rundll32 shell32.dll,Options_RunDLL 7")
              ("Folder Options General"                       . "rundll32 shell32.dll,Options_RunDLL 0")
              ("Power Options Edit current plan settings"     . "control /name Microsoft.PowerOptions /page pagePlanSettings")
              ("Power Options System settings"                . "control /name Microsoft.PowerOptions /page pageGlobalSettings")
              ("Power Options Create a power plan"            . "control /name Microsoft.PowerOptions /page pageCreateNewPlan")
              ("Windows Firewall"                             . "control /name Microsoft.WindowsFirewall")
              ("Power Options Advanced Settings"              . "control powercfg.cpl,,3")
              ("Display Properties window"                    . "control desktop")
              ("Display Properties window Appearance"         . "control color")
              ("Date and Time Properties"                     . "control date/time")
              ("Regional and Language Options"                . "control international")
              ("Mouse Properties"                             . "control mouse")
              ("Keyboard Properties"                          . "control keyboard")
              ("Printers and Faxes"                           . "control printers")
              ("Fonts"                                        . "control fonts")
              ("Folder Options"                               . "control folders")
              ("Novell NetWare"                               . "control netware")
              ("Phone and Modem Options"                      . "control telephony")
              ("Administrative Tools"                         . "control admintools")
              ("Scheduled Tasks"                              . "control schedtasks")
              ("Network Connections"                          . "control netconnections")
              ("Infrared Monitor"                             . "control infrared")
              ("User Accounts"                                . "control userpasswords")
              ("Accessibility Options"                        . "control access.cpl")
              ("Add New Hardware"                             . "control sysdm.cpl add new hardware")
              ("Add/Remove Programs"                          . "control appwiz.cpl")
              ("Date/Time Properties"                         . "control timedate.cpl")
              ("Display Properties"                           . "control desk.cpl")
              ("FindFast"                                     . "control findfast.cpl")
              ("Fonts Folder"                                 . "control fonts")
              ("Internet Properties"                          . "control inetcpl.cpl")
              ("Joystick Properties"                          . "control joy.cpl")
              ("Keyboard Properties"                          . "control main.cpl keyboard")
              ("Microsoft Exchange (or Windows Messaging)"    . "control mlcfg32.cpl")
              ("Microsoft Mail Post Office"                   . "control wgpocpl.cpl")
              ("Modem Properties"                             . "control modem.cpl")
              ("Mouse Properties"                             . "control main.cpl")
              ("Multimedia Properties"                        . "control mmsys.cpl")
              ("Network Properties"                           . "control netcpl.cpl")
              ("Password Properties"                          . "control password.cpl")
              ("PC Card"                                      . "control main.cpl pc card (PCMCIA)")
              ("Power Management (Windows 95)"                . "control main.cpl power")
              ("Power Management (Windows 98)"                . "control powercfg.cpl")
              ("Printers Folder"                              . "control printers")
              ("Regional Settings"                            . "control intl.cpl")
              ("Scanners and Cameras"                         . "control sticpl.cpl")
              ("Sound Properties"                             . "control mmsys.cpl sounds")
              ("System Properties"                            . "control sysdm.cpl"))))
           (select
            (earned::completing-read
             (quote earned::*control-panel*) "Control: "
             (mapcar (function car) collection)))
           (command (alist-get select collection nil nil (function equal))))
      (async-shell-command command))))

;; 控制面板里的程序:1 ends here
;; **** 开机自启动程序


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*开机自启动程序][开机自启动程序:1]]
(when (eq system-type (quote windows-nt))
  (eval
   `(defun earned::windows-startup ()
      "运行开机启动项里的程序"
      (let ((startup (expand-file-name "Microsoft/Windows/Start Menu/Programs/Startup" "~/")))
        (when (and (file-exists-p startup)
                   (functionp (function earned::start-lnk)))
          (dolist (lnk (directory-files startup t (rx (or ".lnk" ".ahk" ".exe"))))
            (earned::start-lnk lnk (quote rename-buffer))))))))

(defun earned::windows-logon-time ()
  (when (executable-find "net")
    (let ((shell
           (shell-command-to-string
            (format "net user %S" user-login-name))))
      ;; Last logon                   ?2020-?09-?26 15:10:22
      (when (string-match "Last logon\\(.*\\)" shell)
        (let ((match (match-string 1 shell)))
          (cl-destructuring-bind (year month day hour minute second)
              (mapcar (function string-to-number)
                      (split-string match "[ \?:\\-]" t))
            (encode-time second minute hour day month year)))))))

(defun earned::emacs-start-at-login-p ()
  "Emacs是否是在登录桌面系统后马上启动的"
  (when (functionp (function earned::windows-logon-time))
    (let ((threshold 10))
      (< (time-to-seconds
          (time-subtract
           after-init-time
           (earned::windows-logon-time)))
         threshold))))

(when (functionp (function earned::windows-startup))
  (eval
   `(defun earned:windows-startup ()
      "手动运行开机启动项里的程序"
      (interactive)
      (earned::windows-startup))))

(defun earned::auto-windows-startup ()
  (let ((symbol (quote earned::*auto-windows-startup*)))
    (if (boundp symbol)
        (unless (symbol-value symbol)
          (when (and (functionp (function earned::emacs-start-at-login-p))
                     (earned::emacs-start-at-login-p)
                     (functionp (function earned::windows-startup)))
            (earned::windows-startup))
          (setf (symbol-value symbol) t))
      (setf (symbol-value symbol) nil)
      (earned::auto-windows-startup))))

;; 开机自启动程序:1 ends here
;; **** 修改窗口和桌面的主题


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*修改窗口和桌面的主题][修改窗口和桌面的主题:1]]
(defun earned:window-personalization-themes ()
  "修改窗口和桌面的主题"
  (interactive)
  (when (functionp (function earned::system-type))
    (let ((command
           (cl-case (earned::system-type)
             (windows-nt "control desk.cpl,Themes,@Themes")
             (termux (when (and (executable-find "pm")
                                (string-match
                                 "package:/system/app/NovaLauncher/NovaLauncher.apk"
                                 (shell-command-to-string "pm path com.teslacoilsw.launcher")))
                       "am start -n com.teslacoilsw.launcher/com.android.launcher3.LauncherWallpaperPickerActivity")))))
      (when command
        (save-window-excursion
          (async-shell-command command))))))

;; 修改窗口和桌面的主题:1 ends here
;; *** completing-read

;; 按照所选的时间顺序排序选项。


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*completing-read][completing-read:1]]
(defun earned::completing-read (symbol prompt collection)
  "`completing-read' & `ivy-read'"
  (when (functionp (function earned::ivy))
    (earned::ivy))
  (when (or (not (boundp symbol))
            (not (= (length (symbol-value symbol))
                    (length collection))))
    (setf (symbol-value symbol)
          collection))
  (let ((collection (symbol-value symbol)))
    (if (functionp (function ivy-read))
        (let ((select (ivy-read prompt collection :predicate nil :require-match t))
              (test (function equal)))
          (unless (funcall test (cl-first collection) select)
            (setf (symbol-value symbol)
                  (cl-remove select collection :test test))
            (push select (symbol-value symbol)))
          select)
      (completing-read prompt collection nil t))))

;; completing-read:1 ends here
;; *** yahoo email


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*yahoo email][yahoo email:1]]
(let ((org-file "~/literate-programming/emacs-lisp-yahoo-mail.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::coal ()
        (when (functionp (function earned::funcall))
          (earned::funcall (function (earned::enlive earned::background earned::ewe earned::notification earned::cookie))))
        (when (and (functionp (function earned::load-org-project-and-require))
                   (featurep (quote enlive))
                   (featurep (quote background))
                   (featurep (quote ewe))
                   (featurep (quote notification)))
          (earned::load-org-project-and-require (quote coal) ,org-file "~/.emacs.d/site-lisp/emacs-lisp-yahoo-mail/coal-user.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (coal-open-online-yahoo-mail
     coal-inbox-background-check
     coal-inbox-open
     coal-inbox-check))
   (quote coal)))

;; yahoo email:1 ends here
;; *** proced-mode


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*proced-mode][proced-mode:1]]
(defun earned::proced-mode-hook ()
  (proced-toggle-auto-update (quote toggle)))

(when (functionp (function earned::add-hook))
  (earned::add-hook (quote proced-mode-hook) (function earned::proced-mode-hook)))

;; proced-mode:1 ends here
;; *** toc-org


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*toc-org][toc-org:1]]
(defun earned:org-insert-toc ()
  "GitHub - snosov1/toc-org: toc-org is an Emacs utility to have an up-to-date table of contents in the org files without exporting (useful primarily for readme files on GitHub): https://github.com/snosov1/toc-org"
  (interactive)
  (when (functionp (function earned::package-install-and-require))
    (earned::package-install-and-require (quote toc-org)))
  (when (and (eq major-mode (quote org-mode))
             (functionp (function toc-org-insert-toc)))
    (let* ((text "* Table of Contents")
           (toc (save-excursion
                  (goto-char (point-min))
                  (re-search-forward (concat "^\\" text) nil t))))
      (if (not toc)
          (insert (concat text " :TOC_3_gh:"))
        (goto-char toc)))
    (goto-char (line-beginning-position))
    (execute-extended-command current-prefix-arg "org-set-tags-command")
    (toc-org-insert-toc)))

;; toc-org:1 ends here
;; *** el2org


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*el2org][el2org:1]]
(defun earned::el2org ()
  (when (functionp (function earned::package-install-and-require))
    (earned::package-install-and-require (quote el2org))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (el2org-generate-org
     el2org-generate-html
     el2org-generate-readme))
   (quote el2org)))

;; el2org:1 ends here
;; *** pinyin-search


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*pinyin-search][pinyin-search:1]]
(defun earned::pinyin-search ()
  (when (functionp (function earned::package-install-and-require))
    (earned::package-install-and-require (quote pinyin-search))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (pinyin-search
     pinyin-search-backward))
   (quote pinyin-search)))

;; pinyin-search:1 ends here
;; *** remove-hook


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*remove-hook][remove-hook:1]]
(defun earned:remove-hook ()
  "interactive `remove-hook'"
  (interactive)
  (let* ((variable
          (if (and help-xref-stack-item
                   (eq (nth 0 help-xref-stack-item)
                       (quote describe-variable)))
              (nth 1 help-xref-stack-item)
            (variable-at-point)))
         (hook
          (if (and variable (symbolp variable)
                   (string-match "-hook$" (symbol-name variable)))
              variable
            (unless (functionp (function help--symbol-completion-table))
              (require (function help-fns)))
            (when (functionp (function help--symbol-completion-table))
              (intern
               (completing-read
                "Specify a hook: "
                (function help--symbol-completion-table)
                (lambda (symbol)
                  (and (boundp symbol)
                       (not (keywordp symbol))
                       (string-match "-hook$" (symbol-name symbol))))
                t)))))
         (collections
          (when hook
            (mapcar (lambda (function)
                      (cons (prin1-to-string function)
                            function))
                    (symbol-value hook))))
         (function
          (when collections
            (alist-get (completing-read "Specify a function: " collections)
                       collections nil nil (function equal))))
         (form
          `(remove-hook (quote ,hook)
                        (function ,function))))
    (when (and hook function
               (y-or-n-p-with-timeout
                (concat (prin1-to-string form)
                        "?")
                10 nil))
      (eval form))))

;; remove-hook:1 ends here
;; *** advice-remove


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*advice-remove][advice-remove:1]]
(defun earned:advice-remove ()
  "interactive `advice-remove'"
  (interactive)
  (cl-labels
      ((function-advices
        (symbol)
        "添加个删除 Advice 函数的按钮 - Emacs-general - Emacs China: https://emacs-china.org/t/advice/7566"
        (unless (functionp (function advice--symbol-function))
          (require (quote nadvice) nil t))
        (when (functionp (function advice--symbol-function))
          (let ((function-def (advice--symbol-function symbol))
                (ad-functions '()))
            (while (advice--p function-def)
              (setq ad-functions (append `(,(advice--car function-def)) ad-functions))
              (setq function-def (advice--cdr function-def)))
            ad-functions))))
    (let* ((variable
            (if help-xref-stack-item
                (nth 1 help-xref-stack-item)
              (function-called-at-point)))
           (symbol
            (if (and variable (functionp variable))
                variable
              (unless (functionp (function help--symbol-completion-table))
                (require (function help-fns)))
              (when (functionp (function help--symbol-completion-table))
                (intern
                 (completing-read
                  "Specify a symbol: "
                  (function help--symbol-completion-table)
                  (lambda (symbol)
                    (or (functionp symbol)
                        (get symbol (quote function-documentation))))
                  t)))))
           (collections
            (when symbol
              (mapcar
               (lambda (function)
                 (cons (prin1-to-string function)
                       function))
               (function-advices symbol))))
           (function
            (when collections
              (alist-get (completing-read "Specify a function: " collections)
                         collections nil nil (function equal))))
           (form `(advice-remove (quote ,symbol)
                                 (function ,function))))
      (when (and symbol function
                 (y-or-n-p-with-timeout
                  (concat (prin1-to-string form)
                          "?")
                  10 nil))
        (eval form)))))

;; advice-remove:1 ends here
;; *** company


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*company][company:1]]
(eval-after-load (quote company)
  `(dolist (list (quote ; Company mode popup closes after C-n - Emacs Stack Exchange: https://emacs.stackexchange.com/questions/17965/company-mode-popup-closes-after-c-n
                  (("C-n" company-select-next-or-abort)
                   ("C-p" company-select-previous-or-abort))))
     (apply (lambda (keys def)
              (define-key company-active-map (kbd keys) def))
            list)))

;; company:1 ends here
;; *** nircmd


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*nircmd][nircmd:1]]
(let ((name "nircmd.exe"))
  "NirCmd - Freeware command-line tool for Windows: http://www.nirsoft.net/utils/nircmd2.html"
  (unless (executable-find name)
    (let ((default-directory
            (expand-file-name
             "nircmd"
             (expand-file-name "Downloads" (getenv "userprofile")))))
      (when (file-exists-p (expand-file-name name default-directory))
        (add-to-list (quote exec-path) default-directory)))))

;; nircmd:1 ends here
;; *** buffer-local-variables


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*buffer-local-variables][buffer-local-variables:1]]
(defun earned:buffer-local-variables ()
  "查看一个页面里生效的所有变量，如果`current-prefix-arg'为空，则查看当前页面。"
  (interactive)
  (let ((buffer (if current-prefix-arg
                    (get-buffer (read-buffer-to-switch "Specify a buffer: "))
                  (current-buffer))))
    (when buffer
      (with-current-buffer buffer
        (pp-eval-expression '(buffer-local-variables))))))

;; buffer-local-variables:1 ends here
;; *** 猎魂觉醒


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*猎魂觉醒][猎魂觉醒:1]]
(let ((org-file "~/literate-programming/猎魂觉醒.org"))
  (when (and (file-exists-p org-file)
             (eq system-type (quote windows-nt)))
    (eval
     `(defun earned::soh ()
        (when (functionp (function earned::funcall))
          (earned::funcall (function (earned::notification earned::cookie))))
        (when (functionp (function earned::package-install-and-require))
          (earned::package-install-and-require (quote spark)))
        (when (functionp (function earned::load-org-project-and-require))
          (earned::load-org-project-and-require (quote soh) ,org-file "~/.emacs.d/site-lisp/soul-of-hunter/soh.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (mapcar
    (function car)
    (quote
     ;; (earned::help (quote soh))
     ((soh-cbg:browse-url "打开＂lh.cbg.163.com＂链接")
      (soh-cbg:change-price "修改出售中某件商品的价格")
      (soh-cbg:my-message "查看藏宝阁的站内消息")
      (soh-cbg:onsale "查看网易藏宝阁所有上架中的商品")
      (soh-cbg:onsale-all "查看网易藏宝阁所有商品")
      (soh-cbg:onsale-sold "查看网易藏宝阁所有已经出售的商品")
      (soh-cbg:read-cookie "将从浏览器里获取登录的权限导入，形如")
      (soh-click:cast "一键精铸当前装备")
      (soh-click:market-buy "抢购［市场］［买入］［我的关注］里的第一条关注的物品")
      (soh-click:recast "选定所消耗的材料后，一键圣痕重铸当前装备")
      (soh-click:start-market-buy-timer "提前打开市场交易页面，在1到2分钟的时候，手动开启计时器，自动刷新、买入。")
      (soh-click:test-cast "测试一键精铸当前装备")
      (soh-click:test-market-buy "测试抢购［市场］［买入］［我的关注］里的第一条关注的物品，最后取消购买。")
      (soh-click:test-recast "选定所消耗的材料后，测试一键圣痕重铸当前装备")
      (soh-cuisine:recipes-benefits-tablist "在‘tabulated-list-mode’里查看料理的效益数据，与‘soh-cuisine:view-recipes-benefits’类似")
      (soh-cuisine:view-recipes-benefits "查看所有具有正向效益的食谱")
      (soh-dd373:singing "在＂https://jf.dd373.com/＂签到")
      (soh-equip:browse-url)
      (soh-equip:equips-by-name "输入装备的名字，在新的页面里显示结果。")
      (soh-equip:render-materials-by-name "像‘soh-sprite::render-answer’值域的格式一样显示装备的进化材料")
      (soh-gimp:dump-procedural-db "保存GIMP的所有可导出的函数到本地")
      (soh-gimp:find-file "在ＧＩＭＰ里打开图片")
      (soh-gimp:start "启动外部GIMP程序")
      (soh-git:directory "查看本地仓库")
      (soh-git:push "将本地文件推送到远程Git仓库")
      (soh-leidian:default-video-ram "手动设置‘soh-leidian::*default-video-ram*’的值")
      (soh-leidian:farm-fertilization "农场批量施肥")
      (soh-leidian:finishing-backpack "整理背包")
      (soh-leidian:get-and-delete-soh-mail "领取猎魂觉醒邮件里的附件并删除")
      (soh-leidian:goto-falling-silver-city-production "前往落银城生产")
      (soh-leidian:goto-farm-task "前往我的农场开始种植任务")
      (soh-leidian:goto-receive-soh-pub-commission-task "前往酒馆进行委托派遣")
      (soh-leidian:launch-soh "启动雷电模拟器的猎魂觉醒游戏")
      (soh-leidian:login-soh "猎魂觉醒登录游戏")
      (soh-leidian:modify-resolution "修改模拟器的分辨率")
      (soh-leidian:poweroff "关闭所有与雷电模拟器相关的外部程序‘soh-leidian::*external-program*’")
      (soh-leidian:shell "‘shell’")
      (soh-leidian:shell-command "在‘shell’的光标处插入常用的终端命令")
      (soh-leidian:video-ram-accelerate "更多的显存和3D视频加速")
      (soh-leidian:virtualbox "打开Oracle VM Virtualbox Manager的窗口界面")
      (soh-leidian:zhan-tuan-group-commission-task "完成战团委托的三次任务")
      (soh-materials:browse-url "打开在线的Git网址")
      (soh-materials:default-materials-to-html "将默认的数据导出为html文件并用浏览器打开")
      (soh-materials:push "将‘soh-materials::*file*’文件推送到‘soh-git::*github-repository*’")
      (soh-mission:today)
      (soh-nircmd:activate-soh-window "将‘soh::*window-title*’窗口显示在最前面")
      (soh-nircmd:activate-window "切换到窗口")
      (soh-nircmd:emacs-window-with-default-transparency)
      (soh-nircmd:maxmize-window "最大化窗口")
      (soh-nircmd:minimize-other-windows "除了当前ＥＭＡＣＳ窗口，最小化所有窗口。")
      (soh-nircmd:minimize-window "最小化窗口")
      (soh-nircmd:transparent-window "Use + or - to adjust the transparency of the window.")
      (soh-notice:browse "浏览游戏公告")
      (soh-notice:display "在ＥＭＡＣＳ里查看公告")
      (soh-notice:update "查看近期内最新的一条公告")
      (soh-notice:views "开始记录公告的阅读量数据，并用显示出来。")
      (soh-notice:views-cancel-timer "手动停止自动收集公告阅读数据的程序")
      (soh-notice:views-start-timer "手动运行自动收集公告阅读数据的程序")
      (soh-notice:views-to-csv "手动输出所有猎魂觉醒公告阅读量数据到＂csv＂文件里")
      (soh-notification:alarm-clock "使用‘notification-api-alarm-clock’来创建定时闹钟")
      (soh-ocr:drop "从‘soh-ocr::*database*’里删除数据。")
      (soh-ocr:manual-labeling "手动标注图片")
      (soh-qr:encode "生成二维码")
      (soh-screenshot:screen "截取当前的屏幕")
      (soh-screenshot:soh "截取＂猎魂觉醒＂窗口的图像")
      (soh-screenshot:start "启用外部程序来进行交互式地截图")
      (soh-sd-next:report)
      (soh-sd-next:reports)
      (soh-sd:report)
      (soh-sd:reports)
      (soh-sprite:ask "输入关键词再返回一个页面")
      (soh-sprite:browse-url "在外部浏览器里打开＂精灵＂页面")
      (soh-sprite:read-token "自动提取浏览器的网址或手动输入‘soh-sprite:*token*’的值")
      (soh-sprite:search "查找当前页面的内容，将符合要求的内容在新的页面里显示，并高亮关键词。")
      (soh-sprite:weapon-skill-rune-upgrade-material "查看武器所有技能符文的升级材料")
      (soh-tesseract:directory "打开‘soh-tesseract::*directory*’文件夹")
      (soh-tesseract:image-to-box "图片转为盒子")
      (soh-tesseract:image-to-box-and-train "把图片生成盒子文件并进行训练")
      (soh-tesseract:image-to-text "识别图片文件")
      (soh-tesseract:train "进行交互式的训练")
      (soh:browse-skill-video "c:/Program Files/SoulOfHunter/Documents/skill_video/patch_list")
      (soh:clear-db "清空安装目录的角色数据")
      (soh:current-soh-material-need-p "查看＂猎魂觉醒＂窗口里的材料是否是需要的材料")
      (soh:current-soh-material-need-p-timer-start "当启用‘soh:current-soh-material-need-p’时以‘soh::*current-soh-material-need-p-timer*’来每隔‘soh::*current-soh-material-need-p-timer-repeat*’秒持续地重复该功能。")
      (soh:current-soh-material-need-p-timer-stop "停止‘soh::*current-soh-material-need-p-timer*’计时器")
      (soh:default-directory "用‘dired’打开默认的文件夹")
      (soh:enter-chat-content "选中或者输入内容，进行编码，再复制，最后在游戏里发送。")
      (soh:eval-region "通用的交互命令，用某个已有的函数对当前的内容进行求值，这个函数只接受一个唯一的字符串自变量。")
      (soh:find-soh-needed-materials-file "打开猎魂觉醒所需要的所有升级材料文件")
      (soh:mark-line "高亮当前行")
      (soh:mark-sexp "高亮当前表达式")
      (soh:minor-mode "绑定一些快捷键")
      (soh:read-from-buffer "如果当前页面符合要求，就渲染当前页面，并在新的页面显示。")
      (soh:same-face-texts "找出使用了相同颜色的文本，在新的页面里显示。")
      (soh:save-buffer "保存带有颜色的文本到文件里，可以使用‘soh:read-from-buffer’来重新高亮。")
      (soh:screenshot-soh-with-ocr "截图并进行标注")
      (soh:shorter-cg "让进入时候加载的ＣＧ更短些")
      (soh:soh-material-need-p "输入材料来查看该材料是否是需要的")
      (soh:start-picker "按键抓抓")
      (soh:start-soh "启动＂猎魂觉醒＂游戏")
      (soh:unmark-line "取消高亮当前行")
      (soh:unmark-sexp "取消高亮当前表达式"))
     ))
   (quote soh)))

;; 猎魂觉醒:1 ends here
;; *** get a geographic address


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*get a geographic address][get a geographic address:1]]
(defun earned::get-geographic-address ()
  "获取计算机的地理地址"
  (let ((json-callback "jQuery1113021067091835221352_1554302398150")
        (url-request-extra-headers (list (cons "Referer" "https://item.mi.com/product/10000138.html"))))
    (let ((response
           (with-current-buffer (url-retrieve-synchronously (format "https://order.mi.com/address/getDefaultAddress?jsonpcallback=%s&ref_goods=2191100007" json-callback) nil t)
             (set-buffer-multibyte t)
             (goto-char (point-min))
             (buffer-substring-no-properties (re-search-forward "\n\n") (point-max)))))
      (when (string-match (format "^%s(\\(.*\\));$" json-callback) response)
        (alist-get (quote data)
                   (json-read-from-string (match-string 1 response)))))))

;; get a geographic address:1 ends here
;; *** chrome


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*chrome][chrome:1]]
(defun earned:start-chrome-with-remote-debugging-port ()
  "https://raw.githubusercontent.com/tobiasmelcher/chrome_debugger/master/chrome_debugger.el
http://localhost:9222/json/list"
  (interactive)
  (let ((chrome
         (or
          (executable-find "chrome")
          (let ((file (expand-file-name "AppData/Local/Google/Chrome/Application/chrome.exe"
                                        (getenv "userprofile"))))
            (when (file-exists-p file)
              file))))
        (port 9222))
    (when chrome
      (let ((arguments (format "--remote-debugging-port=%d" port))
            (onlinep (when (functionp (function earned::onlinep))
                       (earned::onlinep "localhost" port))))
        (if onlinep
            (message (format "Chrome already started with remote debugging port %d" port))
          (async-shell-command (format "%S %s" chrome arguments)))))))

;; chrome:1 ends here
;; *** init


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*init][init:1]]
(defun earned:init ()
  "加载`user-init-file'"
  (interactive)
  (let ((file user-init-file))
    (when (and file (file-exists-p file))
      (load-file file))))

;; init:1 ends here
;; *** tesseract


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*tesseract][tesseract:1]]
(let ((directory (expand-file-name "AppData/Local/Tesseract-OCR" (getenv "userprofile")))
      (tesseract "tesseract"))
  ;; "Windows环境安装tesseract-ocr 4.00并配置环境变量 - 简庆旺 - 博客园: https://www.cnblogs.com/jianqingwang/p/6978724.html"
  (unless (executable-find tesseract)
    (when (file-exists-p directory)
      (add-to-list (quote exec-path) directory)))
  ;; https://raw.githubusercontent.com/tesseract-ocr/tessdata/master/chi_tra.traineddata
  (when (executable-find tesseract)
    (let ((chi-traineddata (expand-file-name "tessdata/chi_sim.traineddata" directory)))
      (unless (file-exists-p chi-traineddata)
        (let ((source "~/Downloads/chi_sim.traineddata"))
          (when (file-exists-p source)
            (copy-file source chi-traineddata)))))))

;; tesseract:1 ends here
;; *** timer list


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*timer list][timer list:1]]
(defun earned:timer-list ()
  "查看`timer-list'"
  (interactive)
  (with-current-buffer (get-buffer-create "earned:timer-list")
    (erase-buffer)
    (insert (format "Output created by function `earned:timer-list' at %s\n\n"
                    (format-time-string "%Y-%m-%d %a %H:%M:%S")))
    (let ((hour-start))
      (dolist (timer timer-list)
        (let ((time (timer--time timer))
              (function (timer--function timer)))
          (let ((hour (cl-third (decode-time time))))
            (unless (and hour-start
                         (< (time-to-seconds (time-subtract time (current-time)))
                            (* 24 60 60))
                         (= hour hour-start))
              (insert (propertize (format-time-string "%Y-%m-%d %H:00\n" time)
                                  (quote face) font-lock-keyword-face))
              (setf hour-start hour))
            (insert (format
                     "           %s %s\n"
                     (propertize (format-time-string "%H:%M" time)
                                 (quote face) font-lock-string-face)
                     function))))))
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))))

;; timer list:1 ends here
;; *** ivy rich

;; 在使用＂counsel-M-x＂后在该命令的右边显示文档


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*ivy rich][ivy rich:1]]
(defun earned::ivy-rich ()
  (when (functionp (function earned::package-install-and-require))
    (earned::package-install-and-require (quote ivy-rich)))
  (setf ivy-rich-display-transformers-list
        `(counsel-M-x
          (:columns
           ((counsel-M-x-transformer
             (:width 40))
            (ivy-rich-counsel-function-docstring
             (:face font-lock-doc-face))))))
  (when (functionp (function ivy-rich-mode))
    (ivy-rich-mode)))

;; ivy rich:1 ends here
;; *** read-from-minibuffer

;; 光不实时调整模式栏的大小这一点就足够了。


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*read-from-minibuffer][read-from-minibuffer:1]]
(defun earned::make-centered-minibuffer-frame (name frame)
  "`earned::read-from-minibuffer'"
  (when (and (stringp name)
             (frame-live-p frame))
    (let ((total-width (frame-inner-width frame))
          (total-height (frame-inner-height frame))
          (parameters (frame-parameters frame))
          (char-width (frame-char-width frame)))
      (let ((blank-width (car (cl-floor (* (/ total-width 1.618) 0.618))))
            (left (eval (alist-get (quote left) parameters)))
            (top (eval (alist-get (quote top) parameters))))
        (make-frame
         `((name . ,name)
           (minibuffer . only)
           ,(cons (quote height)
                  (if (boundp (quote ivy-height))
                      ivy-height
                    10))
           (width . ,(max (/ (- total-width blank-width)
                             char-width)
                          40))
           (left . ,(+ left (/ blank-width 2)))
           (top . ,(+ top (/ total-height 3)))))))))

(when (display-graphic-p)
  (eval
   `(defun earned::read-from-minibuffer (oldfun &rest rest)
      "让 minibuffer 显示在屏幕中间 - Emacs-general - Emacs China: https://emacs-china.org/t/topic/890"
      (let ((select (selected-frame))
            (buffer (generate-new-buffer "*Minibuffer*")))
        (message (format "Popuping frame %s..." (buffer-name buffer)))
        (when (functionp (function earned::make-centered-minibuffer-frame))
          (let ((frame (earned::make-centered-minibuffer-frame (buffer-name buffer) select)))
            (when frame
              (make-frame-visible frame)
              (select-frame-set-input-focus frame (quote norecord))
              (unwind-protect (apply oldfun rest)
                (select-frame-set-input-focus select)
                (kill-buffer buffer)
                (delete-frame frame)))))))))

(when (functionp (function earned::read-from-minibuffer))
  (eval
   `(defun earned:read-from-minibuffer-mode ()
      "开关让 minibuffer 显示在屏幕中间的功能"
      (interactive)
      (let ((mode (quote earned:read-from-minibuffer-mode))
            (symbols (quote
                      (read-from-minibuffer
                       read-string
                       yes-or-no-p
                       y-or-n-p))))
        (unless (boundp mode)
          (setf (symbol-value mode) t))
        (if (symbol-value mode)
            (progn
              (dolist (symbol symbols)
                (advice-add symbol :around (function earned::read-from-minibuffer)))
              (setf (symbol-value mode) nil)
              (message (format "TURN ON %s" mode)))
          (dolist (symbol symbols)
            (advice-remove symbol (function earned::read-from-minibuffer)))
          (setf (symbol-value mode) t)
          (message (format "TURN OFF %s" mode)))))))


;; read-from-minibuffer:1 ends here
;; *** quail-show-guidance

;; 在使用＂quail＂输入法的时候，如果使用了＂earned::read-from-minibuffer＂功能，在弹窗里使用输入法时会再弹出一个窗口而挡住了当前的输入窗口。


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*quail-show-guidance][quail-show-guidance:1]]
(when (functionp (function earned::read-from-minibuffer))
  (eval
   `(defun earned::quail-show-guidance ()
      "Display a guidance for Quail input method in some window.
The guidance is normally displayed at the echo area,
or in a newly created frame (if the current buffer is a
minibuffer and the selected frame has no other windows)."
      ;; At first, setup a buffer for completion.
      (quail-setup-completion-buf)
      (bury-buffer quail-completion-buf)

      ;; Then, show the guidance.
      (when (and (quail-require-guidance-buf)
                 (not input-method-use-echo-area)
                 (null unread-command-events)
                 (null unread-post-input-method-events))
        (if (minibufferp)
            (quail-minibuffer-message
             (format "  [%s]\n%s"
                     current-input-method-title quail-guidance-str))
          ;; Show the guidance in echo area without logging.
          (let ((message-log-max nil))
            (message "%s" quail-guidance-str))))))
  (with-eval-after-load (quote quail)
    (when (functionp (function earned::advice-add))
      (earned::advice-add (quote quail-show-guidance)
                          :override
                          (function earned::quail-show-guidance)))))

;; quail-show-guidance:1 ends here
;; *** gomoku


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*gomoku][gomoku:1]]
(with-eval-after-load (quote gomoku)
  (defun earned:gomoku-auto-plays ()
    "五子棋`gomoku'之电脑对战电脑"
    (interactive)
    (unless gomoku-game-in-progress
      (gomoku))
    (when (eq major-mode (quote gomoku-mode))
      (while gomoku-game-in-progress
        (gomoku-goto-square (gomoku-strongest-square))
        (gomoku-human-plays))))
  (defun earned:gomoku-next-play ()
    "提示五子棋里下一步可以下的地方"
    (interactive)
    (when (and (eq major-mode (quote gomoku-mode))
               gomoku-game-in-progress)
      (gomoku-goto-square (gomoku-strongest-square)))))

;; gomoku:1 ends here
;; *** explorer.exe

;; 类似
;; [[exwm]]
;; ，用＂Emacs＂来管理＂Windows＂的桌面程序。

;; 在登录＂Windows＂后直接启动＂emacs＂程序而不启动＂explorer.exe＂。


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*explorer.exe][explorer.exe:1]]
(when (eq system-type (quote windows-nt))
  (eval
   `(defun earned::change-winlogon-explorer (command)
      "`command'∈{`earned::emacs-Q-command'}"
      (when (and (stringp command)
                 (executable-find "reg"))
        ;; reg delete "HKEY_CURRENT_USER\Software\Microsoft\Windows\CurrentVersion\Policies\Explorer" /f
        (unless (string-match "ERROR: The system was unable to find the specified registry key or value." (shell-command-to-string "reg query HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Policies\\Explorer"))
          (shell-command-to-string "reg delete HKEY_CURRENT_USER\\Software\\Microsoft\\Windows\\CurrentVersion\\Policies\\Explorer /f"))
        ;; reg add "HKEY_LOCAL_MACHINE\SOFTWARE\Microsoft\Windows NT\CurrentVersion\Winlogon" /v "Shell" /d "\"c:\Users\jianjia\Downloads\emacs-26.1-i686\bin\runemacs.exe\" -Q -D --load \"c:\Users\jianjia\AppData\Roaming\.emacs.d\init.el\"" /f
        (dolist (location
                 (list
                  "HKEY_CURRENT_USER\\Software\\Microsoft\\Windows NT\\CurrentVersion\\Winlogon"
                  "HKEY_LOCAL_MACHINE\\SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Winlogon"))
          (shell-command-to-string
           (format "reg add \"%s\" /v \"Shell\" /d %s /f" location
                   (replace-regexp-in-string
                    "/" (regexp-quote "\\")
                    (format "%S"  command)))))))))


(when (and (functionp (function earned::change-winlogon-explorer))
           (functionp (function earned::emacs-Q-command)))
  (eval
   `(defun earned:change-winlogon-explorer ()
      "手动修改在登录＂Windows＂后直接启动＂emacs＂程序或＂explorer.exe＂"
      (interactive)
      (let ((collection
             (list
              (list "explorer" "explorer.exe")
              (list "emacs" (earned::emacs-Q-command))
              (list "emacs -nw"
                    (format "%s -nw"
                            (earned::emacs-Q-command nil "emacs"))))))
        (let ((select
               (completing-read "Specify winlogon command: " collection nil t)))
          (let ((command (cl-second (cl-assoc select collection :test (function equal)))))
            (when command
              (message (earned::change-winlogon-explorer command)))))))))

;; explorer.exe:1 ends here
;; *** AutoHotkey

;; 在替换
;; [[explorer.exe]]
;; 后，需要用全局的快捷键来启用ＥＭＡＣＳ，启动其它程序。


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*AutoHotkey][AutoHotkey:1]]
(defun earned:autohotkey ()
  "手动动态地生成＂autohotkey.ahk＂文件，并创建快捷方式，便于开机自动启动。

Win下最爱效率利器:AutoHotKey - 云轩奕鹤 - 博客园: https://www.cnblogs.com/jadeboy/p/5274995.html"
  (interactive)
  (let ((exe "c:/Program Files/AutoHotkey/AutoHotkey.exe"))
    (when (executable-find exe)
      (let ((directory (expand-file-name "~/Microsoft/Windows/Start Menu/Programs/Startup/")))
        (unless (file-exists-p directory)
          (make-directory directory))
        (when (file-exists-p directory)
          (let ((file (expand-file-name "autohotkey.ahk" directory)))
            (with-temp-file file
              (dolist (item
                       (list
                        ;; # 号代表 Win 键；
                        ;; ! 号代表 Alt 键；
                        ;; ^ 号代表 Ctrl 键；
                        ;; + 号代表 shift 键；
                        ;; :: 号(两个英文冒号)起分隔作用；
                        ;; run，非常常用 的 AHK 命令之一;
                        ;; ; 号代表 注释后面一行内容；
                        (format "^space::^@") ; C-SPC => C-@
                        (format "#r:: run %S /r %S" exe file) ; win+r
                        (format "#e:: run %s" (earned::emacs-Q-command)) ; win+e
                        (format "#+e:: run %s -nw" (earned::emacs-Q-command nil "emacs")) ; win+shift+e
                        (format "#+m:: run %s --eval %S" (earned::emacs-Q-command nil nil (prin1-to-string `(setf earned::*run-with-timer* nil earned::*gc-cons-threshold* nil))) ; win+shift+m
                                (prin1-to-string `(sdf-interactive-websocket:opens)))
                        (format "#s:: run %s -nw --eval %S" (earned::emacs-Q-command nil "emacs")
                                (prin1-to-string (quote (progn (soh-leidian:launch-soh) (kill-emacs))))) ; win+s
                        (format "#x:: run mblctr.exe") ; win+x
                        ;; 屏蔽雷电模拟器的默认快捷键
                        (format "#IfWinActive 雷电模拟器
^2:: send 2
^3:: send 3
#IfWinActive")
                        ))
                (insert
                 (format "%s\n" item))))
            ;; "c:/Users/jianjia/Downloads/nircmd/nircmd.exe" shortcut "C:\Program Files\AutoHotkey\AutoHotkey.exe" "c:\Users\jianjia\AppData\Roaming\literate-programming" autohotkey.lnk "/r autohotkey.ahk" "" "" "" "c:\Users\jianjia\AppData\Roaming\literate-programming"
            (let ((nircmd (executable-find "nircmd")))
              (when nircmd
                (let ((default-directory directory))
                  (shell-command
                   (format "%S shortcut %S %S %S %S %S %S %S %S"
                           nircmd exe directory "AutoHotkey.exe"
                           (format "/r %s" (file-name-nondirectory file))
                           "" "" "" "%cd%")))))))))))

;; AutoHotkey:1 ends here
;; *** htmlize

;; 使用＂org-export-dispatch＂导出ＯＲＧ文件为ＨＴＭＬ时，由于不存在＂htmlize＂包，再要是启用了＂debug-on-error＂，会造成ＥＭＡＣＳ卡死：

;; #+BEGIN_EXAMPLE
;; org-html-fontify-code: Please install htmlize from https://github.com/hniksic/emacs-htmlize
;; #+END_EXAMPLE


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*htmlize][htmlize:1]]
(defun earned::htmlize (&rest r)
  (when (functionp (function earned::package-install-and-require))
    (earned::package-install-and-require (quote htmlize))))

(with-eval-after-load (quote ox-html)
  (when (functionp (function earned::advice-add))
    (earned::advice-add (quote org-html-fontify-code) :before (function earned::htmlize))))

;; htmlize:1 ends here
;; *** mhtml


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*mhtml][mhtml:1]]
(when (functionp (function earned::add-hook))
  (earned::add-hook (quote mhtml-mode-hook) (function font-lock-mode)))

;; mhtml:1 ends here
;; *** company-tabnine


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*company-tabnine][company-tabnine:1]]
(unless (if (functionp (function earned::system-type))
            (memq (earned::system-type)
                  (quote (termux windows-nt)))
          t)
  (eval
   `(defun earned::company-tabnine ()
      (when (functionp (function earned::package-install-and-require))
        (earned::package-install-and-require (quote company-tabnine)))
      (when (functionp (function company-tabnine--executable-path))
        (if (condition-case error (company-tabnine--executable-path)
              (error nil))
            (when (and (boundp (quote company-backends))
                       (functionp (function company-tabnine))
                       (not (memq (function company-tabnine)
                                  company-backends)))
              (add-to-list (quote company-backends) (function company-tabnine))
              (setf company-idle-delay 0
                    company-show-numbers t))
          (when (functionp (function background-eval))
            (background-eval
             (quote
              (when (functionp (function company-tabnine))
                (require (quote company-tabnine) nil t)
                (when (functionp (function company-tabnine-install-binary))
                  (company-tabnine-install-binary)))))))))))

(when (functionp (function earned::add-hook))
  (earned::add-hook (quote company-mode-hook)
                    (function earned::company-tabnine)))

(when (functionp (function earned::company-tabnine))
  (eval
   `(defun earned:company-tabnine-kill-process ()
      "关闭`company-tabnine--process'"
      (interactive)
      (unless (eq major-mode (quote emacs-lisp-mode))
        (when (functionp (function company-tabnine-kill-process))
          (company-tabnine-kill-process))))))

(with-eval-after-load (quote company-tabnine)
  (when (functionp (function earned::run-with-timer))
    (earned::run-with-timer 0 10 (function earned:company-tabnine-kill-process))))

;; company-tabnine:1 ends here
;; *** seconds to date


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*seconds to date][seconds to date:1]]
(defun earned:seconds-to-date ()
  "1570836649 => 2019-10-12 Sat 07:30:49 UTC+08:00"
  (interactive)
  (let ((point (thing-at-point (quote word) t)))
    (when point
      (let ((seconds (string-to-number point)))
        (unless (zerop seconds)
          (message (format-time-string "%Y-%m-%d %a %H:%M:%S UTC%:z" (seconds-to-time seconds))))))))

;; seconds to date:1 ends here
;; *** sdf


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*sdf][sdf:1]]
(let ((org-file "~/literate-programming/emacs-lisp-sdf.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::sdf ()
        (when (functionp (function earned::package-install-and-require))
          (earned::package-install-and-require (quote websocket))
          (earned::package-install-and-require (quote request)))
        (when (functionp (function earned::funcall))
          (earned::funcall (function (earned::cookie))))
        (when (functionp (function earned::load-org-project-and-require))
          (earned::load-org-project-and-require (quote sdf) ,org-file "~/.emacs.d/site-lisp/emacs-lisp-sdf/sdf.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (mapcar
    (function car)
    (quote
     ((sdf-interactive-emacs:closes "关闭所有运行‘sdf’的Emacs")
      (sdf-interactive-emacs:starts)
      (sdf-interactive-file:directory)
      (sdf-interactive-file:load-library)
      (sdf-interactive-process-menu:find-file "通过‘process-menu-mode’到达日志文件")
      (sdf-interactive-proxy:start-filter-host-ports-timer)
      (sdf-interactive-ruanmei:memory-master "启用软媒内存整理Memory (Private Working Set)")
      (sdf-interactive-user-report:browse "在外部浏览器里打开查云赚号的在线客户端和IP数的链接")
      (sdf-interactive-user-report:display "查云赚号的在线客户端和IP数")
      (sdf-interactive-websocket:opens)
      (sdf-interactive-websocket:opens-without-proxy)
      (sdf-interactive-websocket:print)
      (sdf-interactive-websocket:prints)
      (sdf-interactive-worker-income:details "查看收入明细"))
     ))
   (quote sdf)))

;; sdf:1 ends here
;; *** DD373


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*DD373][DD373:1]]
(let ((org-file "~/literate-programming/emacs-lisp-dd373.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::dd373 ()
        (when (functionp (function earned::funcall))
          (earned::funcall (function (earned::ewe))))
        (when (functionp (function earned::load-org-project-and-require))
          (earned::load-org-project-and-require (quote dd373) ,org-file "~/.emacs.d/site-lisp/emacs-lisp-dd373/dd373.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (dd373:autorun dd373:background-autorun))
   (quote dd373)))

;; DD373:1 ends here
;; *** feed


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*feed][feed:1]]
(let ((org-file "~/literate-programming/emacs-lisp-feed.org"))
  (when (file-exists-p org-file)
    (eval
     `(defun earned::feed ()
        (when (functionp (function earned::funcall))
          (earned::funcall (quote (earned::magit))))
        (when (and (functionp (function earned::load-org-project-and-require))
                   (featurep (quote magit)))
          (earned::load-org-project-and-require (quote feed) ,org-file "~/.emacs.d/site-lisp/emacs-lisp-feed/feed.el"))))))

(when (functionp (function earned::autoload))
  (earned::autoload
   (quote
    (feed:background feed:clients feed:feeds))
   (quote feed)))

(when (functionp (function earned::run-with-timer))
  (earned::run-with-timer
   (max (random 3600) 300)
   (* 60 60)
   (function feed:background)))

;; feed:1 ends here
;; *** 查看文件夹里的所有图片


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*查看文件夹里的所有图片][查看文件夹里的所有图片:1]]
(defun earned::images-to-directory (images to)
  "给某个文件夹下的所有图片创建快捷方式，从而实现在一个文件夹下查看所有图片的效果"
  (let ((default-directory (expand-file-name to)))
    (let ((shortcut (executable-find "nircmd")))
      (when (and shortcut
                 (file-exists-p default-directory))
        (unless (file-exists-p to)
          (mkdir to))
        (let ((directory (expand-file-name (format-time-string "%s"))))
          (mkdir directory)
          (dolist (file (directory-files-recursively images "\\.jpg\\|\\.png"))
            (let ((lnk (format-time-string "%s%N.lnk"))
                  (target file)
                  (coding-system-for-write locale-coding-system))
              (shell-command-to-string
               (format "%S shortcut %S %S %S" shortcut target directory lnk)))))))))

;; 查看文件夹里的所有图片:1 ends here
;; *** 写了多少行代码


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*写了多少行代码][写了多少行代码:1]]
(defun earned:total-lines-of-code ()
  "查看Git仓库下的所有有效文件的总行数"
  (interactive)
  (let ((default-directory "~/literate-programming"))
    (when (and (executable-find "git")
               (file-exists-p default-directory))
      (let ((shell (shell-command-to-string "git log --pretty=tformat: --numstat")))
        (let ((number
               (apply (function +)
                      (mapcar (lambda (item)
                                (string-to-number (car (split-string item "\t" t))))
                              (split-string shell "\n" t)))))
          (message (format "%s" number)))))))

;; 写了多少行代码:1 ends here
;; *** web-mode


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*web-mode][web-mode:1]]
(defun earned::web-mode ()
  (earned::package-install-and-require (quote web-mode)))

(defun earned::web-mode-hook ()
  (when (functionp (quote earned::no-indent-tab-whitespace))
    (earned::no-indent-tab-whitespace))
  (when (functionp (function earned::minor-mode))
    (mapcar (function earned::minor-mode)
            (quote
             (company
              aggressive-indent
              rainbow-delimiters
              font-lock
              undo-tree
              symbol-overlay
              paredit
              show-paren))))
  (hs-minor-mode)
  (when (boundp (quote paredit-mode-map))
    (let ((key (kbd "{"))
          (def (function paredit-open-curly)))
      (unless (eq (key-binding key)
                  def)
        (define-key paredit-mode-map key def)))))

(with-eval-after-load (quote web-mode)
  (dolist (symbol
           (quote
            (web-mode-script-padding
             web-mode-style-padding
             web-mode-markup-indent-offset
             web-mode-css-indent-offset
             web-mode-code-indent-offset
             )))
    (setf (symbol-value symbol)
          2)))

(when (functionp (function earned::add-hook))
  (earned::add-hook (quote web-mode-hook)
                    (function earned::web-mode-hook)))

;; web-mode:1 ends here
;; *** paredit


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*paredit][paredit:1]]
(defun earned::paredit-space-for-delimiter-p (oldfun &rest rest)
  "在某些情况下不再给括号自动添加插入空格
`paredit-space-for-delimiter-p',rest∈{(ENDP DELIMITER)}"
  (and (not (memq major-mode
                  (quote
                   (js-mode
                    css-mode
                    web-mode))))
       (apply oldfun rest)))

(with-eval-after-load (quote paredit)
  (when (functionp (function earned::advice-add))
    (earned::advice-add (quote paredit-space-for-delimiter-p) :around (function earned::paredit-space-for-delimiter-p))))

;; paredit:1 ends here
;; *** all matches as kill


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*all matches as kill][all matches as kill:1]]
(defun earned::all-matches-as-strings (regexp buffer-or-name)
  (let ((matches '()))
    (with-current-buffer buffer-or-name
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp regexp nil t 1)
          (push (match-string-no-properties 0)
                matches))))
    matches))

(defun earned:all-matches-as-kill ()
  "复制所有符合一定正则表达式的内容"
  (interactive)
  (let ((regexp (read-string "Regexp: "))
        (buffer-or-name (buffer-name))
        (seperator (read-string "Seperator: ")))
    (let ((string (mapconcat (function identity)
                             (earned::all-matches-as-strings regexp buffer-or-name)
                             seperator)))
      (message (format "%s" string))
      (kill-new string))))

;; all matches as kill:1 ends here
;; *** js-mode


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*js-mode][js-mode:1]]
(defun earned::js-mode-hook ()
  (setf js-switch-indent-offset 4)
  (when (functionp (function earned::web-mode-hook))
    (earned::web-mode-hook)))

(when (functionp (function earned::add-hook))
  (earned::add-hook (quote js-mode-hook)
                    (function earned::js-mode-hook)))

;; js-mode:1 ends here
;; *** redmibook-air-13


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*redmibook-air-13][redmibook-air-13:1]]
(defun earned:redmibook-air-13 ()
  "为Redmibook air 13配置的节能模式"
  (interactive)
  (dolist (pid (list-system-processes))
    (let ((attributes (process-attributes pid)))
      (let ((command (alist-get (quote comm) attributes)))
        (when (cl-member
               command
               (quote
                ("WinStore.App.exe" "SystemSettings.exe" "MicrosoftEdge.exe" "ShellExperienceHost.exe" "SystemSettingsBroker.exe" "ApplicationFrameHost.exe" "WindowsInternal.ComposableShell.Experiences.TextInput.InputApp.exe" "MicrosoftEdgeCP.exe" "taskhostw.exe" "MicrosoftEdgeSH.exe" "smartscreen.exe" "browser_broker.exe" "RuntimeBroker.exe" "SpeechRuntime.exe"
                 "backgroundTaskHost.exe"
                 "conhost.exe"
                 "dllhost.exe"
                 "svchost.exe"))
               :test (function equal))
          (unless (functionp (function proced-send-signal))
            (require (quote proced)))
          (proced-send-signal 9 (list (list pid))))))))

;; redmibook-air-13:1 ends here
;; *** linux-find


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*linux-find][linux-find:1]]
(defun earned:linux-find ()
  "使用Emacs的正则表达式来查找某个目录下符合要求的内容

试图模拟Linux下的find命令"
  (interactive)
  (when buffer-file-name
    (save-buffer))
  (let* ((default-directory (read-directory-name "Directory: " "~/literate-programming/"))
         (regexp (read-string "Regexp: "))
         (name (format "earned:linux-find: %s" (file-name-nondirectory (directory-file-name default-directory)))))
    (when (> (length regexp) 2)
      (when (get-buffer name) (kill-buffer name))
      (let ((buffer (get-buffer-create name))
            (count 0))
        (with-current-buffer buffer
          (dolist (file
                   (cl-sort
                    (directory-files default-directory t)
                    (function >)
                    :key (lambda (filename) (time-to-seconds (nth 5 (file-attributes filename))))))
            (when (string-match "\.org$" file)
              (let ((content
                     (with-temp-buffer
                       (insert-file-contents file)
                       (buffer-string))))
                (let ((matches
                       (reverse
                        (goer--all-matches-as-strings regexp content))))
                  (when matches
                    (insert
                     (format
                      "* [[file:%s][%s]] [0/%s]\n\n"
                      file
                      (file-name-nondirectory file)
                      (length matches))))))))
          (org-mode)
          (view-mode))
        (switch-to-buffer buffer)
        (goto-char (point-min))))))

;; linux-find:1 ends here
;; *** battery-report


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*battery-report][battery-report:1]]
(defun earned:battery-report ()
  "查看电脑电池的状态"
  (interactive)
  (when (executable-find "powercfg")
    (let ((default-directory temporary-file-directory))
      (let ((file (convert-standard-filename (expand-file-name "earned:battery-report.html"))))
        (when (file-exists-p file)
          (delete-file file))
        (shell-command-to-string
         (format "powercfg.exe /batteryreport /output %S" file))
        (eww-open-file file)))))

;; battery-report:1 ends here
;; *** computer-uptime


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*computer-uptime][computer-uptime:1]]
(defun earned:computer-uptime ()
  "查看当前电脑从开机到现在的使用时间"
  (interactive)
  (when (executable-find "powershell")
    (shell-command
     (format "powershell.exe -command %S"
             "(get-date) - (gcim Win32_OperatingSystem).LastBootUpTime"))))

;; computer-uptime:1 ends here
;; *** windows user account control


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*windows user account control][windows user account control:1]]
(when (eq system-type (quote windows-nt))
  (eval
   `(defun earned::window-user-account-control (command)
      "约相当于以右键管理员的权限运行某个程序"
      (shell-command-to-string
       (format "mshta vbscript:CreateObject(\"Shell.Application\").ShellExecute(\"%s\",\"\",\"\",\"runas\",1)(window.close)"
               (executable-find command))))))

;; windows user account control:1 ends here
;; *** fix volume


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*fix volume][fix volume:1]]
(defun earned:fix-volume ()
  "先查看最高分贝为-43.1dB，再调整最高分贝为0，整体提升43.1dB"
  (interactive)
  (let ((ffmpeg (executable-find "ffmpeg")))
    (when ffmpeg
      (let ((file (read-file-name "To fixed volume file: ")))
        (let ((file (expand-file-name file)))
          (let ((string
                 (shell-command-to-string
                  (format "%S -i %S -filter_complex volumedetect -c:v copy -f null /dev/null"
                          ffmpeg file))))
            (when (string-match "max_volume: \\(.*\\) dB"
                                string)
              (let ((up (* (string-to-number (match-string 1 string))
                           -1))
                    (output (concat file "." (file-name-extension file))))
                (shell-command
                 (format "%S -i %S -af \"volume=50dB\" -strict -2 %S -y"
                         ffmpeg file output))
                (message "%s" output)))))))))


;; fix volume:1 ends here
;; *** 提取apk里的AndroidManifest.xml


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*提取apk里的AndroidManifest.xml][提取apk里的AndroidManifest.xml:1]]
(defun earned:aapt-dump-android-manifest ()
  "提取apk里的AndroidManifest.xml"
  (interactive)
  (when (executable-find "pm")
    (let ((name
           (completing-read
            "Specify package: "
            (mapcar (lambda (i)
                      (cl-subseq i (length "package:")))
                    (split-string (shell-command-to-string "pm list packages") nil t))
            nil t)))
      (let ((directory (expand-file-name "~/AndroidManifest/")))
        (unless (file-directory-p directory)
          (make-directory directory t))
        (let ((output (expand-file-name (concat name ".xml") directory)))
          (earned::aapt-dump-android-manifest name output)
          (message (format "%s" output)))))))

(defun earned::aapt-dump-android-manifest (name output)
  (when (executable-find "pm")
    (let ((path (shell-command-to-string
                 (concat "pm path " name))))
      (unless (zerop (length path))
        (let ((apk (cl-subseq path (length "package:") (- (length path) 1))))
          (let ((output (expand-file-name output)))
            (shell-command
             (format "aapt dump xmltree %S AndroidManifest.xml > %S"
                     apk output))
            output))))))

;; 提取apk里的AndroidManifest.xml:1 ends here
;; *** delete blank lines


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*delete blank lines][delete blank lines:1]]
(defun earned:delete-blank-lines ()
  "删除页面内所有空行"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (line-number-at-pos)
              (line-number-at-pos (point-max)))
      (delete-blank-lines)
      (next-line))))

;; delete blank lines:1 ends here
;; ** EOF


;; [[file:~/literate-programming/emacs-init-asynchronously.org::*EOF][EOF:1]]
(when (functionp 'earned::add-hook)
  (earned::add-hook (quote after-init-hook) (function earned::after-init-hook)))

(when (functionp (function earned::when-emacs-Q-load-user-init-file))
  (earned::when-emacs-Q-load-user-init-file))

(provide 'earned)

;; EOF:1 ends here
;; * Footnotes

;; [fn:1] time命令_Linux time命令：测量命令的执行时间或者系统资源的使用情况: http://c.biancheng.net/linux/time.html




