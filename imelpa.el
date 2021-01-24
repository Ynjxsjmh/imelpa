;;; imelpa.el --- Emacs package manager -*- lexical-binding: t -*-

;; Author: Ynjxsjmh
;; Maintainer: Ynjxsjmh
;; Version: 1.0
;; Package-Requires: (emacs "26.3")
;; Homepage: https://github.com/Ynjxsjmh/imelpa
;; Keywords: package-manager


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; `imelpa' can help you to manage those packages don't exist on elpa/melpa.

;;; Code:

(require 'cl-lib)
(require 'json)

(defgroup imelpa nil
  "Manage packages unavailable from melpa/elpa"
  :prefix "imelpa-"
  :group 'applications)

(defcustom imelpa-packages-dir "~/.emacs.d/imelpa/"
  "The directory for the downloaded packages."
  :type 'string
  :group 'imelpa)

(defcustom imelpa-config-filepath "~/.emacs.d/imelpa.json"
  "The default filepath of configuration file."
  :type 'string
  :group 'imelpa)

(defcustom imelpa-config-key-package-name "package_name"
  "The default key of package name in configuration file."
  :type 'string
  :group 'imelpa)

(defcustom imelpa-config-key-package-url "package_url"
  "The default key of package url in configuration file."
  :type 'string
  :group 'imelpa)

(defcustom imelpa-config-key-install-tool "install_tool"
  "The default key of install tool in configuration file."
  :type 'string
  :group 'imelpa)

(cl-defstruct (imelpa-package-desc
               ;; Rename the default constructor from `make-imelpa-package-desc'.
               (:constructor imelpa-package-desc-create))
  "Structure containing information about an individual package.
Slots:
`name'	  Name of the package, as a string.
`version' Version of the package, as a string.
`url'     Url of the package, as a string.
`tool'    Tool to download the package, as a `imelpa-download-tool'.
`dir'	  The directory where the package is installed (if installed)"
  name
  version
  url
  tool
  dir)

(defconst imelpa-github-repo-regexp "github.com/\\([^/]+?\\)/\\([^/]+\\)")
(defconst imelpa-github-content-regexp "github.com/\\([^/]+?\\)/\\([^/]+?\\)/\\(tree\\|blob\\)/\\([^/]+?\\)/\\(.+\\)")

(defun imelpa-init ()
  "Read the configuration file and install uninstalled packages."
  (unless (file-directory-p imelpa-packages-dir)
    (make-directory imelpa-packages-dir))

  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (packages (json-read-file imelpa-config-filepath)))
    (dolist (package packages)
      (let ((package-desc
             (imelpa-package-desc-create
              :name (gethash imelpa-config-key-package-name package)
              :url  (gethash imelpa-config-key-package-url package)
              :tool (gethash imelpa-config-key-install-tool package))))
        (imelpa-require-package package-desc)))))

(defun imelpa-require-package (package)
  "Install the package if it hasn't been installed.
Argument PACKAGE a `imelpa-package-desc' object."
  (unless (imelpa--package-installed-p package)
    (imelpa--package-install package)))

(defun imelpa--package-installed-p (package)
  "Return non-nil if PACKAGE is installed.
Argument PACKAGE a `imelpa-package-desc' object."
  (let ((package-name (imelpa-package-desc-name package))
        (packages-name (directory-files imelpa-packages-dir nil directory-files-no-dot-files-regexp)))
    (member package-name packages-name)))

(defun imelpa--package-install (package)
  "Install target PACKAGE.
Argument PACKAGE a `imelpa-package-desc' object."
  (let* ((name (imelpa-package-desc-name package))
         (tool (imelpa-package-desc-tool package))
         (url (imelpa-package-desc-url package))
         (dir (concat (file-name-as-directory imelpa-packages-dir) name)))
    (cond
     ((equalp tool "git-clone")
      (let ((command (format "git clone %s %s" url dir)))
        (progn (message command)
               (shell-command command))))
     ((equalp tool "wget")
      (let ((command (format "%s %s -P %s" tool url dir)))
        (progn (message command)
               (shell-command command)))))))

(defun imelpa--http-post (url)
  "Send HTTP request to URL,
returning the response as a `plist' object:
:header   HTTP raw heaers
:content  HTTP content
:status   HTTP status code
"
  (let (header
	    content
	    status
        response)
    (with-current-buffer
	    (url-retrieve-synchronously url)
	  ;; status
	  (setq status url-http-response-status)
	  ;; return the header and the data separately
	  (goto-char (point-min))
      (goto-char url-http-end-of-headers)
	  (setq header   (buffer-substring (point-min) (point))
		    content  (buffer-substring (1+ (point)) (point-max)))
      (setq response (plist-put response :content content))
      (setq response (plist-put response :header  header))
      (setq response (plist-put response :status  status)))))

(defun imelpa--parse-github-url (url)
  "Parse github URL.
returning the result as a `plist' object with following keys:
:owner   The owner name
:repo    The repo name
:type    Possible values: tree/blob
:branch  The branch name
:path    The filepath
"
  (let (github
        pattern)

    (cond
     ((string-match imelpa-github-content-regexp url)
      (setq pattern imelpa-github-content-regexp))
     ((string-match imelpa-github-repo-regexp url)
      (setq pattern imelpa-github-repo-regexp))
     (t (error "Not supported github url %s" url)))

    (save-match-data
      (and (string-match pattern url)
           (setq github (plist-put github :owner  (match-string 1 url)))
           (setq github (plist-put github :repo   (match-string 2 url)))
           (setq github (plist-put github :type   (match-string 3 url)))
           (setq github (plist-put github :branch (match-string 4 url)))
           (setq github (plist-put github :path   (match-string 5 url)))))))

(defun imelpa--get-github-content-download-url-and-path (url)
  "Get files download url,
returning the result as a `alist' object with download url as key and path as value."
  (let* ((github (imelpa--parse-github-url url))
         (owner  (plist-get github :owner))
         (repo   (plist-get github :repo))
         (type   (plist-get github :type))
         (branch (plist-get github :branch))
         (path   (plist-get github :path))
         (api    (format "https://api.github.com/repos/%s/%s/contents/%s?ref=%s"
                         owner repo (or path "") (or branch "")))
         (res    (imelpa--http-post api))
         (url-path-list '()))

    (if (eq 200 (plist-get res :status))
        (let* ((data (plist-get res :content))
               (json-object-type 'hash-table)
               (json-array-type  'list)
               (contents (json-read-from-string data)))

          (cond
           ((string= type "blob")
            (let ((url (gethash "download_url" contents))
                  (dir (gethash "path" contents)))
              (setq url-path-list (map-insert url-path-list url dir))))
           ((string= type "tree")
            (dolist (content contents)
              (setq type (gethash "type" content))
              (cond
               ((string= type "file")
                (let ((url (gethash "download_url" content))
                      (dir (gethash "path" content)))
                  (setq url-path-list (map-insert url-path-list url dir))))
               ((string= type "dir")
                (setq url-path-list
                      (append url-path-list
                              (imelpa--get-github-content-download-url-and-path (gethash "html_url" content)))))
               (t (error "Unsupported content type %s" type)))))
           (t (error "Unsupported type %s" type))))
      (error "Failed to contact %s" api))
    url-path-list))

(defun imelpa--get-commit-id-from-github (url)
  (let* ((github (imelpa--parse-github-url url))
         (owner  (plist-get github :owner))
         (repo   (plist-get github :repo))
         (type   (plist-get github :type))
         (branch (plist-get github :branch))
         (path   (plist-get github :path))
         (api    (format "https://api.github.com/repos/%s/%s/commits?sha=%s&path=%s&per_page=1"
                         owner repo (or branch "") (or path "")))
         (res    (imelpa--http-post api)))

    (if (eq 200 (plist-get res :status))
        (let* ((content (plist-get res :content))
               (commits (json-read-from-string content))
               (recent-commit (elt commits 0)))
          (alist-get 'sha recent-commit))
      (error "Failed to contact %s" api))))
