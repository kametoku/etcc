;; etcc.el --- an Emacs TwitCasting Client -*-coding:utf-8-*-

;; Copyright (C) 2018 Tokuya Kameshima

;; Author: Tokuya Kameshima <kametoku at gmail dot com>
;; Keywords: twitcasting, chat
;; Homepage: https://github.com/kametoku/etcc

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; ETCC is a major mode to view Twitcasting lives/movies and post comments.

;; Elisp Packages Requirement:
;; - request
;; - simple-httpd
;; - volatile-highlights (optional)

;; External Program Requirement:
;; - ffmpeg --- to download recorded movie file and HLS
;; - mplayer --- to play HLS

;; Configure:
;; 1. Register your App from the following developer page with the
;;    followng setting once:
;;    https://ssl.twitcasting.tv/developer.php
;;    - Name: any
;;    - Description: any
;;    - Callback URL: the same value as `etcc-auth-redirect-url'
;;      (default: http://localhost:12399/etcc-auth-callback)
;;    - Scope: "Read and Write" or "Read, Write and Broadcast"
;; 2. Note "ClientID" and "ClientSecret" from your App created.
;; 3. Configure ETCC with the above values:
;;    (setq etcc-auth-client-id "ClientID")
;;    (setq etcc-auth-state "ClientSecret")

;; Usage:
;; auth:
;; - type "M-x etcc-auth" to get authorization.
;;
;; view live/movie:
;; - type "M-x etcc-view-movie-from-url" followed by twitcasting
;;   live/movie URL to view movie form the URL.
;; - type "M-x etcc-view-live" followed by the user ID
;;   to view user's live.
;; - type "M-x etcc-view-movie" followed by the movie ID
;;   to view recorded movie.
;;
;; in ETCC buffer,
;; - type "C-c C-p" to start playing the movie.
;; - type "C-c C-i" to view the movie info.
;; - enter your comment after the prompt and type "C-c C-c" to post comment.
;; - type "C-c C-q" to quit viewing movie.

;; To debug api interaction with request:
;; (setq request-log-level 'debug request-log-buffer-name "*request-log*")

;;; Code:

(require 'cl-lib)
(require 'request)
(require 'simple-httpd)

(eval-when-compile (require 'cl))

(defgroup etcc nil
  "ETCC"
  :prefix "etcc-"
  :group 'etcc)

(defcustom etcc-mode-hook nil
  "Hook called in `etcc-mode'."
  :group 'etcc
  :type 'hook)

(defcustom etcc-display-movie-info-hook nil
  "Hook called in `etcc-display-movie-info'."
  :group 'etcc
  :type 'hook)

(defcustom etcc-display-user-info-hook nil
  "Hook called in `etcc-display-user-info'."
  :group 'etcc
  :type 'hook)

(defcustom etcc-auth-client-id nil
  ;; アプリケーションのClientID
  "Client ID provided by Twitcast.
Create your App from the following URL and set the value of \"ClientID\"
to `etcc-auth-client-id':
https://ssl.twitcasting.tv/developer.php"
  :group 'etcc
  :type 'string)

(defcustom etcc-auth-state nil
  ;; 任意のCSRFトークン
  "Client Secret provided by Twitcast.
Create your App from the following URL and set the value of \"ClientSecret\"
to `etcc-auth-state':
https://ssl.twitcasting.tv/developer.php"
  :group 'etcc
  :type 'string)

(defcustom etcc-auth-callback-port 12399
  "Port number of callback URL for Twitcast."
  :group 'etcc
  :type 'number)

(defcustom etcc-auth-redirect-url
  (format "http://localhost:%s/etcc-auth-callback"
          etcc-auth-callback-port)
  "Callback URL for Twitcast.")

(defcustom etcc-tc-base-url "https://apiv2.twitcasting.tv"
  "Twitcasting API base URL."
  :group 'etcc
  :type 'string)

(defcustom etcc-tc-auth-authorize-url
  "https://apiv2.twitcasting.tv/oauth2/authorize"
  "Twitcasting Authorization URL."
  :group 'etcc
  :type 'string)

(defcustom etcc-tc-auth-access-token-url
  "https://apiv2.twitcasting.tv/oauth2/access_token"
  "Twitcasting Authorization Access URL."
  :group 'etcc
  :type 'string)

(defcustom etcc-tc-movie-id-url-regexp
  ;; https://apiv2.twitcasting.tv/movies/:movie_id
  (concat (regexp-quote etcc-tc-base-url) "/movies/\\([0-9]+\\)")
  "A regexp which matches Twitcasting API endpoint URL with movie Id.
The first group matched represents the user's id."
  :group 'etcc
  :type 'string)

(defcustom etcc-comment-limit 50
  "Limit of comments per request.
Upto 50 is supported by Twitcasting API."
  :group 'etcc
  :type 'number)

(defcustom etcc-comment-offset-max 200
  "Max offset of comments."
  :group 'etcc
  :type 'number)

(defcustom etcc-auth-default-time-out 120
  "Timeout in seconds until giving up authorization."
  :group 'etcc
  :type 'number)

(defcustom etcc-browse-url-browser-function browse-url-browser-function
  "Function to display the url in a WWW browser.
The WWW browser is invoked for authorization and for playing HLS."
  :group 'etcc
  :type 'string)

(defcustom etcc-twitcasting-live-url-regexp
  "https?://\\(?:ssl\\.\\)?twitcasting.tv/\\([^/]*\\)\\(/\\(?:index\\)?\\)?$"
  "A regexp which matches Twitcasting live URL.
The first group matched represents the user's id."
  :group 'etcc
  :type 'string)

(defcustom etcc-twitcasting-movie-url-regexp
  "https?://\\(?:ssl\\.\\)?twitcasting.tv/\\([^/]+\\)/.*/\\([0-9]+\\)$"
  "A regexp which matches Twitcasting movie URL.
The first group matched represents the user's id.
The second group matched represents the movie id."
  :group 'etcc
  :type 'string)

(defcustom etcc-movie-url-format
  "http://dl01.twitcasting.tv/%s/download/%s?dl=1"
  "URL format string of recorded movie URL."
  :group 'etcc
  :type 'string)

(defcustom etcc-movie-info-buffer-name "*ETCC-movie-info*"
  "The buffer name to show the movie info."
  :group 'etcc
  :type 'string)

(defcustom etcc-user-info-buffer-name "*ETCC-user-info*"
  "The buffer name to show the user info."
  :group 'etcc
  :type 'string)

(defcustom etcc-prompt-function 'etcc-prompt
  "A function that returns the etcc comment prompt string."
  :group 'etcc
  :type 'function)

(defcustom etcc-mplayer-program "mplayer"
  "Program name of mplayer to play HLS."
  :group 'etcc
  :type 'string)

(defcustom etcc-download-hls-default-directory "~/Downloads"
  "Name of default directory to download HLS."
  :group 'etcc
  :type 'string)

(defcustom etcc-ffmpeg-program "ffmpeg"
  "Program name of ffmpeg to download HLS."
  :group 'etcc
  :type 'string)

(defcustom etcc-speed-commands
  `((" " . scroll-up-command)
    (,(kbd "<DEL>") . scroll-down-command)
    ("<" . etcc-goto-last-comment)
    (">" . etcc-goto-first-comment)
;;     ("<" . etcc-beginning-of-buffer)
;;     (">" . end-of-buffer)
    ("?" . describe-mode)
    ("@" . etcc-mention)
    ("I" . etcc-display-movie-info)
    ("i" . etcc-display-user-info-with-image)
    ("n" . etcc-next-comment)
    ("p" . etcc-previous-comment))
  "Alist of the key and command that is run if the position is read only.
Each element of alist is in (KEY . COMMAND) form.
In a read-only position, pressing KEY calls COMMAND interactively.
Otherwise, call the function globally bound to KEY.
KEY must be a single character string."
  :group 'etcc
  :type 'list)

(defvar etcc-token-type nil)
(defvar etcc-access-token nil)
(defvar etcc-auth-status nil) ; nil, 'waiting, 'succeeded, 'cancelled
(defvar etcc-expires-in nil)
(defvar etcc-auth-last-time nil)
(defvar etcc-current-user nil)
(defvar etcc-mode-map nil)

(defface etcc-my-comment-face
  '((((class color) (background light))
     (:background "beige"))
    (((class color) (background dark))
     (:background "pink2"))
    (t :inverse-video t))
  "Face to highlight my comments."
  :group 'etcc-mode)

(defface etcc-broadcaster-comment-face
  '((((class color) (background light))
     (:background "wheat1"))
    (((class color) (background dark))
     (:background "green"))
    (t :inverse-video t))
  "Face to highlight broadcaster's comments."
  :group 'etcc-mode)

(defface etcc-volatile-highlights-face
  '((((class color) (background light))
     (:background "misty rose"))
    (((class color) (background dark))
     (:background "SkyBlue4"))
    (t :inverse-video t))
  "Face used for volatile highlights."
  :group 'etcc-mode)

(defun etcc-alist-to-plist (alist)
  ;; (("id" . 123) ("user_id" . 456) (is_live . :json-false))
  ;; -> (:id 123 :user-id 456 :is-live nil)
  (let (plist)
    (while alist
      (let* ((elem (car alist))
             (key (intern (format ":%s" (replace-regexp-in-string
                                         "_" "-" (symbol-name (car elem))))))
             (value (cdr elem)))
        (if (eq value :json-false)
            (setq value nil))
        (setq plist (cons value (cons key plist))))
      (setq alist (cdr alist)))
    (nreverse plist)))

;;; Data Types

;; https://apiv2-doc.twitcasting.tv/

;; ユーザを表すオブジェクト
(cl-defstruct etcc-user
  id               ; string  ユーザID
  screen-id        ; string  id同様にユーザを特定する識別子ですが、
                   ;         screen_idはユーザによって変更される場合があります。
  name             ; string  ヒューマンリーダブルなユーザの名前
  image            ; string  ユーザアイコンのURL
  profile          ; string  プロフィール文章
  level            ; int     ユーザのレベル
  last-movie-id    ; string|null  ユーザが最後に配信したライブのID
  is-live          ; bool    現在ライブ配信中かどうか
  supporter-count  ; int     ユーザをサポートしている人数
  supporting-count ; int     ユーザがサポートしている人数
  created)         ; int     ユーザ作成日時のunixタイムスタンプ

(defun make-etcc-user-from-alist (alist)
  (let ((plist (etcc-alist-to-plist alist)))
    (apply 'make-etcc-user plist)))

;;ライブ(録画)を表すオブジェクト
(cl-defstruct etcc-movie
  id                 ; string  ライブID
  user-id            ; string  ライブ配信者のユーザID
  title              ; string  タイトル
  subtitle           ; string|null  テロップ
  last-owner-comment ; string|null  ライブ配信者の最新コメントの文章
  category           ; string|null  カテゴリID
  link               ; string  ライブ(録画)へのリンクURL
  is-live            ; bool    ライブ配信中かどうか
  is-recorded        ; bool    録画が公開されているかどうか
  comment-count      ; int     総コメント数
  large-thumbnail    ; string  サムネイル画像(大)のURL
  small-thumbnail    ; string  サムネイル画像(小)のURL
  country            ; string  配信地域(国コード)
  duration           ; int     配信時間(秒)
  created            ; int     配信開始日時のunixタイムスタンプ
  is-collabo         ; bool    コラボ配信かどうか
  is-protected       ; bool    合言葉配信かどうか
  max-view-count     ; int     最大同時視聴数
  current-view-count ; int     現在の同時視聴者数(配信中ではない場合0)
  total-view-count   ; int     総視聴者数
  hls-url)           ; string  HTTP Live Streaming再生用のURL

(defun make-etcc-movie-from-alist (alist)
  (let ((plist (etcc-alist-to-plist alist)))
    (apply 'make-etcc-movie plist)))

;; コメントを表すオブジェクト
(cl-defstruct etcc-comment
  id             ; string  コメントID
  message        ; string  コメント本文
  from-user      ; object  コメント投稿者の情報 Userオブジェクト
  created)       ; int     コメント投稿日時のunixタイムスタンプ

(defun make-etcc-comment-from-alist (alist)
  (let* ((plist (etcc-alist-to-plist alist))
         (from-user (plist-get plist :from-user)))
    (if from-user
        (let ((etcc-from-user (make-etcc-user-from-alist from-user)))
          (setq plist (plist-put plist :from-user etcc-from-user))))
    (apply 'make-etcc-comment plist)))

;; サポーターユーザを表すオブジェクト
;; (point, total_pointを除いてUserオブジェクトと同じです)
(cl-defstruct etcc-supporter-user
  id               ; string  ユーザID
  screen-id        ; string  id同様にユーザを特定する識別子ですが、
                   ;         screen_idはユーザによって変更される場合があります。
  name	           ; string  ヒューマンリーダブルなユーザの名前
  image	           ; string  ユーザアイコンのURL
  profile	   ; string  プロフィール文章
  last-movie-id	   ; string|null  ユーザが最後に配信したライブのID
  is-live	   ; bool    現在ライブ配信中かどうか
  supporter-count  ; int     ユーザをサポートしている人数
  supporting-count ; int     ユーザがサポートしている人数
  created          ; int     ユーザ作成日時のunixタイムスタンプ
  point            ; int     アイテム・スコア
  total-point)     ; int     累計スコア

(defun make-etcc-supporter-user-from-alist (alist)
  (let* ((plist (etcc-alist-to-plist alist)))
    (apply 'make-etcc-supporter-user plist)))

;; ;; 配信カテゴリを表すオブジェクト
;; (cl-defstruct etcc-category
;;   id                          ; string  カテゴリID
;;   name                        ; string  カテゴリ名
;;   sub-categories)             ; array   Sub categoryオブジェクトの配列

;; (defun make-etcc-category-from-alist (alist)
;;   (let* ((plist (etcc-alist-to-plist alist))
;;          (sub-categories (plist-get plist :sub-categories)))
;;     (if sub-categories
;; ;;         (let ((etcc-from-user (make-etcc-user-from-alist from-user)))
;; ;;           (setq plist (plist-put plist :from-user etcc-from-user)))
;;         )
;;     (apply 'make-etcc-category plist)))

;; ;; 配信サブカテゴリを表すオブジェクト
;; (cl-defstruct etcc-sub-category
;;   id                                    ; string  サブカテゴリID
;;   name                                  ; string  サブカテゴリ名
;;   count)                                ; int     サブカテゴリ配信数

;;; basic functions

(defun etcc-auth-callback (code state)
  (setq data `(("code" . ,code) ;; etcc-auth-code
               ("grant_type" . "authorization_code")
               ("client_id" . ,etcc-auth-client-id)
               ("redirect_uri" . ,etcc-auth-redirect-url)))
  (if state
      (add-to-list 'data
                   `("client_secret" . ,state))) ;; etcc-auth-state
  (request etcc-tc-auth-access-token-url
           ;; "https://apiv2.twitcasting.tv/oauth2/access_token"
           :type "POST"
           :data `(("code" . ,code) ;; etcc-auth-code
                   ("grant_type" . "authorization_code")
                   ("client_id" . ,etcc-auth-client-id)
                   ("client_secret" . ,state) ;; etcc-auth-state
                   ("redirect_uri" . ,etcc-auth-redirect-url))
           :parser 'json-read
           :success 'etcc-auth-callback-success
           :error 'etcc-auth-callback-error))

(cl-defun etcc-auth-callback-success (&key data &allow-other-keys)
  (setq etcc-token-type (assoc-default 'token_type data)) ; should be "bearer"
  (setq etcc-access-token (assoc-default 'access_token data))
  (setq etcc-expires-in (assoc-default 'expires_in data))
  (setq etcc-auth-last-time (current-time))
  (setq etcc-auth-status 'succeeded)
  (httpd-stop)
  (etcc-auth-verify-credentials)
  (message "ETCC: auth succeeded!"))

(cl-defun etcc-auth-callback-error (&rest args &key error-thrown data
                                          &allow-other-keys)
  ;; {"error":{"code":1000,"message":"Invalid token"}}
  (let* ((err (assoc-default 'error data))
         (code (assoc-default 'code err))
         (message (assoc-default 'message err))
         (details (assoc-default 'details err)))
    (httpd-stop)
    (setq etcc-auth-status nil)
    (let ((prompt
           (format "ETCC: authentication failed (%S: %s (code=%s; details=%s)). Retry? "
                   error-thrown message code details)))
      (if (y-or-n-p prompt)
          (etcc-auth)))))

(defservlet* etcc-auth-callback text/plain (code state result)
  (if result
      (progn
        (insert "authentication process cancelled by yourself.")
        (message "ETCC: authentication process cancelled by yourself")
        (setq etcc-auth-status 'cancelled))
    (etcc-auth-callback code state)
    (insert "OK!")))

(defun etcc-browse-url (url &optional args)
  (let ((browse-url-browser-function etcc-browse-url-browser-function))
    (browse-url url args)))

(defun etcc-auth-request (&optional args)
  (interactive "P")
  (when (or (not (eq etcc-auth-status 'waiting))
            (y-or-n-p "Waiting for auth. Will you try another one?"))
    (let ((httpd-port etcc-auth-callback-port)
          (httpd-listings nil))
      (httpd-start))
    (let ((url (format "%s?client_id=%s&response_type=code"
                       etcc-tc-auth-authorize-url
                       etcc-auth-client-id)))
      (if etcc-auth-state
          (setq url (format "%s&state=%s" url etcc-auth-state)))
      (etcc-browse-url url))
    (setq etcc-auth-status 'waiting)))

(defun etcc-auth-kill ()
  "Stop authentication process."
  (interactive)
  (if (eq etcc-auth-status 'waiting)
      (progn
        (httpd-stop)
        (setq etcc-auth-status nil))
    (message "ETCC: no auth is running")))

(defun etcc-auth-wait (&optional time-out)
  "Wait for authentication process to complete."
  (if (not etcc-auth-status)
      (error "ETCC: auth request not submitted."))
  (or time-out (setq time-out etcc-auth-default-time-out))
  (with-timeout (time-out
                 (etcc-auth-kill)
                 (error "ETCC: auth timeout"))
    (message "ETCC: waiting for auth...")
    (condition-case err
        (while (eq etcc-auth-status 'waiting)
          (sleep-for 0.2))
      ((error quit)
       (etcc-auth-kill)
       (error "%s" (error-message-string err))))
    (when (eq etcc-auth-status 'cancelled)
      (setq etcc-auth-status nil)
      (error "ETCC: auth cancelled!"))
    etcc-auth-status))

(defun etcc-auth (&optional force)
  "Wait the authorization code granted."
  (interactive "P")
  (if (or (memq etcc-auth-status '(nil cancelled))
          (and (eq etcc-auth-status 'succeeded) (null etcc-access-token))
          force)
      (etcc-auth-request))
  (if (eq etcc-auth-status 'waiting)
      (etcc-auth-wait)))



(defmacro etcc-request-callback (args &rest body)
  `(cl-function
    (lambda (&key ,@args &allow-other-keys)
      ,@body)))
(put 'etcc-request-callback 'lisp-indent-function 1)

(cl-defun etcc-request (endpoint ;;&rest settings
                                 &key
                                 (type "GET")
                                 (params nil)
                                 (data nil)
                                 (parser 'json-read)
                                 (headers nil)
                                 (success nil)
                                 (error nil)
                                 (status-code nil))
  (etcc-auth)
  (let ((url (format "%s%s" etcc-tc-base-url endpoint)))
    (add-to-list 'headers '("X-Api-Version" . "2.0"))
    (add-to-list 'headers `("Authorization" . ,(format "Bearer %s"
                                                       etcc-access-token)))
    (unless success
      (setq success 'etcc-request-success))
    (unless error
      (setq error 'etcc-request-error))
    (unless (assoc 401 status-code)
      (add-to-list 'status-code '(401 . etcc-request-error-401)))
    (request url
             :type type
             :headers headers
             :params params
             :data data
             :parser parser
             :success success
             :error error
             :status-code status-code)))

(cl-defun etcc-request-success (&rest args &key data response &allow-other-keys)
  "Default callback on request success."
  (let* ((url (request-response-url response)))
    (message "%s: success: DATA: %s" url data)))

(cl-defun etcc-request-error (&rest args &key error-thrown data response
                                    &allow-other-keys)
  "Default error handler of response of `etcc-request'."
  (let* ((err (assoc-default 'error data))
         (code (assoc-default 'code err))
         (message (assoc-default 'message err))
         (details (assoc-default 'details err))
         (url (request-response-url response)))
    (if data
        (let ((msg (format "%s error (%s) from '%s' - %s: %s"
                           message code url
                           (car error-thrown) (cdr error-thrown))))
          (if details
              (setq msg (concat msg " - details: " details)))
          (message msg))
      (message "%s: %s: request: %s"
               (car error-thrown) (cdr error-thrown) url))))

(cl-defun etcc-request-error-401 (&rest args &key data
                                        &allow-other-keys)
  "Error handler for 401 Unauthorized error."
  (let* ((err (cdr (assoc 'error data)))
         (code (cdr (assoc 'code err))))
    (if (= code 1000)
        (progn
          (message "invalid token. try again after authentication succeeds...")
          (etcc-auth t)))))


(defun etcc/movie-id-from-url (url)
  "Return the movie ID as a string from an twitcasting api endpoint URL.
If URL does not match any of api endpoint URL, return nil."
  (save-match-data
    (if (string-match etcc-tc-movie-id-url-regexp url)
        (match-string 1 url))))

(cl-defun etcc-api/get-user-info (user-id &key (success nil) (error nil))
  "Get a user info.
ユーザ情報を取得する。"
  (etcc-request (format "/users/%s" user-id) :success success :error error))

(cl-defun etcc-api/verify-credentials (&key (success nil) (error nil))
  "Verify the credentials.
アクセストークンを検証し、ユーザ情報を取得する。"
  (etcc-request "/verify_credentials" :success success :error error))

(cl-defun etcc-api/get-live-thumbnail-image (user-id
                                             &key
                                             (size "small") ; "large" or "small"
                                             (position "latest") ; "beginning" or "latest"
                                             (success nil)
                                             (error nil))
  "Get the live thumbnail image.
配信中のライブのサムネイル画像を取得する。

Note: USER-ID could be a user ID or a screen ID."
  (let ((params `(("size" . ,size)
                  ("position" . ,position))))
    (etcc-request (format "/users/%s/live/thumbnail" user-id)
                  :params params
                  :parser 'buffer-string
                  :success success :error error)))

(cl-defun etcc-api/get-movie-info (movie-id &key (success nil) (error nil))
  "Get the movie info.
ライブ(録画)情報を取得する。"
  (etcc-request (format "/movies/%s" movie-id) :success success :error error))

(cl-defun etcc-api/get-movies-by-user (user-id &key
                                               (offset 0)
                                               (limit 20)
                                               (success nil) (error nil))
  "Get movies by user of USER-ID.
ユーザーが保有する過去ライブ(録画)の一覧を作成日時の降順で取得する。"
  (let ((params `(("offset" . ,offset)
                  ("limit . ,limit"))))
    (etcc-request (format "/users/%s/movies" user-id)
                  :params params
                  :success success :error error)))

(cl-defun etcc-api/get-current-live (user-id &key (success nil) (error nil)
                                             (status-code nil))
  "Get current live of USER-ID.
ユーザーが配信中の場合、ライブ情報を取得する。"
  (etcc-request (format "/users/%s/current_live" user-id)
                :success success :error error :status-code status-code))

(cl-defun etcc-api/get-comments (movie-id &key
                                          (offset 0)
                                          (limit 10)
                                          (slice-id nil) ;"none"
                                          (success nil) (error nil))
  "Get comments of MOVIE-ID.
コメントを作成日時の降順で取得する。"
  (let ((params `(("offset" . ,offset)
                  ("limit" . ,limit))))
    (if slice-id
        (add-to-list 'params `("slice_id" . ,slice-id)))
    (etcc-request (format "/movies/%s/comments" movie-id)
                  :params params
                  :success success :error error)))

(defun etcc/json-encode (object)
  "Encode OBJECT as json format with converting it to UTF-8 encoding."
  (encode-coding-string (json-encode object) 'utf-8))

(cl-defun etcc-api/post-comment (movie-id comment
                                          &key
                                          (sns "none") ;"reply", "normal" or "none"
                                          (success nil) (error nil))
  "Post comment COMMENT to MOVIE-ID.
コメントを投稿する。 ユーザ単位でのみ実行可能です。"
  (let ((data (etcc/json-encode `(("comment" . ,comment)
                                  ("sns" . ,sns)))))
    (etcc-request (format "/movies/%s/comments" movie-id)
                  :type "POST"
                  :data data
                  :headers '(("Content-Type" . "application/json"))
                  :success success :error error)))

(cl-defun etcc-api/delete-comment (movie-id comment-id
                                            &key (success nil) (error nil))
  "Delete comment COMMEIT-ID of MOVIE-ID.

コメントを削除する。ユーザ単位でのみ実行可能です。
なお、原則として削除できるコメントは、
投稿者がアクセストークンに紐づくユーザと同一のものに限られます。
ただし、Movieのオーナーであるユーザーのアクセストークンを用いる場合は
他ユーザが投稿したコメントを削除することが出来ます。"
  (let ((params `(("comment" . ,comment)
                  ("sns" . ,sns))))
    (etcc-request (format "/movies/%s/comments/%s" movie-id comment-id)
                  :type "DELETE"
                  :params params
                  :success success :error error)))

;;;;

(cl-defun etcc-api/get-supporting-status (user-id target-user-id
                                                  &key
                                                  (success nil)
                                                  (error nil))
  "Get supporting status.
ユーザーが、ある別のユーザのサポーターであるかの状態を取得する。"
  (let ((params `(("target_user_id" . ,target-user-id))))
    (etcc-request (format "/users/%s/supporting_status" user-id)
                  :params params
                  :success success :error error)))

(cl-defun etcc-api/support-user (target-user-ids &key
                                                 (success nil) (error nil))
  "Support user.
指定したユーザーのサポーターになる"
  (let ((data `(("target_user_ids" . ,target-user-ids)))) ; XXX
    (etcc-request "/support"
                  :type "PUT"
                  :data data
                  :success success :error error)))

(cl-defun etcc-api/supporting-list (user-id &key
                                            (offset 0)
                                            (limit 20)
                                            (success nil) (error nil))
  "Supporting list.
指定したユーザーがサポートしているユーザーの一覧を取得する"
  (let ((params `(("offset" . ,offset)
                  ("limit" . ,limit))))
    (etcc-request (format "/users/%s/supporting" user-id)
                  :params params
                  :success success :error error)))

(cl-defun etcc-api/supporter-list (user-id &key
                                            (offset 0)
                                            (limit 20)   ; min:1, max:20
                                            (sort "new") ; "new" or "ranking"
                                            (success nil) (error nil))
  "Supporter List
指定したユーザーをサポートしているユーザーの一覧を取得する。"
  (let ((params `(("offset" . ,offset)
                  ("limit" . ,limit)
                  ("sort" . ,sort))))
    (etcc-request (format "/users/%s/supporters" user-id)
                  :params params
                  :success success :error error)))

;;;

(cl-defun etcc-api/search-users (words &key
                                       (limit 10)
                                       (lang "ja")
                                       (success nil) (error nil))
  (let ((params `(("words" . ,words)
                  ("limit" . ,limit)
                  ("lang" . ,lang))))
    (etcc-request "/search/users"
                  :params params
                  :success success :error error)))

(cl-defun etcc-api/search-live-movies (&key type context
                                            (limit 10)
                                            (lang "ja")
                                            (success nil) (error nil))
  (let ((params `(("type" . ,type)
                  ("context" . ,context)
                  ("limit" . ,limit)
                  ("lang" . ,lang))))
    (etcc-request "/search/users"
                  :params params
                  :success success :error error)))



;;; etcc mode for viewing twitcasting live/movie

(unless etcc-mode-map
  (setq etcc-mode-map (make-sparse-keymap))
  (define-key etcc-mode-map "\C-a" 'etcc-bol)
  (define-key etcc-mode-map "\C-c\C-b" 'etcc-display-download-buffer)
  (define-key etcc-mode-map "\C-c\C-c" 'etcc-post-comment)
  (define-key etcc-mode-map "\C-c\C-d" 'etcc-download)
  (define-key etcc-mode-map "\C-c\C-i" 'etcc-display-movie-info)
  (define-key etcc-mode-map "\C-c\C-j" 'etcc-goto-last-comment)
  (define-key etcc-mode-map "\C-c\C-k" 'etcc-kill-comment)
  (define-key etcc-mode-map "\C-c\C-l" 'etcc-reload)
  (define-key etcc-mode-map "\C-c\C-o" 'etcc-open-link)
  (define-key etcc-mode-map "\C-c\C-p" 'etcc-play-hls)
  (define-key etcc-mode-map "\C-c\C-q" 'etcc-quit)
  (define-key etcc-mode-map "\C-c\C-r" 'etcc-restart-updater)
  (define-key etcc-mode-map "\C-c\C-s" 'etcc-start-updater)
  (define-key etcc-mode-map "\C-c\C-t" 'etcc-browse-thumbnail)
  (define-key etcc-mode-map "\C-c\C-u" 'etcc-kill-comment)
  (define-key etcc-mode-map "\C-c\C-x" 'etcc-stop-download)
  (define-key etcc-mode-map "\C-c@" 'etcc-mention)
  (define-key etcc-mode-map "\C-c?" 'describe-mode)
  (define-key etcc-mode-map "\C-c<" 'etcc-goto-last-comment)
  (define-key etcc-mode-map "\C-c>" 'etcc-goto-first-comment)
  (define-key etcc-mode-map "\C-m" 'etcc-post-comment)
  (define-key etcc-mode-map (kbd "<S-RET>") 'newline)
  (define-key etcc-mode-map (kbd "<M-RET>") 'etcc-post-comment)
  (define-key etcc-mode-map "\M-<" 'etcc-beginning-of-buffer)
  (define-key etcc-mode-map "\C-\M-n" 'etcc-next-comment)
  (define-key etcc-mode-map "\C-\M-p" 'etcc-previous-comment)
  ;; bind keys from " "(32) to "<DEL>"(127) to `etcc-speed-command'
  (let ((char 32))
    (while (< char 128)
      (let ((key (make-string 1 char)))
        (define-key etcc-mode-map key 'etcc-speed-command))
      (setq char (1+ char)))))

(defun etcc-mode ()
  "Major mode for Emacs Twitcasting Client

\\{etcc-mode-map}"
  (interactive)
  (make-local-variable 'etcc-user-id)
  (make-local-variable 'etcc-user-screen-id)
  (make-local-variable 'etcc-user-name)
  (make-local-variable 'etcc-user-image)
  (make-local-variable 'etcc-user-profile)
  (set (make-local-variable 'etcc-movie-is-live) nil)
  (set (make-local-variable 'etcc-movie) nil)
  (set (make-local-variable 'etcc-broadcaster) nil)
  (set (make-local-variable 'etcc-tags) nil)
  (set (make-local-variable 'etcc-comment-all-count) 0)
  (set (make-local-variable 'etcc-last-comment-id) 0)
  (set (make-local-variable 'etcc-comment-offset) 0)
  (set (make-local-variable 'etcc/comment-updater-running) nil)
  (set (make-local-variable 'etcc/comment-updater-status) nil)
  (set (make-local-variable 'etcc/movie-info-updater-running) nil)
  (set (make-local-variable 'etcc/movie-info-updater-status) nil)
  (set (make-local-variable 'etcc-play-hls-proc) nil)
  (set (make-local-variable 'etcc-download-hls-proc) nil)
  (set (make-local-variable 'etcc-download-movie-proc) nil)
  (set (make-local-variable 'etcc-prompt-end) (make-marker))
  (set (make-local-variable 'etcc-comment-region-beg) (make-marker))
  (set (make-local-variable 'etcc-comment-region-end) (make-marker))
  (setq major-mode 'etcc-mode)
  (setq mode-name "ETCC")
  (make-local-variable 'kill-buffer-hook)
  (use-local-map etcc-mode-map)
  (run-hooks 'etcc-mode-hook))

(defun etcc/search-buffer (regexp)
  "Return the fist buffer whose name matches REGEXP."
  (catch 'found
    (mapc (lambda (buf)
            (if (string-match regexp (buffer-name buf))
                (throw 'found buf)))
          (buffer-list))
    nil))

(defun etcc/buffer-name (movie)
  "Return the buffer name of MOVIE."
  (format "*ETCC:%s:%s*" (etcc-movie-id movie) (etcc-movie-title movie)))

(defun etcc/buffer-regexp (movie-id)
  "Return a regexp to match bufer name of MOVIE-ID."
  (concat "^" (regexp-quote (format "*ETCC:%s:" movie-id))))

(defun etcc/buffer (movie-or-movie-id &optional create-p)
  (if (etcc-movie-p movie-or-movie-id)
      (let ((bufname (etcc/buffer-name movie-or-movie-id)))
        (if create-p
            (get-buffer-create bufname)
          (get-buffer bufname)))
    (let ((buf (etcc/search-buffer (etcc/buffer-regexp movie-or-movie-id))))
      (or buf
          (error "No buffer found associated with movie id %s" movie-or-movie-id)))))

(defmacro with-etcc-buffer (movie-or-movie-id &rest body)
  (declare (indent 1) (debug t))
  `(save-current-buffer
     (set-buffer (etcc/buffer ,movie-or-movie-id))
     ,@body))

(defmacro ensure-etcc-buffer (&rest body)
  (declare (indent 0) (debug t))
  `(if (eq major-mode 'etcc-mode)
       (progn ,@body)
    (error "not an ETCC buffer")))

(defmacro etcc-message (format-string &rest args)
  "Display a message only when the containing function was called interactively."
  (declare (indent 0) (debug t))
  `(if (called-interactively-p 'interactive)
       (message ,format-string ,@args)))

(cl-defun etcc-auth-verify-credentials-sentinel (&key data &allow-other-keys)
  (let ((app (assoc-default 'app data))
        (user (assoc-default 'user data)))
    (setq etcc-current-user (make-etcc-user-from-alist user))))

(defun etcc-auth-verify-credentials ()
  (etcc-api/verify-credentials :success 'etcc-auth-verify-credentials-sentinel))

(defun etcc/fontify-string (string face &optional read-only &rest args)
  (add-text-properties 0 (length string) (list 'face face) string)
  (if read-only
      (add-text-properties 0 (length string)
                           '(read-only t
                             front-sticky read-only
                             rear-nonsticky read-only)
                           string))
  (if args
      (add-text-properties 0 (length string) args string))
  string)

(defun etcc/user-string (etcc-user)
  "Return user string of ETCC-USER with fontified."
  (concat 
;;    (etcc/fontify-string (concat (etcc-user-name etcc-user) "")
   (etcc/fontify-string (copy-sequence (etcc-user-name etcc-user))
                        font-lock-function-name-face nil 'etcc-user etcc-user)
   (etcc/fontify-string (concat "@" (etcc-user-screen-id etcc-user))
                        font-lock-constant-face nil 'etcc-user etcc-user)))

(defun etcc/insert-comment (etcc-comment)
  (let* ((id (etcc-comment-id etcc-comment))
         (message (etcc-comment-message etcc-comment))
         (user (etcc-comment-from-user etcc-comment))
         (created (etcc-comment-created etcc-comment))
         (beg (point)))
    (let ((inhibit-read-only t))
      (insert
       (etcc/fontify-string
        (format-time-string "%H:%M:%S " (etcc-comment-created etcc-comment))
        font-lock-variable-name-face)
       (etcc/user-string user)
       "\n         " (replace-regexp-in-string "\\\\n" "\n         " message)
       "\n")
      (add-text-properties beg (1+ beg)
                           `(etcc-comment-id ,(string-to-number id)
                             etcc-comment-begin t))
      (cond ((and (boundp 'etcc-current-user)
                  (equal (etcc-user-id user)
                         (etcc-user-id etcc-current-user)))
             (add-face-text-property beg (point)
                                     'etcc-my-comment-face t))
            ((and (boundp 'etcc-broadcaster)
                  (equal (etcc-user-id user)
                         (etcc-user-id etcc-broadcaster)))
             (add-face-text-property beg (point)
                                     'etcc-broadcaster-comment-face t)))
      (add-text-properties beg (point)
                           `(read-only t
                             etcc-comment ,etcc-comment
                             front-sticky read-only
                             rear-nonsticky read-only)))))

(defun etcc/insert-comments (comments)
  "Insert COMMENTS into the current buffer.
If the volatile-highlights package is installed, the inserted
comments will be highlighted for a while."
  (if (fboundp 'vhl/clear-all)
      (vhl/clear-all))
  (let ((buffer-undo-list t))
    (mapc (lambda (comment)
            (let* ((etcc-comment (if (etcc-comment-p comment)
                                     comment
                                   (make-etcc-comment-from-alist comment)))
                   (comment-id (string-to-number
                                (etcc-comment-id etcc-comment))))
              (let ((beg (point)))
                (etcc/insert-comment etcc-comment)
                (if (and (boundp 'etcc-comment-offset)
                         (not (numberp etcc-comment-offset))
                         (fboundp 'vhl/add-range))
                    (vhl/add-range beg (point) nil
                                   'etcc-volatile-highlights-face)))
              (if (boundp 'etcc-last-comment-id)
                  (setq etcc-last-comment-id
                        (max etcc-last-comment-id comment-id)))))
          comments)))

(defun etcc/insert-live-comments (comments)
  "Insert live comments COMMENTS into the current ETCC buffer.
The comments are inserted at the beginning of comment region."
  (ensure-etcc-buffer
    (goto-char etcc-comment-region-beg)
    (etcc/insert-comments comments)))

(defun etcc/get-next-comment-offset (comments)
  "Return the offset for the next comments to get after the current COMMENTS.
If COMMENTS are empty, return nil, which means no more comments to get.
If the current comment offset exceeds `etcc-comment-offset-max', return t,
which means there exist more comments but reached max offset.
Otherwise, return the next offset incresing current offset by
`etcc-comment-limit'."
  (cond ((= (length comments) 0) nil)   ; no more comments
        ((and (numberp etcc-comment-offset-max)
              (>= etcc-comment-offset etcc-comment-offset-max))
         t)                     ; more comments, but reached max offset
        (t (+ etcc-comment-offset etcc-comment-limit))))

(defun etcc/insert-past-comments (comments)
  "Insert past comments COMMENTS into the current ETCC buffer."
  (ensure-etcc-buffer
    (goto-char etcc-comment-region-end)
    (previous-line)
    (etcc/insert-comments comments)
    (let ((buffer-undo-list t)
          (next-comment-offset (etcc/get-next-comment-offset comments)))
      (cond ((numberp next-comment-offset)
             (message "ETCC: getting more comments..."))
            ((null next-comment-offset)
             (message "ETCC: all comments received."))
            (t
             (insert (etcc/fontify-string "...more comments..."
                                          font-lock-warning-face t))
             (message "ETCC: comment max offset reached."))))))

(cl-defun etcc/comment-updater-sentinel (&key data &allow-other-keys)
  "Callback for comment updater."
  (let* ((movie-id (assoc-default 'movie_id data))
         (all-count (assoc-default 'all_count data))
         (comments (assoc-default 'comments data)))
    (with-etcc-buffer movie-id
      (save-excursion
        (save-restriction
          (widen)
          ;; insert comments
          (let ((buffer-modified-p (buffer-modified-p)))
            (if (numberp etcc-comment-offset)
                (progn
                  (etcc/insert-past-comments comments)
                  (setq etcc-comment-offset
                        (etcc/get-next-comment-offset comments)))
              (etcc/insert-live-comments comments))
            (set-buffer-modified-p buffer-modified-p))))
      (setq etcc-comment-all-count all-count)
      (setq etcc/comment-updater-status 'fetched)
      (cond ((numberp etcc-comment-offset)
             (etcc/comment-updater-next-offset movie-id etcc-comment-offset))
            (etcc-movie-is-live
             (setq etcc/comment-updater-running
                   (run-with-timer 2 nil
                                   'etcc/comment-updater-next
                                   movie-id etcc-last-comment-id)))
            (t
             (etcc/comment-updater-stop (etcc-movie-id etcc-movie)))))))

(cl-defun etcc/comment-updater-sentinel-error (&rest args &key error-thrown
                                                     data response
                                                     &allow-other-keys)
  "Error callback for comment updater."
  (etcc-request-error :error-thrown error-thrown :data data
                      :response response)
  (let* ((err (assoc-default 'error data))
         (code (assoc-default 'code err))
         (message (assoc-default 'message err))
         (details (assoc-default 'details err))
         (url (request-response-url response))
         (movie-id (etcc/movie-id-from-url url)))
    (etcc/comment-updater-stop movie-id)))

(defun etcc/comment-updater-start (movie-id)
  "Start comment updater for MOVIE-ID."
  (with-etcc-buffer movie-id
    (or etcc/comment-updater-running
        (setq etcc/comment-updater-running t))
    (if (numberp etcc-comment-offset)
        (etcc/comment-updater-next-offset movie-id etcc-comment-offset)
      (etcc/comment-updater-next movie-id etcc-last-comment-id))))

(defun etcc/comment-updater-next-offset (movie-id offset)
  (with-etcc-buffer movie-id
    (if etcc/comment-updater-running
        (setq etcc/comment-updater-running
              (etcc-api/get-comments
               movie-id
               :offset offset
               :limit etcc-comment-limit
               :success 'etcc/comment-updater-sentinel
               :error 'etcc/comment-updater-sentinel-error)
              etcc/comment-updater-status 'waiting)
      (setq etcc/comment-updater-status nil))))

(defun etcc/comment-updater-next (movie-id slice-id)
;;   (condition-case nil
  (with-etcc-buffer movie-id
    (if etcc/comment-updater-running
        (setq etcc/comment-updater-running
              (etcc-api/get-comments
               movie-id
               :slice-id slice-id
               :limit etcc-comment-limit
               :success 'etcc/comment-updater-sentinel
               :error 'etcc/comment-updater-sentinel-error)
              etcc/comment-updater-status 'waiting)
      (setq etcc/comment-updater-status nil))))
;;     (error (setq etcc/comment-updater-status nil)
;;            (message "error in etcc/comment-updater-next."))))

(defun etcc/comment-updater-stop (movie-id)
  (with-etcc-buffer movie-id
    (cond ((request-response-p etcc/comment-updater-running)
           (request-abort etcc/comment-updater-running))
          ((timerp etcc/comment-updater-running)
           (cancel-timer etcc/comment-updater-running)))
    (setq etcc/comment-updater-running nil)
    (setq etcc/comment-updater-status nil)))

(defun etcc-stop-comment-updater (&optional no-prompt)
  "Stop the comment updater process of the current buffer's movie."
  (interactive "P")
  (ensure-etcc-buffer
    (cond ((not etcc/comment-updater-running)
           (etcc-message "comment updater not running."))
          ((or no-prompt
               (y-or-n-p "Do you stop subscribing comments? "))
           (etcc/comment-updater-stop etcc-movie)))))

(defun etcc-start-comment-updater (&optional force)
  "Start the comment updater process of the current buffer's movie."
  (interactive "P")
  (ensure-etcc-buffer
    (if (or (not etcc/comment-updater-running) ; explicitly stopped
            (and force
                 (prog1 t
                   (etcc/comment-updater-stop (etcc-movie-id etcc-movie)))))
        (etcc/comment-updater-start (etcc-movie-id etcc-movie))
      (message "ETCC: comment update already started"))))


;;;;;;;;

(defun etcc/update-movie-info (movie broadcaster tags)
  (let ((movie-id (etcc-movie-id movie)))
    ;; find the etcc buffer by movie-id
    (with-etcc-buffer movie-id
      (setq etcc-movie movie)
      (setq etcc-movie-is-live (etcc-movie-is-live etcc-movie))
      (setq etcc-broadcaster broadcaster)
      (setq etcc-tags tags)
      (let ((bufname (etcc/buffer-name movie)))
        (unless (string= (buffer-name) bufname)
          (rename-buffer bufname))))))

(cl-defun etcc/movie-info-updater-sentinel (&key data &allow-other-keys)
  (let* ((movie (assoc-default 'movie data))
         (broadcaster (assoc-default 'broadcaster data))
         (tags (assoc-default 'tags data)))
    (let* ((etcc-movie (make-etcc-movie-from-alist movie))
           (etcc-user (make-etcc-user-from-alist broadcaster))
           (movie-id (etcc-movie-id etcc-movie)))
      (etcc/update-movie-info etcc-movie etcc-user tags)
      (setq etcc/movie-info-updater-status 'fetched)
      (if (etcc-movie-is-live etcc-movie)
          (setq etcc/movie-info-updater-running
                (run-with-timer 5 nil
                                'etcc/movie-info-updater-next
                                movie-id))
        (etcc/movie-info-updater-stop (etcc-movie-id etcc-movie))))))

(cl-defun etcc/movie-info-updater-sentinel-error (&rest args &key error-thrown
                                                        data response
                                                        &allow-other-keys)
  (etcc-request-error :error-thrown error-thrown :data data
                      :response response)
  (let* ((err (assoc-default 'error data))
         (code (assoc-default 'code err))
         (message (assoc-default 'message err))
         (details (assoc-default 'details err))
         (url (request-response-url response))
         (movie-id (etcc/movie-id-from-url url)))
    (etcc/movie-info-updater-stop movie-id)))

(defun etcc/movie-info-updater-next (movie-id)
  (with-etcc-buffer movie-id
    (if etcc/movie-info-updater-running
        (setq etcc/movie-info-updater-running
              (etcc-api/get-movie-info
               movie-id
               :success 'etcc/movie-info-updater-sentinel
               :error 'etcc/movie-info-updater-sentinel-error)
              etcc/movie-info-updater-status 'waiting)
      (setq etcc/movie-info-updater-status nil))))

(defun etcc/movie-info-updater-start (movie-id)
  (with-etcc-buffer movie-id
    (or etcc/movie-info-updater-running
        (setq etcc/movie-info-updater-running t))
    (etcc/movie-info-updater-next movie-id)))

(defun etcc/movie-info-updater-stop (movie-id)
  (with-etcc-buffer movie-id
    (cond ((request-response-p etcc/movie-info-updater-running)
           (request-abort etcc/movie-info-updater-running))
          ((timerp etcc/movie-info-updater-running)
           (cancel-timer etcc/movie-info-updater-running)))
    (setq etcc/movie-info-updater-running nil)
    (setq etcc/movie-info-updater-status nil)))

(defun etcc-stop-movie-info-updater (&optional no-prompt)
  "Stop the movie info updater process of the current buffer's movie."
  (interactive "P")
  (ensure-etcc-buffer
    (cond ((not etcc/movie-info-updater-running)
           (etcc-message "comment updater not running."))
          ((or no-prompt
               (y-or-n-p "Do you stop movie info update? "))
           (etcc/movie-info-updater-stop etcc-movie)))))

(defun etcc-start-movie-info-updater (&optional force)
  "Start the movie info updater process of the current buffer's movie."
  (interactive "P")
  (ensure-etcc-buffer
    (if (or (not etcc/movie-info-updater-running) ; explicitly stopped
            (and force
                 (prog1 t
                   (etcc/movie-info-updater-stop (etcc-movie-id etcc-movie)))))
        (etcc/movie-info-updater-start (etcc-movie-id etcc-movie))
      (message "ETCC: comment update already started"))))

(defun etcc-start-updater (&optional force)
  "Start the updater processes of the current buffer's movie.
Updater processes are comment updater and movie info updater."
  (interactive "P")
  (ensure-etcc-buffer
   (etcc-start-comment-updater force)
   (etcc-start-movie-info-updater force)))

(defun etcc-stop-updater (&optional no-prompt)
  "Stop the updater processes of the current buffer's movie.
Updater processes are comment updater and movie info updater."
  (interactive "P")
  (ensure-etcc-buffer
   (etcc-stop-movie-info-updater no-prompt)
   (etcc-stop-comment-updater no-prompt)))

(defun etcc-restart-updater (&optional force)
  "Restart the updater processes of the current buffer's movie.
Updater processes are comment updater and movie info updater."
  (interactive "P")
  (ensure-etcc-buffer
   (etcc-stop-updater t)
   (etcc-start-updater force)))

(defun etcc/insert-image-from-url (url)
  "Insert image of URL into the current position."
  (let ((buffer (url-retrieve-synchronously url t)))
    (unwind-protect
        (let ((data (with-current-buffer buffer
                      (goto-char (point-min))
                      (search-forward "\n\n")
                      (buffer-substring (point) (point-max)))))
          (insert-image (create-image data nil t)))
      (kill-buffer buffer))))

(defun etcc-insert-alist (alist)
  (mapc (lambda (elem)
          (if (stringp elem)
              (insert (etcc/fontify-string elem 'bold) "\n")
            (let ((key (car elem))
                  (value (cdr elem)))
              (if key
                  (insert (etcc/fontify-string (concat key ": ") 'bold)
                          (or value "") "\n")
                (insert "\n")))))
        alist))

(defun etcc-display-movie-info (&optional display-thumbnail)
  "Show the movie info in a buffer.
If DISPLAY-THUMBNAIL is non-nil,
display the thumbnail image as well.
With a ‘C-u C-u’ prefix argument, display the small size thumbnail."
  (interactive "P")
  (ensure-etcc-buffer
    (let ((buf (get-buffer-create etcc-movie-info-buffer-name))
          (movie etcc-movie)
          (broadcaster etcc-broadcaster)
          (tags etcc-tags)
          pos)
      (set-buffer buf)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq buffer-read-only nil)
      (let ((thumbnail-url (cond ((equal display-thumbnail '(16))
                                  (etcc-movie-small-thumbnail movie))
                                 (display-thumbnail
                                  (etcc-movie-large-thumbnail movie)))))
        (when (> (length thumbnail-url) 0)
          (etcc/insert-image-from-url thumbnail-url)
          (insert "\n\n")))
      (setq pos (point))
      (etcc-insert-alist
       `("MOVIE"
         ("Id" . ,(etcc-movie-id movie))
         ("User Id" . ,(etcc-movie-user-id movie))
         ("Title" . ,(etcc-movie-title movie))
         ("Sub Title" . ,(etcc-movie-subtitle movie))
         ("Last Owner Comment" . ,(etcc-movie-last-owner-comment movie))
         ("Category" . ,(etcc-movie-category movie))
         ("Link" . ,(etcc-movie-link movie))
         ("Is Live" . ,(if (etcc-movie-is-live movie) "Yes" "No"))
         ("Is Recorded" . ,(if (etcc-movie-is-recorded movie) "Yes" "No"))
         ("Comment Count" . ,(number-to-string
                              (etcc-movie-comment-count movie)))
         ("Large Thumbnail" . ,(etcc-movie-large-thumbnail movie))
         ("Small Thumbnail" . ,(etcc-movie-small-thumbnail movie))
         ("Country" . ,(etcc-movie-country movie))
         ("Duration" . ,(etcc/time-string (etcc-movie-duration movie)))
         ("Created" . ,(format-time-string "%Y/%m/%d %H:%M:%S"
                                           (etcc-movie-created movie)))
         ("Is Collabo" . ,(if (etcc-movie-is-collabo movie) "Yes" "No"))
         ("Is Proteccted" . ,(if (etcc-movie-is-protected movie)
                                 "Yes" "No"))
         ("Max View Count" . ,(number-to-string
                               (etcc-movie-max-view-count movie)))
         ("Current View Count" . ,(number-to-string
                                   (etcc-movie-current-view-count movie)))
         ("Total View Count" . ,(number-to-string
                                 (etcc-movie-total-view-count movie)))
         ("HLS URL" . ,(etcc-movie-hls-url movie))
         nil
         "BROADCASTER"
         ("Id" . ,(etcc-user-id broadcaster))
         ("Screen Id" . ,(etcc-user-screen-id broadcaster))
         ("Name" . ,(etcc-user-name broadcaster))
         ("Image" . ,(etcc-user-image broadcaster))
         ("Profile" . ,(etcc-user-profile broadcaster))
         ("Level" . ,(number-to-string (etcc-user-level broadcaster)))
         ("Last Movie Id" . ,(etcc-user-last-movie-id broadcaster))
         ("Is Live" . ,(if (etcc-user-is-live broadcaster) "Yes" "No"))
         ("Supporter Count" . ,(number-to-string
                                (etcc-user-supporter-count broadcaster)))
         ("Supporting Count" . ,(number-to-string
                                 (etcc-user-supporting-count broadcaster)))
         nil
         ("TAGS" . ,(mapconcat (lambda (tag)
                                 (format "#%s" tag)) tags " "))))
      (goto-char (point-min))
      (run-hooks 'etcc-display-movie-info-hook)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer buf))))

(defun etcc/filter-comments (pred &optional etcc-buf)
  "Collect commands that satisfies PRED in the buffer ETCC-BUF
and return the list of `etcc-command' objects.
PRED is a function or a lambda express that takes one parameter
of `etcc-command' object.
If ETCC-BUF is nil, default to the current buffer."
  (let (comments)
    (with-current-buffer (or etcc-buf
                             (current-buffer))
      (ensure-etcc-buffer
        (save-excursion
          (save-restriction
            (widen)
            (goto-char (point-min))
            (while (not (eobp))
              (etcc-next-comment 1 t)
              (let ((comment (get-text-property (point) 'etcc-comment)))
                (if (and comment
                         (funcall pred comment))
                    (add-to-list 'comments comment)))))))
      comments)))

(defun etcc/insert-user-comments (user-id etcc-buf)
  "Insert comments posted by USER-ID in the buffer of ETCC-BUFFER.
If USER-ID is nil, insert all the comments in the current buffer."
  (let* ((pred (if (not user-id)
                   'identity            ; pickup all comments
                 (lambda (comment)
                   (let ((comment-user-id (etcc-user-id
                                           (etcc-comment-from-user comment))))
                     (equal user-id comment-user-id)))))
         (comments (etcc/filter-comments pred etcc-buf)))
    (etcc/insert-comments comments)))

(defun etcc/display-user-info (user &optional display-image etcc-buf)
  "Show the user info USER.
If DISPLAY-image is non-nil,
display the user image as well."
  (let ((buf (get-buffer-create etcc-user-info-buffer-name))
        pos)
    (set-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setq buffer-read-only nil)
    (let ((image-url (if display-image (etcc-user-image user) "")))
      (when (> (length image-url) 0)
        (etcc/insert-image-from-url image-url)
        (insert "\n")))
    (setq pos (point))
    (etcc-insert-alist
     `(("Id" . ,(etcc-user-id user))
       ("Screen Id" . ,(etcc-user-screen-id user))
       ("Name" . ,(etcc-user-name user))
       ("Image" . ,(etcc-user-image user))
       ("Profile" . ,(etcc-user-profile user))
       ("Level" . ,(number-to-string (etcc-user-level user)))
       ("Last Movie Id" . ,(etcc-user-last-movie-id user))
       ("Is Live" . ,(if (etcc-user-is-live user) "Yes" "No"))
       ("Supporter Count" . ,(number-to-string
                              (etcc-user-supporter-count user)))
       ("Supporting Count" . ,(number-to-string
                               (etcc-user-supporting-count user)))))
    (when etcc-buf
      (insert "\n")
      (etcc/insert-user-comments (etcc-user-id user) etcc-buf))
    (goto-char (point-min))
    (run-hooks 'etcc-display-user-info-hook)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (display-buffer buf)))

(defun etcc-user-at (&optional pos)
  "Return object of `etcc-user' at the position of POS.
POS defaults to t current position.
If the 'etcc-user text property is defined at the position,
return the value of 'etcc-user property.
If the 'etcc-comment text property is defined at the position,
return the value of 'etcc-comment-from-user of the comment.
Otherwise, return the broadcaster of the buffer."
  (or pos (setq pos (point)))
  (or (get-text-property pos 'etcc-user)
      (let ((comment (get-text-property pos 'etcc-comment)))
        (and comment
             (etcc-comment-from-user comment)))
      etcc-broadcaster))

(defun etcc-display-user-info (&optional display-image)
  "Display the user info at the current point
with his/her posted comments on the movie.
If DISPLAY-IMAGE is non-nil, display the user's image, too."
  (interactive "P")
  (ensure-etcc-buffer
    (let ((user (etcc-user-at)))
      (etcc/display-user-info user display-image (current-buffer)))))

(defun etcc-display-user-info-with-image (&optional no-image)
  "Invert version of `etcc-display-user-info'."
  (interactive "P")
  (etcc-display-user-info (not no-image)))

(defun etcc-browse-thumbnail (&optional small-thumbnail)
  "Open web browser and display thumbnail of the movie."
  (interactive "P")
  (ensure-etcc-buffer
    (let ((url (if small-thumbnail
                   (etcc-movie-small-thumbnail etcc-movie)
                 (etcc-movie-large-thumbnail etcc-movie))))
      (if url
          (etcc-browse-url url)
        (message "no thumbnail provided.")))))

;;;;;;;;

(defun etcc/unescape-comment (str &optional replace-with-whitespace)
  (cond ((not (stringp str)) str)
        (replace-with-whitespace (replace-regexp-in-string "\\\\[nt]" " " str))
        (t (mapc (lambda (cons)
                   (setq str (replace-regexp-in-string (car cons) (cdr cons)
                                                       str)))
                 '(("\\\\n" . "\n")
                   ("\\\\t" . "\t")))
           str)))

(defun etcc/insert-comment-input-area (movie broadcaster tags)
  (ensure-etcc-buffer
    (goto-char (point-min))
    (insert "\n\n")
    (set-marker etcc-prompt-end (point))
    (let ((inhibit-read-only t))
      (insert (etcc/fontify-string
               (concat
;;                 (etcc-movie-title movie)
;;                 " "
                (or (etcc-movie-subtitle movie)
                    (etcc/unescape-comment
                     (etcc-movie-last-owner-comment movie) t))
                " "
;;                 (mapconcat (lambda (tag) (format "#%s" tag)) tags " ")
                "\n")
               'fringe t)))
    (set-marker etcc-comment-region-beg (point))
    (insert "\n")
    (set-marker etcc-comment-region-end (point))
    (etcc-kill-comment)))

(defun etcc/time-string (time)
  (let ((hour (/ time 60 60))
        (min (mod (/ time 60) 60))
        (sec (mod time 60)))
    (if (> hour 0)
        (format "%d:%2.2d:%2.2d " hour min sec)
      (format "%2.2d:%2.2d " min sec))))

(defun etcc/set-header-line ()
  (ensure-etcc-buffer
    (setq header-line-format
          '((:eval (let* ((duration
                           (if (etcc-movie-is-live etcc-movie)
                               (let* ((created (etcc-movie-created etcc-movie))
                                      (ct (current-time))
                                      (now (+ (* (nth 0 ct) 65536) (nth 1 ct))))
                                 (- now created))
                             (etcc-movie-duration etcc-movie))))
                     (etcc/time-string duration)))
            (etcc-movie-is-live "[Live]")
            (:eval (and (etcc-movie-is-recorded etcc-movie) "[Rec'd]"))
            (etcc/comment-updater-status
             (:eval (if (eq etcc/comment-updater-status 'waiting)
                        "[c]" "[C]")))  ; c ... waiting, C ... up-to-date
            (etcc/movie-info-updater-status
             (:eval (if (eq etcc/movie-info-updater-status 'waiting)
                        "[m]" "[M]")))  ; m ... waiting, M ... up-to-date
            (etcc-play-hls-proc
             (:eval (if (process-live-p etcc-play-hls-proc) "[p]")))
            (etcc-download-hls-proc
             (:eval (if (process-live-p etcc-download-hls-proc) "[d]")))
            (etcc-download-movie-proc
             (:eval (if (process-live-p etcc-download-movie-proc) "[l]")))
            " "
            (:eval (etcc/user-string etcc-broadcaster))
            " / "
            (:eval (etcc-movie-title etcc-movie))
            " | C:" (:eval (number-to-string etcc-comment-all-count))
            " | V:" (:eval (number-to-string (etcc-movie-current-view-count etcc-movie)))
            "/"  (:eval (number-to-string (etcc-movie-max-view-count etcc-movie)))
            "/"  (:eval (number-to-string (etcc-movie-total-view-count etcc-movie)))
))))

(defun etcc/view-movie (movie broadcaster tags)
  (let ((buf (etcc/buffer movie t)))
    (set-buffer buf)
    (buffer-disable-undo)
    (etcc-mode)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (etcc/update-movie-info movie broadcaster tags)
    (etcc/insert-comment-input-area movie broadcaster tags)
    (set-buffer-modified-p nil)
    (buffer-enable-undo)
    ;; start comment updater
    (etcc/comment-updater-start (etcc-movie-id movie))
    ;; start movie info updater
    (if (etcc-movie-is-live movie)
        (etcc/movie-info-updater-start (etcc-movie-id movie)))
    (etcc/set-header-line)
    (switch-to-buffer buf)))

(cl-defun etcc-view-movie-sentinel (&key data &allow-other-keys)
  (let* ((movie (assoc-default 'movie data))
         (broadcaster (assoc-default 'broadcaster data))
         (tags (assoc-default 'tags data)))
    (let ((etcc-movie (make-etcc-movie-from-alist movie))
          (etcc-user (make-etcc-user-from-alist broadcaster)))
      (etcc/view-movie etcc-movie etcc-user tags))))

(defun etcc-view-movie (movie-id)
  (interactive (list
                (read-number "Twitcasting movie Id: ")))
  (etcc-api/get-movie-info movie-id :success 'etcc-view-movie-sentinel))

(defvar etcc-live-user-id-list nil)

(defun etcc-view-live (user-id)
;;   (interactive "s") ;XXX
  (interactive (list (read-string "User Id: " nil 'etcc-live-user-id-list)))
  (etcc-api/get-current-live user-id :success 'etcc-view-movie-sentinel))

(defvar etcc-live-movie-url-list nil)

(defun etcc-view-from-url (url)
  "View Twitcasting live/movie from the given URL."
  (interactive (list (read-string "Twitcasting live/movie URL: "
                                  nil ;; (car etcc-live-movie-url-list)
                                  'etcc-live-movie-url-list)))
  (cond ((string-match etcc-twitcasting-live-url-regexp url)
         (let ((user-id (match-string 1 url)))
           (etcc-view-live user-id)))
        ((string-match etcc-twitcasting-movie-url-regexp url)
         (let ((user-id (match-string 1 url))
               (movie-id (match-string 2 url)))
           (etcc-view-movie movie-id)))
        (t (error "not a twitcasting movie/live URL."))))

;; (defalias 'etcc 'etcc-view-from-url)

(defun etcc-reload (arg)
  "Reload the current Twitcasting movie/live."
  (interactive "P")
  (ensure-etcc-buffer
    (if (yes-or-no-p "Will you reload the movie/live? ")
        (let ((url (etcc-movie-link etcc-movie))
              (movie-id (etcc-movie-id etcc-movie)))
          (etcc-stop-movie-info-updater t)
          (etcc-stop-comment-updater t)
          (etcc-stop-hls)
          (etcc-view-from-url url)))))

(defun etcc-quit (&optional bury)
  "Quit ETCC.
With a prefix argument, bury the etcc buffer
instead of killing it."
  (interactive "P")
  (ensure-etcc-buffer
    (when (yes-or-no-p "Quit ETCC? ")
      (etcc-stop-movie-info-updater t)
      (etcc-stop-comment-updater t)
      (etcc-stop-hls)
;;       (etcc-stop-download t)
      (message "Bye, bye...")
      (sit-for 1)
      (if bury
          (bury-buffer)
        (kill-buffer)))))

(defun etcc-open-link (args)
  "Open the live/movie link in the external browser."
  (interactive "P")
  (ensure-etcc-buffer
    (let ((link (etcc-movie-link etcc-movie)))
      (if link
          (etcc-browse-url link)
        (message "ETCC: link not available.")))))

(defun etcc/hls-play (url)
  (let* ((buf (get-buffer-create (format "*ETCC-hls-play:%s*" url)))
         (proc (start-process "mplayer" buf etcc-mplayer-program
                              "-quiet" url)))
    proc))

(defun etcc-play-hls (&optional arg)
  "Start mplayer to play the HTTP Live Streaming of the current movie buffer.
If ARG is non-nil, start a web browser to play HLS."
  (interactive "P")
  (ensure-etcc-buffer
    (let ((hls-url (etcc-movie-hls-url etcc-movie)))
      (cond ;; ((equal arg '(16))
;;              (etcc-stop-hls))
            ((not hls-url)
             (message "ETCC: No HTTP Live Streaming available."))
            (arg (etcc-browse-url hls-url))
            ((process-live-p etcc-play-hls-proc)
             (message "ETCC: player already started."))
            (t (message "starting HLS player...")
               (let ((proc (etcc/hls-play hls-url)))
                 (setq etcc-play-hls-proc proc))
               (sit-for 0.5)
               (message "starting HLS player... done"))))))

(defun etcc-stop-hls (&optional arg)
  "Stop mplayer."
  (interactive "P")
  (ensure-etcc-buffer
    (if (not (process-live-p etcc-play-hls-proc))
        (etcc-message "HLS player not running.")
      (signal-process etcc-play-hls-proc 'SIGTERM))))

(defun etcc/hls-download (url file)
  ;; ffmpeg -i https://example.com/.../playlist.m3u8 -movflags faststart -c copy -bsf:a aac_adtstoasc rec.mp4
  (let* ((buf (get-buffer-create (format "*ETCC-hls-download:%s*" url)))
         (proc (start-process "ffmpeg" buf "env" "AV_LOG_FORCE_NOCOLOR=yes"
                              etcc-ffmpeg-program
                              "-y" "-i" url "-movflags" "faststart"
                              "-c" "copy" "-bsf:a" "aac_adtstoasc"
                              file)))
    proc))

(defun etcc-display-hls-download-buffer ()
  "Show the buffer of HLS download process."
  (interactive)
  (ensure-etcc-buffer
    (unless (processp etcc-download-hls-proc)
      (error "HLS download not started."))
    (let ((buf (process-buffer etcc-download-hls-proc)))
      (if (buffer-live-p buf)
          (display-buffer buf)
        (message "Buffer was killed.")))))

(defun etcc-download-hls (file)
  "Start to download the HTTP Live Streaming of the current movie buffer."
  (interactive
   (ensure-etcc-buffer
     (unless (etcc-movie-hls-url etcc-movie)
       (error "ETCC: No HLS available for the movie."))
     (let* ((user-screen-id (etcc-user-screen-id etcc-broadcaster))
            (movie-id (etcc-movie-id etcc-movie))
            (default-filename (concat user-screen-id "_"
                                      movie-id ".mp4")))
       (list
        (read-file-name "Download HLS: "
                        etcc-download-hls-default-directory
                        default-filename
                        nil default-filename)))))
  (ensure-etcc-buffer
    (if (or (not (file-exists-p file))
            (yes-or-no-p "file exists. overwrite? "))
        (let* ((hls-url (etcc-movie-hls-url etcc-movie))
               (proc (etcc/hls-download hls-url file)))
          (setq etcc-download-hls-proc proc)
          (etcc-display-hls-download-buffer)))))

(defun etcc-stop-hls-download (&optional no-prompt)
  (interactive "P")
  (ensure-etcc-buffer
    (cond ((or (not (processp etcc-download-hls-proc))
               (not (process-live-p etcc-download-hls-proc)))
           (etcc-message "HLS download not running."))
          ((or no-prompt
               (yes-or-no-p "Are you sure to stop downloading HLS? "))
           (signal-process etcc-download-hls-proc 'SIGTERM)))))


(defun etcc/movie-download (url file)
  "Download a movie file from URL and save it to FILE."
  (let* ((buf (get-buffer-create (format "*ETCC-movie-download:%s*" url)))
         (proc (start-process "ffmpeg" buf "env" "AV_LOG_FORCE_NOCOLOR=yes"
                              etcc-ffmpeg-program
                              "-y" "-i" url
                              "-c" "copy"
                              file)))
    proc))

(defun etcc-display-movie-download-buffer ()
  "Show the buffer of movie download process."
  (interactive)
  (ensure-etcc-buffer
    (unless (processp etcc-download-movie-proc)
      (error "Movie file download not started."))
    (let ((buf (process-buffer etcc-download-movie-proc)))
      (if (buffer-live-p buf)
          (display-buffer buf)
        (message "Buffer was killed.")))))

(defun etcc/movie-file-url (&optional user-screen-id movie-id)
  "Return the recorded movie file URL."
  (unless (and user-screen-id movie-id)
    (ensure-etcc-buffer
      (setq user-screen-id (etcc-user-screen-id etcc-broadcaster))
      (setq movie-id (etcc-movie-id etcc-movie))))
  (format etcc-movie-url-format user-screen-id movie-id))

(defun etcc-download-movie (file)
  "Download the recorded movie file of the current movie buffer."
  (interactive
   (ensure-etcc-buffer
     (let* ((user-screen-id (etcc-user-screen-id etcc-broadcaster))
            (movie-id (etcc-movie-id etcc-movie))
            (default-filename (concat "movie_" user-screen-id "_"
                                      movie-id ".mp4")))
       (list
        (read-file-name "Download Movie: "
                        etcc-download-hls-default-directory ; XXX
                        default-filename
                        nil default-filename)))))
  (ensure-etcc-buffer
    (if (or (not (file-exists-p file))
            (yes-or-no-p "file exists. overwrite? "))
        (let* ((url (etcc/movie-file-url))
               (proc (etcc/movie-download url file)))
          (setq etcc-download-movie-proc proc)
          (etcc-display-movie-download-buffer)))))

(defun etcc-stop-movie-download (&optional no-prompt)
  (interactive "P")
  (ensure-etcc-buffer
    (cond ((or (not (processp etcc-download-movie-proc))
               (not (process-live-p etcc-download-movie-proc)))
           (etcc-message "Movie download not running."))
          ((or no-prompt
               (yes-or-no-p "Are you sure to stop downloading movie? "))
           (signal-process etcc-download-movie-proc 'SIGTERM)))))

(defun etcc-download (arg)
  "Download the recorded movie file or HLS according to the movie/live status.
With a ‘C-u C-u’ prefix argument, show the recorded movie file
URL and save the URL in the kill ring.
If the movie is recorded or with a prefix argument, start to
downnload the movie file.  If the movie is a live, start to
download HLS."
  (interactive "P")
  (ensure-etcc-buffer
    (cond ((equal arg '(16))
           (let* ((url (etcc/movie-file-url)))
             (kill-new url)
             (message "URL: %s" url)))
          ((or arg (etcc-movie-is-recorded etcc-movie))
           (call-interactively 'etcc-download-movie))
          (etcc-movie-is-live
           (call-interactively 'etcc-download-hls))
          (t
           (message "not live or recorded.")))))

(defun etcc-stop-download (arg)
  "Stop download movie file and HLS."
  (interactive "P")
  (ensure-etcc-buffer
    (etcc-stop-hls-download)
    (etcc-stop-movie-download)))

(defun etcc-display-download-buffer ()
  (interactive)
  (ensure-etcc-buffer
    (let ((buf1 (and (processp etcc-download-hls-proc)
                     (process-buffer etcc-download-hls-proc)))
          (buf2 (and (processp etcc-download-movie-proc)
                     (process-buffer etcc-download-movie-proc))))
      (let ((win1 (and (buffer-live-p buf1) (display-buffer buf1)))
            (win2 (and (buffer-live-p buf2) (display-buffer buf2))))
        (unless (or win1 win2)
          (message "No download buffer found."))))))

;;;;;;;;

(defun etcc-prompt ()
  "Return ETCC comment prompt string."
  (if etcc-current-user
      (concat (etcc/user-string etcc-current-user) "> ")
    "> "))

(defun etcc-skip-prompt ()
  "Skip ETCC comment prompt."
  (while (and (not (eolp))
              (get-text-property (point) 'prompt))
    (forward-char)))

(defun etcc-in-comment-prompt-region-p ()
  (< (point) etcc-prompt-end))

(defun etcc/comment-string ()
  "Get comment string in the ETCC buffer."
  (ensure-etcc-buffer
    (save-excursion
      (save-restriction
        (goto-char (point-min))
        (etcc-skip-prompt)
        (let ((str (buffer-substring-no-properties (point) etcc-prompt-end)))
          (replace-regexp-in-string "\\`[\n\t ]+\\|[\n\t ]+\\'" "" str))))))

(defun etcc-post-comment (&optional args)
  (interactive "P")
  (ensure-etcc-buffer
    (let ((comment-string (etcc/comment-string)))
      (cond ((not (etcc-in-comment-prompt-region-p))
             (error "not in comment prompt region."))
            ((or (not (stringp comment-string))
                 (<= (length comment-string) 0))
             (error "Please enter comment."))
            ((not (etcc-movie-is-live etcc-movie))
             (error "not a live."))
            (t
             (let ((etcc-movie-id (etcc-movie-id etcc-movie)))
               (message "posting comment...")
               (etcc-api/post-comment etcc-movie-id comment-string
                                      :success (etcc-request-callback (data)
                                                 (message
                                                  "posting comment...done")))
               (setq etcc-last-post comment-string)
               (etcc-kill-comment)))))))

(defun etcc-kill-comment (&optional args)
  "Kill comment entered and save it in the kill ring.
Then, the prompt is inserted newly."
  (interactive "P")
  (ensure-etcc-buffer
    (save-restriction
      (widen)
      (let ((inhibit-read-only t))
        (let ((comment-string (etcc/comment-string)))
          (delete-region (point-min)
                         etcc-prompt-end)
          (if (and (stringp comment-string)
                   (> (length comment-string) 0))
              (kill-new comment-string)))
        (goto-char (point-min))
        (if etcc-prompt-function
            (let ((prompt (funcall etcc-prompt-function)))
              (add-text-properties 0 (length prompt)
                                   '(read-only t
                                     front-sticky read-only
                                     rear-nonsticky read-only
                                     prompt t)
                                   prompt)
              (insert prompt)))
        (save-excursion
          (insert "\n\n")
          (set-marker etcc-prompt-end (point)))
        (set-buffer-modified-p nil)))))

(defun etcc-mention (&optional arg)
  "Insert \"@user-screen-id \" at the beginning of the comment
to reply to the user at the current position."
  (interactive "P")
  (ensure-etcc-buffer
    (let* ((user (etcc-user-at))
           (user-name (etcc-user-name user))
           (user-screen-id (etcc-user-screen-id user)))
      (etcc-beginning-of-buffer)
      (insert "@" user-screen-id " ")
      (message "mention to: %s" (etcc/user-string user)))))

(defun etcc-bol ()
  "Go to the beginning of line, then skip the prompt, if any."
  (interactive)
  (beginning-of-line)
  (etcc-skip-prompt))

(defun etcc-beginning-of-buffer (&optional arg)
  "Go to the beginning of the buffer, then skip the prompt, if any."
  (interactive "P")
  (beginning-of-buffer)
  (unless arg (etcc-bol)))

(defun etcc-goto-last-comment (&optional arg)
  "Go to the last comment of the movie."
  (interactive "P")
  (goto-char etcc-comment-region-beg))

(defun etcc-goto-first-comment (&optional arg)
  "Go to the first comment of the movie."
  (interactive "P")
  (goto-char etcc-comment-region-end))

(defun etcc-next-comment (&optional arg no-error)
  (interactive "^p")
  (or arg (setq arg 1))
  (cond ((> arg 0)
         (move-beginning-of-line nil)
         (while (> arg 0)
           (let* ((beg (save-excursion
                         (next-line nil)
                         (point)))
                  (pos (text-property-any beg (point-max)
                                          'etcc-comment-begin t)))
             (unless pos
               (if no-error
                   (progn
                     (setq pos (point-max))
                     (setq arg 0))
                 (error "no more comment")))
             (goto-char pos)
             (setq arg (1- arg)))))
        ((< arg 0)
         (move-beginning-of-line nil)
         (while (< arg 0)
           (unless (re-search-backward "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"
                                       nil t)
             (if no-error
                 (progn
                   (setq pos (point-min))
                   (setq arg 0))
               (error "no more comment")))
           (setq arg (1+ arg))))))

(defun etcc-previous-comment (&optional arg)
  (interactive "^p")
  (or arg (setq arg 1))
  (etcc-next-comment (- arg)))

(defun etcc-speed-command (&optional arg)
  "If the cursor is at a read-only position and
the key defined in `etcc-speed-commands' is pressed,
call the command associated with the key in `etcc-speed-commands' interactively.
Otherwise, call the command defined in global key binding interactively."
  (interactive "P")
  (let* ((kv (this-command-keys-vector))
         (key (make-string 1 (aref kv (1- (length kv)))))
         (command (assoc-default key etcc-speed-commands)))
    (if (and (or (get-text-property (point) 'read-only)
                 (eobp))
             command)
        (progn
          (setq last-command command)
          (call-interactively command))
      (call-interactively (global-key-binding key)))))

(provide 'etcc)

;; EOF
