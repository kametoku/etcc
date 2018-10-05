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
;; search live movies:
;; - type "M-x etcc" or "M-x etcc-search-recommend"
;;   to list recommend live movies
;; - type "M-x etcc-search-new" to list new live movies.
;; - type "M-x etcc-search-by-word" to search live movies with words.
;; - type "M-x etcc-search-by-tag" to search live movies with tags.
;; - type "M-x etcc-search-by-category" to search live movies with category.
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
;;
;; Search users and view user info
;; - type "M-x etcc-search-user" to search user with words.
;; - type "M-x etcc-view-user" followed by user's id to view the user
;;   info with ther user's movie list

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

(defcustom etcc-display-comments-hook nil
  "Hook called in `etcc-display-comments'."
  :group 'etcc
  :type 'hook)

(defcustom etcc-search-mode-hook nil
  "Hook called in `etcc-search-mode'."
  :group 'etcc
  :type 'hook)

(defcustom etcc-search-user-mode-hook nil
  "Hook called in `etcc-search-mode'."
  :group 'etcc
  :type 'hook)

(defcustom etcc-user-mode-hook nil
  "Hook called in `etcc-user-mode'."
  :group 'etcc
  :type 'hook)

(defcustom etcc-supporter-mode-hook nil
  "Hook called in `etcc-supporter-mode'."
  :group 'etcc
  :type 'hook)

(defcustom etcc-supporting-mode-hook nil
  "Hook called in `etcc-supporting-mode'."
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

(defcustom etcc-tc-user-id-url-regexp
  ;; https://apiv2.twitcasting.tv/users/:user_id
  (concat (regexp-quote etcc-tc-base-url) "/users/\\([^/?]+\\)")
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

(defcustom etcc-search-live-limit 100
  "Limit of live movies per search.
Note the max limit supported by Twitcasting API is 100."
  :group 'etcc
  :type 'number)

(defcustom etcc-search-user-limit 50
  "Limit of users per search.
Note the max limit supported by Twitcasting API is 50."
  :group 'etcc
  :type 'number)

(defcustom etcc-search-movies-by-user-limit 50
  "Limit of movies per search.
Note the max limit supported by Twitcasting API is 50."
  :group 'etcc
  :type 'number)

(defcustom etcc-supporter-list-limit 20
  "Limit of supporters per request.
Note the max limit supported by Twitcasting API is 20."
  :group 'etcc
  :type 'number)

(defcustom etcc-supporting-list-limit 20
  "Limit of supporting users per request.
Note the max limit supported by Twitcasting API is 20."
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
  :type 'function)

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

(defcustom etcc-comment-buffer-name "*ETCC-comments*"
  "The buffer name to display the comments."
  :group 'etcc
  :type 'string)

(defcustom etcc-movie-list-buffer-name "*ETCC-movies*"
  "The buffer name to display the movie list."
  :group 'etcc
  :type 'string)

(defcustom etcc-user-list-buffer-name "*ETCC-users*"
  "The buffer name to display the user list."
  :group 'etcc
  :type 'string)

(defcustom etcc-supporter-list-buffer-name "*ETCC-supporters*"
  "The buffer name to display the supporter list."
  :group 'etcc
  :type 'string)

(defcustom etcc-supporting-list-buffer-name "*ETCC-supportings*"
  "The buffer name to display the supporting list."
  :group 'etcc
  :type 'string)

(defcustom etcc-comment-string-function #'etcc-comment-string
  "Function to format comment."
  :group 'etcc
  :type 'function)

(defcustom etcc-comment-line-regexp "^[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"
  "A regexp which matches the beginning of the comment line."
  :group 'etcc
  :type 'string)

(defcustom etcc-header-line-format
  '((:eval (let* ((duration
                   (if (etcc-movie-is-live etcc-movie)
                       (let* ((created (etcc-movie-created etcc-movie))
                              (ct (current-time))
                              (now (+ (* (nth 0 ct) 65536) (nth 1 ct))))
                         (- now created))
                     (etcc-movie-duration etcc-movie))))
             (etcc/time-string duration)))
    " "
    (etcc-movie-is-live "[Live]")
    (:eval (and (etcc-movie-is-recorded etcc-movie) "[Rec]"))
    (etcc/comment-updater-status
     ;; c ... waiting, C ... up-to-date
     (:eval (if (eq etcc/comment-updater-status 'waiting) "[c]" "[C]")))
    (etcc/movie-info-updater-status
     ;; m ... waiting, M ... up-to-date
     (:eval (if (eq etcc/movie-info-updater-status 'waiting) "[m]" "[M]")))
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
    "/"  (:eval (number-to-string (etcc-movie-total-view-count etcc-movie))))
  "The header line format for `etcc-mode'.
See `header-line-format' as well."
  :group 'etcc
  :type 'list)

(defcustom etcc-prompt-function #'etcc-prompt
  "A function that returns the etcc comment prompt string."
  :group 'etcc
  :type 'function)

(defcustom etcc-mplayer-program "mplayer"
  "Program name of mplayer to play HLS."
  :group 'etcc
  :type 'string)

(defcustom etcc-download-default-directory "~/Downloads"
  "Name of default directory to download HLS and movie files."
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
    ("D" . etcc-delete-comment)
    ("I" . etcc-display-movie-info)
    ("i" . etcc-display-user-info-with-image)
    ("n" . etcc-next-comment)
    ("p" . etcc-previous-comment)
    ("q" . etcc-quit))
  "Alist of the key and command that is run if the position is read only.
Each element of alist is in (KEY . COMMAND) form.
In a read-only position, pressing KEY calls COMMAND interactively.
Otherwise, call the function globally bound to KEY.
KEY must be a single character string."
  :group 'etcc
  :type 'list)

(defcustom etcc-movie-line-regexp "^[0-9]+:[0-9][0-9]"
  "A regexp which matches the beginning of the movie line."
  :group 'etcc
  :type 'string)

(defcustom etcc-tag-regexp "#\\([^ \t\n\r]+\\)"
  "A regexp which matches tags in `etcc-search-mode' buffer."
  :group 'etcc
  :type 'string)

(defcustom etcc-user-line-regexp "^[^ \t\n].+@"
  "A regexp that matches the beginning of the user line.
The user lines are found in `etcc-search-user' buffer."
  :group 'etcc
  :type 'string)

(defcustom etcc-user-movie-line-regexp "^[0-9]\\{4\\}"
  "A regexp that matches the beginning of the user move line.
The user move lines are found in `etcc-user-mode' buffer."
  :group 'etcc
  :type 'string)

(defvar etcc-token-type nil)
(defvar etcc-access-token nil)
(defvar etcc-auth-status nil) ; nil, 'waiting, 'succeeded, 'cancelled
(defvar etcc-expires-in nil)
(defvar etcc-auth-last-time nil)
(defvar etcc-current-user nil)
(defvar etcc-mode-map nil)

;;; Faces

(defface etcc-user-name-face
  '((t (:inherit font-lock-function-name-face)))
  "Face for ETCC user name.")

(defface etcc-user-id-face
  '((t (:inherit font-lock-constant-face)))
  "Face for ETCC user Id.")

(defface etcc-time-string-face
  '((t (:inherit font-lock-variable-name-face)))
  "Face for ETCC time string in `etcc-mode'.")

(defface etcc-warning-face
  '((t (:inherit font-lock-warning-face)))
  "Face for ETCC warning string.")

(defface etcc-live-face
  '((t (:inherit font-lock-warning-face)))
  "Face for ETCC live string.")

(defface etcc-recorded-face
  '((t (:inherit font-lock-warning-face)))
  "Face for ETCC recorded string.")

(defface etcc-protected-face
  '((t (:inherit font-lock-warning-face)))
  "Face for ETCC protected string.")

(defface etcc-category-face
  '((t (:inherit font-lock-preprocessor-face)))
  "Face for ETCC category string.")

(defface etcc-movie-title-face
  '((t (:inherit font-lock-type-face)))
  "Face for ETCC movie title string.")

(defface etcc-tag-face
  '((t (:inherit font-lock-comment-face)))
  "Face for ETCC tags.")

(defface etcc-last-owner-comment-face
  '((t (:inherit font-lock-builtin-face)))
  "Face for ETCC tags.")

(defface etcc-my-comment-face
  '((((class color) (background light))
     (:background "beige"))
    (((class color) (background dark))
     (:background "pink2"))
    (t :inverse-video t))
  "Face to highlight my comments.")

(defface etcc-broadcaster-comment-face
  '((((class color) (background light))
     (:background "wheat1"))
    (((class color) (background dark))
     (:background "green"))
    (t :inverse-video t))
  "Face to highlight broadcaster's comments.")

(defface etcc-deleted-comment-face
  '((((class color) (background light))
     (:foreground "red" :strike-through t))
    (((class color) (background dark))
     (:strike-through t))
    (t :inverse-video t))
  "Face to highlight deleted comments.")

(defface etcc-volatile-highlights-face
  '((((class color) (background light))
     (:background "misty rose"))
    (((class color) (background dark))
     (:background "SkyBlue4"))
    (t :inverse-video t))
  "Face used for volatile highlights.")

;;; Data Types

;; https://apiv2-doc.twitcasting.tv/

(defun etcc-alist-to-plist (alist)
  "Convert ALIST to a plist.
Ex: ((\"id\" . 123) (\"user_id\" . 456) (is_live . :json-false))
 -> (:id 123 :user-id 456 :is-live nil)"
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
  supporter-count  ; int     このパラメータは非推奨となり、固定値0を返します。
  supporting-count ; int     このパラメータは非推奨となり、固定値0を返します。
  created)         ; int     このパラメータは非推奨となり、固定値0を返します。

(defun make-etcc-user-from-alist (alist &optional
                                        supporter-count supporting-count)
  "Create an `etcc-user' object form ALIST.
SUPPORTER-COUNT specifies the number of supporter.
SUPPORTING-COUNT specifies number of supporting users."
  (let ((plist (etcc-alist-to-plist alist)))
    (if supporter-count
        (setq plist (plist-put plist :supporter-count supporter-count)))
    (if supporting-count
        (setq plist (plist-put plist :supporting-count supporting-count)))
    (apply 'make-etcc-user plist)))

(defun etcc-user-equal (user1 user2)
  "Return non-nil USER1 and USER2 is the same user.
USER1 and USER2 are the objects of `etcc-user'."
  (let ((id1 (etcc-user-id user1))
        (id2 (etcc-user-id user2)))
    (equal id1 id2)))

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
  "Create an `etcc-movie' object from ALIST."
  (let ((plist (etcc-alist-to-plist alist)))
    (apply 'make-etcc-movie plist)))

;; コメントを表すオブジェクト
(cl-defstruct etcc-comment
  id             ; string  コメントID
  message        ; string  コメント本文
  from-user      ; object  コメント投稿者の情報 Userオブジェクト
  created)       ; int     コメント投稿日時のunixタイムスタンプ

(defun make-etcc-comment-from-alist (alist)
  "Create an `etcc-comment' object from ALIST."
  (let* ((plist (etcc-alist-to-plist alist))
         (from-user (plist-get plist :from-user)))
    (if from-user
        (let ((etcc-from-user (make-etcc-user-from-alist from-user)))
          (setq plist (plist-put plist :from-user etcc-from-user))))
    (apply 'make-etcc-comment plist)))

;; サポーターユーザを表すオブジェクト
;; (point, total_pointを除いてUserオブジェクトと同じです)
;; (cl-defstruct etcc-supporter-user
(cl-defstruct (etcc-supporter-user (:include etcc-user))
  point                                 ; int  アイテム・スコア
  total-point)                          ; int  累計スコア

(defun make-etcc-supporter-user-from-alist (alist)
  "Create an `etcc-supporter-user' object from ALIST."
  (let* ((plist (etcc-alist-to-plist alist)))
    (apply 'make-etcc-supporter-user plist)))

;; 配信カテゴリを表すオブジェクト
(cl-defstruct etcc-category
  id                          ; string  カテゴリID
  name                        ; string  カテゴリ名
  sub-categories)             ; array   Sub categoryオブジェクトの配列

(defun make-etcc-category-from-alist (alist)
  "Create an `etcc-category' object from ALIST."
  (let* ((plist (etcc-alist-to-plist alist))
         (sub-categories (plist-get plist :sub-categories)))
    (if sub-categories
        (let ((etcc-sub-categories
               (mapcar (lambda (sub-category)
                         (make-etcc-sub-category-from-alist sub-category))
                       sub-categories)))
          (setq plist (plist-put plist :sub-categories etcc-sub-categories))))
    (apply 'make-etcc-category plist)))

(defun make-etcc-category-list-from-alist (categories)
  "Return the list of `etcc-category' objects from CATEGORIES.
CATEGORIES is an array of plist, where each plist represents
category properties as plist."
  (mapcar (lambda (category)
            (make-etcc-category-from-alist category))
          categories))

;; 配信サブカテゴリを表すオブジェクト
(cl-defstruct etcc-sub-category
  id                                    ; string  サブカテゴリID
  name                                  ; string  サブカテゴリ名
  count)                                ; int     サブカテゴリ配信数

(defun make-etcc-sub-category-from-alist (alist)
  "Create an `etcc-sub-category' object from ALIST."
  (let* ((plist (etcc-alist-to-plist alist)))
    (apply 'make-etcc-sub-category plist)))

;;; basic functions

(defun etcc-auth-callback (code state)
  "Callback for `etcc-auth'.
CODE is the auth code and STATE is the auth state."
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
           (format "ETCC: authorization failed (%S: %s (code=%s; details=%s)). Retry? "
                   error-thrown message code details)))
      (if (y-or-n-p prompt)
          (etcc-auth)))))

(defservlet* etcc-auth-callback text/plain (code state result)
  (if result
      (progn
        (insert "authorization process cancelled by yourself.")
        (message "ETCC: authorization process cancelled by yourself")
        (setq etcc-auth-status 'cancelled))
    (etcc-auth-callback code state)
    (insert "OK!")))

(defun etcc-browse-url (url &optional args)
  "Ask a WWW browser to load URL.
The variable `etcc-browse-url-browser-function' specifies which
browser function to use.  for ARGS, see `browse-url'."
  (let ((browse-url-browser-function etcc-browse-url-browser-function))
    (browse-url url args)))

(defun etcc-auth-request ()
  "Request authentication."
  (interactive)
  (when (or (not (eq etcc-auth-status 'waiting))
            (y-or-n-p "Waiting for auth.  Will you try another one? "))
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
  "Stop authorization process."
  (interactive)
  (if (eq etcc-auth-status 'waiting)
      (progn
        (httpd-stop)
        (setq etcc-auth-status nil))
    (message "ETCC: no auth is running")))

(defun etcc-auth-wait (&optional time-out)
  "Wait for authorization process to complete.
Optional argument TIME-OUT gives time out of the auth request."
  (if (not etcc-auth-status)
      (error "ETCC: auth request not submitted"))
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
  "Wait the authorization code granted.
If FORCE is nonn-nil, force to grant auth."
  (interactive "P")
  (if (or (memq etcc-auth-status '(nil cancelled))
          (and (eq etcc-auth-status 'succeeded) (null etcc-access-token))
          force)
      (etcc-auth-request))
  (if (eq etcc-auth-status 'waiting)
      (etcc-auth-wait)))


;;; Utilities

(defun etcc-next-regexp (regexp type &optional arg no-error)
  "Move to the beginning of the next position matching REGEXP.
TYPE is a string to denote the type of text to match.
ARG is a number that indicates the search direction and the
number of occurrences to search for.
If NO-ERROR is non-nil, do not raise an error when search
failed."
  (or arg (setq arg 1))
  (let ((pos (point)))
    (cond ((> arg 0)
           (while (> arg 0)
             (move-end-of-line nil)
             (cond ((re-search-forward regexp nil t)
                    (goto-char (match-beginning 0))
                    (setq pos (point))
                    (setq arg (1- arg)))
                   (no-error
                    (goto-char (point-max))
                    (setq arg 0))
                   (t
                    (goto-char pos)
                    (error "No more %s" type)))))
          ((< arg 0)
           (move-beginning-of-line nil)
           (while (< arg 0)
             (cond ((re-search-backward regexp nil t)
                    (setq pos (point))
                    (setq arg (1+ arg)))
                   (no-error
                    (goto-char (point-min))
                    (setq arg 0))
                   (t
                    (goto-char pos)
                    (error "No more %s" type))))))))

(defun etcc-previous-regexp (regexp type &optional arg no-error)
  "Move to the beginning of the previous position matching REGEXP.
TYPE is a string to denote the type of text to match.
ARG is a number that indicates the search direction and the
number of occurrences to search for.
If NO-ERROR is non-nil, do not raise an error when search
failed."
  (interactive "^p")
  (or arg (setq arg 1))
  (etcc-next-regexp regexp type (- arg) no-error))


;;; TwitCasting API

(defmacro etcc-request-callback (args &rest body)
  "Macro to define a callback of `etcc-request'.
ARGS is a list of parameter to the callback.
Execute BODY."
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
              (setq msg (format "%s - details: %s" msg details)))
          (message "%s" msg))
      (message "%s: %s: request: %s"
               (car error-thrown) (cdr error-thrown) url))))

(cl-defun etcc-request-error-401 (&rest args &key data
                                        &allow-other-keys)
  "Error handler for 401 Unauthorized error."
  (let* ((err (cdr (assoc 'error data)))
         (code (cdr (assoc 'code err))))
    (if (= code 1000)
        (progn
          (message "invalid token. try again after authorization succeeds...")
          (etcc-auth t)))))


(defun etcc/movie-id-from-url (url)
  "Return the movie ID as a string from an twitcasting api endpoint URL.
If URL does not match any of api endpoint URL, return nil."
  (save-match-data
    (if (string-match etcc-tc-movie-id-url-regexp url)
        (match-string 1 url))))

(defun etcc/user-id-from-url (url)
  "Return the user ID as a string from an twitcasting api endpoint URL.
If URL does not match any of api endpoint URL, return nil."
  (save-match-data
    (if (string-match etcc-tc-user-id-url-regexp url)
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
                                               (slice-id nil) ;"none"
                                               (success nil) (error nil))
  "Get movies by user of USER-ID.
ユーザーが保有する過去ライブ(録画)の一覧を作成日時の降順で取得する。"
  (let ((params `(("offset" . ,offset)
                  ("limit . ,limit"))))
    (if slice-id
        (add-to-list 'params `("slice_id" . ,slice-id)))
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
  (etcc-request (format "/movies/%s/comments/%s" movie-id comment-id)
                :type "DELETE"
                :success success :error error))

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

(cl-defun etcc-api/get-categories (&key (lang "ja") ;検索対象の言語:  "ja", "en"
                                        (success nil) (error nil))
  "Get categories.
配信中のライブがあるカテゴリのみを取得する。
LANG must be \"ja\" or \"en\"."
  (let ((params `(("lang" . ,lang))))
    (etcc-request "/categories"
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

(cl-defun etcc-api/search-live-movies (&key type
                                            (context "")
                                            (limit 10)
                                            (lang "ja")
                                            (success nil) (error nil))
  (let ((params `(("type" . ,type)
                  ("limit" . ,limit)
                  ("lang" . ,lang))))
    (if (member type '("tag" "word" "category"))
        (add-to-list 'params `("context" . ,context)))
    (etcc-request "/search/lives"
                  :params params
                  :success success :error error)))



;;; etcc mode for viewing twitcasting live/movie

(unless etcc-mode-map
  (setq etcc-mode-map (make-sparse-keymap))
  (define-key etcc-mode-map "\C-a" 'etcc-bol)
  (define-key etcc-mode-map "\C-c\C-a" 'etcc-display-comments)
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
  "Major mode for Emacs Twitcasting Client.

\\{etcc-mode-map}"
  (interactive)
  (set (make-local-variable 'etcc-movie-is-live) nil)
  (set (make-local-variable 'etcc-movie) nil)
  (set (make-local-variable 'etcc-broadcaster) nil)
  (set (make-local-variable 'etcc-tags) nil)
  (set (make-local-variable 'etcc-comment-all-count) 0)
  (set (make-local-variable 'etcc-last-comment-id) 1)
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
  "Return the fist buffer whose name match REGEXP."
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

(defun etcc/buffer (movie-or-movie-id &optional create-p no-error)
  "Return the ETCC buffer associated with the movie MOIVE-OR-MOVIE-ID.
If MOIVE-OR-MOVIE-ID is an `etcc-movie' object,
find the buffer that exactly equals to whole buffer name.
Otherwise (MOVIE-OR-MOVIE-ID is a movie id),
find the buffer whose name matches with part of the buffer name.
If the buffer does not exist and CREATE-P is non-nil,
the buffer is created.
If NO-ERROR is non-nil, do not raise an error."
  (if (etcc-movie-p movie-or-movie-id)
      (let ((bufname (etcc/buffer-name movie-or-movie-id)))
        (if create-p
            (get-buffer-create bufname)
          (get-buffer bufname)))
    (let ((buf (etcc/search-buffer (etcc/buffer-regexp movie-or-movie-id))))
      (or buf
          (if no-error
              nil
            (error "No buffer found associated with movie id %s"
                   movie-or-movie-id))))))

(defmacro with-etcc-buffer (movie-or-movie-id &rest body)
  "In the `etcc-mode' buffer for MOVIE-OR-MOVIE-ID, execute BODY."
  (declare (indent 1) (debug t))
  `(save-current-buffer
     (set-buffer (etcc/buffer ,movie-or-movie-id))
     ,@body))

(defmacro ensure-etcc-buffer (&rest body)
  "Ensure that the current buffer is an `etcc-mode' buffer and execute BODY."
  (declare (indent 0) (debug t))
  `(if (eq major-mode 'etcc-mode)
       (progn ,@body)
    (error "Not an ETCC buffer")))

(defmacro etcc-message (format-string &rest args)
  "Display a message only when the containing function was called interactively.
See `message' for FORMAT-STRING and ARGS."
  (declare (indent 0) (debug t))
  `(if (called-interactively-p 'interactive)
       (message ,format-string ,@args)))

(cl-defun etcc-auth-verify-credentials-sentinel (&key data &allow-other-keys)
  (let ((app (assoc-default 'app data))
        (user (assoc-default 'user data))
        (supporter-count (assoc-default 'supporter_count data))
        (supporting-count (assoc-default 'supporting_count data)))
    (setq etcc-current-user
          (make-etcc-user-from-alist user supporter-count supporting-count))))

(defun etcc-auth-verify-credentials ()
  "Verify the credentials and get its user info."
  (etcc-api/verify-credentials :success 'etcc-auth-verify-credentials-sentinel))

(defun etcc/fontify-string (string face &optional read-only &rest args)
  "Fontify STRING with FACE.
If READ-ONLY is non-nil, set the read only property to the string.
ARGS specifies extra properties to the string."
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

(defun etcc/user-string (user &optional name-only)
  "Return user string of USER with fontified.
USER is an `etcc-user' object or a user-id string.
If NAME-ONLY is non-nil, return the string without user id."
  (if (etcc-user-p user)
      (concat
       (etcc/fontify-string (copy-sequence (etcc-user-name user))
                            'etcc-user-name-face nil 'etcc-user user)
       (unless name-only
         (etcc/fontify-string (concat "@" (etcc-user-screen-id user))
                              'etcc-user-id-face nil 'etcc-user user)))
    (etcc/fontify-string (format "@%s" user)
                         'etcc-user-id-face nil 'user-id user)))

(defun etcc-comment-string (etcc-comment)
  "Format the comment string from `etcc-comment' object ETCC-COMMENT."
  (let* ((message (etcc-comment-message etcc-comment))
         (user (etcc-comment-from-user etcc-comment))
         (created (etcc-comment-created etcc-comment)))
    (concat
     (etcc/fontify-string (format-time-string "%H:%M:%S " created)
                          'etcc-time-string-face)
     (etcc/user-string user)
     "\n         " (replace-regexp-in-string "\\\\n" "\n         " message))))

(defun etcc/insert-comment (etcc-comment &optional no-highlight)
  "Insert the comment of ETCC-COMMENT into the current point.
If NO-HIGHLIGHT is non-nil, suppress highlighting the comment."
  (let* ((user (etcc-comment-from-user etcc-comment))
         (comment-string (funcall etcc-comment-string-function etcc-comment))
         (beg (point)))
    (let ((inhibit-read-only t))
      (insert comment-string "\n")
      (unless no-highlight
        (cond ((and (boundp 'etcc-current-user)
                    (equal (etcc-user-id user)
                           (etcc-user-id etcc-current-user)))
               (add-face-text-property beg (point)
                                       'etcc-my-comment-face t))
              ((and (boundp 'etcc-broadcaster)
                    (equal (etcc-user-id user)
                           (etcc-user-id etcc-broadcaster)))
               (add-face-text-property beg (point)
                                       'etcc-broadcaster-comment-face t))))
      (add-text-properties beg (point)
                           (list 'etcc-comment-id
                                 (string-to-number
                                  (etcc-comment-id etcc-comment))))
      (add-text-properties beg (point)
                           `(read-only t
                             etcc-comment ,etcc-comment
                             front-sticky read-only
                             rear-nonsticky read-only)))))

(defun etcc/insert-comments (comments &optional no-highlight)
  "Insert COMMENTS into the current buffer.
If the volatile-highlights package is installed, the inserted
comments will be highlighted for a while.
If NO-HIGHLIGHT is non-nil, suppress highlighting the comment."
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
                (etcc/insert-comment etcc-comment no-highlight)
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
             (message "ETCC: getting more comments... (%d)" next-comment-offset))
            ((null next-comment-offset)
             (message "ETCC: all comments received."))
            (t
             (insert (etcc/fontify-string "...more comments..."
                                          'etcc-warning-face t))
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
  "Run the comment updater for MOVIE-ID with next OFFSET."
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
  "Run the comment updater for MOVIE-ID from SLICE-ID."
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
  "Stop the comment updater for MOVIE-ID."
  (with-etcc-buffer movie-id
    (cond ((request-response-p etcc/comment-updater-running)
           (request-abort etcc/comment-updater-running))
          ((timerp etcc/comment-updater-running)
           (cancel-timer etcc/comment-updater-running)))
    (setq etcc/comment-updater-running nil)
    (setq etcc/comment-updater-status nil)))

(defun etcc-stop-comment-updater (&optional no-prompt)
  "Stop the comment updater process of the current buffer's movie.
If NO-PROMPT is non-nil, stop the process without prompting user."
  (interactive "P")
  (ensure-etcc-buffer
    (cond ((not etcc/comment-updater-running)
           (etcc-message "comment updater not running."))
          ((or no-prompt
               (y-or-n-p "Do you stop subscribing comments? "))
           (etcc/comment-updater-stop etcc-movie)))))

(defun etcc-start-comment-updater (&optional force)
  "Start the comment updater process of the current buffer's movie.
If FORCE is nonn-nil, force to start the process."
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
  "Update the movie info of the current buffer.
MOVIE is an `etcc-movie' object, BROADCASTER is an `etcc-user'
object of the movie's broadcaster, and TAGS are a list of tags."
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
  "Run the movie info updater of MOVIE-ID for the next update."
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
  "Start the movie info updater for MOVIE-ID."
  (with-etcc-buffer movie-id
    (or etcc/movie-info-updater-running
        (setq etcc/movie-info-updater-running t))
    (etcc/movie-info-updater-next movie-id)))

(defun etcc/movie-info-updater-stop (movie-id)
  "Stop the movie info updater for MOVIE-ID."
  (with-etcc-buffer movie-id
    (cond ((request-response-p etcc/movie-info-updater-running)
           (request-abort etcc/movie-info-updater-running))
          ((timerp etcc/movie-info-updater-running)
           (cancel-timer etcc/movie-info-updater-running)))
    (setq etcc/movie-info-updater-running nil)
    (setq etcc/movie-info-updater-status nil)))

(defun etcc-stop-movie-info-updater (&optional no-prompt)
  "Stop the movie info updater process of the current buffer's movie.
If NO-PROMPT is non-nil, stop the process without prompting user."
  (interactive "P")
  (ensure-etcc-buffer
    (cond ((not etcc/movie-info-updater-running)
           (etcc-message "comment updater not running."))
          ((or no-prompt
               (y-or-n-p "Do you stop movie info update? "))
           (etcc/movie-info-updater-stop etcc-movie)))))

(defun etcc-start-movie-info-updater (&optional force)
  "Start the movie info updater process of the current buffer's movie.
If FORCE is nonn-nil, force to start the process."
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
Updater processes are comment updater and movie info updater.
If FORCE is nonn-nil, force to start the processes."
  (interactive "P")
  (ensure-etcc-buffer
   (etcc-start-comment-updater force)
   (etcc-start-movie-info-updater force)))

(defun etcc-stop-updater (&optional no-prompt)
  "Stop the updater processes of the current buffer's movie.
Updater processes are comment updater and movie info updater.
If NO-PROMPT is non-nil, stop the process without prompting user."
  (interactive "P")
  (ensure-etcc-buffer
   (etcc-stop-movie-info-updater no-prompt)
   (etcc-stop-comment-updater no-prompt)))

(defun etcc-restart-updater (&optional force)
  "Restart the updater processes of the current buffer's movie.
Updater processes are comment updater and movie info updater.
If FORCE is nonn-nil, force to restart the processes."
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
  "Insert ALIST as string in the current point.
If an element of ALIST is string, insert it with bold face.
If an element of ALIST is cons cell, insert the car part with bold face,
followed by ':' and the cdr."
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

(defun etcc/display-movie-info (movie broadcaster tags
                                      &optional display-thumbnail)
  "Show the movie info of MOVIE in a buffer.
BROADCASTER is the broadcaster of the movie.
TAGS is a list of tags of the movie.
If DISPLAY-THUMBNAIL is non-nil, display the thumbnail image as well.
If DISPLAY-THUMBNAIL is (16), display the small size thumbnail."
  (let ((buf (get-buffer-create etcc-movie-info-buffer-name))
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
       ("Created" . ,(format-time-string "%Y-%m-%d %H:%M:%S"
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
    (display-buffer buf)))

(defun etcc-display-movie-info (&optional display-thumbnail)
  "Show the movie info in a buffer.
If DISPLAY-THUMBNAIL is non-nil,
display the thumbnail image as well.
With a \\[universal-argument] \\[universal-argument] prefix argument,
display the small size thumbnail."
  (interactive "P")
  (ensure-etcc-buffer
    (etcc/display-movie-info etcc-movie etcc-broadcaster etcc-tags
                             display-thumbnail)))

(defun etcc-display-movie-info-at-point (&optional display-thumbnail)
  "Display the movie info at the current point.
If DISPLAY-THUMBNAIL is non-nil, display the thumbnail as well
\(see `etcc/display-movie-info' for more details\)."
  (interactive "P")
  (let ((movie (etcc-movie-at))
        (broadcaster (etcc-user-at))
        (tags (and (boundp 'etcc-tags) etcc-tags)))
    (if (and movie broadcaster)
        (etcc/display-movie-info movie broadcaster tags display-thumbnail)
      (error "No movie found"))))

(defun etcc/filter-comments (pred &optional etcc-buf)
  "Collect commands that satisfies PRED condition in the buffer ETCC-BUF.
Return the list of `etcc-command' objects.
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

(defun etcc/insert-user-comments (user-id etcc-buf &optional reverse-p)
  "Insert comments posted by USER-ID in the buffer of ETCC-BUF.
If USER-ID is nil, insert all the comments in the current buffer.
If REVERSE-P is non-nil, reverse the order of the comments."
  (let* ((pred (if (not user-id)
                   'identity            ; pickup all comments
                 (lambda (comment)
                   (let ((comment-user-id (etcc-user-id
                                           (etcc-comment-from-user comment))))
                     (equal user-id comment-user-id)))))
         (comments (etcc/filter-comments pred etcc-buf)))
    (if reverse-p
        (setq comments (reverse comments)))
    (etcc/insert-comments comments t)))

(defun etcc/display-user-info (user &optional display-image etcc-buf no-display)
  "Show the user info USER.
If DISPLAY-IMAGE is non-nil,
display the user image as well.
If ETCC-BUF is a buffer, pick up the user's comments from ETCC-BUF
and display them as well.
If NO-DISPLAY is non-nil, do not display the buffer of user info."
  (let ((buf (get-buffer-create etcc-user-info-buffer-name))
        pos)
    (set-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setq buffer-read-only nil)
    (etcc-user-mode)
    (setq etcc-broadcaster user)
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
      (etcc/insert-user-comments (etcc-user-id user) etcc-buf t))
    (goto-char pos)
    (run-hooks 'etcc-display-user-info-hook)
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (unless no-display
      (display-buffer buf))))

(defun etcc-user-at (&optional pos)
  "Return object of `etcc-user' at the position of POS.
POS defaults to the current position.
If the 'etcc-user text property is defined at the position,
return the value of 'etcc-user property.
If the 'etcc-comment text property is defined at the position,
return the value of 'etcc-comment-from-user of the comment.
Otherwise, return the broadcaster of the buffer."
  (or pos (setq pos (point)))
  (or (get-text-property pos 'etcc-user)
      (get-text-property pos 'etcc-broadcaster)
      (let ((comment (get-text-property pos 'etcc-comment)))
        (and comment
             (etcc-comment-from-user comment)))
      (if (boundp 'etcc-broadcaster) etcc-broadcaster)))

(defun etcc-read-user-id (&optional prompt initial-input history)
  "Read an ETCC user id from the minibuffer, prompting with string PROMPT.
INITIAL-INPUT is a string to insert before reading.
HISTORY specifies a history list."
  (or prompt (setq prompt "User Id"))
  (let* ((user (etcc-user-at))
         (default (if (etcc-user-p user) (etcc-user-screen-id user) user))
         (prompt (format (if default "%s (default %s): " "%s: ")
                         prompt default)))
    (read-string prompt initial-input history default)))

(defun etcc-comment-at (&optional pos)
  "Return object of `etcc-comment' at the position of POS.
POS defaults to the current position."
  (get-text-property (or pos (point)) 'etcc-comment))

(defun etcc-broadcaster-p ()
  "Return non-nil if the current user is the broadcaster."
  (etcc-user-equal etcc-current-user etcc-broadcaster))

(defun etcc-display-user-info (&optional display-image)
  "Display the user info at the current point with the posted comments.
If DISPLAY-IMAGE is non-nil, display the user's image, too."
  (interactive "P")
  (let ((user (etcc-user-at)))
    (etcc/display-user-info user display-image
                            (if (eq major-mode 'etcc-mode) (current-buffer)))))

(defun etcc-display-user-info-with-image (&optional no-image)
  "Invert version of `etcc-display-user-info'.
If NO-IMAGE is non-nil, do not display user's image."
  (interactive "P")
  (etcc-display-user-info (not no-image)))

(defun etcc-browse-thumbnail (&optional small-thumbnail)
  "Open web browser and display thumbnail of the movie.
If SMALL-THUMBNAIL is non-nil, display large version of the thumbnail.
Otherwise, display small version of the thumbnail."
  (interactive "P")
  (let* ((etcc-movie (etcc-movie-at))
         (url (if small-thumbnail
                  (etcc-movie-small-thumbnail etcc-movie)
                (etcc-movie-large-thumbnail etcc-movie))))
    (if url
        (etcc-browse-url url)
      (message "no thumbnail provided."))))

(defun etcc-comment-string-simple (etcc-comment)
  "Format the comment string very simply from `etcc-comment' object ETCC-COMMENT."
  (let* ((message (etcc-comment-message etcc-comment))
         (user (etcc-comment-from-user etcc-comment))
         (created (etcc-comment-created etcc-comment)))
    (concat
     (etcc/user-string user t) "  "
     (replace-regexp-in-string "\\\\n" "\n" message))))

(defun etcc-display-comments ()
  "Display all the comments of the movie in a dedicated buffer.
The buffer name is defined by `etcc-comment-buffer-name'."
  (interactive)
  (ensure-etcc-buffer
    (let ((etcc-buf (current-buffer))
          (movie etcc-movie)
          (buf (get-buffer-create etcc-comment-buffer-name)))
      (set-buffer buf)
      (let ((inhibit-read-only t))
        (erase-buffer))
      (setq buffer-read-only nil)
      (insert (etcc-movie-title movie)
              " "
              (format-time-string "%Y-%m-%d(%a)%H:%M"
                                  (etcc-movie-created movie))
              "(" (number-to-string (/ (etcc-movie-duration movie) 60))
              "分)\n"
              (let ((c (etcc-movie-last-owner-comment movie)))
                (if c (concat c "\n") ""))
              "コメント数:" (number-to-string (etcc-movie-comment-count movie))
              " / 最大同時視聴者数:" (number-to-string
                                      (etcc-movie-max-view-count movie))
              "人 / 総視聴者数:" (number-to-string
                                  (etcc-movie-total-view-count movie))
              "人\n"
              (etcc-movie-link movie)
              "\n\n")
      (let ((etcc-comment-string-function #'etcc-comment-string-simple))
        (etcc/insert-user-comments nil etcc-buf))
      (goto-char (point-min))
      (run-hooks 'etcc-display-comments-hook)
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer buf))))

;;;;;;;;

(defun etcc/unescape-comment (str &optional replace-with-whitespace)
  "Unescape the comment string of STR.
If REPLACE-WITH-WHITESPACE is non-nil, some control characters
are replaced with white spaces."
  (cond ((not (stringp str)) str)
        (replace-with-whitespace (replace-regexp-in-string "\\\\[nt]" " " str))
        (t (mapc (lambda (cons)
                   (setq str (replace-regexp-in-string (car cons) (cdr cons)
                                                       str)))
                 '(("\\\\n" . "\n")
                   ("\\\\t" . "\t")))
           str)))

(defun etcc/insert-comment-input-area (movie broadcaster tags)
  "Insert the comment area with MOVIE, BROADCASTER, and TAGS."
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
  "Return the time string for TIME in 'h:mm:ss' or 'mm:ss' format."
  (let ((hour (/ time 60 60))
        (min (mod (/ time 60) 60))
        (sec (mod time 60)))
    (if (> hour 0)
        (format "%d:%2.2d:%2.2d" hour min sec)
      (format "%2.2d:%2.2d" min sec))))

(defun etcc/view-movie (movie broadcaster tags)
  "View the movie of MOVIE, BROADCASTER, and TAGS.
MOVIE is an `etcc-movie' object.
BROADCASTER is an `etcc-user' object for the braodcaster of the movie.
TAGS is a list of tags.
Create the `etcc-mode' buffer and start the comment updater.
If the movie is live, start the movie info updater."
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
    (setq header-line-format etcc-header-line-format)
    (switch-to-buffer buf)))

(cl-defun etcc-view-movie-sentinel (&key data &allow-other-keys)
  (let* ((movie (assoc-default 'movie data))
         (broadcaster (assoc-default 'broadcaster data))
         (tags (assoc-default 'tags data)))
    (let ((etcc-movie (make-etcc-movie-from-alist movie))
          (etcc-user (make-etcc-user-from-alist broadcaster)))
      (etcc/view-movie etcc-movie etcc-user tags))))

(defun etcc-movie-at (&optional pos no-error)
  "Return object of `etcc-movie' at the position of POS.
POS defaults to the current position.
If the 'etcc-movie text property is defined at the position,
return the value of 'etcc-movie property.
Otherwise, return the 'etcc-movie local variable of the buffer.
If NO-ERROR is non-nil, suppress the error when the object is not
found at the position."
  (or pos (setq pos (point)))
  (or (get-text-property pos 'etcc-movie)
      (if (boundp 'etcc-movie) etcc-movie)
      (unless no-error
        (error "Movie not found"))))

(defun etcc-read-movie-id (&optional prompt initial-input history)
  "Read an TwitCast movie id from the minibuffer, prompting with string PROMPT.
INITIAL-INPUT is a string to insert before reading.
HISTORY specifies a history list."
  (or prompt (setq prompt "Movie Id"))
  (let* ((movie (etcc-movie-at nil t))
         (default (if (etcc-movie-p movie) (etcc-movie-id movie) movie))
         (prompt (format (if default "%s (default %s): " "%s: ")
                         prompt default)))
    (read-string prompt initial-input history default)))

(defvar etcc-view-movie-hist nil)

(defun etcc-view-movie (movie-id)
  "View the movie of MOVIE-ID."
  (interactive (list (etcc-read-movie-id "Twitcasting movie Id" nil
                                         'etcc-view-movie-hist)))
  (let ((buf (etcc/buffer movie-id nil t)))
    (if buf
        (switch-to-buffer buf)
      (etcc-api/get-movie-info movie-id :success 'etcc-view-movie-sentinel))))

(defun etcc-view-movie-at-point ()
  "View the movie at the current point."
  (interactive)
  (let* ((movie (etcc-movie-at))
         (movie-id (if (etcc-movie-p movie) (etcc-movie-id movie) movie)))
    (if movie-id
        (etcc-view-movie movie-id)
      (error "No movie id found"))))

(defvar etcc-live-user-id-list nil)

(defun etcc-view-live (user-id)
  "View the live movie of the user USER-ID."
  (interactive (list (etcc-read-user-id "User id to view live" nil
                                        'etcc-live-user-id-list)))
  (etcc-api/get-current-live user-id :success 'etcc-view-movie-sentinel))

(defvar etcc-live-movie-url-list nil)

(defun etcc-view-from-url (url)
  "View Twitcasting live/movie from the given URL."
  (interactive (list (read-string "Twitcasting live/movie URL: " nil
                                  'etcc-live-movie-url-list)))
  (cond ((string-match etcc-twitcasting-live-url-regexp url)
         (let ((user-id (match-string 1 url)))
           (etcc-view-live user-id)))
        ((string-match etcc-twitcasting-movie-url-regexp url)
         (let ((user-id (match-string 1 url))
               (movie-id (match-string 2 url)))
           (etcc-view-movie movie-id)))
        (t (error "Not a twitcasting movie/live URL: %s" url))))

;; (defalias 'etcc 'etcc-view-from-url)

(defun etcc-reload ()
  "Reload the current Twitcasting movie/live."
  (interactive)
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
With a prefix argument (BURY is non-nil), bury the etcc buffer
instead of killing it."
  (interactive "P")
  (ensure-etcc-buffer
    (when (yes-or-no-p "Quit ETCC? ")
      (etcc-stop-movie-info-updater t)
      (etcc-stop-comment-updater t)
      (etcc-stop-hls)
;;       (etcc-stop-download t)
      (message "Bye bye...")
      (sit-for 0.5)
      (if bury
          (bury-buffer)
        (kill-buffer)))))

(defun etcc-open-link ()
  "Open the live/movie link in the external browser."
  (interactive)
  (let ((link (etcc-movie-link (etcc-movie-at))))
    (if link
        (etcc-browse-url link)
      (error "ETCC: link not available"))))

(defun etcc/hls-play (url)
  "Play the HLS movie at URL."
  (let* ((buf (get-buffer-create (format "*ETCC-hls-play:%s*" url)))
         (proc (start-process "mplayer" buf etcc-mplayer-program
                              "-quiet" url)))
    proc))

(defun etcc-play-hls (&optional arg)
  "Start mplayer to play the HTTP live streaming of the current movie.
If ARG is non-nil, start a web browser to play HLS.
If the HLS is not available but the movie is recorded,
start to play the recoreded movie, instead."
  (interactive "P")
  (ensure-etcc-buffer
    (let ((url (or (etcc-movie-hls-url etcc-movie)
                   (and (etcc-movie-is-recorded etcc-movie)
                        (etcc/movie-file-url)))))
      (cond ((not url)
             (message "ETCC: No HTTP live streaming or record available."))
            (arg (etcc-browse-url url))
            ((process-live-p etcc-play-hls-proc)
             (message "ETCC: player already started."))
            (t (message "starting HLS player...")
               (let ((proc (etcc/hls-play url)))
                 (setq etcc-play-hls-proc proc))
               (sit-for 0.5)
               (message "starting HLS player... done"))))))

(defun etcc-stop-hls ()
  "Stop mplayer."
  (interactive)
  (ensure-etcc-buffer
    (if (not (process-live-p etcc-play-hls-proc))
        (etcc-message "HLS player not running.")
      (signal-process etcc-play-hls-proc 'SIGTERM))))

(defun etcc/hls-download (url file)
  "Download the HLS at URL to the file FILE."
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
      (error "HLS download not started"))
    (let ((buf (process-buffer etcc-download-hls-proc)))
      (if (buffer-live-p buf)
          (display-buffer buf)
        (message "Buffer was killed.")))))

(defun etcc-download-hls (file)
  "Start to download the HTTP Live Streaming of the current movie buffer.
The HLS is saved to the file FILE."
  (interactive
   (ensure-etcc-buffer
     (unless (etcc-movie-hls-url etcc-movie)
       (error "ETCC: No HLS available for the movie"))
     (let* ((user-screen-id (etcc-user-screen-id etcc-broadcaster))
            (movie-id (etcc-movie-id etcc-movie))
            (default-filename (concat user-screen-id "_"
                                      movie-id ".mp4")))
       (list (read-file-name "Download HLS: "
                             etcc-download-default-directory default-filename
                             nil default-filename)))))
  (ensure-etcc-buffer
    (if (or (not (file-exists-p file))
            (yes-or-no-p "file exists. overwrite? "))
        (let* ((hls-url (etcc-movie-hls-url etcc-movie))
               (proc (etcc/hls-download hls-url file)))
          (setq etcc-download-hls-proc proc)
          (etcc-display-hls-download-buffer)))))

(defun etcc-stop-hls-download (&optional no-prompt)
  "Stop downloading HLS.
If NO-PROMPT is non-nil, stop the download without prompting user."
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
      (error "Movie file download not started"))
    (let ((buf (process-buffer etcc-download-movie-proc)))
      (if (buffer-live-p buf)
          (display-buffer buf)
        (message "Buffer was killed.")))))

(defun etcc/movie-file-url (&optional user-screen-id movie-id)
  "Return the recorded movie file URL.
USER-SCREEN-ID is the user screen Id of the movie's broadcaster.
MOVIE-ID is the movie Id.  If USER-SCREEN-ID and MOVIE-ID are
nil, These parameters are determined from `etcc-broadcaster' and
`etcc-movie' buffer local variable."
  (unless (and user-screen-id movie-id)
    (ensure-etcc-buffer
      (setq user-screen-id (etcc-user-screen-id etcc-broadcaster))
      (setq movie-id (etcc-movie-id etcc-movie))))
  (format etcc-movie-url-format user-screen-id movie-id))

(defun etcc-download-movie (file)
  "Download the recorded movie file of the current movie buffer.
The movie is saved to the file FILE."
  (interactive
   (ensure-etcc-buffer
     (let* ((user-screen-id (etcc-user-screen-id etcc-broadcaster))
            (movie-id (etcc-movie-id etcc-movie))
            (default-filename (concat "movie_" user-screen-id "_"
                                      movie-id ".mp4")))
       (list (read-file-name "Download Movie: "
                             etcc-download-default-directory default-filename
                             nil default-filename)))))
  (ensure-etcc-buffer
    (if (or (not (file-exists-p file))
            (yes-or-no-p "file exists. overwrite? "))
        (let* ((url (etcc/movie-file-url))
               (proc (etcc/movie-download url file)))
          (setq etcc-download-movie-proc proc)
          (etcc-display-movie-download-buffer)))))

(defun etcc-stop-movie-download (&optional no-prompt)
  "Stop downloading the movie file.
If NO-PROMPT is non-nil, stop the download without prompting user."
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
With a \\[universal-argument] \\[universal-argument] prefix argument ARG,
show the recorded movie file URL and save the URL in the kill ring.
If the movie is recorded or with a prefix argument ARG, start to
download the movie file.  If the movie is a live, start to
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

(defun etcc-stop-download ()
  "Stop download movie file and HLS."
  (interactive)
  (ensure-etcc-buffer
    (etcc-stop-hls-download)
    (etcc-stop-movie-download)))

(defun etcc-display-download-buffer ()
  "Display the buffer of the download process for the current movie."
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
  "Return non-nil if the point is in the region of comment prompt area."
  (< (point) etcc-prompt-end))

(defun etcc/comment-string ()
  "Get comment string in the ETCC buffer."
  (ensure-etcc-buffer
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (etcc-skip-prompt)
        (let ((str (buffer-substring-no-properties (point) etcc-prompt-end)))
          (replace-regexp-in-string "\\`[\n\t ]+\\|[\n\t ]+\\'" "" str))))))

(defun etcc-post-comment ()
  "Post comment after the commen prompt to the live in the current buffer.
The current position should be in the comment prompt area and the
comment should not be empty."
  (interactive)
  (ensure-etcc-buffer
    (let ((comment-string (etcc/comment-string)))
      (cond ((not (etcc-in-comment-prompt-region-p))
             (error "Not in comment prompt region"))
            ((or (not (stringp comment-string))
                 (<= (length comment-string) 0))
             (error "Please enter comment"))
            ((not (etcc-movie-is-live etcc-movie))
             (error "Not a live"))
            (t
             (let ((etcc-movie-id (etcc-movie-id etcc-movie)))
               (message "posting comment...")
               (etcc-api/post-comment etcc-movie-id comment-string
                                      :success (etcc-request-callback (data)
                                                 (message
                                                  "posting comment...done")))
               (setq etcc-last-post comment-string)
               (etcc-kill-comment)))))))

(cl-defun etcc-delete-comment-sentinel (&rest args &key data response
                                              &allow-other-keys)
  "Callback on deleting comment."
  (let* ((comment-id (assoc-default 'comment_id data))
         (url (request-response-url response))
         (movie-id (etcc/movie-id-from-url url)))
    (with-etcc-buffer movie-id
      (save-excursion
        (save-restriction
          (widen)
          (when (etcc-goto-comment (string-to-number comment-id))
            (let ((inhibit-read-only t)
                  (beg (point)))
              (etcc-next-comment 1 t)
              (add-face-text-property beg (point)
                                      'etcc-deleted-comment-face t)))
          (message "deleting comment... done"))))))

(defun etcc-delete-comment ()
  "Delete the comment at the point."
  (interactive)
  (ensure-etcc-buffer
    (let* ((user (etcc-user-at))
           (comment (etcc-comment-at))
           (comment-user (if comment (etcc-comment-from-user comment))))
      (cond ((not comment)
             (error "Not a comment"))
            ((and (not (etcc-user-equal etcc-current-user etcc-broadcaster))
                  (not (etcc-user-equal etcc-current-user comment-user)))
             (error "Not your comment"))
            ((y-or-n-p "Delete comment? ")
             (etcc-api/delete-comment (etcc-movie-id etcc-movie)
                                      (etcc-comment-id comment)
                                      :success 'etcc-delete-comment-sentinel)
             (message "deleting comment..."))))))

(defun etcc-kill-comment ()
  "Kill comment entered and save it in the kill ring.
Then, the prompt is inserted newly."
  (interactive)
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

(defun etcc-mention (&optional keep-position)
  "Insert mention \"@user-screen-id \" at the beginning of the comment.
The user-screen-id is from the user at the current position.
If KEEP-POSITION is non-nil, the cursor position does not change
after inserting the mention. Otherwise, the cursor position is
move to a position after the mention."
  (interactive "P")
  (ensure-etcc-buffer
    (let* ((user (etcc-user-at))
           (user-name (etcc-user-name user))
           (user-screen-id (etcc-user-screen-id user))
           (pos (point)))
      (etcc-beginning-of-buffer)
      (insert "@" user-screen-id " ")
      (if keep-position (goto-char pos))
      (message "mention to: %s" (etcc/user-string user)))))

(defun etcc-bol ()
  "Go to the beginning of line, then skip the prompt, if any."
  (interactive)
  (beginning-of-line)
  (etcc-skip-prompt))

(defun etcc-beginning-of-buffer (&optional arg)
  "Go to the beginning of the buffer, then skip the prompt, if any.
If ARG is non-nil, go to the beginning of the buffer."
  (interactive "P")
  (beginning-of-buffer)
  (unless arg (etcc-bol)))

(defun etcc-goto-last-comment ()
  "Go to the last comment of the movie."
  (interactive)
  (goto-char etcc-comment-region-beg))

(defun etcc-goto-first-comment ()
  "Go to the first comment of the movie."
  (interactive)
  (goto-char etcc-comment-region-end))

(defun etcc-next-comment (&optional arg no-error)
  "Move to the beginning of the next ARG comment.
If NO-ERROR is non-nil, do not raise an error when the target
comment is not found."
  (interactive "^p")
  (etcc-next-regexp etcc-comment-line-regexp "comment" arg no-error))

(defun etcc-previous-comment (&optional arg no-error)
  "Move to the beginning of the previous ARG comment.
If NO-ERROR is non-nil, do not raise an error when the target
comment is not found."
  (interactive "^p")
  (or arg (setq arg 1))
  (etcc-next-comment (- arg) no-error))

(defun etcc-goto-comment (comment-id)
  "Go to the beginning of the comment COMMENT-ID."
  (interactive "n")
  (let ((pos (text-property-any (point-min) (point-max)
                                'etcc-comment-id comment-id)))
    (if pos (goto-char pos))))

(defun etcc-speed-command ()
  "Execute the command with a single key stroke.
If the cursor is at a read-only position and the key defined in
`etcc-speed-commands' is pressed, call the command associated
with the key in `etcc-speed-commands' interactively.
Otherwise, call the command defined in global key binding interactively."
  (interactive)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; etcc mode for searching twitcasting live/movie

(defun etcc-define-keys (mode-map)
  "Common keybind for etcc modes.
MODE-MAP is a keyap."
  (define-key mode-map (kbd "<DEL>") 'scroll-down-command)
  (define-key mode-map " " 'scroll-up-command)
  (define-key mode-map "/" 'etcc-search-live)
  (define-key mode-map "?" 'describe-mode)
  (define-key mode-map "B" 'etcc-search-user)
  (define-key mode-map "C" 'etcc-search-by-category)
  (define-key mode-map "G" 'etcc-display-supporting-at)
  (define-key mode-map "L" 'etcc-view-live)
  (define-key mode-map "M" 'etcc-view-movie)
  (define-key mode-map "N" 'etcc-search-new)
  (define-key mode-map "R" 'etcc-search-recommend)
  (define-key mode-map "S" 'etcc-display-supporter-at)
  (define-key mode-map "T" 'etcc-search-by-tag)
  (define-key mode-map "U" 'etcc-view-from-url)
  (define-key mode-map "W" 'etcc-search-by-word)
  (define-key mode-map "X" 'etcc-display-my-supporting)
  (define-key mode-map "s/" 'etcc-search-live)
  (define-key mode-map "sb" 'etcc-search-user)
  (define-key mode-map "sc" 'etcc-search-by-category)
  (define-key mode-map "sn" 'etcc-search-new)
  (define-key mode-map "sr" 'etcc-search-recommend)
  (define-key mode-map "ss" 'etcc-search-by-word)
  (define-key mode-map "st" 'etcc-search-by-tag)
  (define-key mode-map "su" 'etcc-search-user)
  (define-key mode-map "sw" 'etcc-search-by-word)
  (define-key mode-map "vl" 'etcc-view-live)
  (define-key mode-map "vm" 'etcc-view-movie)
  (define-key mode-map "vs" 'etcc-display-supporter-at)
  (define-key mode-map "vu" 'etcc-view-user)
  (define-key mode-map "vx" 'etcc-display-my-supporting)
  (define-key mode-map "vU" 'etcc-view-from-url))

(defvar etcc-search-mode-map nil)

(unless etcc-search-mode-map
  (setq etcc-search-mode-map (make-sparse-keymap))
  (etcc-define-keys etcc-search-mode-map)
  (define-key etcc-search-mode-map "\C-i" 'etcc-search-next-tag)
  (define-key etcc-search-mode-map "\C-\M-i" 'etcc-search-previous-tag)
  (define-key etcc-search-mode-map "\C-m" 'etcc-search-view-movie)
  (define-key etcc-search-mode-map "\C-c\C-i" 'etcc-search-display-movie-info)
  (define-key etcc-search-mode-map "I" 'etcc-search-display-movie-info)
  (define-key etcc-search-mode-map "Q" 'etcc-search-kill-buffer)
  (define-key etcc-search-mode-map "g" 'etcc-search-refresh)
  (define-key etcc-search-mode-map "i" 'etcc-view-user-at-point)
  (define-key etcc-search-mode-map "l" 'etcc-search-previous-search)
  (define-key etcc-search-mode-map "n" 'etcc-search-next-movie)
  (define-key etcc-search-mode-map "p" 'etcc-search-previous-movie)
  (define-key etcc-search-mode-map "q" 'etcc-search-quit)
  (define-key etcc-search-mode-map "r" 'etcc-search-next-search))

(defun etcc-movie-info-string (movie broadcaster tags)
  "Format the comment string from `etcc-movie' object MOVIE.
BROADCASTER is an `etcc-user' object of the movie's boradcaster.
TAGS is a list of tags."
  (concat
   (let* ((category (etcc-movie-category movie))
          (fmt (if category "%-45s %24s\n" "%s\n")))
     (format fmt
             (concat
              (etcc/time-string (etcc-movie-duration movie))
              " "
              (etcc/user-string broadcaster))
             (etcc/fontify-string (format ":%s:" (etcc-category category))
                                  'etcc-category-face)))
   (format "    %-15s %s\n"
           (concat
            "C:" (number-to-string
                  (etcc-movie-comment-count movie))
            " V:" (number-to-string
                   (etcc-movie-current-view-count movie)))
           (etcc/fontify-string
            (concat (etcc-movie-title movie)
                    (let ((subtitle (etcc-movie-subtitle movie)))
                      (if subtitle
                          (concat " " subtitle))))
            'etcc-movie-title-face))
   "    "
   (if tags
       (mapconcat (lambda (tag)
                    (etcc/fontify-string (format "#%s" tag)
                                         'etcc-tag-face))
                  tags " "))
   "\n"))

(defun etcc/insert-movie-info (movie broadcaster tags movie-info-string-func
                                     &optional no-thumbnail)
  "Insert the live movie info MOVIE into the current position.
MOVIE is an `etcc-movie' object..
BROADCASTER is an `etcc-user' object of the movie's boradcaster.
TAGS is a list of tags.
MOVIE-INFO-STRING-FUNC is a function to be called to insert movie
info string.
If NO-THUMBNAIL is non-nil, do not display the thumbnail of the movie."
  (setq no-thumbnail t)                 ; XXX
  (let ((beg (point))
        (movie-info-str (funcall movie-info-string-func
                                 movie broadcaster tags)))
    (insert movie-info-str)
    (unless no-thumbnail
      (insert "    ")
      (let ((url (etcc-movie-small-thumbnail movie)))
        (if (> (length url) 0)
            (etcc/insert-image-from-url url)
          (insert "[no thumbnail]"))
        (insert"\n")))
    (insert "\n")
    (add-text-properties beg (point) (list 'etcc-movie movie
                                           'etcc-broadcaster broadcaster
                                           'etcc-tags tags))))

(defun etcc/insert-movies (movies)
  "Insert the movie info each of MOVIES.
Called from `etcc/view-movie-list'."
  (mapc (lambda (data)
          (let ((movie (assoc-default 'movie data))
                (broadcaster (assoc-default 'broadcaster data))
                (tags (assoc-default 'tags data)))
            (let ((etcc-movie (make-etcc-movie-from-alist movie))
                  (etcc-user (make-etcc-user-from-alist broadcaster)))
              (etcc/insert-movie-info etcc-movie etcc-user tags
                                      #'etcc-movie-info-string))))
        movies))

(defun etcc/view-movie-list (movies type context insert-movies-func)
  "Insert the movie list MOVIES in a dedicated buffer.
TYPE is the search type that was used to search mvoies.
CONTEXT is the search context that was used to search mvoies.
INSERT-MOVIES-FUNC is a function to be called to insert movie info."
  (let ((buf (get-buffer-create etcc-movie-list-buffer-name)))
    (set-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setq buffer-read-only nil)
    (etcc-search-mode)
    (setq etcc-search-type type)
    (setq etcc-search-context context)
    (setq etcc-search-count (length movies))
    (switch-to-buffer buf)
    (funcall insert-movies-func movies)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (setq header-line-format
          `("TwitCasting Live: " etcc-search-type
            ,(cond ((member etcc-search-type '("tag" "word"))
                    (concat ": " etcc-search-context))
                   ((equal etcc-search-type "category")
                    (concat ": "
                            (let ((sub-category
                                   (assoc-default etcc-search-context
                                                  etcc-category-alist)))
                              (if (etcc-sub-category-p sub-category)
                                  (etcc-sub-category-name sub-category)
                                sub-category)))))
            " (" (:eval (number-to-string etcc-search-count)) " movies)"
            " - "
            ,(format-time-string "%m/%d %H:%M" (current-time))))
;;     (setq header-line-format etcc-header-line-format)
    ))

(defun etcc/parse-url (url &optional downcase allow-newlines)
  "Return cons cell or string from URL.

If URL has parameters such as \"?...\", return a cons cell
of (base-url . qury-list) format, where base-url is url before
the parameters and query-list is a list of query parameters (key val).
If DOWNCASE is non-nil, convert key to downcase.
If ALLOW-NEWLINES, it allows newlines in the parameters.
See `url-parse-query-string' for more details."
  (save-match-data
    (if (string-match "\\(.*\\)\\?\\(.*\\)" url)
        (let ((base-url (match-string 1 url))
              (query (match-string 2 url)))
          (cons base-url
                (url-parse-query-string query downcase allow-newlines)))
      url)))

(cl-defun etcc-search-live-sentinel (&key data response &allow-other-keys)
  (let* ((movies (assoc-default 'movies data))
         (url (etcc/parse-url (request-response-url response)))
         (query (cdr url))
         (type (car (assoc-default "type" query)))
         (context (car (assoc-default "context" query))))
    (if context
        (setq context (decode-coding-string context 'utf-8)))
    (etcc/view-movie-list movies type context #'etcc/insert-movies)
    (message "Searching lives...done - %d movies found" (length movies))))

(defvar etcc-search-type-hist nil)
(defvar etcc-search-tag-context-hist nil)
(defvar etcc-search-word-context-hist nil)
(defvar etcc-search-category-context-hist nil)

(defun etcc-read-search-type (&optional default)
  "Read the search type in the minibuffer.
DEFAULT is a default type."
  (or default (setq default "recommend"))
  (completing-read (format "Search type (default %s): " default)
                   '("tag" "word" "category" "new" "recommend")
                   nil t nil 'etcc-search-type-hist
                   default))

(defvar etcc-categories nil)
(defvar etcc-category-collection nil) ; "id - name (count)" => etcc-sub-category
(defvar etcc-category-alist nil)      ; "id" => etcc-sub-category

(defun etcc-category (id)
  "Return an object of `etcc-sub-category' whose id is ID."
  (let ((cat (assoc-default id etcc-category-alist)))
    (if (etcc-sub-category-p cat)
        (etcc-sub-category-name cat)
      (or cat id))))

(defun etcc-category-string (category)
  "Return category string for CATEGORY.
CATEGORY is either `etcc-sub-category' object or sub category id string."
  (let ((cat (cond
              ((etcc-sub-category-p category)
               category)
              ((stringp category)  ; sub category id
               (assoc-default category etcc-category-alist))
              (t (error "Invalid parameter: %s" category)))))
    (if cat
        (format "%s - %s (%d)"
                (etcc-sub-category-id cat)
                (etcc-sub-category-name cat)
                (etcc-sub-category-count cat))
      category)))

(cl-defun etcc-update-categories-sentinel (&key data &allow-other-keys)
  "Callback for `etcc-update-categories'."
  (let* ((categories (make-etcc-category-list-from-alist
                      (assoc-default 'categories data)))
         collection alist)
    (mapc (lambda (category)
            (mapc (lambda (sub-category)
                    (add-to-list 'collection
                                 (cons (etcc-category-string sub-category)
                                       sub-category))
                    (add-to-list 'alist
                                 (cons (etcc-sub-category-id sub-category)
                                       sub-category)))
                  (etcc-category-sub-categories category)))
          categories)
    (setq etcc-categories (reverse categories)
          etcc-category-collection (reverse collection)
          etcc-category-alist (reverse alist))
    (message "updating categories...done")))

(defun etcc-update-categories (&optional no-wait)
  "Update category list of current live.
If NO-WAIT is non-nil, update asyncronously."
  (interactive "P")
  (let ((response (etcc-api/get-categories
                   :success 'etcc-update-categories-sentinel)))
    (message "updating categories...")
    (unless no-wait
      (while (not (request-response-done-p response))
        (sit-for 0.2))
      (let ((status-code (request-response-status-code response))
            (error-thrown (request-response-error-thrown response)))
        (unless (= status-code 200)
          (error "Unable to update categories: %s %s"
                 (car error-thrown) (cdr error-thrown)))))))

(defun etcc-category-at (&optional pos)
  "Return sub category id at the position of POS.
POS defaults to the current position.
If the 'etcc-sub-category text property is defined at the position,
return the value of `etcc-sub-category-id' of the property.
If the 'etcc-movie text property is defined at the position,
return the value of 'etcc-movie-category of the movie.
Otherwise, return nil."
  (or pos (setq pos (point)))
  (let ((sub-category (get-text-property pos 'etcc-sub-category))
        (movie (or (get-text-property pos 'etcc-movie)
                   (if (boundp 'etcc-movie) etcc-movie))))
    (cond ((etcc-sub-category-p sub-category)
           (etcc-sub-category-id sub-category))
          ((etcc-movie-p movie)
           (etcc-movie-category movie)))))

(defun etcc-read-category (prompt &optional history default no-update)
  "Read a sub category id in the minibuffer, prompting with PROMPT.
HISTORY specifies a history list.
DEFAULT is a default category.
If NO-UPDATE is non-nil, categories are not updated from API."
  (unless no-update
    (etcc-update-categories))
  (if default
      (setq default (etcc-category-string default)))
  (let* ((prompt (format (if default "%s (default %s): " "%s: ")
                         prompt default))
         (selection (completing-read prompt etcc-category-collection nil t nil
                                     history default))
         (cat (assoc-default selection etcc-category-collection)))
    (save-match-data
      (cond ((etcc-sub-category-p cat)
             (etcc-sub-category-id cat))
            ((string-match "^[^ ]+" selection)
             (match-string 0 selection))
            (t selection)))))

(defun etcc-read-search-context (type &optional default)
  "Read a search context of TYPE in he minibuffer.
TYPE specifies the search type, \"tag\", \"word\", or \"category\".
DEFAULT is a default context."
  (if (symbolp type)
      (setq type (symbol-name type)))
  (cond ((member type '("tag" "word"))
         (let* ((fmt (if default
                         "Search live with %s (default %s): "
                       "Search live with %s: "))
                (prompt (format fmt type default))
                (hist (if (equal type "tag")
                          'etcc-search-tag-context-hist
                        'etcc-search-word-context-hist)))
           (read-string prompt nil hist default)))
        ((equal type "category")
         (etcc-read-category "Search live with category"
                             'etcc-search-category-context-hist
                             default))
        (t (error "Unknown type: %s" type))))

(defun etcc-search-live (type context &optional limit no-history)
  "Search the lives of TYPE and CONTEXT.
TYPE specifies the search type and CONTEXT specify the search context.
Prefix arg LIMIT is the max number of movies to be listed
If NO-HISTORY is non-nil, do not record the search query in the
search history."
  (interactive
   (let* ((type (if current-prefix-arg
                    (etcc-read-search-type "recommend")
                  "word"))
          (context (etcc-read-search-context type))
          (limit (if current-prefix-arg
                     (read-number "Limit: " etcc-search-live-limit)
                   etcc-search-live-limit)))
     (list type context limit)))
  (if (symbolp type)
      (setq type (symbol-name type)))
  (unless (and (equal type "category")
               (called-interactively-p 'any))
    (etcc-update-categories t))
  (etcc-api/search-live-movies :type type :context context
                               :limit (or limit etcc-search-live-limit)
                               :success 'etcc-search-live-sentinel)
  (unless no-history
    (etcc-search-history-push (list type context limit)))
  (message "Searching lives..."))

(defun etcc-prefix-numeric-value (arg)
  "Return numeric meaning of raw prefix argument ARG.
Return the value like `prefix-numeric-value' but return nil if ARG is nil."
  (if arg (prefix-numeric-value arg)))

(defun etcc-search-new (&optional limit)
  "Search new live.
Prefix arg LIMIT is the max number of movies to be listed."
  (interactive "P")
  (etcc-search-live 'new nil (etcc-prefix-numeric-value limit)))

(defun etcc-search-recommend (&optional limit)
  "Search recommend live.
Prefix arg LIMIT is the max number of movies to be listed."
  (interactive "P")
  (etcc-search-live 'recommend nil (etcc-prefix-numeric-value limit)))

(defun etcc (&optional arg)
  "Search lives.
Without prefix ARG, search recommend live movies.
With a prefix ARG, call `etcc-search-live' and prompt search
type, query, and limit."
  (interactive "P")
  (if arg
      (call-interactively 'etcc-search-live)
    (etcc-search-recommend)))

(defvar etcc-search-history nil
  "List of search history.
First elemet of the list is the current condition to search.")

(defvar etcc-search-history-forward nil
  "List of search history for forward.")

(defun etcc-search-history-reset ()
  "Reset the search history."
  (setq etcc-search-history nil)
  (setq etcc-search-history-forward nil))

(defun etcc-search-history-push (elem)
  "Push search query ELEM to the search hisotyr."
  (setq etcc-search-history-forward nil)
  (setq etcc-search-history (cons elem etcc-search-history)))

(defun etcc-search-history-current ()
  "Return the current search."
  (car etcc-search-history))

(defun etcc-search-history-previous (&optional count)
  "Return the previous COUNT search query."
  (or count (setq count 1))
  (if (<= count 0)
      (etcc-search-history-current)
    (let (elem)
      (while (and (> count 0) (cdr etcc-search-history))
	(setq elem (car etcc-search-history))
	(setq etcc-search-history (cdr etcc-search-history))
	(setq etcc-search-history-forward
              (cons elem etcc-search-history-forward))
	(setq count (1- count)))
      (if elem
	  (etcc-search-history-current)))))

(defun etcc-search-history-next (&optional count)
  "Return the next COUNT search query."
  (or count (setq count 1))
  (if (<= count 0)
      (etcc-search-history-current)
    (let (elem)
      (while (and (> count 0) etcc-search-history-forward)
	(setq elem (car etcc-search-history-forward))
	(setq etcc-search-history-forward (cdr etcc-search-history-forward))
	(setq etcc-search-history (cons elem etcc-search-history))
	(setq count (1- count)))
      elem)))

(defun etcc-search-previous-search (&optional count)
  "Perform previous search.
If COUNT is a positive number, move backward COUNT times in the history.
If COUNT is a negative number, moving forward is performed."
  (interactive "p")
  (or count (setq count 1))
  (let (func his)
    (if (>= count 0)
	(setq func #'etcc-search-history-previous)
      (setq func #'etcc-search-history-next)
      (setq count (- count)))
    (setq his (funcall func count))
    (if his
	(etcc-search-live (nth 0 his) (nth 1 his) (nth 2 his) t)
      (error "No more history"))))

(defun etcc-search-next-search (&optional count)
  "Perform next COUNT search.  See also `etcc-search-previous-search'."
  (interactive "p")
  (or count (setq count 1))
  (etcc-search-previous-search (- count)))

  
(defun etcc-tag-at (&optional pos)
  "Return the tag at the point POS."
  (or pos (setq pos (point)))
  (save-excursion
    (goto-char pos)
    (save-match-data
      (if (or (looking-at etcc-tag-regexp)
              (and (re-search-backward "\\(^\\| \\)" nil t)
                   (progn
                     (if (equal (match-string 1) " ")
                         (forward-char))
                     t)
                   (looking-at etcc-tag-regexp)))
          (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

(defun etcc-search-by-tag (context &optional limit)
  "Search live by tags CONTEXT.
Prefix arg LIMIT is the max number of movies to be listed."
  (interactive (list (etcc-read-search-context 'tag (etcc-tag-at))
                     (etcc-prefix-numeric-value current-prefix-arg)))
  (etcc-search-live 'tag context limit))

(defun etcc-search-by-word (context &optional limit)
  "Search live by words CONTEXT.
Prefix arg LIMIT is the max number of movies to be listed."
  (interactive (list (etcc-read-search-context 'word)
                     (etcc-prefix-numeric-value current-prefix-arg)))
  (etcc-search-live 'word context limit))

(defun etcc-search-by-category (context &optional limit)
  "Search live by category CONTEXT.
Prefix arg LIMIT is the max number of movies to be listed."
  (interactive (list (etcc-read-search-context 'category (etcc-category-at))
                     (etcc-prefix-numeric-value current-prefix-arg)))
  (etcc-search-live 'category context limit))

(defun etcc-search-mode ()
  "Major mode for Emacs Twitcasting Client - search movies.

\\{etcc-search-mode-map}"
  (interactive)
  (set (make-local-variable 'etcc-search-no-thumbnail) nil)
  (set (make-local-variable 'etcc-search-type) nil)
  (set (make-local-variable 'etcc-search-context) nil)
  (set (make-local-variable 'etcc-search-count) nil)
  (setq major-mode 'etcc-search-mode)
  (setq mode-name "ETCC-Search")
  (make-local-variable 'kill-buffer-hook)
  (use-local-map etcc-search-mode-map)
  (run-hooks 'etcc-search-mode-hook))

(defun etcc-search-refresh (&optional limit)
  "Search the live with the current conditions in the buffer.
Prefix arg LIMIT is the max number of movies to be listed."
  (interactive "P")
  (etcc-search-live etcc-search-type etcc-search-context
                    (etcc-prefix-numeric-value limit)))

(defun etcc-search-view-movie ()
  "Start to view the movie at the current point."
  (interactive)
  (let* ((movie (get-text-property (point) 'etcc-movie))
         (broadcaster (get-text-property (point) 'etcc-broadcaster))
         (tags (get-text-property (point) 'etcc-tags))
         (buf (etcc/buffer movie nil t)))
    (if buf
        (switch-to-buffer buf)
      (etcc/view-movie movie broadcaster tags))))

(defun etcc-search-display-movie-info (&optional display-thumbnail)
  "Show the movie info in the current point.
If DISPLAY-THUMBNAIL is non-nil,
display the thumbnail image as well.
With a \\[universal-argument] \\[universal-argument prefix
argument, display the small size thumbnail."
  (interactive "P")
;;   (ensure-etcc-search-buffer
  (let ((movie (get-text-property (point) 'etcc-movie))
        (broadcaster (get-text-property (point) 'etcc-broadcaster))
        (tags (get-text-property (point) 'etcc-tags)))
      (etcc/display-movie-info movie broadcaster tags display-thumbnail)))

(defun etcc-search-next-movie (&optional arg no-error)
  "Move to the beginning of the next ARG movie.
If NO-ERROR is non-nil, do not raise an error when the target
movie is not found."
  (interactive "^p")
  (etcc-next-regexp etcc-movie-line-regexp "movie" arg no-error))

(defun etcc-search-previous-movie (&optional arg)
  "Move to the beginning of the ARG previous movie."
  (interactive "^p")
  (or arg (setq arg 1))
  (etcc-search-next-movie (- arg)))

(defun etcc-search-next-tag (&optional arg)
  "Go to the next ARG tag."
  (interactive "^p")
  (or arg (setq arg 1))
  (let ((pos (point)))
    (cond ((> arg 0)
           (if (looking-at etcc-tag-regexp)
               (setq arg (1+ arg)))
           (while (> arg 0)
             (cond ((re-search-forward etcc-tag-regexp nil t)
                    (setq pos (match-beginning 0))
                    (setq arg (1- arg)))
                   (t
                    (message "No more tags")
                    (setq arg 0)))))
          ((< arg 0)
           (while (< arg 0)
             (cond ((re-search-backward etcc-tag-regexp nil t)
                    (setq pos (point))
                    (setq arg (1+ arg)))
                   (t
                    (message "No more tags")
                    (setq arg 0))))))
    (goto-char pos)))

(defun etcc-search-previous-tag (&optional arg)
  "Go to the previous ARG tag."
  (interactive "^p")
  (or arg (setq arg 1))
  (etcc-search-next-tag (- arg)))

(defun etcc-search-kill-buffer ()
  "Exit ETCC Search buffer."
  (interactive)
  (if (yes-or-no-p "Quit ETCC and kill the buffer? ")
      (kill-buffer)))

(defun etcc-search-quit (&optional kill-buffer)
  "Bury ETCC Search buffer.
With a prefix argument KILL-BUFFER, kill the etcc search buffer."
  (interactive "P")
  (if kill-buffer
      (etcc-search-kill-buffer)
    (bury-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar etcc-search-user-mode-map nil)

(unless etcc-search-user-mode-map
  (setq etcc-search-user-mode-map (make-sparse-keymap))
  (etcc-define-keys etcc-search-user-mode-map)
  (define-key etcc-search-user-mode-map "\C-m" 'etcc-view-user-at-point)
;;   (define-key etcc-search-user-mode-map "I" 'etcc-search-user-display-movie-info)
  (define-key etcc-search-user-mode-map "Q" 'etcc-search-user-kill-buffer)
  (define-key etcc-search-user-mode-map "g" 'etcc-search-user-refresh)
  (define-key etcc-search-user-mode-map "i" 'etcc-view-user-at-point)
  (define-key etcc-search-user-mode-map "n" 'etcc-search-user-next-user)
  (define-key etcc-search-user-mode-map "p" 'etcc-search-user-previous-user)
  (define-key etcc-search-user-mode-map "q" 'etcc-search-user-quit))

(defun etcc/insert-user-info (user)
  "Insert the user info USER into the current position.
USER is an `etcc-user' object."
  (let ((beg (point))
        (live-string (etcc/fontify-string "[LIVE]" 'etcc-live-face)))
    (let ((fmt (if (etcc-user-is-live user) "%-31s %s" "%s")))
      (insert (format fmt (etcc/user-string user) live-string)))
    (insert "\n    ")
    (if (etcc-supporter-user-p user)
        (insert (format "P:%d/%d "
                        (etcc-supporter-user-point user)
                        (etcc-supporter-user-total-point user))))
    (insert (format "L:%d" (etcc-user-level user)))
    (let ((profile (etcc-user-profile user)))
      (if (> (length profile) 0)
          (insert " | " (replace-regexp-in-string "\n" " " profile))))
    (insert "\n\n")
    (add-text-properties beg (point) (list 'etcc-user user))))

(defun etcc/insert-users (users constructor)
  "Insert the user info each of USERS.
CONSTRUCTOR is a function to create `etcc-user' or `etcc-supporter-user'
object from alist."
  (mapc (lambda (user)
          (let ((etcc-user (funcall constructor user)))
            (etcc/insert-user-info etcc-user)))
        users))

(defun etcc/view-user-list (users words lang)
  "Insert the user list USERS in a dedicated buffer.
WORDS is the search word that was used to search users.
LANG is the search language that was used to search users."
  (let ((buf (get-buffer-create etcc-user-list-buffer-name)))
    (set-buffer buf)
    (let ((inhibit-read-only t))
      (erase-buffer))
    (setq buffer-read-only nil)
    (etcc-search-user-mode)
    (setq etcc-search-user-words words)
    (setq etcc-search-user-lang lang)
    (setq etcc-search-user-count (length users))
    (switch-to-buffer buf)
    (etcc/insert-users users #'make-etcc-user-from-alist)
    (goto-char (point-min))
    (set-buffer-modified-p nil)
    (setq buffer-read-only t)
    (setq header-line-format
          '("TwitCasting search user: " etcc-search-user-words
            " (" (:eval (number-to-string etcc-search-user-count)) " users)"))
;;     (setq header-line-format etcc-header-line-format)
    ))

(cl-defun etcc-search-user-sentinel (&key data response &allow-other-keys)
  (let* ((users (assoc-default 'users data))
         (url (etcc/parse-url (request-response-url response)))
         (query (cdr url))
         (words (car (assoc-default "words" query)))
         (lang (car (assoc-default "lang" query))))
    (if words
        (setq words (decode-coding-string words 'utf-8)))
    (etcc/view-user-list users words lang)
    (message "Searching users...done - %d users found" (length users))))

(defvar etcc-search-user-words-hist nil)

(defun etcc-search-user (words &optional limit)
  "Search the users with WORDS.
Prefix arg LIMIT is the max number of users to be listed."
  (interactive (list (etcc-read-user-id "Search User" nil
                                        'etcc-search-user-words-hist)
                     (if current-prefix-arg
                         (read-number "Limit: " etcc-search-user-limit)
                       etcc-search-user-limit)))
  (etcc-api/search-users words
                         :limit (or limit etcc-search-user-limit)
                         :success 'etcc-search-user-sentinel)
  (message "Searching users..."))

(defun etcc-search-user-mode ()
  "Major mode for Emacs Twitcasting Client - Search Words.

\\{etcc-search-user-mode-map}"
  (interactive)
  (set (make-local-variable 'etcc-search-user-words) nil)
  (set (make-local-variable 'etcc-search-user-lang) nil)
  (set (make-local-variable 'etcc-search-user-count) nil)
  (setq major-mode 'etcc-search-user-mode)
  (setq mode-name "ETCC-Search-User")
  (use-local-map etcc-search-user-mode-map)
  (run-hooks 'etcc-search-user-mode-hook))

(defun etcc-search-user-refresh (&optional limit)
  "Search the user with the current conditions in the buffer again.
Prefix arg LIMIT is the max number of movies to be listed."
  (interactive "P")
  (etcc-search-user etcc-search-user-words
                    (etcc-prefix-numeric-value limit)))

(defun etcc-search-user-view-live ()
  "Start to view the user's live movie at the current point."
  (interactive)
  (let ((user (get-text-property (point) 'etcc-user)))
    (etcc-view-live (etcc-user-screen-id user))))

(defun etcc-search-user-display-user-info (&optional display-thumbnail)
  "Show the user info in the current point.
If DISPLAY-THUMBNAIL is non-nil,
display the thumbnail image as well.
With a \\[universal-argument] \\[universal-argument] prefix argument,
display the small size thumbnail."
  (interactive "P")
  (let ((user (get-text-property (point) 'etcc-user)))
    (etcc/display-user-info user display-thumbnail)))

(defun etcc-search-user-next-user (&optional arg no-error)
  "Move to the beginning of the next ARG user.
If NO-ERROR is non-nil, do not raise an error when the target
user is not found."
  (interactive "^p")
  (etcc-next-regexp etcc-user-line-regexp "user" arg no-error))

(defun etcc-search-user-previous-user (&optional arg)
  "Move to the beginning of the previous ARG user.
If NO-ERROR is non-nil, do not raise an error when the target
user is not found."
  (interactive "^p")
  (or arg (setq arg 1))
  (etcc-search-user-next-user (- arg)))

(defun etcc-search-user-kill-buffer ()
  "Exit ETCC Search-User buffer."
  (interactive)
  (if (yes-or-no-p "Quit ETCC and kill the buffer? ")
      (kill-buffer)))

(defun etcc-search-user-quit (&optional kill-buffer)
  "Bury ETCC Search-User buffer.
With a prefix argument KILL-BUFFER, kill the etcc search-user buffer."
  (interactive "P")
  (if kill-buffer
      (etcc-search-user-kill-buffer)
    (bury-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar etcc-supporter-mode-map nil)

(unless etcc-supporter-mode-map
  (setq etcc-supporter-mode-map (make-sparse-keymap))
  (etcc-define-keys etcc-supporter-mode-map)
  (define-key etcc-supporter-mode-map "\C-m" 'etcc-view-user-at-point)
  (define-key etcc-supporter-mode-map "\C-c\C-n" 'etcc-display-supporter-next)
  (define-key etcc-supporter-mode-map "Q" 'etcc-search-user-kill-buffer)
  (define-key etcc-supporter-mode-map "g" 'etcc-supporter-refresh)
  (define-key etcc-supporter-mode-map "i" 'etcc-view-user-at-point)
  (define-key etcc-supporter-mode-map "n" 'etcc-supporter-next-user)
  (define-key etcc-supporter-mode-map "p" 'etcc-supporter-previous-user)
  (define-key etcc-supporter-mode-map "q" 'etcc-search-user-quit))

(defun etcc/display-sup-list (users offset &optional append)
  "Insert the supporter list USERS in the current buffer.
OFFSET is the offset of the list.
If APPEND is non-nil, insert the list at the end of the buffer.
Otherwise, erase the buffer and insert the list."
  (if append
      (goto-char (point-max))
    (let ((inhibit-read-only t))
      (erase-buffer)))
  (setq buffer-read-only nil)
  (setq etcc-offset (cond ((= (length users) 0) nil)
                          (offset)
                          (t "0")))
  (save-excursion
    (etcc/insert-users users #'make-etcc-supporter-user-from-alist))
  (set-buffer-modified-p nil)
  (setq buffer-read-only t))

(defun etcc/display-supporter-list (users user-id total offset sort
                                          &optional append)
  "Insert the supporter list USERS of USER-ID in the dedicated buffer.
TOTAL is the total number of users.
OFFSET is the offset of the list.
SORT specifies the order of list, ether of \"new\" or \"ranking\".
If APPEND is non-nil, insert the list at the end of the buffer."
  (let ((buf (get-buffer-create etcc-supporter-list-buffer-name)))
    (set-buffer buf)
    (etcc-supporter-mode)
    (setq etcc-total total)
    (setq etcc-broadcaster user-id)
    (setq etcc-sort (or sort "new"))
;;     (switch-to-buffer buf)
    (display-buffer buf)
    (etcc/display-sup-list users offset append)
    (setq header-line-format
          '("TwitCasting supporter of "
            (:eval (etcc/user-string etcc-broadcaster))
            " (" (:eval (number-to-string etcc-total)) " supporters)"
            " [" etcc-sort "]")))
  (message "Fetching supporters...done - %d supporters fetched"
           (length users)))

(defun etcc-display-sup-sentinel (data response user-type display-list-func
                                       &optional append)
  "Callback for `etcc-display-supporter' and `etcc-display-supporting'.
DATA and RESPONSE are data and response from API.
USER-TYPE is either 'supporters or 'supporting.
DISPLAY-LIST-FUNC is the function to be called to display user list.
If APPEND is non-nil, insert the list at the end of the buffer."
  (let* ((total (assoc-default 'total data))
         (users (assoc-default user-type data))
         (url (etcc/parse-url (request-response-url response)))
         (base-url (car url))
         (query (cdr url))
         (user-id (etcc/user-id-from-url base-url))
         (offset (car (assoc-default "offset" query)))
         (sort (car (assoc-default "sort" query))))
    (funcall display-list-func users user-id total offset sort append)))

(cl-defun etcc-display-supporter-sentinel (&key data response &allow-other-keys)
  (etcc-display-sup-sentinel data response 'supporters
                             #'etcc/display-supporter-list))

(defvar etcc-display-supporter-hist nil)

(defun etcc-display-supporter (user &optional sort)
  "View the supporter list of the user USER-ID.
USER is a user Id or user screen Id.
If SORT is nil, list the supporters in the order of new.
If SORT is non-nil, list the supporters in the order of ranking."
  (interactive (list (etcc-read-user-id nil nil 'etcc-display-supporter-hist)
                     current-prefix-arg))
  (let ((user-id (if (etcc-user-p user)
                     (etcc-user-screen-id user)
                   user))
        (sort (cond ((stringp sort) sort)
                    (sort "ranking")
                    (t "new"))))
    (etcc-api/supporter-list user-id :offset 0
                             :limit etcc-supporter-list-limit :sort sort
                             :success 'etcc-display-supporter-sentinel)
    (message "Fetching supporters...")))

(cl-defun etcc-display-supporter-next-sentinel (&key data response
                                                     &allow-other-keys)
  (etcc-display-sup-sentinel data response 'supporters
                             #'etcc/display-supporter-list t))

(defun etcc-display-supporter-next ()
  "Get the next bulk of supporter list and display it."
  (interactive)
  (if (not etcc-offset)
      (error "No more supporters"))
  (let ((offset (+ (string-to-number etcc-offset)
                   etcc-supporter-list-limit)))
    (etcc-api/supporter-list etcc-broadcaster
                             :offset offset
                             :limit etcc-supporter-list-limit
                             :sort etcc-sort
                             :success 'etcc-display-supporter-next-sentinel)
    (message "Fetching next supporters...")))

(defun etcc-display-supporter-at (&optional sort)
  "View the supporter list of the user at the point.
If SORT is nil, list the supporters in the order of new.
If SORT is non-nil, list the supporters in the order of ranking."
  (interactive "P")
  (etcc-display-supporter (etcc-user-at) sort))

(defun etcc-supporter-mode ()
  "Major mode for Emacs Twitcasting Client - View supporters.

\\{etcc-supporter-mode-map}"
  (interactive)
  (set (make-local-variable 'etcc-total) nil)
  (set (make-local-variable 'etcc-broadcaster) nil)
  (set (make-local-variable 'etcc-offset) nil)
  (set (make-local-variable 'etcc-sort) nil)
  (setq major-mode 'etcc-supporter-mode)
  (setq mode-name "ETCC-Supporter")
  (use-local-map etcc-supporter-mode-map)
  (run-hooks 'etcc-supporter-mode-hook))

(defun etcc-supporter-refresh (&optional other-sort)
  "View the supporter with the current conditions in the buffer again.
If OTHER-SORT is non-nil, list the supporters in the order of the other key."
  (interactive "P")
  (let ((sort (cond ((not other-sort) etcc-sort)
                    ((equal etcc-sort "new") "ranking")
                    (t "new"))))
    (etcc-display-supporter etcc-broadcaster sort)))

(defun etcc-supporter-next-user (&optional arg)
  "Move to the beginning of the next ARG user."
  (interactive "^p")
  (condition-case err
      (etcc-next-regexp etcc-user-line-regexp "user" arg)
    (error
     (if (> arg 0)
         (etcc-display-supporter-next)))))

(defun etcc-supporter-previous-user (&optional arg)
  "Move to the beginning of the previous ARG user."
  (interactive "^p")
  (or arg (setq arg 1))
  (etcc-search-user-next-user (- arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar etcc-supporting-mode-map nil)

(unless etcc-supporting-mode-map
  (setq etcc-supporting-mode-map (make-sparse-keymap))
  (etcc-define-keys etcc-supporting-mode-map)
  (define-key etcc-supporting-mode-map "\C-m" 'etcc-view-user-at-point)
  (define-key etcc-supporting-mode-map "\C-c\C-n" 'etcc-supporting-next-list)
  (define-key etcc-supporting-mode-map "Q" 'etcc-supporting-kill-buffer)
  (define-key etcc-supporting-mode-map "g" 'etcc-supporting-refresh)
  (define-key etcc-supporting-mode-map "i" 'etcc-view-user-at-point)
  (define-key etcc-supporting-mode-map "n" 'etcc-supporting-next-user)
  (define-key etcc-supporting-mode-map "p" 'etcc-supporting-previous-user)
  (define-key etcc-supporting-mode-map "q" 'etcc-search-user-quit))

(defun etcc/display-supporting-list (users user-id total offset sort
                                           &optional append)
  "Insert the supporting user list USERS of USER-ID in the dedicated buffer.
TOTAL is the total number of users.
OFFSET is the offset of the list.
SORT is a dummy parameter, not used in this function.
If APPEND is non-nil, insert the list at the end of the buffer."
  (let ((buf (get-buffer-create etcc-supporting-list-buffer-name)))
    (set-buffer buf)
    (etcc-supporting-mode)
    (setq etcc-total total)
    (setq etcc-broadcaster user-id)
;;     (switch-to-buffer buf)
    (display-buffer buf)
    (etcc/display-sup-list users offset append)
    (setq header-line-format
          '("TwitCasting supporting users of "
            (:eval (etcc/user-string etcc-broadcaster))
            " (" (:eval (number-to-string etcc-total)) " users)")))
  (message "Fetching supporting...done - %d supporting users fetched"
           (length users)))

(cl-defun etcc-display-supporting-sentinel (&key data response &allow-other-keys)
  (etcc-display-sup-sentinel data response 'supporting
                             #'etcc/display-supporting-list))

(defvar etcc-display-supporting-hist nil)

(defun etcc-display-supporting (user)
  "View the supporting user list of the user USER.
USER is an `etcc-user' object, user Id, or user screen Id."
  (interactive (list (etcc-read-user-id nil nil 'etcc-display-supporting-hist)))
  (let ((user-id (if (etcc-user-p user)
                     (etcc-user-screen-id user)
                   user)))
    (etcc-api/supporting-list user-id :offset 0
                              :limit etcc-supporting-list-limit
                              :success 'etcc-display-supporting-sentinel)
    (message "Fetching supportings...")))

(cl-defun etcc-supporting-next-list-sentinel (&key data response
                                                   &allow-other-keys)
  (etcc-display-sup-sentinel data response 'supporting
                             #'etcc/display-supporting-list t))

(defun etcc-supporting-next-list ()
  "Get the next bulk of supporting list and display it."
  (interactive)
  (if (not etcc-offset)
      (error "No more supporting users"))
  (let ((offset (+ (string-to-number etcc-offset)
                   etcc-supporting-list-limit)))
    (etcc-api/supporting-list etcc-broadcaster
                              :offset offset
                              :limit etcc-supporting-list-limit
                              :success 'etcc-supporting-next-list-sentinel)
    (message "Fetching next supportings...")))

(defun etcc-display-supporting-at ()
  "View the supporting list of the user at the point."
  (interactive)
  (etcc-display-supporting (etcc-user-at)))

(defun etcc-display-my-supporting ()
  "Display the current user's supporting users."
  (interactive)
  (unless etcc-current-user
    (etcc-auth t)
    (while (not etcc-current-user)
      (sleep-for 0.2)))
  (etcc-display-supporting etcc-current-user))

(defun etcc-supporting-mode ()
  "Major mode for Emacs Twitcasting Client - View supporting users.

\\{etcc-supporting-mode-map}"
  (interactive)
  (set (make-local-variable 'etcc-total) nil)
  (set (make-local-variable 'etcc-broadcaster) nil)
  (set (make-local-variable 'etcc-offset) nil)
;;   (set (make-local-variable 'etcc-sort) nil)
  (setq major-mode 'etcc-supporting-mode)
  (setq mode-name "ETCC-Supporting")
  (use-local-map etcc-supporting-mode-map)
  (run-hooks 'etcc-supporting-mode-hook))

(defun etcc-supporting-refresh ()
  "View the supporting users in the buffer again."
  (interactive)
  (etcc-display-supporting etcc-broadcaster))

(defun etcc-supporting-next-user (&optional arg)
  "Move to the beginning of the next ARG user."
  (interactive "^p")
  (condition-case err
      (etcc-next-regexp etcc-user-line-regexp "user" arg)
    (error
     (if (> arg 0)
         (etcc-supporting-next-list)))))

(defun etcc-supporting-previous-user (&optional arg)
  "Move to the beginning of the previous ARG user."
  (interactive "^p")
  (or arg (setq arg 1))
  (etcc-search-user-next-user (- arg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar etcc-user-mode-map nil)

(unless etcc-user-mode-map
  (setq etcc-user-mode-map (make-sparse-keymap))
  (etcc-define-keys etcc-user-mode-map)
  (define-key etcc-user-mode-map "\C-c\C-i" 'etcc-display-movie-info-at-point)
  (define-key etcc-user-mode-map "\C-c\C-o" 'etcc-open-link)
  (define-key etcc-user-mode-map "\C-c\C-t" 'etcc-browse-thumbnail)
  (define-key etcc-user-mode-map "\C-m" 'etcc-view-movie-at-point)
  (define-key etcc-user-mode-map "I" 'etcc-display-movie-info-at-point)
  (define-key etcc-user-mode-map "g" 'etcc-user-refresh)
  (define-key etcc-user-mode-map "n" 'etcc-user-next-movie)
  (define-key etcc-user-mode-map "p" 'etcc-user-previous-movie)
  (define-key etcc-user-mode-map "q" 'bury-buffer))

(defun etcc-user-mode ()
  "Major mode for Emacs Twitcasting Client - view user info and movies.

\\{etcc-user-mode-map}"
  (interactive)
  (set (make-local-variable 'etcc-broadcaster) nil)
  (setq major-mode 'etcc-user-mode)
  (setq mode-name "ETCC-user")
;;   (make-local-variable 'kill-buffer-hook)
  (use-local-map etcc-user-mode-map)
  (run-hooks 'etcc-user-mode-hook))

(defun etcc-user-refresh (&optional no-image)
  "View the user info and movies in the buffer again.
If NO-IMAGE is non-nil, do not display user's image."
  (interactive "P")
  (let ((user-id (etcc-user-screen-id etcc-broadcaster)))
    (etcc-view-user user-id no-image)))

(defun etcc-user-next-movie (&optional arg no-error)
  "Move to the beginning of the next ARG user movie.
If NO-ERROR is non-nil, do not raise an error when the target
user movie is not found."
  (interactive "^p")
  (etcc-next-regexp etcc-user-movie-line-regexp "user movie" arg no-error))

(defun etcc-user-previous-movie (&optional arg)
  "Move to the beginning of the previous ARG user movie.
If NO-ERROR is non-nil, do not raise an error when the target
user movie is not found."
  (interactive "^p")
  (or arg (setq arg 1))
  (etcc-user-next-movie (- arg)))

(defun etcc-user-movie-info-string (movie broadcaster tags)
  "Format the movie info string from `etcc-movie' object MOVIE.
BROADCASTER is an `etcc-user' object of the movie's boradcaster.
TAGS is a list of tags."
  (concat (format-time-string "%Y-%m-%d %a %H:%M "
                              (etcc-movie-created movie))
          "(" (number-to-string (/ (+ (etcc-movie-duration movie) 59) 60)) "分)"
          (if (etcc-movie-is-live movie)
              (concat " " (etcc/fontify-string "[LIVE]" 'etcc-live-face)))
          (if (etcc-movie-is-recorded movie)
              (concat " " (etcc/fontify-string "[REC]" 'etcc-recorded-face)))
          (if (etcc-movie-is-protected movie)
              (concat " " (etcc/fontify-string "[PWD]" 'etcc-protected-face)))
          " "  (etcc/fontify-string (or (etcc-movie-subtitle movie)
                                        (etcc-movie-title movie)
                                        "[no title]")
                                    'etcc-movie-title-face)
          "\n"
          (format "    C:%d | V:%d/%d"
                  (etcc-movie-comment-count movie)
                  (max (etcc-movie-current-view-count movie)
                       (etcc-movie-max-view-count movie))
                  (etcc-movie-total-view-count movie))
          (let ((c (etcc/unescape-comment
                    (etcc-movie-last-owner-comment movie) t)))
            (if c (concat " | " (etcc/fontify-string
                                 c 'etcc-last-owner-comment-face))))
          "\n"))

;; (defun etcc/insert-movies-by-user (movies broadcaster)
(defun etcc/insert-user-movies (movies broadcaster)
  "Insert user movies MOVIES at the current position.
BROADCASTER is an `etcc-user' object of broadcaster."
  (mapc (lambda (movie)
          (let ((etcc-movie (make-etcc-movie-from-alist movie)))
            (etcc/insert-movie-info etcc-movie broadcaster nil
                                    #'etcc-user-movie-info-string)))
        movies))

(cl-defun etcc-get-movies-by-user-sentinel (&key data response
                                                 &allow-other-keys)
  (let* ((total-count (assoc-default 'total_count data))
         (movies (assoc-default 'movies data))
         (url (request-response-url response))
         (user-id (etcc/user-id-from-url url)))
    (let ((buf (get-buffer etcc-user-info-buffer-name)))
      (set-buffer buf)
      (setq buffer-read-only nil)
      (goto-char (point-max))
      (insert "\n")
      (save-excursion
        (etcc/insert-user-movies movies etcc-broadcaster))
      (if (= (length movies) 0)
          (insert "...no movies...\n"))
      (set-buffer-modified-p nil)
      (setq buffer-read-only t)
      (display-buffer buf))
    (message "Getting movies by user %s...done - %d movies found"
             user-id (length movies))))

(defun etcc/view-user (etcc-user &optional no-image)
  "Call the API to view user info of ETCC-USER and user's movie list.
If NO-IMAGE is non-nil, do not display the user's image."
  (etcc/display-user-info etcc-user (not no-image) nil t)
  (let ((user-id (etcc-user-screen-id etcc-user)))
    (message "Getting movies by user %s..." user-id)
    (etcc-api/get-movies-by-user user-id
                                 :offset 0
                                 :limit 50 ; XXX
                                 :success 'etcc-get-movies-by-user-sentinel)))

(cl-defun etcc-get-user-info-sentinel (&key data response (no-image nil)
                                            &allow-other-keys)
  (let* ((user (assoc-default 'user data))
         (supporter-count (assoc-default 'supporter_count data))
         (supporting-count (assoc-default 'supporting_count data))
         (etcc-user (make-etcc-user-from-alist
                     user supporter-count supporting-count)))
    (message "Getting user info of %s..." (etcc-user-screen-id etcc-user))
    (etcc/view-user etcc-user no-image)))

(cl-defun etcc-get-user-info-sentinel/no-image (&key data response
                                                     &allow-other-keys)
  (etcc-get-user-info-sentinel :data data :response response
                               :no-image t))

(defun etcc-view-user (user &optional no-image)
  "View the user info of USER with listing user's movies.
USER is an `etcc-user' object, user ID, or user screen ID.
If NO-IMAGE is non-nil, do not display the user's image."
  (interactive (list (etcc-read-user-id)
                     current-prefix-arg))
  (let ((user-id (if (etcc-user-p user)
                     (etcc-user-screen-id user)
                   user))
        (callback (if no-image
                      'etcc-get-user-info-sentinel/no-image
                    'etcc-get-user-info-sentinel)))
    (etcc-api/get-user-info user-id
                            :success callback)
    (message "Getting user info of %s..." user-id)))

(defun etcc-view-user-at-point (&optional no-image)
  "Display the info and movie list of the user at the point.
If NO-IMAGE is non-nil, do not display the user's image."
  (interactive "P")
  (etcc-view-user (etcc-user-at) no-image))

(provide 'etcc)

;;; etcc.el ends here
