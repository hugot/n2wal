;; -*- lexical-binding: t; -*-
(require 'json)
(require 'url)
(require 'mm-decode)
(require 'cl)

(defmacro n2wal-with-json-preset (&rest body)
  "Execute BODY with preferred json settings"
  (declare (indent 0) (debug t))
  `(let ((json-key-type 'symbol)
         (json-array-type 'list)
         (json-object-type 'alist))
     ,@body))

(defmacro n2wal-with-data-from-response-buffer (response-buffer &rest body)
  (declare (indent 1) (debug t))
  `(with-current-buffer (car (with-current-buffer ,response-buffer (mm-dissect-buffer t)))
     (goto-char (point-min))
     ,@body))

(defun n2wal-get-json-from-response-buffer (buffer)
  (n2wal-with-data-from-response-buffer buffer
    (n2wal-with-json-preset
      (json-read))))

;; When helm is available, it should be the preferred selection
;; method, otherwise we'll fall back to ido.
(if (require 'helm nil 'noerror)
    (defun n2wal-remote-feeds-picker ()
      "Pick a miniflux feed using helm as selection method"
      (split-string (helm :sources (helm-build-sync-source "*N2WAL REMOTE FEEDS*"
                                     :candidates (n2wal-get-remote-feed-list))) " - "))
  (progn
    (require 'ido)
    (defun n2wal-remote-feeds-picker ()
      "Pick a miniflux feed using ido as selection method"
      (split-string (ido-completing-read "Pick a feed:" (n2wal-get-remote-feed-list)) " - "))))

(defun n2wal-add-feed-and-quit ()
  "Add a feed and prompt to quit emacs"
  (call-interactively 'n2wal-add-feed)
  (if (string= "" (read-string "Feed added, press RET (ENTER) to quit emacs"))
      (kill-emacs)))

(defun n2wal-save-token-payload (token-payload)
  (n2wal-save-data "wallabag-token"
                   `((token . ,(alist-get 'access_token token-payload))
                     (refresh-token . ,(alist-get 'refresh_token token-payload))
                     (expiration-time . ,(+ (float-time) (alist-get 'expires_in token-payload)))
                     (token-type . ,(alist-get 'token_type token-payload)))))

(defun n2wal-retrieve-and-store-wallabag-token (host client-id client-secret username password)
  (let* ((url-request-data
           (format "grant_type=password&client_id=%s&client_secret=%s&username=%s&password=%s"
                  client-id
                  client-secret
                  username
                  password))
         (url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
         (response (url-retrieve-synchronously (concat "https://" host "/oauth/v2/token"))))
        (let ((response-data (n2wal-get-json-from-response-buffer response)))
          (if (alist-get 'error response-data)
              `(n2wal-error ,(format
                        "Got error response from %s: %s: %s"
                        host
                        (alist-get 'error response-data)
                        (alist-get 'error_description response-data)))
            (progn
              (n2wal-save-token-payload response-data)
              '(success))))))

(defun n2wal-refresh-wallabag-token ()
  (interactive)
  (let* ((token (n2wal-get-data "wallabag-token"))
         (wallabag-config (alist-get 'wallabag (n2wal-get-data "config")))
         (url-request-data
          (format "grant_type=refresh_token&refresh_token=%s&client_id=%s&client_secret=%s"
                  (alist-get 'refresh-token token)
                  (alist-get 'client-id wallabag-config)
                  (alist-get 'client-secret wallabag-config)))
         (url-request-method "POST")
         (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")))
         (response (url-retrieve-synchronously (concat "https://"
                                                       (alist-get 'host wallabag-config)
                                                       "/oauth/v2/token"))))
    (n2wal-save-token-payload (n2wal-get-json-from-response-buffer response))))

(defun n2wal-make-miniflux-client (host username password)
  "Create an anonymous function that can be called unlimited
times to retrieve data from a miniflux instance"
  (lambda (method route &optional data)
    (let* ((url-request-method method)
           (url-request-extra-headers
            `(("Accepts" . "application/json")))
           (url-request-data (if data data))
           (response-buffer
            (url-retrieve-synchronously
             (concat "https://" username ":" password "@" host "/" route)
             t)))
      (if (with-current-buffer response-buffer
            (goto-char (point-min))
            ;; For now, it is enough to just check for a 200 response,
            ;; but this, of course, does not reflect the meaning of
            ;; HTTP status codes so this check should be modified as
            ;; needed when adding new functionalities.
            (not (looking-at "HTTP/1.1 \\(200\\|204\\)")))
          `(n2wal-error
            ,(format "Request to minflux host returned unexpected results:\n%s: \"%s\""
                     host
                     (with-current-buffer response-buffer (buffer-string))))
        response-buffer))))

(defun n2wal-make-wallabag-client (host token refresh-token expiration-time)
  (lambda (method route &optional data)
    (if (> (float-time) expiration-time)
        (progn
          (n2wal-refresh-wallabag-token)
          (let ((new-token-data  (n2wal-get-data "wallabag-token")))
            (setq token (alist-get 'token new-token-data))
            (setq refresh-token (alist-get 'refresh-token new-token-data))
            (setq expiration-time (alist-get 'expiration-time new-token-data)))))

    (let ((url-request-method method)
          (url-request-extra-headers
           `(("Accepts" . "application/json")
             ("Content-Type" . "application/json")
             ("Authorization" . ,(format "Bearer %s" token))))
          (url-request-data (if data data)))
      (url-retrieve-synchronously (concat "https://" host "/" route) t))))

(defun n2wal-read-config (config-location)
  "Read the JSON config in the file at CONFIG-LOCATION"
  (n2wal-with-json-preset
    (json-read-file config-location)))

(defun n2wal-check-miniflux-config (config)
  "Check if CONFIG is valid and can be used to connect to
miniflux. Returns either an `n2wal-error` (this can be discovered
with `n2wal-error-p`) or a list with `success` as car."
  (let* ((client (n2wal-make-miniflux-client
                 (alist-get 'host config)
                 (alist-get 'user config)
                 (alist-get 'password config)))
         (response (funcall client "GET" "v1/feeds")))
    (if (n2wal-error-p response)
        response
      '(success))))

(defun n2wal-data-dir ()
  "Get the path of the directory where n2wal saves its data"
  (concat (getenv "HOME") "/.n2wal.d"))

(defun n2wal-get-data (filename)
  "Open a json file in the data directory and return the parsed data"
  (let ((filepath (concat (n2wal-data-dir) "/" filename ".json")))
    (if (file-exists-p filepath)
        (with-temp-buffer
          (insert-file-contents filepath)
          (goto-char (point-min))
          (n2wal-with-json-preset
            (json-read))))))

(defun n2wal-save-data (filename data)
  "Save json data to a json file in the data directory"
  (unless (file-directory-p (n2wal-data-dir)) (make-directory (n2wal-data-dir)))
  (let ((filepath (concat (n2wal-data-dir) "/" filename ".json")))
    (with-temp-buffer
      (n2wal-with-json-preset
        (insert (json-encode data))

        ;; The config might be edited by hand, so we should keep it
        ;; pretty.
        (if (string= filename "config") (json-pretty-print-buffer))
        (write-file filepath)))))

(defun n2wal-error-p (thing)
  (and (listp thing) (eq 'n2wal-error (car thing))))

(defun n2wal-create-miniflux-config (&optional host user password)
  "Interactively construct the config parameters for the miniflux connection"
  (let ((config `((host . ,(read-string "Miniflux instance host:" host))
                  (user . ,(read-string "Miniflux user:" user))
                  (password . ,(read-passwd "Miniflux password:")))))
    (let ((check-result (n2wal-check-miniflux-config config)))
    (if (not (n2wal-error-p check-result))
        config
      (progn
        (if (string= "yes"
                     (cadr
                      (read-multiple-choice
                       (format "Error : \"%s\", would you like to try again?" (cadr check-result))
                       '((?y "yes") (?n "no")))))
            (n2wal-create-miniflux-config
             (alist-get 'host config)
             (alist-get 'user config)
             (alist-get 'password config))))))))

(defun n2wal-create-wallabag-config (&optional host client-id client-secret username password)
  "Interactively construct the config parameters for the wallabag connection"
  (let* ((config `((host . ,(read-string "Wallabag instance host:" host))
                  (client-id . ,(read-string "Wallabag client id:" client-id))
                  (client-secret . ,(read-string "Wallabag client secret:" client-secret))
                  (username . ,(read-string "Wallabag username:" username))))
         (password (read-passwd "Wallabag password:" password))
         (token (n2wal-retrieve-and-store-wallabag-token
                 (alist-get 'host config)
                 (alist-get 'client-id config)
                 (alist-get 'client-secret config)
                 (alist-get 'username config)
                 password)))
    (if (not (n2wal-error-p token))
        config
      (if (string= "yes"
                   (cadr
                    (read-multiple-choice
                     (format "Error: \"%s\" you like to try again? :" (cadr token))
                     '((?y "yes") (?n "no")))))
          (n2wal-create-wallabag-config
           (alist-get 'host config)
           (alist-get 'client-id config)
           (alist-get 'client-secret config)
           (alist-get 'username config)
           password)))))

(defun n2wal-create-config ()
  "Create the basic configuration with connection- and
authentication info that will be used to connect to the miniflux
and wallabag instances. The config will also contain the list of
miniflux feeds that should be synced"
  (interactive)
  (let ((miniflux-config (n2wal-create-miniflux-config))
        (wallabag-config (n2wal-create-wallabag-config)))
    (if (and miniflux-config wallabag-config)
        (n2wal-save-data "config" `((miniflux . ,miniflux-config)
                                    (wallabag . ,wallabag-config)
                                    (feeds . ())))
      nil)))

(defun n2wal-show-feeds ()
  "Show list of feeds in a dedicated buffer"
  (interactive)
  (pop-to-buffer (get-buffer-create "*N2WAL FEEDS*"))
  (erase-buffer)
  (insert "FEEDS:\n")
  (let ((config (n2wal-get-data "config")))
    (dolist (feed (alist-get 'feeds config))
      (insert (format "\"id\": %d, \"title\": \"%s\"\n"
                      (alist-get 'id feed)
                      (alist-get 'title feed))))))

(defun n2wal-get-remote-feed-list ()
  "Retrieve the remote feeds from the miniflux server"
  (let* ((miniflux-config (alist-get 'miniflux (n2wal-get-data "config")))
         (client (n2wal-make-miniflux-client
                  (alist-get 'host miniflux-config)
                  (alist-get 'user miniflux-config)
                  (alist-get 'password miniflux-config)))
         (response (funcall client "GET" "v1/feeds")))
    (mapcar* (lambda (feed)
               (concat (number-to-string (alist-get 'id feed))
                       " - "
                       (alist-get 'title feed)))
             (n2wal-get-json-from-response-buffer response))))

(defun n2wal-add-feed (id title)
  "Add miniflux feed to list of feeds that should be synced."
  (interactive (n2wal-remote-feeds-picker))
  (let ((config (n2wal-get-data "config")))
    (push `((id . ,(string-to-number id)) (title . ,title))
          (alist-get 'feeds config))
    (n2wal-save-data "config" config))
  (n2wal-show-feeds))

(defun n2wal-get-unread-entries-for-feed (miniflux-client feed)
  "Retrieve unread entries for a feed. MINIFLUX-CLIENT should be a
function like the one returned by `n2wal-make-miniflux-client`"
  (n2wal-get-json-from-response-buffer
   (funcall miniflux-client
            "GET"
            (format "v1/feeds/%d/entries?limit=100&order=published_at&status=unread"
                    (alist-get 'id feed)))))

(defun n2wal-get-pending-entries-for-feed (feed)
  (n2wal-get-data (format "feed-%d" (alist-get 'id feed))))

(defun n2wal-save-pending-entries-for-feed (feed entries)
  (n2wal-save-data (format "feed-%d" (alist-get 'id feed)) entries))

(defun n2wal-add-pending-entry (wallabag-client miniflux-entry feed)
  "Add feed entry to list of pending entries. Pending entries
will be checked for deletion or archiving"
  (let* ((response (funcall wallabag-client
                           "POST"
                           "api/entries.json"
                           (n2wal-with-json-preset
                             (json-encode `((url . ,(alist-get 'url miniflux-entry)))))))
         (pending-feed-entries (n2wal-get-pending-entries-for-feed feed))
         (response-data (n2wal-get-json-from-response-buffer response)))
    (push `((miniflux-id . ,(alist-get 'id miniflux-entry))
            (wallabag-id . ,(alist-get 'id response-data)))
          pending-feed-entries)
    (n2wal-save-pending-entries-for-feed feed pending-feed-entries)))

(defun n2wal-get-wallabag-article (wallabag-client article-id)
  (n2wal-get-json-from-response-buffer
   (funcall wallabag-client "GET" (format "api/entries/%d" article-id))))

(defun n2wal-propagate-read-in-wallabag-to-miniflux (miniflux-client wallabag-client feed)
  "Mark all articles that were read on wallabag as read at the miniflux instance"
  (let ((pending-entries (n2wal-get-pending-entries-for-feed feed)))


    (let ((read-entry-ids))
      (dolist (pending-entry pending-entries)
        (let ((wallabag-entry
               (n2wal-get-wallabag-article wallabag-client (alist-get 'wallabag-id pending-entry))))
          (if (or (and (eq 'error (caar wallabag-entry))
                       (= 404 (alist-get 'code (cdr (car wallabag-entry)))))
                  (= 1 (alist-get 'is_archived wallabag-entry)))
              (progn
                (message "Miniflux article %d has been archived or deleted from wallabag."
                         (alist-get 'miniflux-id pending-entry))
                (push (alist-get 'miniflux-id pending-entry) read-entry-ids)))))

      (if read-entry-ids
          (progn
            ;; Mark the read article as read at the miniflux instance
            (funcall miniflux-client
                     "PUT"
                     "v1/entries"
                     (n2wal-with-json-preset
                       (json-encode
                        `((entry_ids . ,read-entry-ids)
                          (status . "read")))))

            (setq pending-entries
                  (seq-filter (lambda (entry)
                                (not (member (alist-get 'miniflux-id entry) read-entry-ids)))
                              (n2wal-get-pending-entries-for-feed feed)))

            (n2wal-save-pending-entries-for-feed feed pending-entries))))))

(defun n2wal-add-unread-miniflux-articles-to-wallabag (miniflux-client wallabag-client feed)
  (let ((unread-entries (alist-get 'entries
                                   (n2wal-get-unread-entries-for-feed miniflux-client feed)))
        (pending-entries (n2wal-get-pending-entries-for-feed feed)))

    (dolist (unread-entry unread-entries)
      (if (not (seq-find (lambda (pending-entry)
                           (= (alist-get 'miniflux-id pending-entry)
                              (alist-get 'id unread-entry)))
                         pending-entries))
          (progn
            (n2wal-add-pending-entry wallabag-client unread-entry feed)
            (message "Added article %d - \"%s\" to wallabag"
                     (alist-get 'id unread-entry)
                     (alist-get 'title unread-entry)))))))


(defun n2wal-sync-feed (config miniflux-client feed)
  (message "Syncing articles of miniflux feed %d: \"%s\""
           (alist-get 'id feed)
           (alist-get 'title feed))
  (let* ((wallabag-config (alist-get 'wallabag config))
         (wallabag-token-data (n2wal-get-data "wallabag-token"))
         (wallabag-client (n2wal-make-wallabag-client
                           (alist-get 'host wallabag-config)
                           (alist-get 'token wallabag-token-data)
                           (alist-get 'refresh-token wallabag-token-data)
                           (alist-get 'expiration-time wallabag-token-data))))
    (n2wal-propagate-read-in-miniflux-to-wallabag miniflux-client wallabag-client feed)
    (n2wal-propagate-read-in-wallabag-to-miniflux miniflux-client wallabag-client feed)
    (n2wal-add-unread-miniflux-articles-to-wallabag miniflux-client wallabag-client feed)))

(defun n2wal-get-miniflux-article (miniflux-client article-id)
  (n2wal-get-json-from-response-buffer
   (funcall miniflux-client "GET" (format "v1/entries/%d" article-id))))

(defun n2wal-delete-wallabag-article (wallabag-client article-id)
  (n2wal-get-json-from-response-buffer
   (funcall wallabag-client "DELETE" (format "api/entries/%d" article-id))))

(defun n2wal-make-wallabag-client-from-config ()
  (let ((config (n2wal-get-data "config"))
        (token (n2wal-get-data "wallabag-token")))
    (n2wal-make-wallabag-client
     (alist-get 'host (alist-get 'wallabag config))
     (alist-get 'token token)
     (alist-get 'refresh-token token)
     (alist-get 'expiration-time token))))


(defun n2wal-delete-feed (wallabag-client feed-id)
  (interactive (list (n2wal-make-wallabag-client-from-config)
                     (read-number "Feed id: ")))
  (let* ((config (n2wal-get-data "config"))
         (feed (seq-find
                (lambda (config-feed) (= (alist-get 'id config-feed) feed-id))
                (alist-get 'feeds config))))

    (dolist (pending-entry (n2wal-get-pending-entries-for-feed feed))
      (message "Deleting article %d from wallabag" (alist-get 'wallabag-id pending-entry))
      (let ((deletion-response (n2wal-delete-wallabag-article
                                wallabag-client
                                (alist-get 'wallabag-id pending-entry))))
        (if (alist-get 'error deletion-response)
            (message "An error occured while deleting wallabag-article %d: %s"
                     (alist-get 'wallabag-id pending-entry)
                     (pp (alist-get 'error deletion-response))))))

    (setcdr (assoc 'feeds config 'eq)
            (seq-filter (lambda (feed)
                          (not (= (alist-get 'id feed) feed-id)))
                        (cdr (assoc 'feeds config 'eq))))

    (n2wal-delete-data (format "feed-%d" feed-id))

    (n2wal-save-data "config" config)))

(defun n2wal-delete-data (filename)
  (let ((filepath (concat (n2wal-data-dir) "/" filename ".json")))
    (delete-file filepath)))

(defun n2wal-propagate-read-in-miniflux-to-wallabag (miniflux-client wallabag-client feed)
  "Delete all articles that were read in miniflux from the
wallabag instance, unless they were archived on the wallabag side."
  (let ((pending-entries (n2wal-get-pending-entries-for-feed feed))
        (read-article-ids '()))

    (dolist (pending-entry pending-entries)
      (let* ((miniflux-id (alist-get 'miniflux-id pending-entry))
             (wallabag-id (alist-get 'wallabag-id pending-entry))
             (miniflux-article (n2wal-get-miniflux-article miniflux-client miniflux-id)))
        (if (string= "read" (alist-get 'status miniflux-article))
            (progn
              (push wallabag-id read-article-ids)
              (message "Detected that miniflux article %d (wallabag article %d) was read"
                       miniflux-id
                       wallabag-id)))))

    (dolist (article-id read-article-ids)
      (let* ((response (n2wal-get-wallabag-article wallabag-client article-id))
             (error (alist-get 'error response)))
        (if error
            (if (and (alist-get 'code error) (= 404 (alist-get 'code error)))
                (message "Article %d has already been deleted from wallabag" article-id)
              (error "Something went wrong while trying to retrieve article %d from wallabag: %s"
                     article-id
                     (pp error)))
          (progn
            (if (= 1 (alist-get 'is_archived response))
                (message "Wallabag article %d has already been archived" article-id)
              (progn
                (let* ((deletion-response (n2wal-delete-wallabag-article wallabag-client article-id))
                       (error (alist-get 'error deletion-response)))
                  (if error
                      (error "Something went wrong while deleting wallabag article %d: %s"
                             article-id
                             (pp deletion-response))
                    (message "Deleted article %d from wallabag" article-id)))))))))

    (setq pending-entries (seq-filter (lambda (entry)
                                        (not (member (alist-get 'wallabag-id entry)
                                                     read-article-ids)))
                                      pending-entries))

    (n2wal-save-pending-entries-for-feed feed pending-entries)))

(defun n2wal-refresh-feed (miniflux-client feed)
  (let* ((feed-id (alist-get 'id feed))
         (response (funcall miniflux-client
                           "PUT"
                           (format "v1/feeds/%d/refresh" feed-id))))
    (message "Refreshing feed %d on miniflux server" feed-id)
    (if (n2wal-error-p response)
        (error "Failed to sync feed %d: \"%s\"" feed-id (cadr response)))))

(defun n2wal-sync-feeds ()
  (interactive)
  (let* ((config (n2wal-get-data "config"))
         (miniflux-config (alist-get 'miniflux config))
         (miniflux-client
          (n2wal-make-miniflux-client
           (alist-get 'host miniflux-config)
           (alist-get 'user miniflux-config)
           (alist-get 'password miniflux-config))))

    (dolist (feed (alist-get 'feeds config))
      (n2wal-refresh-feed miniflux-client feed)

      (n2wal-sync-feed config miniflux-client feed))))
