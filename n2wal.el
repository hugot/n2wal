;; -*- lexical-binding: t; -*-
(require 'json)
(require 'url)
(require 'mm-decode)
(require 'cl)
(require 'helm)

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
              `(error ,(format
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
    (let ((url-request-method method)
          (url-registered-auth-schemes `(("basic" n2wal-fail-basic-auth . 4)))
          (url-request-extra-headers
           `(("Accepts" . "application/json")
             ("Authorization" . ,(concat "Basic "
                                         (base64-encode-string (concat username ":" password))))))
          (url-request-data (if data data)))
      (url-retrieve-synchronously (concat "https://" host "/" route)))))

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
              (url-registered-auth-schemes `(("basic" n2wal-fail-basic-auth . 4)))
              (url-request-extra-headers
               `(("Accepts" . "application/json")
                 ("Content-Type" . "application/json")
                 ("Authorization" . ,(format "Bearer %s" token))))
              (url-request-data (if data data)))
          (url-retrieve-synchronously (concat "https://" host "/" route)))))

(defun n2wal-read-config (config-location)
  "Read the JSON config in the file at CONFIG-LOCATION"
  (n2wal-with-json-preset
    (json-read-file config-location)))

(defun n2wal-execute (config))

(defun n2wal-fail-basic-auth (&rest args)
  (throw 'n2wal-auth-error nil))

(defun n2wal-check-miniflux-config (config)
  "Check if CONFIG is valid and can be used to connect to miniflux"
  (let ((client (n2wal-make-miniflux-client
                 (alist-get 'host config)
                 (alist-get 'user config)
                 (alist-get 'password config))))
    (let ((url-registered-auth-schemes `(("basic" n2wal-fail-basic-auth . 4))))
      (if (catch 'n2wal-auth-error
            (buffer-live-p (funcall client "GET" "v1/feeds")))
          t
        (progn
          (message "Failed to login at miniflux host %s" (alist-get 'host config))
          nil)))))

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
        (write-file filepath)))))

(defun n2wal-create-miniflux-config (&optional host user password)
  "Interactively construct the config parameters for the miniflux connection"
  (let ((config `((host . ,(read-string "Miniflux instance host:" host))
                  (user . ,(read-string "Miniflux user:" user))
                  (password . ,(read-passwd "Miniflux password:")))))
    (if (n2wal-check-miniflux-config config)
        config
      (progn
        (if (string= "yes"
                     (cadr
                      (read-multiple-choice
                       "Failed to authenticate to miniflux host, would you like to try again? :"
                       '((?y "yes") (?n "no")))))
            (n2wal-create-miniflux-config
             (alist-get 'host config)
             (alist-get 'user config)
             (alist-get 'password config)))))))

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
    (if (not (eq (car token) 'error))
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

(defun n2wal-remote-feeds-picker ()
  (split-string (helm :sources (helm-build-sync-source "*N2WAL REMOTE FEEDS*"
                                 :candidates (n2wal-get-remote-feed-list))) " - "))

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

(defun n2wal-sync-feed (config feed)
  (let* ((miniflux-config (alist-get 'miniflux config))
         (miniflux-client
          (n2wal-make-miniflux-client
           (alist-get 'host miniflux-config)
           (alist-get 'user miniflux-config)
           (alist-get 'password miniflux-config)))

         (wallabag-config (alist-get 'wallabag config))
         (wallabag-token-data (n2wal-get-data "wallabag-token"))
         (wallabag-client (n2wal-make-wallabag-client
                           (alist-get 'host wallabag-config)
                           (alist-get 'token wallabag-token-data)
                           (alist-get 'refresh-token wallabag-token-data)
                           (alist-get 'expiration-time wallabag-token-data)))

         (unread-entries (alist-get 'entries
                                    (n2wal-get-unread-entries-for-feed miniflux-client feed)))
         (pending-entries (n2wal-get-pending-entries-for-feed feed)))

    (dolist (unread-entry unread-entries)
      (if (not (seq-find (lambda (pending-entry)
                           (= (alist-get 'miniflux-id pending-entry)
                              (alist-get 'id unread-entry)))
                         pending-entries))
          (n2wal-add-pending-entry wallabag-client unread-entry feed)))

    (let ((read-entry-ids))
      (dolist (pending-entry pending-entries)
        (let ((wallabag-entry (n2wal-get-json-from-response-buffer
                               (funcall wallabag-client
                                        "GET"
                                        (format "api/entries/%d"
                                                (alist-get 'wallabag-id pending-entry))))))
          (if (or (and (eq 'error (caar wallabag-entry))
                       (= 404 (alist-get 'code (cdr (car wallabag-entry)))))
                  (= 1 (alist-get 'is_archived wallabag-entry)))
              (push (alist-get 'miniflux-id pending-entry) read-entry-ids))))

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

(defun n2wal-sync-feeds ()
  (interactive)
  (let ((config (n2wal-get-data "config")))
    (dolist (feed (alist-get 'feeds config))
      (n2wal-sync-feed config feed))))
