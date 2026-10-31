;;; zotero-connect.el --- Search a local Zotero library -*- lexical-binding: t; -*-

;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: bibliography, tools

;; Uses Zotero's local API.  Zotero must be running with local access enabled.

(require 'json)
(require 'url)
(require 'subr-x)
(require 'seq)
(require 'bibtex)

(defgroup zotero-connect nil
  "Search the local Zotero library."
  :group 'applications)

(defcustom zotero-connect-api-url "http://127.0.0.1:23119/api/"
  "Base URL of the local Zotero API."
  :type 'string)

(defun zotero-connect--get (path)
  "Return the body of the local API response for PATH."
  (let* ((url-request-extra-headers '(("Zotero-API-Version" . "3")))
         (url-show-status nil)
         (buffer (condition-case err
                     (url-retrieve-synchronously
                      (concat zotero-connect-api-url path) t t 10)
                   (error (user-error "Cannot connect to Zotero: %s"
                                      (error-message-string err))))))
    (unless buffer
      (user-error "Cannot connect to Zotero; start Zotero and enable local API access"))
    (unwind-protect
        (with-current-buffer buffer
          (unless (and (boundp 'url-http-response-status)
                       (= url-http-response-status 200))
            (user-error "Zotero API returned HTTP %s (check local API access)"
                        (if (boundp 'url-http-response-status)
                            url-http-response-status "unknown")))
          (goto-char (point-min))
          (unless (re-search-forward "\r?\n\r?\n" nil t)
            (error "Invalid response from Zotero"))
          (decode-coding-string (buffer-substring-no-properties (point) (point-max))
                                'utf-8))
      (kill-buffer buffer))))

(defun zotero-connect--items (library)
  "Return regular top-level items in Zotero LIBRARY."
  (let ((items (json-parse-string
                (zotero-connect--get (concat library "/items/top"))
                :object-type 'alist :array-type 'list
                :null-object nil :false-object nil)))
    (mapcar (lambda (item) (cons (cons 'zotero-connect-library library) item))
            (seq-filter (lambda (item)
                          (not (member (alist-get 'itemType (alist-get 'data item))
                                       '("attachment" "note" "annotation"))))
                        items))))

(defun zotero-connect--candidate (item)
  "Return a display string for ITEM."
  (let* ((data (alist-get 'data item))
         (title (or (alist-get 'title data) "Untitled"))
         (creator (or (alist-get 'creatorSummary (alist-get 'meta item)) ""))
         (date (or (alist-get 'date data) "")))
    (format "%s  —  %s  %s  [%s]"
            (replace-regexp-in-string "[\n\r\t]+" " " title)
            creator date (alist-get 'key item))))

(defun zotero-connect--select ()
  "Prompt for a Zotero item and return it."
  (let* ((library (zotero-connect--select-library))
         (items (zotero-connect--items library))
         (candidates (mapcar (lambda (item) (cons (zotero-connect--candidate item) item))
                             items)))
    (unless candidates (user-error "No items found in the selected Zotero library"))
    (cdr (assoc (completing-read "Zotero: " candidates nil t) candidates))))

(defun zotero-connect--bibtex (item)
  "Fetch a BibTeX entry for ITEM."
  (let ((entry (zotero-connect--get
                (format "%s/items/%s?format=bibtex"
                        (alist-get 'zotero-connect-library item)
                        (url-hexify-string (alist-get 'key item))))))
    (unless (string-match "@[[:alpha:]]+[[:space:]]*[{(][[:space:]]*\\([^,[:space:]]+\\)[[:space:]]*," entry)
      (user-error "Zotero did not return a valid BibTeX entry"))
    entry))

(defun zotero-connect-search ()
  "Search a library and select an item in Zotero."
  (interactive)
  (let* ((item (zotero-connect--select))
         (library (alist-get 'zotero-connect-library item))
         (target (if (equal library "users/0") "library" library)))
    (unless (eq 0 (call-process "open" nil nil nil
                                (format "zotero://select/%s/items/%s"
                                        target (alist-get 'key item))))
      (user-error "Could not open the selected item in Zotero"))))

(defun zotero-connect-insert-bibtex ()
  "Search Zotero and insert a BibTeX entry at point."
  (interactive)
  (insert (string-trim-right (zotero-connect--bibtex (zotero-connect--select))) "\n"))

(defun zotero-connect-copy-bibtex ()
  "Search Zotero and copy the selected BibTeX entry."
  (interactive)
  (kill-new (zotero-connect--bibtex (zotero-connect--select)))
  (message "BibTeX copied"))

(defun zotero-connect--select-library ()
  "Select the personal library or a locally available group library."
  (let* ((groups (json-parse-string (zotero-connect--get "users/0/groups")
                                    :object-type 'alist :array-type 'list))
         (choices (cons '("My Library" . "users/0")
                        (mapcar (lambda (group)
                                  (let ((id (alist-get 'id group)))
                                    (cons (format "%s [group %s]"
                                                  (alist-get 'name (alist-get 'data group)) id)
                                          (format "groups/%s" id))))
                                groups))))
    (cdr (assoc (completing-read "Zotero library: " choices nil t) choices))))

(defun zotero-connect--collection-path (collection collections)
  "Return COLLECTION's name with its parent names from COLLECTIONS."
  (let ((data (alist-get 'data collection))
        (seen nil)
        (parts nil))
    (while data
      (let ((key (alist-get 'key data)))
        (when (member key seen) (error "Circular Zotero collection hierarchy"))
        (push key seen)
        (push (or (alist-get 'name data) "Untitled") parts)
        (let* ((parent (alist-get 'parentCollection data))
               (parent-item (and (stringp parent)
                                 (seq-find (lambda (item)
                                             (equal (alist-get 'key item) parent))
                                           collections))))
          (setq data (alist-get 'data parent-item)))))
    (string-join parts " / ")))

(defun zotero-connect--select-export-path (library)
  "Choose all of LIBRARY or one collection, returning its API path."
  (let* ((collections (json-parse-string
                       (zotero-connect--get (concat library "/collections"))
                       :object-type 'alist :array-type 'list
                       :null-object nil :false-object nil))
         (choices (cons (cons "All items in library" (concat library "/items/top"))
                        (mapcar (lambda (collection)
                                  (let ((key (alist-get 'key collection)))
                                    (cons (format "%s [%s]"
                                                  (zotero-connect--collection-path collection collections)
                                                  key)
                                          (format "%s/collections/%s/items/top" library key))))
                                collections))))
    (cdr (assoc (completing-read "Export collection: " choices nil t) choices))))

(defun zotero-connect-export-library (bib-file)
  "Replace BIB-FILE's buffer with a library or collection BibTeX export.
The buffer is left unsaved for review; no existing edits are discarded."
  (interactive (list (read-file-name "Export to BibTeX file: " nil
                                     (expand-file-name "references.bib") nil
                                     "references.bib")))
  (let* ((library (zotero-connect--select-library))
         (path (zotero-connect--select-export-path library))
         (entry (zotero-connect--get (concat path "?format=bibtex")))
         (buffer (find-file-noselect bib-file)))
    (unless (or (string-blank-p entry)
                (string-match-p "@[[:alpha:]]+[[:space:]]*[{(]" entry))
      (user-error "Zotero did not return a valid BibTeX export"))
    (with-current-buffer buffer
      (when buffer-read-only (user-error "The bibliography buffer is read-only"))
      (when (buffer-modified-p)
        (user-error "Save or revert %s before updating it" bib-file))
      (when (and (string-blank-p entry) (not (zerop (buffer-size)))
                 (not (yes-or-no-p "Zotero export is empty; replace the bibliography? ")))
        (user-error "Export cancelled"))
      (let ((inhibit-read-only nil))
        (atomic-change-group
          (save-restriction
            (widen)
            (erase-buffer)
            (insert entry)))))
    (pop-to-buffer buffer)
    (message "Zotero export loaded; review and save %s" bib-file)))

(defun zotero-connect--bib-candidates (bib-file)
  "Return completion candidates from BIB-FILE, including unsaved edits."
  (let ((source (get-file-buffer bib-file)) candidates)
    (with-temp-buffer
      (if source
          (let ((destination (current-buffer)))
            (with-current-buffer source
              (save-restriction
                (widen)
                (let ((contents (buffer-substring-no-properties (point-min) (point-max))))
                  (with-current-buffer destination (insert contents))))))
        (insert-file-contents bib-file))
      (bibtex-mode)
      (bibtex-map-entries
       (lambda (key _begin _end)
         (when key
           (let* ((fields (bibtex-parse-entry t))
                  (type (cdr (assoc "=type=" fields)))
                  (title (or (cdr (assoc-string "title" fields t)) "Untitled"))
                  (author (or (cdr (assoc-string "author" fields t)) ""))
                  (year (or (cdr (assoc-string "year" fields t)) "")))
             (unless (member (downcase (or type "")) '("string" "preamble" "comment"))
               (push (cons (format "%s  —  %s  %s  [%s]"
                                   (replace-regexp-in-string "[\n\r\t]+" " " title)
                                   (replace-regexp-in-string "[\n\r\t]+" " " author)
                                   year key)
                           key)
                     candidates)))))))
    (nreverse candidates)))

(defun zotero-connect-insert-citation (bib-file)
  "Select a reference from BIB-FILE and insert a LaTeX citation."
  (interactive (list (read-file-name "BibTeX file: " nil nil t nil
                                     (lambda (file) (or (file-directory-p file)
                                                        (string-suffix-p ".bib" file))))))
  (when buffer-read-only (user-error "The current buffer is read-only"))
  (let ((candidates (zotero-connect--bib-candidates bib-file)))
    (unless candidates (user-error "No BibTeX entries found in %s" bib-file))
    (let* ((choice (completing-read "Citation: " candidates nil t))
           (key (cdr (assoc choice candidates))))
      (insert (format "\\cite{%s}" key)))))

(provide 'zotero-connect)
;;; zotero-connect.el ends here
