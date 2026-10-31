;;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'zotero-connect)

(ert-deftest zotero-group-selected-for-bibtex-and-open ()
  (let ((item '((zotero-connect-library . "groups/123") (key . "ABCD1234"))) path target)
    (cl-letf (((symbol-function 'zotero-connect--get)
               (lambda (p) (setq path p) "@article{example, title={Example}}"))
              ((symbol-function 'zotero-connect--select) (lambda () item))
              ((symbol-function 'call-process)
               (lambda (_program _infile _destination _display &rest args)
                 (setq target (car args)) 0)))
      (zotero-connect--bibtex item)
      (should (equal path "groups/123/items/ABCD1234?format=bibtex"))
      (zotero-connect-search)
      (should (equal target "zotero://select/groups/123/items/ABCD1234")))))

(ert-deftest zotero-collection-path-uses-parent ()
  (let* ((parent '((key . "PARENT") (data . ((key . "PARENT") (name . "Parent")))))
         (child '((key . "CHILD") (data . ((key . "CHILD") (name . "Child") (parentCollection . "PARENT"))))))
    (should (equal (zotero-connect--collection-path child (list parent child)) "Parent / Child"))))

(ert-deftest zotero-collection-selection-uses-selected-group ()
  (cl-letf (((symbol-function 'zotero-connect--get)
             (lambda (_) "[{\"key\":\"ABC\",\"data\":{\"key\":\"ABC\",\"name\":\"Papers\",\"parentCollection\":false}}]"))
            ((symbol-function 'completing-read)
             (lambda (_prompt choices &rest _) (caadr choices))))
    (should (equal (zotero-connect--select-export-path "groups/123")
                   "groups/123/collections/ABC/items/top"))))

;;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(require 'zotero-connect)

(ert-deftest citation-selects-bibtex-key-without-changing-bib-file ()
  (let* ((file (make-temp-file "citation" nil ".bib"))
         (initial "@article{smith2024,\n title = {An {Interesting} Paper},\n author = {Jane Smith},\n year = {2024}\n}\n"))
    (unwind-protect
        (progn
          (with-temp-file file (insert initial))
          (let ((candidates (zotero-connect--bib-candidates file)))
            (should (= (length candidates) 1))
            (should (equal (cdar candidates) "smith2024"))
            (should (string-match-p "Interesting" (caar candidates))))
          (cl-letf (((symbol-function 'completing-read) (lambda (_ choices &rest _) (caar choices))))
            (with-temp-buffer
              (zotero-connect-insert-citation file)
              (should (equal (buffer-string) "\\cite{smith2024}"))))
          (with-temp-buffer (insert-file-contents file) (should (equal (buffer-string) initial))))
      (delete-file file))))

(ert-deftest citation-reads-unsaved-bibliography-buffer ()
  (let* ((file (make-temp-file "citation" nil ".bib"))
         (buf (find-file-noselect file)))
    (unwind-protect
        (progn
          (with-current-buffer buf (insert "@book{draft, title={Unsaved}}\n"))
          (should (equal (cdar (zotero-connect--bib-candidates file)) "draft"))
          (should (with-current-buffer buf (buffer-modified-p))))
      (with-current-buffer buf (set-buffer-modified-p nil))
      (kill-buffer buf)
      (delete-file file))))
