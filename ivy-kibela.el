;;; ivy-kibela.el --- Ivy interface to Kibela  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  mugijiru

;; Author: mugijiru <106833+mugijiru@users.noreply.github.com>
;; Maintainer: mugijiru <106833+mugijiru@users.noreply.github.com>
;; URL: https://github.com/mugijiru/ivy-kibela
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.1") (ivy "0.13.0") (graphql "0.1.1") (request "0.3.3"))
;; Keywords: matching ivy kibela

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Ivy interface to Kibela
;;

;;; Code:

(require 'ivy)
(require 'graphql)
(require 'request)

(defcustom ivy-kibela-team nil
  "Kibela team name for login."
  :group 'ivy-kibela
  :type 'string)

(defcustom ivy-kibela-access-token nil
  "Kibela access token for login"
  :group 'ivy-kibela
  :type 'string)

(defcustom ivy-kibela-default-command 'recent
  "Call this command if you execute `M-x ivy-kibela'"
  :group 'ivy-kibela
  :type '(choice (const :tag "Recent" recent)
                 (const :tag "Search" search)))

(defun ivy-kibela-endpoint ()
  (concat "https://" ivy-kibela-team ".kibe.la/api/v1"))

(defconst ivy-kibela-recent-query
  (graphql-query
   ((notes
     :arguments ((first . 100))
     (edges
      (node id title url)))))
  "Fetch notes query")

(defconst ivy-kibela-recent-browsing-notes-query
  (graphql-query
   ((noteBrowsingHistories
     :arguments((first . 100))
     (edges
      (node
       (note
        id
        title
        url))))))
  "Fetch recent browsing notes query")

(defconst ivy-kibela-search-query
  (graphql-query
   (:arguments (($query . String!))
    (search
     :arguments ((first . 100) (query . ($ query)))
     (edges
      (node title url)))))
  "Search query")

(defun ivy-kibela-headers ()
  "HTTP request headers."
  `(("Content-Type" . "application/json")
    ("Accept" . "application/json")
    ("Authorization" . ,(concat "Bearer " ivy-kibela-access-token))))

(defun ivy-kibela-build-collection-from-notes (notes)
  (mapcar (lambda (note)
          (let ((str (assoc-default 'title note))
                (id (assoc-default 'id note))
                (url (assoc-default 'url note)))
            (propertize str 'id id 'url url)))
          notes))

(defun ivy-kibela-action (title)
  (let ((url (get-text-property 0 'url title)))
    (if url
        (browse-url url))))

(defun ivy-kibela-recent (&optional action)
  (interactive)
  (request
    (ivy-kibela-endpoint)
    :type "POST"
    :data (json-encode `(("query" . ,ivy-kibela-recent-query)))
    :parser 'json-read
    :encoding 'utf-8
    :headers (ivy-kibela-headers)
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((json-data (assq 'data (graphql-simplify-response-edges data)))
                       (notes (assoc-default 'notes json-data))
                       (collection (ivy-kibela-build-collection-from-notes notes)))
                  (ivy-read "Kibela notes: "
                            collection
                            :caller 'ivy-kibela
                            :action (or action #'ivy-kibela-action)))))))

(defun ivy-kibela-recent-browsing-notes (&optional action)
  "Fetch recent browsing notes."
  (interactive)
  (request
    (ivy-kibela-endpoint)
    :type "POST"
    :data (json-encode `(("query" . ,ivy-kibela-recent-browsing-notes-query)))
    :parser 'json-read
    :encoding 'utf-8
    :headers (ivy-kibela-headers)
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (let* ((json-data (assq 'data (graphql-simplify-response-edges data)))
                       (browsing-histories (assoc-default 'noteBrowsingHistories json-data))
                       (notes (seq-uniq (mapcar (lambda (history) (assoc-default 'note history)) browsing-histories)))
                       (collection (ivy-kibela-build-collection-from-notes notes)))
                  (ivy-read "Kibela recent browsing notes: "
                            collection
                            :caller 'ivy-kibela
                            :action (or action #'ivy-kibela-action)))))))

(defun ivy-kibela-search ()
  (interactive)
  (ivy-read "Kibela notes: "
            #'ivy-kibela-search-request
            :dynamic-collection t
            :caller 'ivy-kibela-search
            :action #'ivy-kibela-action))

(defun ivy-kibela-search-request (str)
  (or
   (ivy-more-chars)
   (progn
     (let ((query ivy-kibela-search-query))
       (setq ivy-kibela-request-response
             (request
               (ivy-kibela-endpoint)
               :type "POST"
               :data (json-encode `(("query" . ,query) ("variables" . ,(list (cons "query" str)))))
               :parser 'json-read
               :encoding 'utf-8
               :headers (ivy-kibela-headers)
               :success (cl-function
                         (lambda (&key data &allow-other-keys)
                           (let* ((json-data (assq 'data (graphql-simplify-response-edges data)))
                                  (notes (assoc-default 'search json-data))
                                  (collection (ivy-kibela-build-collection-from-notes notes)))
                             (ivy-update-candidates collection)))))))
     '("" "working..."))))

(defvar ivy-kibela-request-response nil)

(defun ivy-kibela ()
  (interactive)
  (cond
   ((eq ivy-kibela-default-command 'recent)
    (ivy-kibela-recent))
   ((eq ivy-kibela-default-command 'search)
    (ivy-kibela-search))
   (t (message "Unexpected command"))))

(defun ivy-kibela-unwind ()
  "Delete any open kibela connections."
  (if ivy-kibela-request-response
      (request-abort ivy-kibela-request-response))
  (setq ivy-kibela-request-response nil))

(defun ivy-kibela-transformer (str)
  str)

(ivy-configure 'ivy-kibela
    :display-transformer-fn #'ivy-kibela-transformer)

(provide 'ivy-kibela)
;;; ivy-kibela.el ends here
