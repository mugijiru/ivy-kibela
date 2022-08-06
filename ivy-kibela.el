;;; ivy-kibela.el --- ivy interface for kibe.la      -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Mugijiru

;; Author: Mugijiru <mugijiru@manjaro>
;; Package-Requires: ((emacs "28.1") (ivy "0.13.0") (graphql "0.1.1") (request 0.3.3))
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Find kibela note interface for ivy.

;;; Code:

(require 'ivy)
(require 'graphql)

(defcustom ivy-kibela-team nil
  "Kibela team name for login."
  :group 'ivy-kibela
  :type 'string)

(defcustom ivy-kibela-access-token nil
  "Kibela access token for login"
  :group 'ivy-kibela
  :type 'string)

(defun ivy-kibela-endpoint ()
  (concat "https://" ivy-kibela-team ".kibe.la/api/v1"))

(defconst ivy-kibela-query
  (graphql-query
   ((notes
     :arguments ((first . 100))
     (edges
      (node title url)))))
  "Fetch notes query")

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
                (url (assoc-default 'url note)))
            (propertize str 'url url)))
          notes))

(defun ivy-kibela-action (title)
  (let ((url (get-text-property 0 'url title)))
    (if url
        (browse-url url))))

(defun ivy-kibela ()
  (interactive)
  (request
    (ivy-kibela-endpoint)
    :type "POST"
    :data (json-encode `(("query" . ,ivy-kibela-query)))
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
                            :action #'ivy-kibela-action))))))

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
     ;; (ivy-kibela-unwind)
     (let ((query ivy-kibela-search-query))
       (setq ivy-kibela-request-response
             (request
               (ivy-kibela-endpoint)
               :type "POST"
               :data (json-encode `(("query" . ,query) ("variables" . ,(list (cons "query" str)))))
               :parser 'json-read
               :encoding 'utf-8
               :headers (ivy-kibela-headers)
               ;; :unwind #'ivy-kibela-unwind
               ;; :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys) (message "Got error: %S" error-thrown)))
               :success (cl-function
                         (lambda (&key data &allow-other-keys)
                           (let* ((json-data (assq 'data (graphql-simplify-response-edges data)))
                                  (notes (assoc-default 'search json-data))
                                  (collection (ivy-kibela-build-collection-from-notes notes)))
                             (ivy-update-candidates collection)))))))
     '("" "working..."))))

(defvar ivy-kibela-request-response nil)

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
