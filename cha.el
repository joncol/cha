;;; cha.el --- Clubhouse API integration for org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jonas Collberg

;; Author: Jonas Collberg <jonas.collberg@gmail.com>
;; Version: 1.0
;; URL: https://github.com/joncol/cha.el
;; Package-Requires: ((dash "2.17.0") (dash-functional "1.2.0") (emacs "26.1") (ox-gfm "20170628.2102") (s "1.12.0"))
;; Keywords: tools

;;; Commentary:

;; This library enables integration with the Clubhouse project management
;; platform (https://clubhouse.io/).

;; Based on https://github.com/candera/emacs/blob/master/clubhouse-api.el, which
;; in turn is based on https://github.com/glittershark/org-clubhouse.

(require 'dash)
(require 'dash-functional)
(require 'ob-core)
(require 's)
(require 'url)

;;; Code:

(defun cha--vec->list (vec)
  "Convert vector `VEC' to a list."
  (append vec nil))

(defun cha--list->alist (list)
  "Convert `LIST' (of even length) into a list of dotted pairs (an alist)."
  (->> list (-partition 2) (-map (lambda (pair)
                                   `(,(car pair) . ,(cadr pair))))))

(defvar cha-clubhouse-default-project nil)

(defvar cha-clubhouse-api-auth-token-path nil)

(defvar cha-clubhouse-api-auth-token nil)

(defvar cha-clubhouse-dry-run-mode nil)

(defun cha--decrypt-file-contents (path)
  "Return the contents of file at `PATH', gpg-decrypted."
  (save-mark-and-excursion
    (let ((temp-file (make-temp-file "cha--decrypt-file-contents")))
      (unwind-protect
          (progn
            (epa-decrypt-file path temp-file)
            (with-temp-buffer
              (insert-file-contents temp-file)
              (buffer-string)))
        (delete-file temp-file)))))

(defun cha--cached-clubhouse-api-auth-token ()
  "Return the Clubhouse API auth token. Caches the result."
  (or cha-clubhouse-api-auth-token
      (setq cha-clubhouse-api-auth-token
            (cha--decrypt-file-contents cha-clubhouse-api-auth-token-path))))

(defvar cha--clubhouse-api-base-url* "https://api.clubhouse.io/api/v2")

(defun cha--auth-url (url &optional params)
  "Return AUTH url made up of `URL' and `PARAMS'."
  (concat url
          "?"
          (url-build-query-string
           (cons `("token" ,(cha--cached-clubhouse-api-auth-token)) params))))

(defun cha--baseify-url (url)
  "Add base URL to `URL', if necessary."
  (if (s-starts-with? cha--clubhouse-api-base-url* url) url
    (concat cha--clubhouse-api-base-url*
            (if (s-starts-with? "/" url) url
              (concat "/" url)))))

(cl-defun cha--api-request (method path &key data (params '()))
  "Make a request towards a Clubhouse API.
The actual route to call is determined by `METHOD' and `PATH'.
`DATA' is passed as JSON body. Also, additional `PARAMS' can be set."
  (let* ((url-request-method method)
         (url-request-extra-headers '(("Content-Type" . "application/json")))
         (url-request-data data)
         (url (-> path
                  (cha--baseify-url)
                  (cha--auth-url params))))
    (if cha-clubhouse-dry-run-mode
        (message "method: %s path: %s data: %s" method path data)
      (progn
        (with-current-buffer (url-retrieve-synchronously url)
          (if (string= "HTTP/1.1 2"
                       (buffer-substring-no-properties
                        (point-min)
                        (+ (point-min)
                           (length "HTTP/1.1 2"))))
              (progn
                (goto-char url-http-end-of-headers)
                (prog1 (json-read)
                  (kill-buffer)))
            (error "HTTP Request Failed: %s %s %s %s" method path data
                   (buffer-substring-no-properties (point-min)
                                                   (point-max)))))))))

(defun clubhouse-api-get-story-op (story-id)
  "Retrieve the story with ID `STORY-ID' from Clubhouse."
  (cha--api-request "GET" (format "stories/%d" story-id)))

(defun clubhouse-api-update-story-op (story-id &rest properties)
  (cha--api-request "PUT" (format "stories/%d" story-id)
                    :data (-> properties cha--list->alist json-encode (encode-coding-string 'utf-8))))

(defun clubhouse-api-create-story-op (project-id name &rest properties)
  (cha--api-request "POST" "stories"
                    :data (-> properties
                              (-concat `(:project_id ,project-id :name ,name))
                              cha--list->alist
                              json-encode
                              (encode-coding-string 'utf-8))))

;; TODO: This needs to support continuation in the form of the `nextToken` stuff in the search result
(defun clubhouse-api-search-stories-op (query)
  (cha--vec->list
   (alist-get 'data
              (cha--api-request "GET" "search/stories"
                                :data (-> `((:query . ,query))
                                          json-encode
                                          (encode-coding-string 'utf-8))))))

(cl-defun to-id-name-pairs
    (seq &optional (id-attr 'id) (name-attr 'name))
  (->> seq
       (cha--vec->list)
       (-map (lambda (resource)
               (cons (alist-get id-attr   resource)
                     (alist-get name-attr resource))))))

(defun reject-archived (item-list)
  (-reject (lambda (item) (or (equal :json-true (alist-get 'archived item))
                              (equal t (alist-get 'archived item))))
           item-list))

(cl-defun clubhouse-api-fetch-as-id-name-pairs
    (resource &optional
              (id-attr 'id)
              (name-attr 'name))
  "Return the given resource from clubhouse as (id . name) pairs"
  (let ((resp-json (cha--api-request "GET" resource)))
    (-> resp-json
        (cha--vec->list)
        (reject-archived)
        (to-id-name-pairs id-attr name-attr))))

;; TODO: Replace this with proper caching
(defvar-local clubhouse-api-last-project-list nil
  "Returns the result of the last call to `clubhouse-api-projects`")

(defun clubhouse-api-projects ()
  (setq-local clubhouse-api-last-project-list
              (clubhouse-api-fetch-as-id-name-pairs "projects")))

(defun clubhouse-api-project-list ()
  "Returns the list of projects from the cache, or retrieves it if necessary."
  (or clubhouse-api-last-project-list
      (clubhouse-api-projects)))

;; TODO: Replace this with proper caching
(defvar-local clubhouse-api-last-epic-list nil
  "Returns the result of the last call to `clubhouse-api-epics`")

(defun clubhouse-api-epics ()
  (setq-local clubhouse-api-last-epic-list
              (clubhouse-api-fetch-as-id-name-pairs "epics")))

(defun clubhouse-api-epic-list ()
  "Returns the list of epics from the cache, or retrieves it if necessary."
  (or clubhouse-api-last-epic-list
      (clubhouse-api-epics)))

(defun clubhouse-api-project-stories (project-id)
  (clubhouse-api-fetch-as-id-name-pairs (format "projects/%d/stories" project-id)))

(defun clubhouse-api-project-stories-full (project-id)
  "Retrieves the non-archived stories for a project, including all their attributes."
  (-> (cha--api-request "GET" (format "projects/%d/stories" project-id))
      (cha--vec->list)
      (reject-archived)))

(defvar-local clubhouse-api-workflow-cache nil)

(defun clubhouse-api-workflows ()
  "Retrieves the list of workflows."
  (or clubhouse-api-workflow-cache
      (setq-local clubhouse-api-workflow-cache
                  (cha--vec->list (cha--api-request "GET" "workflows")))))

(defun clubhouse-api-lookup-workflow-state (workflow-state-id)
  "Returns the workflow state given its ID."
  (->> (clubhouse-api-workflows)
       (-mapcat (lambda (wf) (cha--vec->list (alist-get 'states wf))))
       (-first (lambda (state) (= workflow-state-id (alist-get 'id state))))))

(defun clubhouse-api-lookup-workflow-state-name (workflow-state-name)
  "Returns the workflow state given its name."
  (->> (clubhouse-api-workflows)
       (-mapcat (lambda (wf) (cha--vec->list (alist-get 'states wf))))
       (-first (lambda (state) (string= workflow-state-name (alist-get 'name state))))))

(defun clubhouse-api-pair-name (x)
  (cdr x))

(defun clubhouse-api-pair-id (x)
  (car x))

(defun clubhouse-api-find-pair-by-name (name pairs)
  (-first (lambda (pair) (string= (clubhouse-api-pair-name pair) name))
          pairs))

(defun clubhouse-api-find-pair-by-id (id pairs)
  (-first (lambda (pair) (= (clubhouse-api-pair-id pair) id))
          pairs))

(defun clubhouse-api-prompt-for-project ()
  "Returns an (id . name) pair for a project selected by the user."
  (let* ((projects (clubhouse-api-project-list))
         (project-name (completing-read "Select a project: "
                                        (-map #'clubhouse-api-pair-name projects)
                                        nil
                                        t
                                        nil
                                        nil
                                        cha-clubhouse-default-project)))
    (clubhouse-api-find-pair-by-name project-name projects)))

(defun clubhouse-api-prompt-for-epic ()
  "Returns an (id . name) pair for an epic selected by the user."
  (let* ((epics (clubhouse-api-epics))
         (epic-name (completing-read "Select an epic: "
                                     (-insert-at 0 "[None]" (-map #'clubhouse-api-pair-name epics))
                                     nil
                                     nil
                                     nil
                                     nil)))
    (clubhouse-api-find-pair-by-name epic-name epics)))

(defun clubhouse-api-prompt-for-label ()
  "Return an (id, name) pair for a label selected by the user."
  (let* ((lbls (clubhouse-api-fetch-as-id-name-pairs "labels"))
         (label-name (completing-read
                      "Select a label: "
                      (-insert-at 0
                                  "[None]"
                                  (-map #'clubhouse-api-pair-name
                                        lbls)))))
    (clubhouse-api-find-pair-by-name label-name lbls)))

(defun clubhouse-api-prompt-for-story (&optional project-id)
  "Returns an (id . name) pair for a story selected by the user."
  (let* ((project-id (or project-id (clubhouse-api-pair-id (clubhouse-api-prompt-for-project))))
         (stories (clubhouse-api-project-stories project-id))
         (story-map (let* ((story-map (make-hash-table :test 'equal)))
                      (-each stories
                        (lambda (story)
                          (puthash (format "#%d: %s"
                                           (clubhouse-api-pair-id story)
                                           (clubhouse-api-pair-name story))
                                   story
                                   story-map)))
                      story-map))
         (story-name (completing-read "Select a story: " (hash-table-keys story-map))))
    (gethash story-name story-map)))

(defun cha--prompt-for-story-name ()
  "Prompt for, and return a story name."
  (read-string "Story name: " (org-entry-get nil "ITEM")))

(defvar-local cha--story-description "")

(defun cha--convert-org-mode-to-markdown (org-mode-string)
  "Convert `ORG-MODE-STRING' to markdown."
  (with-temp-buffer
    (setq-local org-export-with-toc nil)
    (setq-local org-export-show-temporary-export-buffer nil)

    (insert org-mode-string)
    (org-gfm-export-as-markdown)

    (with-current-buffer "*Org GFM Export*"
      (string-trim (buffer-string)))))

(defun cha--cache-description ()
  "Get the text under the current headline."

  ;; If were not at a headline, it means we're in the descrition of the story,
  ;; so move up to the previous headline.
  (when (not (eq 'headline (org-element-type (org-element-at-point))))
    (org-previous-visible-heading 1))

  (let ((end-of-contents (or (org-element-property
                              :contents-end
                              (org-element-at-point))
                             (point-max)))
        (beginning-of-contents
         (org-element-property :contents-begin (org-element-at-point))))
    (if beginning-of-contents
        (progn
          (goto-char beginning-of-contents)

          ;; Point is now on the first character after the headline. Find out
          ;; what type of element is here using org-element-at-point.
          (let* ((first-element (org-element-at-point)))
            ;; If this is a property drawer we need to skip over it. It will
            ;; have an :end property containing the buffer location of the first
            ;; character after the property drawer. Go there if necessary.
            (when (eq 'property-drawer (car first-element))
              (goto-char (org-element-property :end first-element))))

          ;; We're now at the beginning of the section text.

          ;; Convert the description to markdown format and cache it for later
          ;; use.
          (setq cha--story-description
                (let ((s
                       (buffer-substring-no-properties (point)
                                                       (- end-of-contents 1))))
                  (cha--convert-org-mode-to-markdown
                   (if (s-contains? ":LOGBOOK:" s)
                       (s-left (s-index-of ":LOGBOOK:" s) s)
                     s)))))
      (setq cha--story-description ""))))

(defun clubhouse-api-prompt-for-story-type ()
  "Prompts for and returns a story type."
  (completing-read "Story type: " '("feature" "bug" "chore")
                   nil                  ; predicate
                   t                    ; require-match
                   nil                  ; initial-input
                   nil                  ; history
                   "feature"            ; default
                   ))

(defun clubhouse-api-goto-description ()
  "Sets point to the beginning of the Description header."
  (goto-char (point-min))
  (re-search-forward "^\\*\\* Description\\s-*$")
  (beginning-of-line))

(defun clubhouse-api-update-story-properties (story)
  "Updates the properties header to reflect the latest values"
  (save-excursion
    (goto-char (point-min))
    ;; (org-set-property "Name" clubhouse-api-story-name)
    (org-set-property "ClubhouseType" "Story")
    (org-set-property "ID" (number-to-string (alist-get 'id story)))
    (org-set-property "URL" (alist-get 'app_url story))
    (org-set-property "Project"
                      (let* ((project-id (alist-get 'project_id story)))
                        (format "%d: %s"
                                project-id
                                (clubhouse-api-pair-name
                                 (clubhouse-api-find-pair-by-id project-id
                                                                (clubhouse-api-project-list))))))
    (org-set-property "StoryType" (alist-get 'story_type story))
    (org-set-property "Estimate" (let ((estimate (alist-get 'estimate story)))
                                   (if estimate
                                       (number-to-string estimate)
                                     "")))
    (org-set-property "Epic" (let ((epic-id (alist-get 'epic_id story)))
                               (if epic-id
                                   (format "%d: %s"
                                           epic-id
                                           (clubhouse-api-pair-name
                                            (clubhouse-api-find-pair-by-id epic-id
                                                                           (clubhouse-api-epic-list))))
                                 "")))
    (org-set-property "State" (->> story
                                   (alist-get 'workflow_state_id)
                                   clubhouse-api-lookup-workflow-state
                                   (alist-get 'name)))
    (org-set-property "Labels" (mapconcat (lambda (x) (alist-get 'name x))
                                          (alist-get 'labels story) ", "))
    (org-set-property "LastUpdated" (alist-get 'updated_at story))))

(defun clubhouse-api-populate-story-edit-buffer (story)
  "Populates a story edit buffer with the contents of a story."
  (let* ((story-id (alist-get 'id story))
         (story-name (alist-get 'name story)))
    (erase-buffer)
    (insert (format "* %d: %s\n" story-id story-name))
    (insert "** Description\n"
            "#+BEGIN_SRC markdown\n"
            (->> (alist-get 'description story)
                 (replace-regexp-in-string "^" "  ")
                 (replace-regexp-in-string "^  $" ""))
            "\n"
            "#+END_SRC")

    (goto-char (point-min))
    ;; (save-match-data
    ;;   (clubhouse-api-goto-description))
    ;; (beginning-of-line)
    ;; (forward-line)
    (clubhouse-api-update-story-properties story)
    (set-buffer-modified-p nil)))

(defun cha--edit-story (story)
  "Implementation side of cha-edit-story.

Edit provided `STORY'"
  (let* ((story-id (alist-get 'id story))
         (story-name (alist-get 'name story))
         ;; Changing the major mode blows away buffer locals,
         ;; so we preseve this one
         (last-projects (or clubhouse-api-last-project-list
                            (clubhouse-api-project-list))))
    (pop-to-buffer (format "Clubhouse Story %d: %s" story-id story-name))
    (org-mode)
    (setq-local clubhouse-api-last-project-list last-projects)
    (let* ((modified? (buffer-modified-p))
           (confirmed? (or (not modified?)
                           (yes-or-no-p "Buffer has been modified. Changes will be lost. Proceed anyway? "))))
      (when confirmed?
        (clubhouse-api-story-edit-minor-mode 1)
        (clubhouse-api-populate-story-edit-buffer story)))))

(defun cha-edit-story (story-number)
  "Edit an existing Clubhouse story.
Prompt for a story (or use `STORY-NUMBER'), then pop up a buffer with its
description ready for editing."
  (interactive "P")
  (let* ((story-id (if story-number
                       (read-number "Story number: ")
                     (or (->> (org-entry-get nil "ClubhouseUrl")
                              (s-split "/" )
                              last
                              car
                              string-to-number)
                         (clubhouse-api-pair-id (clubhouse-api-prompt-for-story)))))
         (story (clubhouse-api-get-story-op story-id)))
    (cha--edit-story story)))

(defun cha-create-story ()
  "Create a new Clubhouse story."
  (interactive)
  (save-excursion
    (cha--cache-description)
    (let* ((story-name (cha--prompt-for-story-name))
           (project (clubhouse-api-prompt-for-project))
           (story-type (clubhouse-api-prompt-for-story-type))
           (epic (clubhouse-api-prompt-for-epic))
           (lbls (clubhouse-api-create-label-params
                  (org-get-tags nil t)))
           ;; (estimate (read-number "Estimate: "))
           (created-story (clubhouse-api-create-story-op
                           (clubhouse-api-pair-id project)
                           story-name
                           :story_type story-type
                           :description cha--story-description
                           ;; :estimate estimate
                           :epic_id (clubhouse-api-pair-id epic)
                           :labels lbls)))
      (message "Story %d created" (alist-get 'id created-story))
      ;; (cha--edit-story created-story)
      (org-set-property "ClubhouseUrl" (alist-get 'app_url created-story)))))

(defun clubhouse-api-create-label-params (label-names)
  "Create a list of alists for labels with names LABEL-NAMES."
  (-map (lambda (x) (cha--list->alist `(:name ,x))) label-names))

(defun clubhouse-api-refresh-story (force?)
  "Update the current story.
Update the current story buffer with the latest online version of the story.
Fails with an error message if the local buffer contains changes unless `FORCE?`
is true."
  (interactive "P")
  (let* ((story-id (-> "ID"
                       clubhouse-api-story-edit-get-header-value
                       string-to-number))
         (modified? (buffer-modified-p))
         (confirmed? (or (not modified?)
                         force?)))
    (if (not confirmed?)
        (message "Story contains local edits that would be overwritten. Not refreshing.")
      (progn
        (clubhouse-api-populate-story-edit-buffer (clubhouse-api-get-story-op story-id))
        (message "Story was updated")))))

(defun clubhouse-api-story-edit-get-description ()
  "Returns the description portion of a story edit buffer."
  (save-excursion
    (save-match-data
      (clubhouse-api-goto-description)
      (forward-line)
      (re-search-forward  "^#\\+BEGIN_SRC markdown")
      (second (org-babel-get-src-block-info)))))

(defun clubhouse-api-story-edit-get-story-name ()
  "Returns the name of a story in a story edit buffer"
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (->> (-> (org-element-at-point)
               second
               (plist-get :raw-value))
           (s-split ":")
           rest
           (-interpose ":")
           (apply #'concat)
           s-trim))))

(defun clubhouse-api-story-edit-get-header-value (property-name)
  "Returns the value of header `property-name`."
  (let ((vals (org-property-values property-name)))
    (when vals (-> vals first s-trim))))

(defun clubhouse-api-story-edit-get-header-values (property-name)
  "Return the value of header PROPERTY-NAME."
  (-map 's-trim
        (when-let* ((prop-name (first (org-property-values property-name))))
          (s-split "," prop-name))))

;; TODO: Also save the headers that make sense to save, like name and workflow state
(defun clubhouse-api-save-story (&optional quit)
  "Save a story by sending it to Clubhouse via their API, and optionally QUIT."
  (interactive)
  (let* ((story-id (-> "ID"
                       clubhouse-api-story-edit-get-header-value
                       string-to-number))
         (story (clubhouse-api-get-story-op story-id))
         (last-updated (clubhouse-api-story-edit-get-header-value "LastUpdated")))
    (if (string< last-updated (alist-get 'updated_at story))
        (message "Story has changed since loaded. Refusing to save. TODO: Give option to merge or whatever.")
      (let* ((lbls (clubhouse-api-story-edit-get-header-values "Labels"))
             (updated-story (clubhouse-api-update-story-op
                             story-id
                             :description (clubhouse-api-story-edit-get-description)
                             :story_type (clubhouse-api-story-edit-get-header-value "StoryType")
                             :name (clubhouse-api-story-edit-get-story-name)
                             :estimate (let ((estimate (clubhouse-api-story-edit-get-header-value "Estimate")))
                                         (when estimate
                                           (string-to-number estimate)))
                             :labels (clubhouse-api-create-label-params lbls)
                             :workflow_state_id (let ((workflow-state-name (clubhouse-api-story-edit-get-header-value "State")))
                                                  (alist-get 'id (clubhouse-api-lookup-workflow-state-name workflow-state-name))))))
        (clubhouse-api-update-story-properties updated-story)
        (set-buffer-modified-p nil)
        (message "Story successfully updated.")
        (when quit
          (clubhouse-api-quit))))))

(defun clubhouse-api-pop-to-stories-buffer (buffer-name stories)
  "Creates buffer `buffer-name` and populates it with org text
containing `stories`."
  (pop-to-buffer buffer-name)
  (erase-buffer)
  (org-mode)
  (insert "* Stories\n")
  (if (zerop (length stories))
      (insert "No stories\n")
    (-each (->> stories
                (-group-by (lambda (story) (alist-get 'workflow_state_id story))))
      (lambda (group)
        (let* ((workflow-state-id (first group)))
          (insert "** "
                  (->> (clubhouse-api-lookup-workflow-state workflow-state-id)
                       (alist-get 'name))
                  "\n")
          (-each (rest group)
            (lambda (story)
              (insert "*** [["
                      (alist-get 'app_url story)
                      "][#"
                      (number-to-string (alist-get 'id story))
                      ": "
                      (alist-get 'name story)
                      "]] :"
                      (alist-get 'story_type story)
                      ":\n")
              (let* ((start (point))
                     (story story)
                     (map (make-sparse-keymap))
                     (open-story #'(lambda ()
                                     (interactive)
                                     (cha--edit-story (clubhouse-api-get-story-op (alist-get 'id story))))))
                (define-key map (kbd "C-c C-o") open-story)
                (define-key map (kbd "RET") open-story)
                (define-key map (kbd "<mouse-2>") open-story)
                (insert-button "Edit" 'action open-story 'keymap map)
                ;; (add-text-properties start (point) `(local-map ,map face org-link))
                )
              (insert "\n")))))))
  (goto-char (point-min))
  (outline-show-all)
  (outline-hide-leaves)
  (org-align-all-tags)
  (set-buffer-modified-p nil)
  (message "Done"))

(defun clubhouse-api-browse-project ()
  "Pops up an org buffer that shows all the stories in a project."
  (interactive)
  (let* ((project (clubhouse-api-prompt-for-project))
         (project-id (clubhouse-api-pair-id project))
         (project-name (clubhouse-api-pair-name project)))
    (clubhouse-api-pop-to-stories-buffer (format "Clubhouse Project %d: %s" project-id project-name)
                                         (clubhouse-api-project-stories-full project-id))))

(defun clubhouse-api-search-stories (query)
  "Pops up an org buffer that shows stories for a given search."
  (interactive "MQuery: ")
  (clubhouse-api-pop-to-stories-buffer (format "Clubhouse Search %s" query)
                                       (clubhouse-api-search-stories-op query)))

(defun clubhouse-api-quit (&optional print-abort-message)
  "Quit the editing of the current story, and optionally PRINT-ABORT-MESSAGE."
  (interactive)
  (quit-window t)
  (when print-abort-message
    (message "Story editing aborted.")))

(defvar clubhouse-api-story-edit-minor-mode-map
  (let ((map (make-keymap)))
    map))

(define-minor-mode clubhouse-api-story-edit-minor-mode
  "Minor mode for interacting with Clubhouse.io via their API"
  :lighter " Clubhouse"
  :keymap clubhouse-api-story-edit-minor-mode-map
  ;; Because it's a global prefix, we have to set it locally rather
  ;; than putting it in the mode map
  (use-local-map (copy-keymap org-mode-map))
  (local-set-key (kbd "C-c C-c") (lambda ()
                                   (interactive)
                                   (clubhouse-api-save-story t)))
  (local-set-key (kbd "C-x C-s") 'clubhouse-api-save-story)
  (local-set-key (kbd "C-c C-r") 'clubhouse-api-refresh-story)
  (local-set-key (kbd "C-c C-k") (lambda ()
                                   (interactive)
                                   (clubhouse-api-quit t))))

(provide 'cha)

;;; cha.el ends here
