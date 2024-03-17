;;; structurizr-mode.el --- Major mode for editing Structurizr dsl files

;; Author: Giles Paterson <giles@vurt.uk>
;; Created: 15 June 2022
;; Version: 0.1
;; Keywords Structurizer C4 dsl major-mode

;; Copyright (c) 2022 Giles Paterson <giles@vurt.uk>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Commentary:
;;
;; This mode is intended to aid in the creation of Structurizr dsl files using emacs.
;;
;; It takes heavy inspiration from the Emacs Wiki Mode Tutorial (https://www.emacswiki.org/emacs/ModeTutorial) and Xah Lee's major mode tutorial (http://xahlee.info/emacs/emacs/elisp_syntax_coloring.html)

;;; Code:

;; Define mode hook
(defvar structurizr-mode-hook nil)

(add-to-list 'auto-mode-alist '("\\.dsl\\'" . structurizr-mode))

;; create the keyword list for font-lock
;; each category of keyword is given a particular face
(setq structurizr-font-lock-keywords
      (let* (
             ;; define several categories of keywords
             (x-keywords '("enterprise" "workspace" "views" "model" ))
             (x-types '("person" "softwareSystem" "deploymentEnvironment" "deploymentNode" "deploymentGroup" "infrastructureNode" "containerInstance" "softwareSystemInstance" "systemLandscape" "systemContext" "container" "component" "filtered" "dynamic" "deployment" "styles" "themes" "branding" "element" "relationship" "group" "properties" "perspectives"))
             (x-relationship '("->"))
             (x-properties '("include" "exclude" "autoLayout" "tags" "url" "title" "shape" "icon" "width" "height" "background" "colour" "color" "stroke" "fontSize" "border" "opacity" "metadata" "description" "thickness" "dashed" "routing" "position" "technology"))

             ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-relationship-regexp (regexp-opt x-relationship))
             (x-properties-regexp (regexp-opt x-properties 'words)))

        `(
          (,x-types-regexp . 'font-lock-type-face)
          (,x-properties-regexp . 'font-lock-variable-name-face)
          (,x-keywords-regexp . 'font-lock-keyword-face)
          (,x-relationship-regexp . 'font-lock-function-name-face)
          ;; note: order above matters, because once coloured, that part won't change.
          ;; in general, put longer words first
          )))

(defvar structurizr-mode-syntax-table nil "Syntax table for `structurizr-mode'.")

(setq structurizr-mode-syntax-table
      (let ((syntax-table (make-syntax-table)))
                                        ; Comment styles are the same as C++/Java
        (modify-syntax-entry ?/ ". 124" syntax-table)
        (modify-syntax-entry ?* ". 23b" syntax-table)
        (modify-syntax-entry ?\n ">" syntax-table)
        syntax-table))

;; indentation
(defvar structurizr-indent-offset 4
  "*Indentation offset for `structurizr-mode'.")

(defun structurizr-indent-line ()
  "Indent current line as Structurizr dsl"
  (interactive)
  (let ((indent-col 0))
    (save-excursion
      (beginning-of-line)
      (condition-case nil
          (while t
            (backward-up-list 1)
            (when (looking-at "{")
              (setq indent-col (+ indent-col structurizr-indent-offset))))
        (error nil)))
    (save-excursion
      (back-to-indentation)
      (when (and (looking-at "}") (>= indent-col structurizr-indent-offset))
        (setq indent-col (- indent-col structurizr-indent-offset))))
    (indent-line-to indent-col)))


;;;###autoload
(define-derived-mode structurizr-mode fundamental-mode "Structurizr"
  "Major mode for editing Structurizr dsl"
  ;; indentation
  (make-local-variable 'structurizr-indent-offset)
  (set (make-local-variable 'indent-line-function) 'structurizr-indent-line)
  ;; syntax table
  (set-syntax-table structurizr-mode-syntax-table)
  ;; code for syntax highlighting
  (setq font-lock-defaults '((structurizr-font-lock-keywords)))
  (run-hooks 'structurizr-mode-hook))


;; add the mode to the `features' list
(provide 'structurizr-mode)

;;; structurizr-mode.el ends here
