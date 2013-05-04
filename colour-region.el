;;; colour-region.el --- Toggle regions of the buffer with different text snippets

;; Filename: colour-region.el
;; Description: Toggle regions of the buffer with different text snippets
;; Author: Joe Bloggs <vapniks@yahoo.com>
;; Maintainer: Joe Bloggs <vapniks@yahoo.com>
;; Copyleft (Ↄ) 2013, Joe Bloggs, all rites reversed.
;; Created: Sometime in 2008 (can't remember when exactly)
;; Version: 0.1
;; Last-Updated: 2013-05-04 21:37:20
;;           By: Joe Bloggs
;; URL: https://github.com/vapniks/colour-region
;; Keywords: 
;; Compatibility: GNU Emacs 24.3.1
;; Package-Requires:  
;;
;; Features that might be required by this library:
;;
;; 
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.
;; If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: 
;;
;; 
;; Toggle regions of the buffer with different text snippets
;; 

;;; Installation:
;;
;; Put colour-region.el in a directory in your load-path, e.g. ~/.emacs.d/
;; You can add a directory to your load-path with the following line in ~/.emacs
;; (add-to-list 'load-path (expand-file-name "~/elisp"))
;; where ~/elisp is the directory you want to add 
;; (you don't need to do this for ~/.emacs.d - it's added by default).
;;
;; Add the following to your ~/.emacs startup file.
;;
;; (require 'colour-region)

;;; Customize:
;;
;; `colour-region-formats' : List of text-properties to apply to each region type.
;; `colour-region-save-on-kill' : If set to t then save colour-regions when buffer is killed.
;; `colour-region-load-on-find-file' : If set to t then always load colour-regions when a new file is opened.

;;
;; All of the above can customized by:
;;      M-x customize-group RET colour-region RET
;;

;;; Change log:
;;	
;; 2013/05/04
;;      * First released.
;; 

;;; Acknowledgements:
;;
;; This code is heavily based on hide-region by Mathias Dahl, available here: http://www.emacswiki.org/emacs/hide-region.el
;;

;;; TODO
;;
;;  colour-region-kill, colour-region-copy
;; 

;;; Require


;;; Code:


(defgroup colour-region nil
  "Functions to define associated regions within buffer, 
and apply different overlays to them."
  :prefix "colour-region-"
  :group 'convenience)

(defcustom colour-region-formats
  '((("" nil
      (:background "blue" :foreground "black") nil ""
      (:background "blue" :foreground "black")
      (face . (:foreground "blue")))
     ("@[1:" t
      (:background "blue" :foreground "black") nil "]@"
      (:background "blue" :foreground "black")
      (invisible . t)
      (intangible . t))
     ("@[1:" nil
      (:background "blue" :foreground "black") nil "]@"
      (:background "blue" :foreground "black")
      (face . (:background "blue"))))
    (("" nil
      (:background "red" :foreground "black") nil ""
      (:background "red" :foreground "black")
      (face . (:foreground "red")))
     ("@[2:" t
      (:background "red" :foreground "black") nil "]@"
      (:background "red" :foreground "black")
      (invisible . t)
      (intangible . t))
     ("@[2:" nil
      (:background "red" :foreground "black") nil "]@"
      (:background "red" :foreground "black")
      (face . (:background "red"))))
    (("" nil
      (:background "green" :foreground "black") nil ""
      (:background "green" :foreground "black")
      (face . (:foreground "green")))
     ("@[3:" t
      (:background "green" :foreground "black") nil "]@"
      (:background "green" :foreground "black")
      (invisible . t)
      (intangible . t))
     ("@[3:" nil
      (:background "green" :foreground "black") nil "]@"
      (:background "green" :foreground "black")
      (face . (:background "green"))))
    (("" nil
      (:background "yellow" :foreground "black") nil ""
      (:background "yellow" :foreground "black")
      (face . (:foreground "yellow")))
     ("@[4:" t
      (:background "yellow" :foreground "black") nil "]@"
      (:background "yellow" :foreground "black")
      (invisible . t)
      (intangible . t))
     ("@[4:" nil
      (:background "yellow" :foreground "black") nil "]@"
      (:background "yellow" :foreground "black")
      (face . (:background "yellow"))))
    (("" nil
      (:background "cyan" :foreground "black") nil ""
      (:background "cyan" :foreground "black")
      (face . (:foreground "cyan")))
     ("@[5:" t
      (:background "cyan" :foreground "black") nil "]@"
      (:background "cyan" :foreground "black")
      (invisible . t)
      (intangible . t))	 
     ("@[5:" nil
      (:background "cyan" :foreground "black") nil "]@"
      (:background "cyan" :foreground "black")
      (face . (:background "cyan"))))
    (("" nil
      (:background "pink" :foreground "black") nil ""
      (:background "pink" :foreground "black")
      (face . (:foreground "pink")))
     ("@[6:" t
      (:background "pink" :foreground "black") nil "]@"
      (:background "pink" :foreground "black")
      (invisible . t)
      (intangible . t))
     ("@[6:" nil
      (:background "pink" :foreground "black") nil "]@"
      (:background "pink" :foreground "black")
      (face . (:background "pink")))))
  "List of text-properties to apply to each region type."
  :type '(repeat sexp)
  :group 'colour-region)

(defcustom colour-region-save-on-kill t
  "If set to t then save colour-regions when buffer is killed.
If set to prompt then prompt to save. If nil then don't save.
This is a buffer local variable."
  :group 'colour-region
  :type '(choice (const :tag "Always save" t)
		 (const :tag "Prompt" prompt)
		 (const :tag "Never save" nil)))

(make-variable-buffer-local 'colour-region-save-on-kill)

(defcustom colour-region-load-on-find-file t
  "If set to t then always load colour-regions when a new file is opened.
If set to prompt then prompt to load. If nil then don't load."
  :group 'colour-region
  :type '(choice (const :tag "Always load" t)
		 (const :tag "Prompt" prompt)
		 (const :tag "Never load" nil)))

(make-variable-buffer-local 'colour-region-load-on-find-file)

;;; create new colour-region format list and append to colour-region-formats
(defun colour-region-create-new-type nil
  "Create new colour-region format list and append to colour-region-formats."
  (let* ((newtypenum (1+ (length colour-region-formats)))
	 (newtypestring (number-to-string newtypenum))
	 (newhiddenformat
          (list (concat "@[" newtypestring ":")
                t '(:background ? :foreground "black")
                nil "]@"
                '(:background ? :foreground "black")
                '(invisible . t)
                '(intangible . t)))
	 (newunhiddenformat (list (concat "@[" newtypestring ":")
                                  t '(:background ? :foreground "black")
                                  nil "]@"
                                  '(:background ? :foreground "black")
                                  '(face . (:background "cyan"))))
	 (newformatlist (list newhiddenformat newunhiddenformat)))
    (setcar (nthcdr (1- newtypenum) colour-region-formats) (cons newformatlist nil))))

(defvar colour-regions nil
  "List of colour-regions.
Each element is a list containing the following elements (in order):
buffer name
start position
end position 
comment 
current type
current state
index of current content in following list
list of content lists, each of which contains the following elements:
     state associated with content
     comment for content
     text string to display
an overlay corresponding to the colour-region")

(make-variable-buffer-local 'colour-regions)

(defvar colour-region-kill-ring nil
  "Variable to store killed/copied regions (that may be yanked with colour-region-yank).")

(defun colour-region-apply-according-to-prefix (func)
  "Apply function FUNC to colour region(s) according to current prefix arg.

With no prefix argument apply to nearest colour-region.
With non-zero prefix argument apply to all colour-regions of type corresponding to argument.
With prefix argument of zero apply to all colour-regions in current buffer."
  (if current-prefix-arg
      ;; if prefix argument is 0, change all colour-regions in current buffer
      (if (equal current-prefix-arg 0)
          (dolist (current colour-regions)
            (if (equal (buffer-name) (car current))
                (apply func cregion))) 
        ;; else if prefix argument is other number, change corresponding colour-regions
        (if (and (<= current-prefix-arg (length colour-region-formats)) (> current-prefix-arg 0))
            (dolist (current colour-regions)
              (if (and (equal (buffer-name) (car current)) 
                       (equal (nth 4 current) (1- current-prefix-arg)))
                  (apply func cregion)))))
    ;; otherwise (e.g. no prefix argument) just change the one closest to point
    (let ((bestindex (colour-region-find-nearest (lambda (hideregion) t))))
      (if bestindex (let ((current (nth bestindex colour-regions)))
                      (apply func cregion))
        ;; else give message to user
        (message "No colour-regions found in current buffer!")))))

;;; Create new colour-region and place in colour-regions.
(defun colour-region-new (comment)
  "Create a new colour-region for selected region (if no region is selected inform user):
1) Prompt user for comment for colour-region. 
2) If a positive prefix argument is given set colour-region type to that corresponding with prefix argument.
   Otherwise use type 1 colour-region.
3) Set state of colour-region to 1.
4) Add colour-region to colour-regions variable.
5) Apply overlay with format in colour-region-formats corresponding to state and type of colour-region.

Actually internal type and state values start from 0 not 1, 
but since I use prefix argument of 0 to mean all buffers, I use 1 to indicate initial type."
  (interactive 
   ;; prompt for comment string for colour region
   (list 
    (if mark-active 
	(read-string "Comment string (default is first line of region): " nil nil 
		     ;; set default comment string to first line of region
		     (buffer-substring-no-properties (region-beginning) 
						     (let ((oldpoint (point)) lineend) 
						       (goto-char (region-beginning)) 
						       (setq lineend (line-end-position)) 
						       (goto-char oldpoint) 
						       (min lineend (region-end))))))))
  (if mark-active
      ;; make new colour-region
      (let ((colourregion (list (buffer-name) (region-beginning) (region-end) comment
                                ;; use prefix argument-1 as colour-region type, or 0 if none given
                                (if (and current-prefix-arg 
                                         (<= current-prefix-arg (length colour-region-formats)) 
                                         (> current-prefix-arg 0))
                                    (1- current-prefix-arg) 0) 0 0
                                (list 
                                 (list 0 comment 
                                       (buffer-substring-no-properties 
                                        (region-beginning) (region-end))))
                                (make-overlay (region-beginning) (region-end)))))
	;; apply colour-region
	(colour-region-apply-overlay colourregion)
	;; append new colour-region to colour-regions
	(setq colour-regions (if (equal colour-regions (list nil))
				 (list colourregion)
			       (append colour-regions (list colourregion))))
	;; deactivate the mark
	(deactivate-mark))
    (message "No region selected!")))

;;; Save text-part/create new text part for colour-region(s)
(defun colour-region-store-text nil
  "If region is selected then run colour-region-new function.
Otherwise save currently displayed text, comment and state of colour-region(s).
Insert the new text-part into the next position in the text-parts list of this colour-region(s). 

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive)
  (if (and mark-active (not (equal (point) (mark))))
      ;; if a region has been selected then just call colour-region-new
      (call-interactively 'colour-region-new)
    (if current-prefix-arg
	;; if prefix argument is 0, save text of all colour-regions in current buffer
	(progn
	  (if (equal current-prefix-arg 0)
	      (dolist (current colour-regions) 
		(if (equal (buffer-name) (car current))
		    (colour-region-apply-save-text-part current 1)))
	    ;; else if prefix argument is other number, save corresponding colour-regions
	    (if (and (<= current-prefix-arg (length colour-region-formats)) (> current-prefix-arg 0))
		(dolist (current colour-regions)
		  (if (and (equal (buffer-name) (car current))
			   (equal (nth 4 current) (1- current-prefix-arg)))
		      (colour-region-apply-save-text-part current 1)))))
	  (message "New text region saved"))
      ;; otherwise (e.g. no prefix argument) just save the one closest to point
      (let ((bestindex (colour-region-find-nearest (lambda (colourregion) t))))
	(if bestindex 
	    (progn
	      (colour-region-apply-save-text-part (nth bestindex colour-regions) 1)
	      (message "New text region saved"))
	  ;; else give message to user
	  (message "No colour-regions found in current buffer!"))))))

;;; Toggle overlay state of colour-region(s), or create new colour-region if region is selected.
(defun colour-region-toggle-overlay nil
  "If region is selected then run colour-region-new function.
Otherwise toggle overlay state of colour-region(s): 
       if colour-region(s) is in final state, set it to state 0, 
       otherwise set it to next state.

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive)
  (if (and mark-active (not (equal (point) (mark))))
      ;; if a region has been selected then just call colour-region-new
      (call-interactively 'colour-region-new)
    (if current-prefix-arg
	;; if prefix argument is 0, toggle all colour-regions in current buffer
	(if (equal current-prefix-arg 0)
	    (dolist (current colour-regions) 
	      (if (equal (buffer-name) (car current))
		  (colour-region-apply-toggle-overlay current)))
	  ;; else if prefix argument is other number, toggle corresponding colour-regions
	  (if (and (<= current-prefix-arg (length colour-region-formats)) (> current-prefix-arg 0))
	      (dolist (current colour-regions) 
		(if (and (equal (buffer-name) (car current))
			 (equal (nth 4 current) (1- current-prefix-arg)))
		    (colour-region-apply-toggle-overlay current)))))
      ;; otherwise (e.g. no prefix argument) just toggle the one closest to point
      (let ((bestindex (colour-region-find-nearest (lambda (colourregion) t))))
        (if bestindex (colour-region-apply-toggle-overlay (nth bestindex colour-regions))
          ;; else give message to user
          (message "No colour-regions found in current buffer!"))))))

;;; Toggle text-state of colour-region(s) or create new colour-region is region is selected
(defun colour-region-toggle-text nil
  "If region is selected then run colour-region-new function.
Otherwise save current text in current text-region of colour-region(s),
and toggle to next text-state: 
       if colour-region(s) is in final state, set it to state 0, 
       otherwise set it to next state.

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive)
  (if (and mark-active (not (equal (point) (mark))))
      ;; if a region has been selected then just call colour-region-new
      (call-interactively 'colour-region-new)
    (if current-prefix-arg
	;; if prefix argument is 0, toggle all colour-regions in current buffer
	(if (equal current-prefix-arg 0)
	    (dolist (current colour-regions) 
	      (if (equal (buffer-name) (car current))
		  (colour-region-apply-toggle-text-part current)))
	  ;; else if prefix argument is other number, toggle corresponding colour-regions
	  (if (and (<= current-prefix-arg (length colour-region-formats)) (> current-prefix-arg 0))
	      (dolist (current colour-regions) 
		(if (and (equal (buffer-name) (car current))
			 (equal (nth 4 current) (1- current-prefix-arg)))
		    (colour-region-apply-toggle-text-part current)))))
      ;; otherwise (e.g. no prefix argument) just toggle the one closest to point
      (let ((bestindex (colour-region-find-nearest (lambda (colourregion) t))))
        (if bestindex (colour-region-apply-toggle-text-part (nth bestindex colour-regions))
          ;; else give message to user
          (message "No colour-regions found in current buffer!"))))))

;;; Remove colour-region overlay and delete it from colour-regions.
(defun colour-region-remove nil
  "Remove colour-region(s), and delete from colour-regions.

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive)
  (if current-prefix-arg
      ;; if prefix argument is 0, remove all colour-regions in current buffer
      (if (equal current-prefix-arg 0)
	  (dolist (current colour-regions)
	    (if (equal (buffer-name) (car current))
		(colour-region-apply-remove current))) 
	;; else if prefix argument is other number, remove corresponding colour-regions
	(if (and (<= current-prefix-arg (length colour-region-formats)) (> current-prefix-arg 0))
	    (dolist (current colour-regions)
	      (if (and (equal (buffer-name) (car current)) 
		       (equal (nth 4 current) (1- current-prefix-arg)))
		  (colour-region-apply-remove current)))))
    ;; otherwise (e.g. no prefix argument) just remove the one closest to point
    (let ((bestindex (colour-region-find-nearest (lambda (hideregion) t))))
      (if bestindex (colour-region-apply-remove (nth bestindex colour-regions))
        ;; else give message to user
        (message "No colour-regions found in current buffer!")))))

;;; Apply elisp function to colour-region(s)
(defun colour-region-func (func)
  "Apply a user-supplied elisp function to colour-region(s).
The function (func) should take two arguments: the start and end positions of a region.
If applied to several colour-regions (i.e. when a prefix argument is used) func is applied 
to colour-regions one at a time in the order in which they appear in the current buffer.

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive (list (read-minibuffer "Function to apply: ")))
  ;; remove all overlays from current buffer
  ;; (they will be reapplied later
  (dolist (current colour-regions)
    (if (equal (nth 0 current) (buffer-name))
	(remove-overlays (nth 1 current) (nth 2 current))))
  ;; sort colour-regions by end position
  ;; to make adjusting overlays easier later
  (setq colour-regions
	(sort colour-regions 
	      (lambda (overlayA overlayB) 
		(< (nth 2 overlayA) (nth 2 overlayB)))))
  (if current-prefix-arg
      ;; if prefix argument is 0, apply to all colour-regions in current buffer
      (if (equal current-prefix-arg 0)
	  (dotimes (i (length colour-regions))
	    ;; only consider overlays on current buffer
	    (if (equal (buffer-name) (car (nth i colour-regions)))
		;; need to keep track of changes to buffer positions
		(let* ((oldpoint (point))
		       (overlaytochange (nth i colour-regions))
		       (overlaystart (nth 1 overlaytochange))
		       (overlayend (nth 2 overlaytochange))
		       newpoint change)
		  (goto-char overlayend)
		  ;; call function on current overlay
		  (funcall func overlaystart overlayend)
		  ;; work out how much buffer positions have changed
		  (setq newpoint (point))
		  (setq change (- newpoint overlayend))
		  (goto-char oldpoint)
		  ;; adjust overlays positions in colour-regions appropriately
		  (dotimes (j (- (length colour-regions) i))
		    (setq overlaytochangeB (nth (+ i j) colour-regions))
		    (if (equal (nth 0 overlaytochangeB) (buffer-name))
			(setcar (nthcdr (+ i j) colour-regions)
				(list (nth 0 overlaytochangeB)
				      (if (equal j 0)
					  (nth 1 overlaytochangeB)
                                        (+ (nth 1 overlaytochangeB) change))
				      (+ (nth 2 overlaytochangeB) change)
				      (nth 3 overlaytochangeB)
				      (nth 4 overlaytochangeB)
				      (nth 5 overlaytochangeB))))))))
	;; else if prefix argument is other number, apply to all corresponding colour-regions
	(if (and (< current-prefix-arg 7) (> current-prefix-arg 0))
	    (dotimes (i (length colour-regions))
	      ;; only consider overlays on current buffer of correct type
	      (if (and (equal (buffer-name) (car (nth i colour-regions)))
		       (equal (nth 4 (nth i colour-regions)) current-prefix-arg))
		  ;; need to keep track of changes to buffer positions
		  (let* ((oldpoint (point))
			 (overlaytochange (nth i colour-regions))
			 (overlaystart (nth 1 overlaytochange))
			 (overlayend (nth 2 overlaytochange))
			 newpoint change)
		    (goto-char overlayend)
		    ;; call function on current overlay
		    (funcall func overlaystart overlayend)
		    ;; work out how much buffer positions have changed
		    (setq newpoint (point))
		    (setq change (- newpoint overlayend))
		    (goto-char oldpoint)
		    ;; adjust overlays in colour-regions appropriately
		    (dotimes (j (- (length colour-regions) i))
		      (setq overlaytochangeB (nth (+ i j) colour-regions))
		      (if (equal (nth 0 overlaytochangeB) (buffer-name))
			  (setcar (nthcdr (+ i j) colour-regions)
				  (list (nth 0 overlaytochangeB)
					(if (equal j 0)
					    (nth 1 overlaytochangeB)
					  (+ (nth 1 overlaytochangeB) change))
					(+ (nth 2 overlaytochangeB) change)
					(nth 3 overlaytochangeB)
					(nth 4 overlaytochangeB)
					(nth 5 overlaytochangeB))))))))))
    ;; if no prefix argument was given apply to nearest colour-region in current buffer
    (let ((bestindex (colour-region-find-nearest (lambda (colourregion) t))))
      (if bestindex
	  ;; If a match was found then apply func to appropriate overlay,
	  ;; and shift other overlays appropriately.
	  ;; Need to keep track of changes to buffer positions.
	  (let* ((oldpoint (point))
		 (overlaytochange (nth bestindex colour-regions))
		 (overlaystart (nth 1 overlaytochange))
		 (overlayend (nth 2 overlaytochange))
		 newpoint change)
	    (goto-char overlayend)
	    ;; call function on current overlay
	    (funcall func overlaystart overlayend)
	    ;; work out how much buffer positions have changed
	    (setq newpoint (point))
	    (setq change (- newpoint overlayend))
	    (goto-char oldpoint)
	    ;; adjust overlays in colour-regions appropriately
	    (dotimes (j (- (length colour-regions) bestindex))
	      (setq overlaytochangeB (nth (+ bestindex j) colour-regions))
	      (if (equal (nth 0 overlaytochangeB) (buffer-name))
		  (setcar (nthcdr (+ bestindex j) colour-regions)
			  (list (nth 0 overlaytochangeB)
				(if (equal j 0)
				    (nth 1 overlaytochangeB)
				  (+ (nth 1 overlaytochangeB) change))
				(+ (nth 2 overlaytochangeB) change)
				(nth 3 overlaytochangeB)
				(nth 4 overlaytochangeB)
				(nth 5 overlaytochangeB))))))
	;; else give message to user
	(message "No colour-region found in current buffer!"))))
  ;; re-apply overlays to all colour-regions
  (dolist (overlaytochange colour-regions)
    (if (equal (buffer-name) (car overlaytochange))
	(colour-region-apply-overlay overlaytochange))))


;;; jump to next colour-region in current buffer
(defun colour-region-next nil
  "Move point to next colour-region in current buffer.

If no prefix argument is given, move to next colour-region in current buffer.
If a prefix argument of 0 is given, move to first colour-region in current buffer.
If a positive non-zero prefix argument is given, move to next colour-region with type 
corresponding to that prefix argument."
  (interactive)
  (let (predicate1 predicate2 (best nil))
    (if current-prefix-arg
	(if (equal current-prefix-arg 0)
	    (setq predicate1 
		  (lambda (colourregion) t)
		  predicate2 
		  (lambda (colourregion) (> (nth 1 colourregion) best)))
 	  (if (and (<= current-prefix-arg (length colour-region-formats)) (> current-prefix-arg 0))
	      (setq predicate1 
		    (lambda (colourregion) (equal (nth 4 colourregion) (1- current-prefix-arg)))
		    predicate2
		    (lambda (colourregion) (<= (nth 1 current) best)))))
      (setq predicate1 
	    (lambda (colourregion) t)
	    predicate2 
	    (lambda (colourregion) (<= (nth 1 current) best))))
    (dolist (current colour-regions)
      (if (and (equal (nth 0 current) (buffer-name)) 
               (> (nth 1 current) (point)) 
               (funcall predicate1 current))
          (if (not best)
              (setq best (nth 1 current))
            (if (funcall predicate2 current)
                (setq best (nth 1 current))))))
    (if best (goto-char best)
      (message "No further colour-regions found in current buffer!"))))

;;; jump to previous colour-region in current buffer
(defun colour-region-previous nil
  "Move point to previous colour-region in current buffer.

If no prefix argument is given, move to previous colour-region in current buffer.
If a prefix argument of 0 is given, move to last colour-region in current buffer.
If a positive non-zero prefix argument is given, move to previous colour-region with type 
corresponding to that prefix argument."
  (interactive)
  (let (predicate1 predicate2 (best nil))
    (if current-prefix-arg
	(if (equal current-prefix-arg 0)
	    (setq predicate1 
		  (lambda (colourregion) t)
		  predicate2 
		  (lambda (colourregion) (< (nth 1 colourregion) best)))
 	  (if (and (<= current-prefix-arg (length colour-region-formats)) (> current-prefix-arg 0))
	      (setq predicate1 
		    (lambda (colourregion) (equal (nth 4 colourregion) (1- current-prefix-arg)))
		    predicate2
		    (lambda (colourregion) (>= (nth 1 current) best)))))
      (setq predicate1 
	    (lambda (colourregion) t)
	    predicate2 
	    (lambda (colourregion) (>= (nth 1 current) best))))
      (dolist (current colour-regions)
	(if (and (equal (nth 0 current) (buffer-name)) 
		 (< (nth 1 current) (point)) 
		 (funcall predicate1 current))
	    (if (not best)
		(setq best (nth 1 current))
	      (if (funcall predicate2 current)
		  (setq best (nth 1 current))))))
      (if best (goto-char best)
	(message "No further colour-regions found in current buffer!"))))



;;; copy colour-region to colour-region-kill-ring
(defun colour-region-copy nil
  "Copy colour-region to colour-region-kill-ring.
With no prefix argument copy nearest colour-region.
With non-zero prefix argument copy all colour-regions of type corresponding to argument.
With prefix argument of zero copy all colour-regions in current buffer."
  (interactive)
  (colour-region-apply-according-to-prefix ???))

;;; kill colour-region to colour-region-kill-ring
(defun colour-region-kill nil
  "Kill colour-region and hidden text to colour-region-kill-ring.
With no prefix argument kill nearest colour-region.
With non-zero prefix argument kill all colour-regions of type corresponding to argument.
With prefix argument of zero kill all colour-regions in current buffer."
  (interactive)
  (colour-region-apply-according-to-prefix ???))

;;; change the comment of colour-region(s)
(defun colour-region-change-comment (comment)
  "Change the comment of a colour-region(s).

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive
   (list (read-string "Comment string (default is first line of region): " nil nil 
                      ;; set default comment string to first line of region
                      (buffer-substring-no-properties (region-beginning) 
                                                      (let ((oldpoint (point)) lineend) 
                                                        (goto-char (region-beginning)) 
                                                        (setq lineend (line-end-position)) 
                                                        (goto-char oldpoint) 
                                                        (min lineend (region-end)))))))
  (colour-region-apply-according-to-prefix
   (lambda (cregion)
     (setcar (nthcdr 3 current) comment)
     (colour-region-apply-overlay current))))

;;; Change the type of colour-region(s)
(defun colour-region-change-type (type)
  "Change the type of a colour-region(s). 
Prompts for a new type number, if the number entered is larger than the number of 
currently available types then a new type is created (with value one higher than the 
previous highest type number) and the colour-region is set to that type.
If the number entered is invalid or less than 1, then the type is not changed.

If no prefix argument is given, apply to nearest colour-region in current buffer.
If a prefix argument of 0 is given, apply to all colour-regions in current buffer.
If a positive non-zero prefix argument is given, apply to all colour-regions in current buffer
with type corresponding to that prefix argument."
  (interactive (string-to-number (read-no-blanks-input "New type (default is current type): ")))
  ;; if number entered is higher than highest type available, create new type and use it
  (if (> type (length colour-region-formats))
      (progn
	(setq type (length colour-region-formats))
	(colour-region-create-new-type))
    (setq type (1- type)))
  (colour-region-apply-according-to-prefix
   (lambda (cregion)
     (if (> type -1)
         (progn
           (setcar (nthcdr 4 current) type)
           (colour-region-apply-overlay current))))))

;;; find the colour-region satisfying given predicate that is nearest to point
;; in the current buffer.
(defun colour-region-find-nearest (predicate)
  "Find the index in colour-regions of the colour-region that is nearest to point
in the current buffer, and that returns non-nil when passed to 'predicate' function.
Returns nil if no colour-region satisfying 'predicate' is found in current buffer.

'predicate' must be a function that takes a colour-region as it's only argument."
  ;; if no prefix argument was given, then toggle nearest colour-region
  (let (current nearestoverlaypos (bestindex nil))
    ;; first loop through colour-regions to find nearest overlay
    (dotimes (i (length colour-regions))
      (setq current (nth i colour-regions))
      ;; only consider overlays on current buffer that satisfy predicate
      (if (and (equal (buffer-name) (car current)) (funcall predicate current))
	  ;; if this is the first time we find an overlay matching this buffer
	  ;; then it is the nearest overlay found so far, so set variables appropriately
	  (if (not bestindex)
	      (setq nearestoverlaypos (nth 1 current) bestindex i) 
	    ;; else if start position of overlay is closer to point 
	    ;; than nearestoverlaypos is...
	    (if (< (abs (- (nth 1 current) (point))) (abs (- nearestoverlaypos (point))))
		(setq nearestoverlaypos (nth 1 current) bestindex i)
	      ;; otherwise, if end position of overlay is closer to point
	      ;; than nearestoverlaypos..
	      (if (< (abs (- (nth 2 current) (point))) (abs (- nearestoverlaypos (point))))
		  (setq nearestoverlaypos (nth 2 current) bestindex i))))))
    bestindex))


;;; apply appropriate overlay properties (according to colour-region-formats) to colourregion
(defun colour-region-apply-overlay (colourregion)
  ;; get colourregion properties
  (let* ((storedoverlay (nth (1- (length colourregion)) colourregion))
	 (start (overlay-start storedoverlay))
	 (end (overlay-end storedoverlay))
	 (comment (nth 3 colourregion))
	 (regiontype (nth 4 colourregion))
	 (formattype (nth 5 colourregion))
	 (formatlist (nth formattype (nth regiontype colour-region-formats)))
	 (beforestring (concat (nth 0 formatlist)
			       (if (nth 1 formatlist)
				   comment "")))
	 (beforestringformat (nth 2 formatlist))
	 (afterstring (concat (if (nth 3 formatlist)
				  comment "")
			      (nth 4 formatlist)))
	 (afterstringformat (nth 5 formatlist)))
    ;; make new overlay and apply appropriate properties
    (remove-overlays start end)
    (let ((newoverlay (make-overlay start end)))
      (overlay-put newoverlay 'before-string
		   (propertize beforestring 'face beforestringformat))
      (overlay-put newoverlay 'after-string
		   (propertize afterstring 'face afterstringformat))
      (dotimes (i (- (length formatlist) 6))
	(let ((currentproperty (nth (+ i 6) formatlist)))
	  (overlay-put newoverlay (car currentproperty) (cdr currentproperty))))
      ;; update overlay stored in colourregion
      (setcar (nthcdr (1- (length colourregion)) colourregion) newoverlay))))

;;; toggle overlay state of colourregion
(defun colour-region-apply-toggle-overlay (colourregion)
  (let ((currentstate (nth 5 colourregion))
	(textpart (nth (nth 6 colourregion) (nth 7 colourregion))))
    ;; if current state of colourregion is last state, change to first state,
    ;; otherwise change to next state
    (if (equal currentstate (1- (length (nth (nth 4 colourregion) colour-region-formats))))
	(progn
	  (setcar (nthcdr 5 colourregion) 0)
	  (setcar textpart 0))
      (progn
	(setcar (nthcdr 5 colourregion) (1+ currentstate))
	(setcar textpart (1+ currentstate))))
    ;; apply overlay for new state
    (colour-region-apply-overlay colourregion)))

;;; toggle text-part of colourregion
(defun colour-region-apply-toggle-text-part (colourregion)
  ;; only toggle if there is more than one stored text region
  (if (> (length (nth 7 colourregion)) 1)
      (let* ((oldtextnum (nth 6 colourregion))
	     ;; if current text is last one, change to first one
	     ;; otherwise change to next one
	     (newtextnum (if (equal oldtextnum (1- (length (nth 7 colourregion))))
			     (setq newtextnum 0)
			   (setq newtextnum (1+ oldtextnum))))
	     (newtextpart (nth newtextnum (nth 7 colourregion)))
	     (newtext (nth 2 newtextpart))
	     (storedoverlay (nth (1- (length colourregion)) colourregion))
	     (start (overlay-start storedoverlay))
	     (end (overlay-end storedoverlay))
	     (newend (+ start (length newtext)))
	     (diff (- newend end))
	     (currentpoint (point))
	     (newpoint (if (< currentpoint end) currentpoint (+ currentpoint diff))))
	;; store current text-part
	(colour-region-apply-save-text-part colourregion 0)
	;; set state, comment and text-part number to correspond to new text-part
	(setcar (nthcdr 5 colourregion) (nth 0 newtextpart))
	(setcar (nthcdr 3 colourregion) (nth 1 newtextpart))
	(setcar (nthcdr 6 colourregion) newtextnum)
	;; delete old text and copy new text into buffer
	(delete-region start end)
	(goto-char start)
	(insert newtext)
	;; move overlay to fit new text
	(move-overlay storedoverlay start newend)
	;; apply overlay and move point back to correct position
	(colour-region-apply-overlay colourregion)
	(goto-char newpoint))))

;;; Save currently displayed text, comment and state of colourregion, as new text-part in
;; text-parts list of colourregion.
;; If pos is 0, copy over text-part corresponding to current display, 
;; else place it pos positions further/behind current text-part in list according
;; to whether pos is positive/negative.
;; New position is calculated modulo the length of the new list so that large pos values
;; don't cause problems.
(defun colour-region-apply-save-text-part (colourregion pos)
  "Save currently displayed text, comment and state of colourregion, as new text-part in
text-parts list of colourregion.
If pos is 0, copy over text-part corresponding to current display, 
else place it pos positions further/behind current text-part in list according
to whether pos is positive/negative.
New position is calculated modulo the length of the new list so that large pos values
don't cause problems."
  (let* ((comment (nth 3 colourregion))
	 (state (nth 5 colourregion))
	 (storedoverlay (nth (1- (length colourregion)) colourregion))
	 (start (overlay-start storedoverlay))
	 (end (overlay-end storedoverlay))
	 (text (buffer-substring-no-properties start end))
	 (newtextpart (list state comment text))
	 (textparts (nth 7 colourregion))
	 (oldpos (nth 6 colourregion))
	 ;; will insert newtextpart just before newpos, so set accordingly:
	 (newpos (if (> pos 0)
		     (mod (+ oldpos pos) (1+ (length textparts)))
		   (mod (+ oldpos pos 1) (1+ (length textparts))))))
    ;; if pos is 0, just replace current textpart (in position oldpos)
    (if (equal pos 0)
	(setcar (nthcdr oldpos textparts) newtextpart)
      ;; else insert newtextpart into textparts, just before newpos
      ;; i.e. change the cdr of the position just before newpos to a list with
      ;; newtextpart at the beginning followed by the rest of textparts 
      ;; (which is the newpos'th cdr or nil if we are at the end of the list)
      (if (> newpos 0)
	  (setcdr (nthcdr (1- newpos) textparts) (cons newtextpart (nthcdr newpos textparts)))
	(setcdr (nthcdr (1- (length textparts)) textparts) (cons newtextpart nil))))
    ;; set index of current text-part to correspond with newtextpart
    (setcar (nthcdr 6 colourregion) newpos)))

;;; remove specific colour-region
(defun colour-region-apply-remove (colourregion)
  "Remove colourregion from buffer and colour-regions."
  (let ((storedoverlay (nth (1- (length colourregion)) colourregion)))
    (remove-overlays (overlay-start storedoverlay)
		     (overlay-end storedoverlay))
    (setq colour-regions (delq colourregion colour-regions))))

;;; kill specific colour-region and stick it in colour-region-kill-ring
(defun colour-region-apply-kill (colourregion)
  "Kill colourregion (including hidden text) from buffer and colour-regions,
and place on colour-region-kill-ring."
  (colour-region-apply-remove colourregion)
  (let ((text (buffer-substring-no-properties
               (nth 1 colourregion)
               (nth 2 colourregion))))
    (setq colour-region-kill-ring
          (append colour-region-kill-ring (list (list colourregion text))))
    (kill-region (nth 1 colourregion)
                 (nth 2 colourregion))))

;;; copy specific colour-region and stick it in colour-region-kill-ring
(defun colour-region-apply-copy (colourregion)
  "Copy colourregion and put it in colour-region-kill-ring"
  (let ((text (buffer-substring-no-properties
               (nth 1 colourregion)
               (nth 2 colourregion))))
    (setq colour-region-kill-ring
          (append colour-region-kill-ring (list (list colourregion text))))))

;;; yank ith colour-region and text from colour-region-kill-ring, at point
(defun colour-region-apply-yank (i)
  "Yank ith colour-region and corresponding text from colour-region-kill-ring, 
and copy to current buffer at point"
  (let* ((colourregiontoyank (nth 0 (nth i colour-region-kill-ring)))
	 (regionlength (- (nth 2 colourregiontoyank) (nth 1 colourregiontoyank)))
	 (text (nth 1 (nth i colour-region-kill-ring)))
	 (newcolourregion
          (list (nth 0 colourregiontoyank)
                (point)
                (+ (point) regionlength)
                (nth 3 colourregiontoyank)
                (nth 4 colourregiontoyank)
                (nth 5 colourregiontoyank))))
    (if (nth 5 newcolourregion)
	(colour-region-apply-hide newcolourregion)
      (colour-region-apply-unhide newcolourregion))
    (setq colour-regions (append colour-regions (list newcolourregion)))))

;;; Update the start and end information in colourregion
(defun colour-region-apply-update-start-end (colourregion)
  ;; update start and end information
  (let ((storedoverlay (nth (1- (length colourregion)) colourregion)))
    (if (overlayp storedoverlay)
	(progn
	  (setcar (nthcdr 1 colourregion)
                  (overlay-start storedoverlay))
	  (setcar (nthcdr 2 colourregion)
                  (overlay-end storedoverlay))))))

;;; Create and apply a new overlay for colourregion based on 
;; start, end, type and status values
(defun colour-region-apply-update-overlay (colourregion)
  (let* ((start (nth 1 colourregion))
	 (end (nth 2 colourregion))
	 (newoverlay (make-overlay start end)))
    (setcar (nthcdr (1- (length colourregion)) colourregion) newoverlay)
    (colour-region-apply-overlay colourregion)))

;;; Update the start and end information in colour regions
(defun colour-region-update-start-end nil
  (dolist (current colour-regions)
    (colour-region-apply-update-start-end current)))

;;; Create and update overlays in colour-regions according to
;; start, end, type and status values
(defun colour-region-update-overlays nil
  (dolist (current colour-regions)
    (colour-region-apply-update-overlay current)))

;;; Hook for saving colour-regions when emacs is killed.
;; Note: colour-regions will not be usable after running this function until it
;; is restored with colour-region-update-overlays
(defun colour-region-kill-emacs-hook nil
  ;; need to adjust colour-regions for each buffer, since it is a buffer local variable
  (dolist (thisbuffer (buffer-list)) 
    (with-current-buffer thisbuffer
      (if (and (buffer-file-name) 
	       colour-region-save-on-kill 
	       (> (length colour-regions) 0))
	  (if (eq colour-region-save-on-kill 'prompt)
	      (if (y-or-n-p (concat "Save colour-regions for " (buffer-name) " ? "))
		  (colour-region-save))
	    (colour-region-save))))))

(defun colour-region-kill-buffer-hook nil
  "Save colour-regions if buffer is killed, and colour-region-save-on-kill is t.
Prompt for save if colour-region-save-on-kill equals 'prompt."
  (if (and colour-region-save-on-kill (> (length colour-regions) 0))
      (if (eq colour-region-save-on-kill 'prompt)
	  (if (y-or-n-p 
	       (concat "Save colour-regions for " 
		       (buffer-name) " ? "))
	      (colour-region-save))
	(colour-region-save))))

(defun colour-region-find-file-hook nil
  "If colour-region-load-on-find-file is t, load colour-regions from filename returned by colour-region-default-save-file function. If colour-region-load-on-find-file is equal to 'prompt, then prompt the user first. If colour-region-load-on-find-file is nil, or the filename returned by colour-region-default-save-file doesn't exist, then don't load."
  (let ((filename (colour-region-default-save-file)))
    (if (and colour-region-load-on-find-file
	     (file-exists-p filename))
	(if (eq colour-region-load-on-find-file 'prompt)
	    (if (y-or-n-p 
		 (concat "Load colour-regions from "
			 filename " ? "))
		(colour-region-load))
	  (colour-region-load)))))

;;; Initialization function
(defun colour-region-initialize nil
  "Initialize colour-region; setup hooks."
  (add-hook 'find-file-hook 'colour-region-find-file-hook t)
  (add-hook 'kill-buffer-hook 'colour-region-kill-buffer-hook t)
  (add-hook 'kill-emacs-hook 'colour-region-kill-emacs-hook))

;;; function to save all the colour-regions in current buffer
;; (there are probably better ways of doing this, but it'll do for now)
(defun colour-region-save (&optional filename)
  "Save colour-regions for the current buffer (if there are any) from filename.
If filename is not provided then the colour-regions are read from the filename returned by 
the colour-region-default-save-file function."
  (interactive)
  (let ((filename2 (or filename
		       (colour-region-default-save-file))))
    (progn
      ;; update start and end information for all colour-regions 
      (colour-region-update-start-end)
      ;; remove overlays from colour-regions so that we can save it with session
      (dolist (current colour-regions)
	(let ((storedoverlay (nth (1- (length current)) current)))
	  (if (overlayp storedoverlay) 
	      (setcar (nthcdr (1- (length current)) current) t))))
      ;; write colour-regions to file (with message explaining contents of file)
      (write-region 
       (concat ";; This file contains the contents of the colour-regions variable associated with the buffer " (buffer-name) ".\n;; It is used by the emacs library colour-region.el in the functions colour-region-save and colour-region-load\n\n(setq colour-regions '" (prin1-to-string colour-regions) ")")
       nil
       filename2)
      ;; restore overlays to colour-regions
      (colour-region-update-overlays))))


(defun colour-region-load (&optional filename)
  "Load colour-regions for the current buffer from filename.
If filename is not provided then the colour-regions are stored in the filename returned by 
the colour-region-default-save-file function."
  (interactive)
  (let ((string)
	(savedfile (if filename
		       (if (file-name-directory filename) filename
			 (concat default-directory filename))
		     (colour-region-default-save-file))))
    (if (not (file-regular-p savedfile))
	(message (concat "Can't read" savedfile " !"))
      (with-temp-buffer
	(goto-char (point-min))
	(insert-file-contents savedfile)
	(setq string (buffer-string)))
      (eval (read string))
      (colour-region-update-overlays))))

(defun colour-region-default-save-file nil
  "Returns the default filename for the current buffer for saving colour-regions"
  (concat default-directory ".colour-regions_for_" (buffer-name)))

(provide 'colour-region)

;; (magit-push)
;; (yaoddmuse-post "EmacsWiki" "colour-region.el" (buffer-name) (buffer-string) "update")

;;; colour-region.el ends here




