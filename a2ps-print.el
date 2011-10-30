;;;a2ps-print.el: Postscript printing hook for a2ps.
;;Time-stamp: <01/02/08 13:05:38 bruce>

;;This file is available as part of a2ps/contrib/, or at
;; <url: ftp://ftp.cppsig.org/pub/tools/emacs/a2ps-print.el >
;;but may move to <url: http://www.sourceforge.net/projects/emacro/ >
;; in the near future.
;;This requires a2ps to be installed in your PATH
;;a2ps is available at <url: ftp://ftp.enst.fr/pub/unix/a2ps/ >

;; Keywords: languages, faces, a2ps, print, postscript
;;Modified from enscript.el by Jim Robinson robinson@wdg.mot.com
;;Written by Bruce Ingalls bingalls@panix.com
;;Tested on a2ps v4.13b on emacs v20.7.3 & xemacs 21.1.12 on solaris 2.6

;; This file is not part of GNU Emacs nor XEmacs.
;; This file is contributed with a2ps.
;; 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;

;; $Id: a2ps-print.el,v 1.01 1998/06/19 19:15:00 ingalls Exp $
(defconst a2ps-print-version "1.01")

;;Put the following into your .emacs (both Emacs & XEmacs):
;;(require 'a2ps-print)
;;(define-key function-key-map [f22] [print])	;display f22 as print (screen)
;;(global-set-key [print] 'a2ps-buffer)
;;(global-set-key [(shift print)] 'a2ps-region)	;print selected text

;;Emacs menu
;;(define-key global-map [menu-bar files a2ps-buffer]
;;  '("a2ps Print Buffer" . a2ps-buffer))

;;XEmacs file menu
;;(add-menu-button '("File") ["a2ps Print Buffer" a2ps-buffer "--"])

;you can pass (up to 4?) command line switches to a2ps.
;Here's a recommended sample for your .emacs:
;;(setq a2ps-switches `("--line-numbers=1" ))

;;a2ps v1.01+ supports remote printing via 'ssh lp -d server file.txt'
;; with this trick:
;;(setq a2ps-command "ssh")
;;(setq a2ps-switches '("printServerName" "a2ps" "-P" "printerName"))
;;There may be a bug in a2ps v4.13b, which causes this to bomb for more than
;; ~2 pages, so test this from the command line, if you have problems.

;;; Commentary:
;; Lets you connect to a2ps via the menu, for beautiful
;;  postscript formatting and printing of your source code

;;; Changes:
;;2001-01-18 v1.01 Bruce Ingalls  <bingalls@panix.com>
;;	* Menu code for both Emacs & XEmacs
;;	* Now displays "(print)" in menu for PrintScreen key
;;	* Fixed -H(eader) switch
;;	* Now checkdoc'ed.
;;	* Now supports require()
;;	* Now has version stamp of 1.01
;;	* New email & uri notices.
;;	* Simplified wth Defcustom declaration of all a2ps-switches
;;	* There might no longer be a limit of 4 args to pass to a2ps

;;; Known Bugs:
;;Currently does not work with html source. One hack is to rename html files
;;with a .xml extension.
;;Chokes on buffers with special characters, such as backquote.
;;I do not yet have an example to integrate a2ps-print with ghostscript.
;;Workaround is to print to a file.

;;; Code:
(require 'tabify)

;;You may wish to change these Public Variables:
(defcustom a2ps-switches `("--line-numbers=1")
  "*List of string args to pass to a2ps when it is invoked.")

(defcustom a2ps-command "a2ps" "Shell command for printing a file.")

;;Call these Public Functions:
(defun a2ps-buffer (argp)
  "Print buffer contents as with Unix command `a2ps'.
`a2ps-switches' is a list of extra switches (strings) to pass to a2ps.
Argument ARGP should be nil, is unused, and may disappear as obsolete."
  (interactive "P")
  (a2ps-region-1 (point-min) (point-max)))

(defun a2ps-region (start end argp)
  "Print region contents as with Unix command `a2ps'.
`a2ps-switches' is a list of extra switches (strings) to pass to a2ps.
Argument START beginning of region to print.
Argument END End of region to print.
Argument ARGP should be nil, is unused, and may disappear as obsolete."
  (interactive "r\nP")
  (a2ps-region-1 start end))

;;Do not change or use the private code below:
(defun a2ps-region-1 (start end)
  "Print region contents as with Unix command `a2ps'.
`a2ps-switches' is a list of extra switches (strings) to pass to a2ps.
Argument START beginning of region to print.
Argument END End of region to print."

  (let ((width tab-width)
        (name  (buffer-name))
        )

    (save-excursion
      (message "Spooling...")
      (if (/= tab-width 8)
	  (let ((oldbuf (current-buffer)))
	    (set-buffer (get-buffer-create " *spool temp*"))
	    (widen) (erase-buffer)
	    (insert-buffer-substring oldbuf)
	    (setq tab-width width)
	    (untabify (point-min) (point-max))
	    (setq start (point-min) end (point-max))))
 
      (apply 'call-process-region
	     (append (list start end a2ps-command nil
;                           nil				;do not log
                           '(nil "/tmp/a2ps-prn.log")	;debug log
                           nil)
;                     a2ps-switches))
                     (append a2ps-switches (list (format "--stdin=\"%s\"" name)))))
      (message "Spooling...done"))))

(provide 'a2ps-print)
;;; a2ps-print.el ends here
