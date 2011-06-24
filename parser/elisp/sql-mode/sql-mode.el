;;; sql-mode.el --- Mode for editing and testing SQL.

;; Copyright (C) 1994,1999-2002 Rob Riepel.

;; Author: Rob Riepel <riepel@networking.stanford.edu>
;; Maintainer: Rob Riepel <riepel@networking.stanford.edu>
;; Keywords: sql isql sqlplus sybase oracle mysql postgresql

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;  This file is implements a SQL-mode for emacs.  The code is a hacked up
;;  version of Lynn Slater's sql.el.  Some of the comments were pinched from
;;  Jim Lange's sqlplus.  Both were available from the elisp archives.  Font
;;  lock support by Christian Egli <christian.egli@stest.ch>.
;;
;;  SQL-mode is for editing and testing SQL statements in a standard text
;;  buffer.  SQL-mode turns on abbrev-mode with abbreviations for common SQL
;;  keywords.  The most useful feature of SQL-mode is sending SQL statements
;;  to a SQL interpreter.
;;
;;  The following commands can be added to a global initialization file or
;;  to any user's .emacs file to conveniently use SQL-mode.
;;
;;    (autoload 'sql "sql-mode"
;;      "Start the interactive SQL interpreter in a new buffer." t)
;;
;;    (autoload 'sql-mode "sql-mode"
;;      "Mode for editing SQL files and running a SQL interpreter." t)
;;
;;    (autoload 'sql-buffer "sql-mode"
;;      "Create or move to the sql-mode \"*SQL commands*\" buffer." t)
;;
;;    (setq auto-mode-alist (cons '("\\.sql$" . sql-mode) auto-mode-alist))
;;
;;  Use describe-mode while in sql-mode for further instructions.

;;; Code:


;;;  Revision Information

(defconst sql-revision "$Id: sql-mode.el,v 4.0 2004/10/29 08:40:15 riepel Exp $")


;;;  Variables

(defvar sql-buffer-name "" "Buffer where SQL commands are run.")
(defvar sql-buffer-prefix "*" "Beginning of SQL buffer name.")
(defvar sql-buffer-postfix "*" "End of SQL buffer name.")

(defvar sql-command "isql" "SQL interpreter program.")
(defvar sql-server "" "SQL server name.")
(defvar sql-username "" "SQL interpreter username.")
(defvar sql-process-name "" "SQL interpreter process name.")

(defvar sql-magic-go t "*If non-NIL, submit SQL when 'go' is entered.")
(defvar sql-magic-semicolon t "*If non-NIL, submit SQL when a semicolon is entered.")
(defvar sql-display-commands t "*If non-NIL, display SQL commands in the results buffer.")

(defvar sql-mode-abbrev-table nil "Abbrev table used in SQL mode buffers.")
(defvar sql-mode-map nil "Keymap used in SQL mode.")

(defvar sql-get-vendor-hist (list "sybase" "oracle" "sybase" "mysql" "postgres"))
(defvar sql-get-server-hist (make-list 2 ""))
(defvar sql-get-username-hist (make-list 2 (user-login-name)))
(defvar sql-postgres-dbname-hist (make-list 1 ""))

(defvar sql-output-separator "@--"
  "String printed between sets of SQL command output.")

(defvar sql-separator-regexp                            ; 'go' isql (Sybase)
  "[ \n\t]+go\\b\\|;[ \t]*\\(?:--.*\\)*$\\|^/[ \t]*$"   ; ';' sqlplus, et. al.
  "Regexp used to seperate groups of SQL statements.")  ; '/' sqlplus (Oracle)
   ;; alternative version -- "^[ \t]*go\\b\\|;[ \t]*$"

(defvar sql-terminator "\ngo"
  "String to send to the SQL interpreter to initiate execution.")


;;;  Font Lock Support

(defvar sql-commands
  '("add" "all" "alter" "and" "any" "arith_overflow" "as" "asc" "at"
    "authorization" "begin" "between" "break" "browse" "bulk" "by" "cascade"
    "char_convert" "check" "checkpoint" "close" "clustered" "commit" "compute"
    "confirm" "constraint" "continue" "controlrow" "create" "current" "cursor"
    "database" "dbcc" "deallocate" "declare" "default" "delete" "desc" "disk"
    "distinct" "double" "drop" "dummy" "dump" "else" "end" "endtran" "escape"
    "except" "exec" "execute" "exists" "exit" "fetch" "fillfactor" "for"
    "foreign" "from" "goto" "grant" "group" "having" "holdlock" "identity"
    "identity_insert" "if" "in" "index" "insert" "intersect" "into" "is"
    "isolation" "key" "kill" "level" "like" "lineno" "load" "mirror"
    "mirrorexit" "national" "noholdlock" "nonclustered" "not"
    "numeric_truncation" "of" "off" "offsets" "on" "once" "only" "open" "option"
    "or" "order" "over" "perm" "permanent" "plan" "precision" "prepare"
    "primary" "print" "privileges" "proc" "procedure" "processexit" "public"
    "raiserror" "read" "readtext" "recofigure" "reconfigure" "references"
    "replace" "reserved_pgs" "return" "revoke" "role" "rollback" "rowcount"
    "rows" "rule" "save" "schema" "select" "set" "setuser" "shared" "shutdown"
    "some" "statistics" "stripe" "syb_identity" "syb_restree" "syb_terminate"
    "table" "temp" "temporary" "textsize" "to" "tran" "transaction" "trigger"
    "truncate" "tsequal" "union" "unique" "update" "use" "user" "user_option"
    "using" "values" "varying" "view" "waitfor" "where" "while" "with" "work"
    "writetext"))

(defvar sql-system-procedures
  '("go" "sp_addmessage" "sp_addtype" "sp_binddefault" "sp_depends"
    "sp_droptype" "sp_help" "sp_helpdb" "sp_helptext" "sp_who"))

(defvar sql-type-defs
  '("tinyint" "smallint" "int" "numeric" "decimal" "float" "double" "real"
    "smalldatetime" "datetime" "char" "varchar" "nchar" "nvarchar" "text"
    "smallmoney" "money" "binary" "varbinary" "image" "bit"))

(defvar sql-font-lock-keywords
  ;; well, XEmacs and older Emacsen (19 et al) don't come with
  ;; regexp-opt. They also seem to be missing font-lock-builtin-face.
  (let ((regexp-opt
	 (if (featurep 'regexp-opt)
	     (function regexp-opt)
	   ;; Let's define a poor mans regexp-opt.
	   (function
	    (lambda (strings paren)
	      (let ((open-paren (if paren "\\(" ""))
		    (close-paren (if paren "\\)" "")))
		(concat open-paren (mapconcat 'regexp-quote strings  "\\|")
			close-paren)))))))
    (list
     ;; commands
     (cons (concat "\\<" (funcall regexp-opt sql-commands t) "\\>")
	   'font-lock-function-name-face)
     ;; system procedures
     (cons (concat "\\<" (funcall regexp-opt sql-system-procedures t) "\\>")
	   'font-lock-keyword-face)
     ;; type defs
     (cons (concat "\\<" (funcall regexp-opt sql-type-defs t) "\\>")
	   'font-lock-type-face)
     ;; variable names
     '("@\\(\\s_\\|\\sw\\)+" 0 font-lock-variable-name-face t)
     ;; builtin variable names
     '("@@\\(\\s_\\|\\sw\\)+" 0
       ;; XEmacs and Emacs < 20 don't have 'font-lock-builtin-face
       (if (facep 'font-lock-builtin-face) font-lock-builtin-face
	 font-lock-variable-name-face) t)
     ;; temp tables
     '("#[A-Za-z0-9_]+" 0
       ;; XEmacs and Emacs < 20 don't have 'font-lock-constant-face
       (if (facep 'font-lock-constant-face) font-lock-constant-face
	 font-lock-variable-name-face) t)
     ;; comments
     '("-- .*$" 0 font-lock-comment-face t)
     '("# .*$"  0 font-lock-comment-face t)
     '("##.*$"  0 font-lock-comment-face t)))
  "Expressions to highlight in SQL mode.")

;;;  imenu Support

(defvar sql-imenu-generic-expression
  '(("Rules" "^\\s-*create\\s-+rule\\s-+\\(\\sw+\\)" 1)
    ("Triggers" "^\\s-*create\\s-+trigger\\s-+\\(\\sw+\\)" 1)
    ("Procedures" "^\\s-*create\\s-+proc\\(edure\\)?\\s-+\\(\\sw+\\)" 2)
    ("Indexes" "^\\s-*create\\s-+\\(unique\\|\\(non\\)?clustered\\)\\s-+index\\s-+\\(\\sw+\\)" 3)
    ("Tables" "^\\s-*create\\s-+table\\s-+\\(\\sw+\\)" 1))
  "Imenu generic expression for sql-mode.  See `imenu-generic-expression'.")


;;;  Markers

(defvar sql-buffer-mark (make-marker)
  "Marks the current SQL command in the SQL output buffer.")

(make-variable-buffer-local 'sql-buffer-mark)

(defvar sql-region-beginning-mark (make-marker)
  "Marks the beginning of the region to sent to the SQL process.")
(defvar sql-region-end-mark (make-marker)
  "Marks the end of the region to sent to the SQL process.")


;;;  SQL-mode Keymap

(if (not sql-mode-map)
    (progn
      (setq sql-mode-map (make-sparse-keymap))
      (define-key sql-mode-map "O"        'sql-magic-go)
      (define-key sql-mode-map "o"        'sql-magic-go)
      (define-key sql-mode-map ";"        'sql-magic-semicolon)
      (define-key sql-mode-map "\C-c\C-t" 'sql-toggle-magic)
      (define-key sql-mode-map "\C-c\C-d" 'sql-toggle-display)
      (define-key sql-mode-map "\C-cg"    'sql-goto-error)
      (define-key sql-mode-map "\C-c\C-e" 'sql-buffer-erase)
      (define-key sql-mode-map "\C-cb"    'sql-buffer-bottom)
      (define-key sql-mode-map "\C-ct"    'sql-buffer-top)
      (define-key sql-mode-map "\C-cp"    'sql-buffer-prev-command)
      (define-key sql-mode-map "\C-cn"    'sql-buffer-next-command)
      (define-key sql-mode-map "\C-c\C-w" 'sql-buffer-display-window)
      (define-key sql-mode-map "\C-c\C-l" 'sql-buffer-redisplay-current)
      (define-key sql-mode-map "\C-c\C-a" 'sql-buffer-beginning-of-line)
      (define-key sql-mode-map "\C-c\C-b" 'sql-buffer-scroll-right)
      (define-key sql-mode-map "\C-c\C-f" 'sql-buffer-scroll-left)
      (define-key sql-mode-map "\C-c\C-p" 'sql-buffer-scroll-down)
      (define-key sql-mode-map "\C-c\C-n" 'sql-buffer-scroll-up)
      (define-key sql-mode-map "\C-c\C-i" 'sql-send-interrupt)
      (define-key sql-mode-map "\C-ch"    'sql-send-whole-buffer)
      (define-key sql-mode-map "\C-c\C-o" 'sql-send-oracle-block)
      (define-key sql-mode-map "\C-c\C-r" 'sql-send-region)
      (define-key sql-mode-map "\C-c\C-c" 'sql-send-current)
      (define-key sql-mode-map "\C-c\C-u" 'sql-set-user)))


;;;  SQL-mode Menu

;; Menu definitions, only relevent if you have the easymenu.el package
;; (standard in the latest Emacs and XEmacs distributions).

(defvar sql-mode-menu nil
  "Keymap for sql-mode's menu.")

(if (condition-case nil
        (require 'easymenu)
      (error nil))
    (easy-menu-define
     sql-mode-menu sql-mode-map "SQL Mode menu"
     '("SQL"
       ["Login to db server..." sql t]
       ["Send interrupt"        sql-send-interrupt (get-buffer sql-buffer-name)]
       ["Clear output buffer"   sql-buffer-erase (get-buffer sql-buffer-name)]
       "-"
       ["Execute current"       sql-send-current (get-buffer sql-buffer-name)]
       ["Execute Oracle block"  sql-send-oracle-block (get-buffer sql-buffer-name)]
       ["Execute region"        sql-send-region (and (mark) (get-buffer sql-buffer-name))]
       ["Execute buffer"        sql-send-whole-buffer (get-buffer sql-buffer-name)]
       "-"
       ["Go to top of output"     sql-buffer-top (get-buffer sql-buffer-name)]
       ["Go to bottom of output"  sql-buffer-bottom (get-buffer sql-buffer-name)]
       ["Go to previous command"  sql-buffer-prev-command (get-buffer sql-buffer-name)]
       ["Go to next command"      sql-buffer-next-command (get-buffer sql-buffer-name)]
       ["Go to current command"   sql-buffer-redisplay-current (get-buffer sql-buffer-name)]
       ["Go to beginning of line" sql-buffer-beginning-of-line (get-buffer sql-buffer-name)]
       "-"
       ["Comment region"        comment-region (mark)]
       ["Uncomment region"      (comment-region (point) (mark) '(4)) (mark)])))


;;;  SQL-mode Abbreviations

(progn
  (define-abbrev-table 'sql-mode-abbrev-table ())
  (define-abbrev sql-mode-abbrev-table "arc" "archivelog"   nil)
  (define-abbrev sql-mode-abbrev-table "s"   "select"       nil)
  (define-abbrev sql-mode-abbrev-table "f"   "from"         nil)
  (define-abbrev sql-mode-abbrev-table "fr"  "from"         nil)
  (define-abbrev sql-mode-abbrev-table "w"   "where"        nil)
  (define-abbrev sql-mode-abbrev-table "o"   "order by"     nil)
  (define-abbrev sql-mode-abbrev-table "ob"  "order by"     nil)
  (define-abbrev sql-mode-abbrev-table "g"   "group by"     nil)
  (define-abbrev sql-mode-abbrev-table "gb"  "group by"     nil)
  (define-abbrev sql-mode-abbrev-table "dec" "declare"      nil)
  (define-abbrev sql-mode-abbrev-table "del" "delete"       nil)
  (define-abbrev sql-mode-abbrev-table "nu"  "number"       nil)
  (define-abbrev sql-mode-abbrev-table "da"  "date"         nil)
  (define-abbrev sql-mode-abbrev-table "co"  "connect"      nil)
  (define-abbrev sql-mode-abbrev-table "sy"  "synonym"      nil)
  (define-abbrev sql-mode-abbrev-table "tr"  "trigger"      nil)
  (define-abbrev sql-mode-abbrev-table "up"  "update"       nil)
  (define-abbrev sql-mode-abbrev-table "ins" "insert"       nil)
  (define-abbrev sql-mode-abbrev-table "gr"  "grant"        nil)
  (define-abbrev sql-mode-abbrev-table "gra" "grant all to" nil)
  (define-abbrev sql-mode-abbrev-table "pu"  "public"       nil)
  (define-abbrev sql-mode-abbrev-table "un"  "unique"       nil)
  (define-abbrev sql-mode-abbrev-table "cl"  "cluster"      nil)
  (define-abbrev sql-mode-abbrev-table "we"  "whenever"     nil)
  (define-abbrev sql-mode-abbrev-table "ta"  "table"        nil)
  (define-abbrev sql-mode-abbrev-table "pr"  "priviledges"  nil)
  (define-abbrev sql-mode-abbrev-table "dr"  "drop"         nil)
  (define-abbrev sql-mode-abbrev-table "ro"  "rollback"     nil)
  (define-abbrev sql-mode-abbrev-table "rb"  "rollback"     nil)
  (define-abbrev sql-mode-abbrev-table "tr"  "transaction"  nil)
  (define-abbrev sql-mode-abbrev-table "us"  "using"        nil)
  (define-abbrev sql-mode-abbrev-table "u"   "using"        nil)
  (define-abbrev sql-mode-abbrev-table "vc"  "varchar"      nil))


;;;  SQL-mode

(defun sql-mode nil
  "
Mode for editing SQL files and running SQL interpreters (such as
'isql' (from Sybase) or 'sqlplus' (from Oracle)).  Entry into this
mode runs the hook 'sql-mode-hook'.  It also enables abbrev-mode,
with abbreviations for SQL keywords.  Use '\\[list-abbrevs]' for a
full list.

Use 'M-x sql-mode' to invoke SQL-mode for the current buffer.

Use 'M-x sql' or '\\[sql-set-user]' (sql-set-user) to start the SQL interpreter.
Both commands prompt for the database vendor, server, and user for the
SQL interpreter and start it if necessary.  You can run multiple SQL
interpreters and switch between them using'\\[sql-set-user]' (sql-set-user).

Each time you start a SQL interpreter or switch to an existing SQL
interpreter session, sql-mode prompts you for a database vendor
(e.g. 'mysql' or 'sybase'), server, and user.  You may provide all
three at the 'Vendor: ' prompt using the format:

  Vendor: vendor:server/user   (e.g. mysql:dbserver/buffy)

This shortcut is especially useful when using '\\[sql-set-user]' (sql-set-user)
to switch between different SQL interpreter sessions.

Use 'M-x sql-buffer' to create a \"*SQL Commands*\" buffer and start
the SQL interpreter.  Subsequent uses of 'M-x sql-buffer' will bring
back an existing \"*SQL Commands*\" buffer or create a new one if
necessary.

SQL mode automatically sends SQL to the SQL interpreter when you
enter a SQL command terminator ('go' or ';').  Set sql-magic-go
and/or sql-magic-semicolon to NIL to disable this automatic
submission feature.  Or use '\\[sql-toggle-magic]' (sql-toggle-magic) to
toggle this behavior off and on.

Existing SQL can be sent to the SQL interpreter using '\\[sql-send-current]'
(sql-send-current).  Just position the cursor on or near the SQL
statement you wish to send and press '\\[sql-send-current]' to run it and
display the results.

Mode Specific Bindings:

\\{sql-mode-map}"

  (interactive)
  (setq major-mode 'sql-mode)
  (setq mode-name "SQL")
  (use-local-map sql-mode-map)
  (setq local-abbrev-table sql-mode-abbrev-table)
  (abbrev-mode 1)
  (setq abbrev-all-caps 1)
  (set (make-local-variable 'comment-start) "-- ")
  (set (make-local-variable 'font-lock-defaults)
       '(sql-font-lock-keywords nil t
                                ;  /* and */ delimit a comment
                                ((?/ . ". 14") (?* . ". 23")
                                 ; -- and \n delimit a comment
                                 (?- . ". 12b") (?\n . ">   b") (?\f . ">   b")
                                 ; both ' and " are string delimiters
                                 (?\' . "\"") (?\" . "\""))))
  (set (make-local-variable 'font-lock-keywords-case-fold-search) t)
  ;; This is obsolete in Emacs 20
  (set (make-local-variable 'font-lock-comment-start-regexp) "/[*/]")

  (if sql-mode-menu
      (easy-menu-add sql-mode-menu))
  (set (make-local-variable 'imenu-generic-expression)
       sql-imenu-generic-expression)
  (setq imenu-syntax-alist '((?. . "w") (?_ . "w")))
  (setq imenu-case-fold-search t)
  (run-hooks 'sql-mode-hook))


;;;  SQL-mode Settings for Specific Vendors

(defun sql-set-vendor (vendor)
  "Set a few things depending on the DB vendor"
  (cond
   ((string= vendor "mysql")       (sql-mysql))
   ((string= vendor "oracle")      (sql-oracle))
   ((string= vendor "pgsql")       (sql-postgres))
   ((string= vendor "postgres")    (sql-postgres))
   ((string= vendor "postgresql")  (sql-postgres))
   ((string= vendor "sybase")      (sql-sybase))))

(defun sql-sybase nil
  "Configure SQL-mode for Sybase ``isql'' interaction."
  (setq sql-terminator "\ngo")
  (setq sql-command "isql")
  (fset 'sql-start-interpreter 'sql-start-isql))

(defun sql-mysql nil
  "Configure SQL-mode for MySQL ``mysql'' interaction."
  (setq sql-terminator ";")
  (setq sql-command "mysql")
  (fset 'sql-start-interpreter 'sql-start-mysql))

(defun sql-oracle nil
  "Configure SQL-mode for Oracle ``sqlplus'' interaction."
  (setq sql-terminator ";")
  (setq sql-command "sqlplus")
  (fset 'sql-start-interpreter 'sql-start-sqlplus))

(defun sql-postgres nil
  "Configure SQL-mode for PostgreSQL ``psql'' interaction."
  (setq sql-terminator ";")
  (setq sql-command "psql")
  (fset 'sql-start-interpreter 'sql-start-psql))

(defun sql-start-isql nil
  "Start up the Sybase isql program."
  (start-process sql-process-name sql-buffer-name sql-command "-w"
                 "2048" "-n" "-S" sql-server "-U" sql-username "-P"
                 (sql-ange-ftp-read-passwd "Password: ")))

(defun sql-start-mysql nil
  "Start up the MySQL mysql program."
  (start-process sql-process-name sql-buffer-name sql-command
		 "-h" sql-server "-u" sql-username
		 "-B" "-t" "-f" "-v" "-v" "-v" "-n"
		 (concat "-p" (sql-ange-ftp-read-passwd "Password: "))))

(defun sql-start-sqlplus nil
  "Start up the Oracle sqlplus program."
  (start-process
   sql-process-name sql-buffer-name sql-command "-s"
   (concat sql-username "/" (sql-ange-ftp-read-passwd "Password: ")
           (if (string= sql-server "") "" (concat "@" sql-server)))))

(defun sql-start-psql nil
  "Start up the PostgreSQL psql program."
  ;; Developed for and works with psql versions 7.0.3 and 7.1.2
  (set-process-filter
   (start-process sql-process-name sql-buffer-name sql-command
		  "-h" sql-server "-U" sql-username "-W" "-P" "pager=" "-d"
		  (sql-string-prompt "Database: " 'sql-postgres-dbname-hist 1))
   (function
    (lambda (process output)
      (cond
       ;; The password can't be specified on the command line; it has to
       ;; be sent after psql prompts for it.  psql writes 'Password: ' to
       ;; STDOUT and waits for the password to be input.
       ((equal output "Password: ")
	(send-string (get-process sql-process-name)
		     (concat (sql-ange-ftp-read-passwd "Password: ") "\n")))
       ;; We want to get rid of psql prompt, which otherwise would appear
       ;; in the sql buffer.  Unfortunately, psql resets the variables
       ;; PROMPT[123] even if they are set on the command line, so we
       ;; have to set them here, after we've seen the default prompt.
       ((string-match "[#>] $" output)
	(send-string (get-process sql-process-name)
		     "\\unset PROMPT1\n\\unset PROMPT2\n\\unset PROMPT3\n")
        ;; Resetting the prompt is the last thing this filter
	;; needs to do, so get rid of it.
	(set-process-filter process nil))
       ;; Anything else we pass through unmolested
       (t (sql-echo-in-buffer sql-buffer-name output)))))))


;;;  Utilitities

(defun sql-echo-in-buffer (buffer-name string &optional force-display)
  "Displays string in the named buffer, creating the buffer if needed.
If force-display is true, the buffer will appear if not already shown."
  (let ((buffer (get-buffer-create buffer-name)))
    (if force-display (display-buffer buffer))
    (set-buffer buffer)
    (goto-char (point-max))
    (insert string)
    (if force-display
        (set-window-point (get-buffer-window buffer-name) (point-max)))))

(defun sql-verify-buffer nil
  "Generates reasonable error messages abouut the SQL connection."
  (if (not (get-buffer sql-buffer-name))
      (error "No SQL output buffer!  Use 'M-x sql' to initialize the SQL interpreter."))
  (if (not (get-buffer-process sql-buffer-name))
      (error "Buffer '%s' is not talking to anybody!" sql-buffer-name)))

(defun sql-send-strings (strings)
  "Sends strings to the SQL process.
Also shows the string at the top of the SQL output buffer."
  (sql-verify-buffer)
  (sql-echo-in-buffer sql-buffer-name
                      (concat "\n" sql-output-separator "\n\n"))
  (sql-buffer-bottom)
  (sql-buffer-mark-current)
  (and sql-display-commands (sql-echo-in-buffer sql-buffer-name (apply 'concat strings)))
  (sql-echo-in-buffer sql-buffer-name (if sql-display-commands "\n\n" "\n"))
  (let ((string (apply 'concat strings))
        (process  (get-buffer-process sql-buffer-name)))
    (send-string process (concat string "\n"))
    (if (eq (current-buffer) (process-buffer process))
        (set-marker (process-mark process) (point))))
  (sql-buffer-redisplay-current))

(defun sql-toggle-display nil
  "Toggle display of SQL commands in the results buffer."
  (interactive)
  (setq sql-display-commands (not sql-display-commands))
  (and (interactive-p)
       (message "SQL commands will%s be displayed in the results buffer."
		(if sql-display-commands "" " not"))))

(defun sql-mark-current nil
  "Marks the current SQL for sending to the SQL process.
Marks are placed around a region defined by matching pairs
of the expression listed in 'sql-seperater-regexp."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (or (and (re-search-backward sql-separator-regexp nil t)
             (goto-char (match-end 0)))
        (goto-char (point-min)))
    (skip-chars-forward " \t\n")
    (setq sql-region-beginning-mark (copy-marker (point)))
    (re-search-forward sql-separator-regexp)
    (setq sql-region-end-mark (copy-marker (match-beginning 0)))))


;;;  Transmission Commands

(defun sql-send-region (start end)
  "Send a region to the SQL process."
  (interactive "r")
  (save-excursion
    (sql-send-strings (list (buffer-substring start end) sql-terminator))))

(defun sql-send-current nil
  "Send the current SQL command(s) to the SQL process."
  (interactive)
  (sql-mark-current)
  (sql-send-region sql-region-beginning-mark sql-region-end-mark))

(defun sql-send-oracle-block nil
  "Send the current SQL block to the SQL process."
  (interactive)
  (let ((sql-separator-regexp "^/[ \t]*$") (sql-terminator "/"))
    (sql-mark-current)
    (sql-send-region sql-region-beginning-mark sql-region-end-mark)))

(defun sql-send-whole-buffer nil
  "Send the current SQL buffer to the SQL process."
  (interactive)
  (sql-send-region (point-min) (point-max)))

(defun sql-magic-go (arg)
  "Insert ``o'' and submit the current SQL to the interpreter."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (and sql-magic-go (save-excursion
                      (beginning-of-line)
                      (and (looking-at "go\\b") (sql-send-current)))))

(defun sql-magic-semicolon (arg)
  "Insert semicolon and submit the current SQL to the interpreter."
  (interactive "P")
  (self-insert-command (prefix-numeric-value arg))
  (and sql-magic-semicolon (sql-send-current)))

(defun sql-toggle-magic nil
  "Toggle the magic 'go' and semicolon."
  (interactive)
  (setq sql-magic-semicolon (not sql-magic-semicolon))
  (setq sql-magic-go sql-magic-semicolon)
  (and (interactive-p)
       (message "The magic is %s" (if sql-magic-go "here!" "gone."))))


;;;  SQL-Output Buffer Operations

(defun sql-show-buffer (&optional fcn &rest args)
  "Makes the SQL output buffer visible in the other window."
  (interactive)
  (sql-verify-buffer)
  (let ((window  (selected-window)))
    (if (not (eq (window-buffer window) (get-buffer sql-buffer-name)))
        (switch-to-buffer-other-window sql-buffer-name))
    (if fcn (condition-case nil (apply fcn args) (error nil)))
    (select-window window)))

(fset 'sql-buffer-display-window 'sql-show-buffer)

(defun sql-buffer-scroll-up nil
  "Scroll-up in the SQL output buffer window."
  (interactive)
  (sql-show-buffer 'scroll-up))

(defun sql-buffer-scroll-down nil
  "Scroll-down in the SQL output buffer window."
  (interactive)
  (sql-show-buffer 'scroll-down))

(defun sql-buffer-scroll-left (num)
  "Scroll-left in the SQL output buffer window."
  (interactive "p")
  (sql-show-buffer 'scroll-left (* num (/ (window-width) 2))))

(defun sql-buffer-scroll-right (num)
  "Scroll-right in the SQL output buffer window."
  (interactive "p")
  (sql-show-buffer 'scroll-right (* num (/ (window-width) 2))))

(defun sql-buffer-beginning-of-line nil
  "Return to the beginning of line in the SQL output buffer window."
  (interactive)
  (sql-show-buffer
   'scroll-right (window-hscroll (get-buffer-window sql-buffer-name))))

(defun sql-buffer-mark-current nil
  "Mark the current position in the SQL output window."
  (sql-show-buffer 'sql-buffer-make-mark))

(defun sql-buffer-make-mark nil
  "Set the sql-buffer-marker."
  (setq sql-buffer-mark (copy-marker (point))))

(defun sql-buffer-redisplay-current nil
  "Go to the current sql-buffer-mark."
  (interactive)
  (sql-show-buffer 'sql-goto-mark))

(defun sql-goto-mark nil
  (goto-char sql-buffer-mark)
  (recenter 0))

(defun sql-buffer-top nil
  "Goto the top of the SQL output buffer."
  (interactive)
  (sql-show-buffer 'sql-beginning-of-buffer))

(defun sql-beginning-of-buffer nil (goto-char (point-min)))

(defun sql-buffer-bottom nil
  "Goto the bottom of the SQL output buffer."
  (interactive)
  (sql-show-buffer 'sql-end-of-buffer))

(defun sql-end-of-buffer nil (goto-char (point-max)) (recenter -1))

(defun sql-buffer-erase nil
  "Clear the SQL output buffer."
  (interactive)
  (sql-show-buffer 'erase-buffer))

(defun sql-buffer-next-command nil
  "Search for the next command in the SQL output buffer."
  (interactive)
  (sql-show-buffer 'sql-next-command))

(defun sql-next-command nil
  "Search for the next command in the SQL output buffer."
  (cond ((re-search-forward  sql-output-separator nil t)
         (forward-line 2)
         (recenter 0))
        (t (beep) (message "No more commands."))))

(defun sql-buffer-prev-command nil
  "Search for the previous command in the SQL output buffer."
  (interactive)
  (sql-show-buffer 'sql-previous-command))

(defun sql-previous-command nil
  "Search for the previous command in the SQL output buffer."
  (let ((start (point)))
    (re-search-backward  sql-output-separator nil t)
    (cond ((re-search-backward  sql-output-separator nil t)
           (forward-line 2)
           (recenter 0))
          (t
           (message "No more commands.") (beep)
           (goto-char start)))))

(defun sql-send-interrupt nil
  "Send an interrupt the the SQL interpreter process."
  (interactive)
  (interrupt-process sql-process-name))


;;;  Miscellaneous

(defun sql-goto-error (n)
  "Moves to the n'th line in the most recently executed SQL."
  (interactive "NLine number of error: ")
  (goto-char sql-region-beginning-mark)
  (forward-line (1- n)))

(defun sql-insert-gos nil
  "Inserts 'go' statements between each apparent block of SQL code."
  (interactive)
  (while (not (eobp))
    (forward-line 1)
    (if (and (looking-at "[a-z]") (not (looking-at "go")))
        (progn (insert "go\n")))))


;;;  SQL Interpreter

(defun sql nil
  "Start the interactive SQL interpreter in a new buffer."
  (interactive)
  (sql-set-server-and-user)
  (sql-set-vendor sql-vendor)
  (get-buffer-create sql-buffer-name)            ; move to the buffer.
  (or (get-buffer-process sql-buffer-name)       ; already got a process?
   (progn                                        ; no, start it up!
     (set-buffer sql-buffer-name)
     (setq truncate-lines t)
     (sql-start-interpreter)))
  (set-buffer sql-buffer-name)
  (sql-show-buffer))

(defun sql-set-server-and-user nil
  "Set server, username, process, and buffer for the SQL interpreter."
  (interactive)
  (call-interactively 'sql-get-vendor)
  ;; skip server query if ``vendor:server'' was input.
  (let ((colon (string-match ":" sql-vendor)))
    (cond (colon
	   (setq sql-server (substring sql-vendor (1+ colon) nil))
	   (setq sql-vendor (substring sql-vendor 0 colon)))
	  (t
	   (call-interactively 'sql-get-server))))
  ;; skip username query if ``server/user'' was input.
  (let ((stroke (string-match "/" sql-server)))
    (cond (stroke
           (setq sql-username (substring sql-server (1+ stroke) nil))
           (setq sql-server (substring sql-server 0 stroke)))
          (t
           (call-interactively 'sql-get-username))))
  (setq sql-process-name
	(concat sql-vendor ":" sql-server "/" sql-username))
  (setq sql-buffer-name
        (concat sql-buffer-prefix sql-process-name sql-buffer-postfix)))

(defun sql-get-vendor (vendor)
  "Set the SQL server vendor name."
  (interactive (list (sql-string-prompt "Vendor: " 'sql-get-vendor-hist 2)))
  (setq sql-vendor vendor))

(defun sql-get-server (server)
  "Set the SQL server name."
  (interactive (list (sql-string-prompt "Server: " 'sql-get-server-hist 2)))
  (setq sql-server server))

(defun sql-get-username (username)
  "Set the SQL server username."
  (interactive (list (sql-string-prompt "Username: " 'sql-get-username-hist 2)))
  (setq sql-username username))

(defun sql-string-prompt (prompt &optional symbol offset)
  "Read a string using PROMPT.
With optional HIST-SYMBOL and HIST-OFFSET, read with history."
  (cond
   (symbol
    (let ((initial nil) (history nil))
      (if (and offset
	       (setq initial (nth (- offset 1) (symbol-value symbol))))
	  (setq history (cons symbol offset))
	(setq history symbol))
      (read-from-minibuffer prompt initial nil nil history)))
   (t (read-string prompt))))

(defun sql-ange-ftp-read-passwd (prompt &optional default)
  "Read a password, echoing `.' for each character typed.
End with RET, LFD, or ESC.  DEL or C-h rubs out.  C-u kills line.
Optional DEFAULT is password to start with."
  (let ((pass nil)
        (c 0)
        (echo-keystrokes 0)
        (cursor-in-echo-area t))
    (while (progn (message "%s%s"
                           prompt
                           (make-string (length pass) ?.))
                  (setq c (read-char))
                  (and (/= c ?\r) (/= c ?\n) (/= c ?\e)))
      (if (= c ?\C-u)
          (setq pass "")
        (if (and (/= c ?\b) (/= c ?\177))
            (setq pass (concat pass (char-to-string c)))
          (if (> (length pass) 0)
              (setq pass (substring pass 0 -1))))))
    (message nil)
    (or pass default "")))

(setq sql-ange-ftp-tmp-keymap (make-sparse-keymap))
(define-key sql-ange-ftp-tmp-keymap "\C-m" 'exit-minibuffer)

(defun sql-ange-ftp-repaint-minibuffer nil
  "Gross hack to set minibuf_message = 0, so that the contents of the
minibuffer will show."
  (if (eq (selected-window) (minibuffer-window))
      (if (fboundp 'allocate-event)
          ;; lemacs
          (let ((unread-command-event (character-to-event ?\C-m
                                                          (allocate-event)))
                (enable-recursive-minibuffers t))
            (read-from-minibuffer "" nil sql-ange-ftp-tmp-keymap nil))
        ;; v18 GNU Emacs
        (let ((unread-command-char ?\C-m)
              (enable-recursive-minibuffers t))
          (read-from-minibuffer "" nil sql-ange-ftp-tmp-keymap nil)))))

(fset 'sql-set-user 'sql)


;;;  SQL Command Buffer

(defun sql-buffer nil
  "Create or move to the sql-mode \"*SQL commands*\" buffer.
Start the SQL interpreter if necessary."
  (interactive)
  (let ((buff (get-buffer "*SQL Commands*")))
    (if buff (switch-to-buffer buff)
      (switch-to-buffer (get-buffer-create "*SQL Commands*"))
      (sql-mode)))
  (condition-case nil (sql-show-buffer) (error (sql))))

(provide 'sql-mode)

;;; sql-mode.el ends here
