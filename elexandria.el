;;; elexandria.el --- Alexandria-inspired library  -*- lexical-binding: t; -*-

;;; Commentary:

;; A collection of Emacs macros and functions that seem like they
;; ought to be built-in.  Inspired by CL's Alexandria package.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

;;;; Requirements

;; MAYBE: Can/should we use `eval-when-compile' on these?

(require 'cl-lib)
(require 'eieio)
(require 'dash)
(require 'seq)

;;;; Macros

;;;;; Binding

;; TODO: `with-dict' and `with-plist-vals' from emacs-package-dev-handbook

;;;;; EIEIO

(defmacro defclass* (name superclasses slots &rest options-and-doc)
  ;; MAYBE: Change instance-initform to instance-init.  This would no longer set
  ;; the slot to the value of the expression, but would just evaluate it.  The
  ;; expression could set the slot value with `setq' if necessary.
  "Like `defclass', but supports instance initforms.
Each slot may have an `:instance-initform', which is evaluated in
the context of the object's slots when each instance is
initialized, similar to Python's __init__ method."
  ;; TODO: Add option to set all slots' initforms (e.g. to set them all to nil).
  (declare (indent defun))
  (let* ((slot-inits (-non-nil (--map (let ((name (car it))
                                            (initer (plist-get (cdr it) :instance-initform)))
                                        (when initer
                                          (list 'setq name initer)))
                                      slots)))
         (slot-names (mapcar #'car slots))
         (around-fn-name (intern (concat (symbol-name name) "-initialize")))
         (docstring (format "Inititalize instance of %s." name)))
    `(progn
       (defclass ,name ,superclasses ,slots ,@options-and-doc)
       (when (> (length ',slot-inits) 0)
         (cl-defmethod initialize-instance :after ((this ,name) &rest _)
           ,docstring
           (with-slots ,slot-names this
             ,@slot-inits))))))

(defmacro oref* (&rest slots)
  "Access SLOTS of nested EIEIO objects.
The first of SLOTS should be an object, while the rest should be
slot symbols.  Accessing each slot should return an object for
which the next slot is valid, except for the last slot, which may
return any value."
  (cl-labels ((rec (slots)
                   `(oref ,(if (and (consp (cdr slots))
                                    (cddr slots))
                               (rec (cdr slots))
                             (cadr slots))
                          ,(car slots))))
    (rec (nreverse slots))))

(defmacro with-slots* (slots-objects &rest body)
  "Access slots of nested objects, evaluating BODY.
Creates nested `with-slots' forms, so each slot is a generalized
variable.  For example:

\(with-slots* (((id session) room)
              ((user) session))
             user)

Is transformed to:

\(with-slots (id session) room
  (with-slots (user) session
    user))"
  (declare (indent defun))
  (cl-loop for (slots object) in (reverse slots-objects)
           do (setq body `((with-slots ,slots ,object ,@body)))
           finally return (car body)))

;;;;; Files

(defmacro with-file-buffer (file options &rest body)
  "Evaluate BODY and return its value in a temp buffer for FILE.
OPTIONS is a plist of the following options:

`:insert': When non-nil (the default, when unspecified), insert
file's contents before evaluating BODY, leaving point before the
contents.

`:must-exist': When non-nil, signal an error if no file exists at
FILE.

`:write': When non-nil, write the contents of the buffer to FILE
after evaluating BODY.

`:overwrite': When nil (the default, when unspecified), signal an
error instead of overwriting an existing file at FILE.  If `ask',
ask for confirmation before overwriting an existing file.  If t,
overwrite a file at FILE unconditionally.

`:visit': Passed to function `write-region', which see.

`:lockname:' Passed to function `write-region', which see.

`:append': Passed to function `write-region', which see.  (When
using this option, you will probably want to specify `:insert
nil' as well.)

`:fsync': When non-nil (the default, when unspecified), bind
`write-region-inhibit-fsync' (which see) to this value.

`:precious': Bind `file-precious-flag' (which see) to this
value (when unspecified, nil)."
  (declare (indent 2) (debug (stringp form body)))
  `(let ((write-region-inhibit-fsync ,(when (plist-member options :fsync)
                                        (not (plist-get options :fsync))))
         (file-precious-flag ,(plist-get options :precious)))
     (with-temp-buffer
       ,(when (or (not (plist-member options :insert))
                  (plist-get options :insert))
          `(if (file-readable-p ,file)
               (save-excursion
                 (insert-file-contents ,file))
             (when ,(plist-get options :must-exist)
               (error "File not readable: %s" ,file))))
       (prog1
           (progn
             ,@body)
         ,(when (plist-get options :write)
            `(write-region nil nil ,file
                           ,(plist-get options :append)
                           ,(plist-get options :visit)
                           ,(plist-get options :lockname)
                           ,(pcase-exhaustive (plist-get options :overwrite)
                              ('nil ''excl)
                              ((or 'ask ''ask) ''ask)
                              ('t nil))))))))

;;;;; Keymaps

;; MAYBE: Remove the defvar part?  It could be useful for keymaps which are not put in defvars.

(defmacro defkeymap (name copy docstring &rest maps)
  "Define a new keymap variable (using `defvar').

NAME is a symbol, which will be the new variable's symbol.  COPY
may be a keymap which will be copied, or nil, in which case the
new keymap will be sparse.  DOCSTRING is the docstring for
`defvar'.

MAPS is a sequence of alternating key-value pairs.  The keys may
be a string, in which case they will be passed as arguments to
`kbd', or a raw key sequence vector.  The values may be lambdas
or function symbols, as would be normally passed to
`define-key'."
  (declare (indent defun))
  (let* ((map (if copy
                  (copy-keymap copy)
                (make-sparse-keymap (prin1-to-string name)))))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    `(defvar ,name ',map ,docstring)))

;;;;; Lists

(defmacro alist (&rest args)
  "Make an alist from ARGS.
ARGS should be a list of alternating key forms and value forms,
which will be evaluated at runtime."
  `(list ,@(cl-loop for (key value) on args by #'cddr
                    collect `(cons ,key ,value))))

;;;;; Symbols

(defmacro with-gensyms* (symbols &rest body)
  "Make a gensym for each of SYMBOLS, and replace each value instance of SYMBOLS in BODY with the corresponding gensym.
This is like the common `with-gensyms' macro, except it uses
`symbol-macrolet' to replace SYMBOLS, avoiding the need to
quasiquote the BODY and unquote each gensym."
  ;; FIXME: Needs more testing.
  (declare (indent defun))
  (let* ((gensyms (cl-loop for symbol in symbols
                           collect (list symbol (cl-gensym (symbol-name symbol))))))
    `(cl-symbol-macrolet ,gensyms
       ,@body)))

;;;;; Markers

(defmacro with-markers (markers &rest body)
  "Evaluate BODY with MARKERS bound, then clear each marker.
MARKERS should be a list of the form (SYMBOL FORM), where FORM
evaluates to a marker or nil."
  (declare (indent defun))
  (let ((varlist (cl-loop for m in markers
                          if (atom m)
                          collect m
                          else
                          collect (list (car m) `(copy-marker ,(cadr m)))))
        (marker-list (cl-loop for m in markers
                              if (atom m)
                              collect m
                              else
                              collect (car m))))
    `(let ,varlist
       ,@body
       (cl-loop for m in (list ,@marker-list)
                do (set-marker m nil)))))

;;;;; Processes

(defmacro call-process-with-args (process &rest args)
  "Return standard output of running PROCESS with ARGS.
Uses `call-process'.  Raises error if PROCESS exits with non-zero
status."
  (declare (indent defun))
  `(with-temp-buffer
     (unless (= 0 (call-process ,process nil t nil
                                ,@args))
       (error ,(concat process " failed")))
     (buffer-substring-no-properties (point-min) (point-max))))

;;;;; Region

(defmacro el-with-narrow (beg end &rest body)
  "Evaluate BODY with buffer narrowed to region between BEG and END.
Start with point at beginning of region.  Excursion and
restriction are saved."
  (declare (indent defun))
  `(save-excursion
     (save-restriction
       (narrow-to-region ,beg ,end)
       (goto-char (point-min))
       ,@body)))

(defmacro el-with-region (&rest body)
  "Evaluate BODY with buffer narrowed to current region.
Raise error if there is no active region.  Uses
`el-with-narrow'."
  (declare (indent defun))
  `(progn
     (unless (region-active-p)
       (error "No active region"))
     (el-with-narrow (region-beginning) (region-end)
       ,@body)))

(defmacro el-with-region-or-buffer (&rest body)
  "Evaluate BODY on whole buffer, or region if active.
Uses `el-with-narrow'."
  (declare (indent defun))
  `(save-mark-and-excursion
     (unless (region-active-p)
       (mark-whole-buffer))
     (el-with-narrow (region-beginning) (region-end)
       ,@body)))

;;;;; Regular expressions

;; FIXME: Doesn't work properly with e.g. `regexp' or `eval' forms in the `rx' form.
;; (defmacro rxs (&rest rest)
;;   "Like `rx', but evaluated at runtime with `rx-to-string'.
;; Unlike `rx-to-string', does not require a backquoted list
;; beginning with, e.g. `seq'.  It can take arguments exactly like
;; `rx'."
;;   ;; Using `backquote' was easier than using double backquote
;;   ;; characters for some reason.
;;   `(rx-to-string (backquote (seq ,@rest))))

;;;;; Strings

(defmacro format$ (string &rest objects)
  "Interpolated `format'.
Any word in STRING beginning with \"$\" is replaced with the
contents of the variable named that word.  OBJECTS are applied
in-order to %-sequences in STR.  Words surrounded by \"${}\" may
contain %-sequences.

For example:

  (format$ \"%s $name\" greeting)

Is expanded to:

  (format \"%s %s\" greeting name)

Variable names must contain only alphanumeric characters, -, or
_.  Any other character will be considered not part of a variable
name, which allows placing such characters adjacent to variable
names.  For example:

  (format$ \"[$date-time] %s $username>\" greeting)

Is expanded to:

  (format \"[%s] %s %s>\" date-time greeting username)

Including %-sequences, this:

  (format$ \"Amount: ${amount%.02f}  $name  %s\" date)

Expands to:

  (format \"Amount: %.02f  %s  %s\" amount name date)"
  (cl-macrolet ((concatf (place string)
                         `(setf ,place (concat ,place ,string)))
                (peek (seq)
                      `(when (> (length ,seq) 1)
                         (seq-take ,seq 1))))
    (let* (current-var current-char current-% current-{ (new-str "") vars)
      (while (setq current-char (when (not (string-empty-p string))
                                  (prog1 (seq-take string 1)
                                    (setq string (seq-drop string 1)))))
        (pcase current-char
          ;; FIXME: Other whitespace chars.  (Use pcase rx matcher in Emacs 26!)
          (" " (progn
                 (or (pcase current-%
                       (`nil nil)
                       (_ (pcase current-{
                            (`t (progn
                                  ;; Space as part of %-sequence
                                  (concatf current-% current-char)))
                            (_ (progn
                                 ;; Space after %-sequence
                                 (concatf new-str current-%))))))
                     (pcase current-var
                       (`nil nil)
                       (_ (progn
                            ;; Space after var
                            (push (intern current-var) vars)))))
                 (unless current-{
                   (concatf new-str current-char)
                   (setq current-var nil
                         current-% nil))))
          ("%" (pcase (peek string)
                 ("%" (progn
                        ;; %%
                        (concatf new-str "%%")
                        (seq-drop string 1)))
                 (" " (pcase current-{
                        (`t (progn
                              ;; Part of %-sequence
                              (setq current-% current-char)))
                        (_ (progn
                             ;; % alone
                             (concatf new-str current-char)))))
                 (_ (progn
                      ;; New %-sequence
                      (setq current-% current-char)
                      (unless current-{
                        (push (pop objects) vars))))))
          ("$" (pcase (peek string)
                 ("$" (progn
                        ;; "$$"
                        (concatf new-str "$$")
                        ;; FIXME: Using seq-drop here seems incorrect
                        (seq-drop string 1)))
                 (" " (progn
                        ;; Plain "$"
                        (concatf new-str "$")))
                 (`nil (progn
                         ;; End of string
                         (concatf new-str "$")))
                 ("{" (progn
                        ;; New variable with % control string
                        (setq current-var t
                              current-{ t)
                        (setq string (seq-drop string 1))))
                 (_ (progn
                      ;; New var
                      (concatf new-str "%s")
                      (setq current-var t)))))
          ((pred (string-match-p (rx (or alnum "-" "_" "." "+" "#"))))
           ;; Character could be part of var name or %-sequence
           (or (pcase current-%
                 (`nil nil)
                 (_ (progn
                      ;; Part of %-sequence
                      (concatf current-% current-char))))
               (pcase current-var
                 (`nil (progn
                         ;; Non-var character
                         (concatf new-str current-char)))
                 (`t (progn
                       ;; New var name
                       (setq current-var current-char)))
                 (_ (progn
                      ;; Partial var name
                      (concatf current-var current-char))))))
          ("}" (progn
                 (if (and current-var current-%)
                     (progn
                       ;; Closing ${} sequence
                       (push (intern current-var) vars)
                       (concatf new-str current-%)
                       (setq current-var nil
                             current-% nil
                             current-{ nil))
                   ;; Plain }
                   (concatf new-str current-char))))
          (_ (progn
               (if (or (pcase current-%
                         (`nil nil)
                         (_ (progn
                              ;; After %-sequence
                              t)))
                       (pcase current-var
                         (`nil nil)
                         (_ (progn
                              ;; After var
                              (push (intern current-var) vars)))))
                   (progn
                     (concatf new-str current-char)
                     (setq current-var nil
                           current-% nil))
                 ;; Character not part of var name
                 (concatf new-str current-char))))))
      (cond (current-%
             ;; String ended with %-sequence
             (concatf new-str current-%))
            (current-var
             ;; String ended with variable
             (push (intern current-var) vars)))
      `(format ,new-str ,@(nreverse vars)))))

;;;;; URL-retrieve

(defvar-local url-with-retrieve-async-timeout-timer nil
  "When a response buffer has a timeout, this variable stores the
  timer object so that it may be canceled if the request
  completes successfully.")

(cl-defun url-with-retrieve-async (url &key cbargs silent inhibit-cookies data
                                       (method "GET") extra-headers query timeout success error
                                       parser (query-on-exit t))
  ;; FIXME: Ensure docstring is up-to-date with all recent changes.

  ;; TODO: Work around url calling callbacks multiple times.  Sigh.  See
  ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=20159> and
  ;; <https://github.com/skeeto/elisp-latch/issues/1#issuecomment-397910988>.

  ;; TODO: Encode all data as UTF-8 and tell the server we're using that
  ;; encoding.  Save everyone the trouble by doing the right thing
  ;; automatically.  See:
  ;; <https://github.com/jgkamat/matrix-client-el/commit/d3b0233ffb555a8d01bdafcfbe24d6c96b1dab1d>
  ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=23750>
  ;; <https://github.com/dakrone/es-mode/issues/62>
  ;; <https://github.com/magit/ghub/issues/35>
  ;; <https://github.com/magit/ghub/commit/45cd8c8341f2065aad6ed72200c10842f8087498>
  "Retrieve URL asynchronously with `url-retrieve'.

Arguments CBARGS, SILENT, and INHIBIT-COOKIES are passed to
`url-retrieve', which see.

DATA is bound to `url-request-data', which see.

METHOD may be a symbol or string, which is bound as a capitalized
string to `url-request-method', which see.

EXTRA-HEADERS is an alist of header-value pairs, which is bound
to `url-request-extra-headers', which see.

QUERY is an alist of key-value pairs which is appended to the URL
as the query.

TIMEOUT may be a number of seconds, after which the error
callback will run if the request hasn't completed by then.

SUCCESS may be a function symbol or a body form, which is called
with zero arguments upon successful completion of the request.
In the call to SUCCESS, these variables will be bound:

`status': See `url-retrieve'.
`cbargs': See `url-retrieve'.
`headers': The HTTP response headers as a string.
`data': The HTTP response body as a string.

ERROR may be a function symbol or a body form, which is called
with zero arguments if the request fails.  In the error call,
these variables will be bound, in addition to the ones bound for
SUCCESS:

`errors': The list of `url' error symbols for the most recent
error, e.g. `(error http 404)' for an HTTP 404 error.

In the SUCCESS and ERROR calls, the current buffer is the
response buffer, and it is automatically killed when the call
completes.

PARSER may be a function which parses the response body and
returns a value to bind `data' to.  The point is positioned after
the headers, at the beginning of the body, before calling the
function.  For example, `json-read' may be used to parse JSON
documents, after which the parsed JSON would be available in
SUCCESS and ERROR as `data'.  Or, if the body is not needed,
`ignore' could be used to prevent the body from being parsed."
  (declare (indent defun))
  (let* ((success-body-fn (cl-typecase success
                            (function success)
                            (otherwise (byte-compile
                                        `(cl-function
                                          (lambda (&key cbargs status headers data)
                                            ,success))))))
         (error-body-fn (cl-typecase error
                          (function error)
                          (otherwise (byte-compile
                                      `(cl-function
                                        (lambda (&key cbargs status error headers data url)
                                          ,error))))))
         (url-request-data data)
         (url-request-method (upcase (cl-typecase method
                                       (symbol (symbol-name method))
                                       (string method))))
         ;; TODO: Note that extra-headers must be an alist, and both keys and values must be strings.
         (url-request-extra-headers extra-headers)
         ;; FIXME: Document how `url-http-attempt-keepalives' is set.
         (url-http-attempt-keepalives (and (not timeout)
                                           url-http-attempt-keepalives))
         (callback (lambda (status &optional cbargs)
                     (unwind-protect
                         ;; This is called by `url-http-activate-callback' with the response buffer
                         ;; as the current buffer.

                         ;; Check for errors
                         (pcase status
                           ;; NOTE: This may need to be updated to correctly handle multiple errors
                           (`(:error . ,_) (funcall error-body-fn
						    :url url
                                                    :cbargs cbargs
                                                    :status status
                                                    :error (plist-get status :error)))
                           ((or 'nil
                                `(:peer (:certificate . ,_))
                                `(:redirect . ,_))
                            (if (not url-http-end-of-headers)
                                ;; HACK: It seems that the callback can be called with `nil' when
                                ;; the connection fails before getting any headers, like:
                                ;; url-http-end-of-document-sentinel(#<process matrix.org<5>>
                                ;; "connection broken by remote peer\n"), in which case
                                ;; `url-http-end-of-headers' is nil, so we need to call the error
                                ;; fn.  Would like to structure this more cleanly.
                                (funcall error-body-fn
                                         :url url
                                         :cbargs cbargs
                                         :status status
                                         :error (plist-get status :error))
                              (let ((headers (buffer-substring (point) url-http-end-of-headers))
                                    (data (if parser
                                              (progn
                                                (goto-char (1+ url-http-end-of-headers))
                                                (funcall parser))
                                            (buffer-substring (1+ url-http-end-of-headers) (point-max)))))
                                (funcall success-body-fn
                                         :cbargs cbargs
                                         :status status
                                         :headers headers
                                         :data data))))
                           (_ (error "Response status unrecognized; please report this error: %s" (pp-to-string status))))
                       (when url-with-retrieve-async-timeout-timer
                         (cancel-timer url-with-retrieve-async-timeout-timer))
                       (unless (kill-buffer (current-buffer))
                         (warn "Unable to kill response buffer: %s" (current-buffer))))))
         url-obj query-string query-params response-buffer)
    (when query
      ;; Build and append query string to URL
      (progn
        ;; Transform alist to plain list for `url-build-query-string'
        (setq query-params (cl-loop for (key . val) in query
                                    when val
                                    collect (list key val)))
        (setq url-obj (url-generic-parse-url url))
        (setq query-string (url-build-query-string query-params))
        (setf (url-filename url-obj) (concat (url-filename url-obj) "?" query-string))
        (setq url (url-recreate-url url-obj))))
    (setq response-buffer (url-retrieve url callback cbargs silent inhibit-cookies))
    (when timeout
      (with-current-buffer response-buffer
        (setq-local url-with-retrieve-async-timeout-timer
                    (run-with-timer timeout nil
                                    (lambda ()
                                      (when (and (buffer-live-p response-buffer)
                                                 (get-buffer-process response-buffer))
                                        (with-current-buffer response-buffer
                                          ;; Since we are handling the timeout ourselves, when we kill the
                                          ;; process, url.el considers it a "success", and therefore does not kill
                                          ;; the buffer (it seems to only kill its own buffers when it detects a
                                          ;; HTTP response error code, which we aren't getting).  So we first add
                                          ;; an errors list to the first element of the callback args (the
                                          ;; `status' arg), then we delete the process, causing the process's
                                          ;; sentinel to be called, which then calls the callback, which detects
                                          ;; the error and calls the error-body-fn.

                                          ;; FIXME: Sometimes this seems to stop catching timeouts.
                                          ;; When that happens, it seems that the response buffer
                                          ;; process does not get deleted, as it remains listed in
                                          ;; `list-processes'.  Maybe the solution is to bind
                                          ;; `url-http-attempt-keepalives' to nil when a timeout is
                                          ;; set, because maybe that would prevent processes from
                                          ;; being left around, which seems to contribute to the
                                          ;; problem.

                                          ;; NOTE: This may be loosely relevant: <https://github.com/jorgenschaefer/circe/issues/327>
                                          (setq url-callback-arguments (list (list :error 'timeout) url-callback-arguments))
                                          ;; Since `get-buffer-process' is a C function, we just call it again
                                          ;; instead of storing the buffer process in a variable.
                                          (delete-process (get-buffer-process response-buffer))
                                          (setq url-with-retrieve-async-timeout-timer nil))))))))
    (unless query-on-exit
      (set-process-query-on-exit-flag (get-buffer-process response-buffer) nil))
    response-buffer))

;;;; Functions

;;;;; Lists

(defun el-partition-by-column (list columns)
  ;; TODO: Probably rename `columns' to `num-columns' or `number'.
  "Partition LIST into COLUMNS columns.
Unlike some partition functions, columns are filled before rows.

If COLUMNS is more than can be evenly filled, the appropriate
smaller number will be used (for example, with a list of 8 items,
if 5 columns were specified, 4 columns will actually be used."
  (cl-loop with length = (length list)
           with num-rows = (+ (/ length columns)
                              (pcase (% length columns)
                                (0 0)
                                (_ 1)))
           with result = (-repeat num-rows nil)
           with row = 0
           with col = 0
           for elem = (pop list)
           while elem
           do (progn
                (push elem (nth row result))
                (incf row)
                (when (= row num-rows)
                  (setq row 0)
                  (incf col)))
           finally return (mapcar #'nreverse result)))

;;;;; Math

(defsubst clamp (min number max)
  "Return NUMBER clamped to between MIN and MAX, inclusive."
  (max min (min max number)))

;;;; Footer

(provide 'elexandria)

;;; elexandria.el ends here

;; Local Variables:
;; fill-column: 80
;; End:
