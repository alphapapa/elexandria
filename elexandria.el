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

;;;;; Keymaps

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
                (make-sparse-keymap name))))
    (cl-loop for (key fn) on maps by #'cddr
             do (progn
                  (when (stringp key)
                    (setq key (kbd key)))
                  (define-key map key fn)))
    `(defvar ,name ,map ,docstring)))

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

  (format$ \"Amount: ${amount% .02f}  $name  %s\" date)

Expands to:

  (format \"Amount: % .02f  %s  %s\" amount name date)"
  (cl-macrolet ((concatf (place string)
                         `(setf ,place (concat ,place ,string))))
    (cl-labels ((peek (seq)
                      (when (> (length seq) 1)
                        (seq-take seq 1))))
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
        `(format ,new-str ,@(nreverse vars))))))

;;;;; URL-retrieve

(cl-defmacro with-url-retrieve-sync (url silent inhibit-cookies &rest body)
  "Retrieve URL synchronously with `url-retrieve-synchronously'.
SILENT and INHIBIT-COOKIES are passed to `url-retrieve-synchronously', which see.

BODY may be a function symbol or body form, which will be called
when the request completes.  These variables will be bound in the
call to BODY:

`headers': The HTTP response headers as a string.
`body': The HTTP response body as a string.

After BODY is called, the response buffer will be killed automatically."
  ;; FIXME: Add enhancements from -async version
  (declare (indent defun))
  (let ((body (cl-typecase body
                (function body)
                (otherwise `(lambda ()
                              ,body)))))
    `(with-gensyms* (response-buffer headers response)
       (let* ((response-buffer (url-retrieve-synchronously ,url ,silent ,inhibit-cookies))
              headers response)
         (when response-buffer
           (unwind-protect
               (with-current-buffer response-buffer
                 (setq headers (buffer-substring (point) url-http-end-of-headers)
                       response (buffer-substring url-http-end-of-headers (point-max))))
             (unless (kill-buffer response-buffer)
               (warn "Unable to kill response buffer: %s" response-buffer)))
           (funcall ,body))))))

(cl-defmacro with-url-retrieve-async (url &key cbargs silent inhibit-cookies data method extra-headers query success error
                                          parse-body-fn)
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

SUCCESS may be a function symbol or a body form, which is called
with zero arguments upon successful completion of the request.
In the call to SUCCESS, these variables will be bound:

`status': See `url-retrieve'.
`cbargs': See `url-retrieve'.
`headers': The HTTP response headers as a string.
`body': The HTTP response body as a string.

ERROR may be a function symbol or a body form, which is called
with zero arguments if the request fails.  In the error call,
these variables will be bound, in addition to the ones bound for
SUCCESS:

`errors': The list of `url' error symbols for the most recent
error, e.g. `(error http 404)' for an HTTP 404 error.

In the SUCCESS and ERROR calls, the current buffer is the
response buffer, and it is automatically killed when the call
completes.

PARSE-BODY-FN may be a function which parses the body and returns
a value to bind `body' to.  The point is positioned after the
headers, at the beginning of the body, before calling the
function.  For example, `json-read' may be used to parse JSON
documents, after which the parsed JSON would be available in
SUCCESS and ERROR as `body'.  Or, if the body is not needed,
`ignore' could be used to prevent the body from being parsed."
  (declare (indent defun))
  (with-gensyms* (success-body-fn error-body-fn url-obj filename query-string query-params)
    (let* ((success-body-fn (cl-typecase success
                              (function success)
                              (otherwise `(lambda ()
                                            ,success))))
           (error-body-fn (cl-typecase error
                            (function error)
                            (otherwise `(lambda ()
                                          (let ((errors (plist-get status :error)))
                                            ,error))))))
      `(let* ((url-request-data ,data)
              (url-request-method (upcase (cl-typecase ,method
                                            (symbol (symbol-name ,method))
                                            (string ,method))))
              (url-request-extra-headers ,extra-headers)
              (callback (cl-function
                         (lambda (status &rest cbargs)
                           (unwind-protect
                               ;; This is called by `url' with the current buffer already being the
                               ;; response buffer.

                               ;; FIXME: We can use `cl-symbol-macrolet' instead of `let' here,
                               ;; which only evaluates `headers' and `body' if they are present in
                               ;; the body-fns, which would be good, to avoid making strings when
                               ;; they are not used.  However, if the body-fns are passed to the
                               ;; macro as function symbols rather than body forms, the macrolet
                               ;; would not activate, because it can't see into the other functions'
                               ;; bodies (or if it could, that would probably be way too complicated
                               ;; and a bad idea).  So we might want to figure out a way to make the
                               ;; strings optional.  Or maybe we could set a parser arg, similar to
                               ;; `request', so that e.g. `json-read' could read the response buffer
                               ;; directly, instead of turning the response into a string, then
                               ;; using `json-read-from-string', which inserts back into a temp
                               ;; buffer, which is wasteful.  However, for the headers, I think it's
                               ;; reasonable to always bind them as a string, because the headers
                               ;; aren't very long, especially compared to a long HTML or JSON
                               ;; document.
                               (let ((headers (buffer-substring (point) url-http-end-of-headers))
                                     (body (if ,parse-body-fn
                                               (progn
                                                 (goto-char (1+ url-http-end-of-headers))
                                                 (funcall ,parse-body-fn))
                                             (buffer-substring (1+ url-http-end-of-headers) (point-max)))))
                                 ;; Check for errors
                                 (pcase status
                                   ;; NOTE: This may need to be updated to correctly handle multiple errors
                                   (`(:error . ,_) (funcall ,error-body-fn))
                                   ((or 'nil `(:peer (:certificate . ,_) . ,_)) (funcall ,success-body-fn))
                                   (_ (error "Response status unrecognized; please report this error: %s" status))))
                             (unless (kill-buffer (current-buffer))
                               (warn "Unable to kill response buffer: %s" (current-buffer)))))))
              url-obj filename query-string query-params)
         (if-let ((query ,query))
             ;; Build and append query string to URL
             (progn
               ;; Transform alist to plain list for `url-build-query-string'
               (setq query-params (cl-loop for (key . val) in query
                                           when val
                                           collect (list key val)))
               (setq url-obj (url-generic-parse-url ,url))
               (setq query-string (url-build-query-string query-params))
               (setf (url-filename url-obj) (concat (url-filename url-obj) "?" query-string))
               (setq url (url-recreate-url url-obj)))
           ;; No query
           (setq url ,url))
         ;;  (message "\n\nDEBUG: %s" (list 'url-retrieve url callback ,cbargs ,silent ,inhibit-cookies))
         (url-retrieve url callback ,cbargs ,silent ,inhibit-cookies)))))

;;;; Functions

;;;;; Math

(defun clamp (number min max)
  "Return NUMBER clamped to range bound by MIN and MAX.
Return MIN if NUMBER is less than or equal to MIN, or MAX if it's
greater than or equal to MAX, or NUMBER if it's between them."
  (cond ((<= number min) min)
        ((>= number max) max)
        (t number)))

;;;; Footer

(provide 'elexandria)

;;; elexandria.el ends here
