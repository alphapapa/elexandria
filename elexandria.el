;;; Code:

;;;; Requirements

(require 'cl-lib)
(require 'eieio)
(require 'dash)
(require 'seq)

;;;; Macros

;;;;; EIEIO

(defmacro defclass* (name superclasses slots &rest options-and-doc)
  "Like `defclass', but supports instance initforms.
Each slot may have an `:instance-initform', which is evaluated in
the context of the object's slots when each instance is
initialized."
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

;;;;; Symbols

(defmacro with-gensyms* (symbols &rest body)
  "Make a gensym for each of SYMBOLS, and replace each value instance of SYMBOLS in BODY with the corresponding gensym.
This is like the common `with-gensyms' macro, except it uses
`symbol-macrolet' to replace SYMBOLS, avoiding the need to
quasiquote the BODY and unquote each gensym."
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

(defmacro rxs (&rest rest)
  "Like `rx', but evaluated at runtime with `rx-to-string'.
Unlike `rx-to-string', does not require a backquoted list
beginning with, e.g. `seq'.  It can take arguments exactly like
`rx'."
  ;; Using `backquote' was easier than using double backquote
  ;; characters for some reason.
  `(rx-to-string (backquote (seq ,@rest))))

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
