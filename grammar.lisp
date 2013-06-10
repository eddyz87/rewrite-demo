(in-package :rw)
;; TODO:
;; - unit tests
;; - super/inline checks
;; - pattern meta programming
;; - null alts in the printer

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defun hash-values (ht)
    (let ((lst))
      (maphash (lambda (k v)
                 (declare (ignore k))
                 (push v lst))
               ht)
      lst))
  
  (defun mk-symbol-name (&rest strings-or-symbols)
    (string-upcase
      (apply #'concatenate
             (cons 'string
                   (mapcar (lambda (x)
                             (if (symbolp x)
                               (symbol-name x)
                               x))
                           strings-or-symbols)))))
  
  ;; TO DO check for errors
  (defun extract-options (list &rest opt-names)
    "extracts options designed by opt-names from the list
     option name is one of:
       symbol               - only symbol is removed from the list
       (symbol vals-number) - symbol and vals-number elements are removed from the list
     returns new list and a set of values for options"
    (let ((opt-names (mapcar (lambda (x) (cond ((symbolp x) (cons x 0))
                                               ((listp x) (cons (first x) (second x)))
                                               (t (error "Unexpexcted option form: ~A" x))))
                             opt-names)))
  ;;    (format t "opt-names=~A~%" opt-names)
      (labels ((%next (list accum bindings)
  ;;                (format t "accum=~A~%" accum)
                  (if list
                    (let* ((el (car list))
                           (num (cdr (assoc el opt-names))))
  ;;                    (format t "el=~A num=~A assoc=~A~%" el num (assoc el opt-names))
                      (if num
                        (let ((vals (if (= num 0)
                                      t
                                      (subseq list 1 (+ num 1))))
                              (tail (subseq list (+ num 1))))
                          (rplacd (assoc el bindings) vals)
                          (%next tail accum bindings))
                        (%next (cdr list) (cons el accum) bindings)))
                    (values (reverse accum) bindings))))
        (multiple-value-bind (list bindings)
                             (%next list nil (mapcar (lambda (x) (cons (car x) nil))
                                                     opt-names))
  ;;        (format t "list=~A bindings=~A~%" list bindings)
          (apply #'values (cons list (mapcar #'cdr bindings)))))))

    (defun print-term (stream print-table term)
      (let ((printer (and (listp term)
                          (gethash (car term) print-table))))
        (if printer
          (funcall printer stream (cdr term))
          (format stream "~A " term stream))))

;;(print (extract-options '(1 2) '(:f 2) ':g))
;;(print (extract-options '(1 2 :f 3 4 :g 5) '(:f 2) ':g))

(defmacro with-error-context ((&rest context-description) &body body)
  (let ((e (gensym)))
    `(handler-bind
       ((error (lambda (,e) (error "~A~%in context of: ~A"
                                   ,e
                                   (funcall #'format nil ,@context-description)))))
       ,@body)))

(defstruct rule
  (name      nil :type symbol)
  (alts      nil :type list)
  (supertype nil :type (or symbol nil))
  (is-value  nil :type boolean))

(defun new-rule (rule &key name alts supertype is-value)
  (let ((r1 (copy-rule rule)))
    (setf (rule-name r1) (or name (rule-name rule))
          (rule-alts r1) (or alts (rule-alts rule))
          (rule-supertype r1) (or supertype (rule-supertype rule))
          (rule-is-value r1) (or is-value (rule-is-value rule)))
    r1))

(defstruct alt
  (constructor nil :type (or symbol nil))
  (is-inline   nil :type boolean)
  (symbols     nil :type list)) ;; list of symb

(defstruct symb 
  (name              nil :type symbol)
  (is-ignored-in-ast nil :type boolean)
  (string            nil :type (or (satisfies null) (satisfies stringp))))

;; TODO: yacc:define-parser defines it's own "defstruct grammar"
;;       will have to put these to different packages and rename "grammar1" to "grammar"
(defstruct grammar1
  (name            nil :type symbol)
  (rules           nil :type list)
  (terminals       (make-hash-table :test #'equal) :type hash-table)
  (start           nil :type (or symbol nil))
  (start-map       nil :type list)
  (meta-var-starts nil :type list))

(defun new-grammar1 (g &key name rules terminals start start-map meta-var-starts)
  (let ((g1 (copy-grammar1 g)))
    (setf (grammar1-name g1) (or name (grammar1-name g))
          (grammar1-rules g1) (or rules (grammar1-rules g))
          (grammar1-terminals g1) (or terminals (grammar1-terminals g))
          (grammar1-start g1) (or start (grammar1-start g))
          (grammar1-start-map g1) (or start-map (grammar1-start-map g))
          (grammar1-meta-var-starts g1) (or meta-var-starts (grammar1-meta-var-starts g)))
    g1))

(defun parse-grammar1 (s-expr)
  (let ((name (car s-expr))
        (rules (cdr s-expr))
        (terminals (make-hash-table :test #'equal))) ;; string -> (cons symbol . transformation func)
    (assert (symbolp name))
    (assert (listp rules))
    (labels ((%terminal (str &key (quote t) name transform)
               (let ((str (if quote
                            (cl-ppcre:quote-meta-chars str)
                            str)))
                 (car (or (gethash str terminals)
                          (setf (gethash str terminals) (cons (if name
                                                                name
                                                                (gensym str))
                                                              (or transform
                                                                  'identity)))))))

             (%parse-rule (rule)
               (with-error-context ("rule: ~A" rule)
                 (assert (listp rule)) ;; TO DO add error messages
                 (let* ((name (car rule))
                        (alts (cdr rule))
                        (is-meta (eq name 'meta-var)))
                   (assert name)
                   (assert (symbolp name))
                   (assert (listp alts))
                   (multiple-value-bind (alts value supertype)
                                        (extract-options alts ':value '(:super 1))
                      (let ((alts (if value
                                    (progn
                                      (assert (stringp (car alts)))
                                      (assert (null (cdr alts)))
                                      (list
                                        (%parse-alt (symbol-name name)
                                                    (list (%terminal
                                                            (car alts)
                                                            :quote nil
                                                            :name (when is-meta 'meta-var-terminal)
                                                            :transform (when is-meta 'intern)))
                                                    :constructor (when is-meta 'variable)
                                                    )))
                                    (mapcar (lambda (a) (%parse-alt (symbol-name name) a))
                                            alts))))
                        (make-rule
                          :name name
                          :alts alts
                          :supertype (car supertype)
                          :is-value value))))))

             (%parse-alt (base-name symbols &key constructor)
               (with-error-context ("alternative: ~A" symbols)
                 (assert (listp symbols) () "expected symbols to be of type list, but actual type is ~A" (type-of symbols))
                 (multiple-value-bind (symbols inline) (extract-options symbols '(:inline 0))
                   (make-alt
                     :constructor (or constructor
                                      (unless inline (gensym base-name)))
                     :is-inline (not (not inline))
                     :symbols (mapcar (lambda (el)
                                     (cond
                                       ((symbolp el) (make-symb :name el))
                                       ((stringp el) (make-symb :name (%terminal el)
                                                                :is-ignored-in-ast t
                                                                :string el))
                                       (t (error "Alternative element has to be symbol or string ~
                                                 but ~A of type ~A found, while parsing rule ~A"
                                                 el (type-of el) base-name))))
                                 symbols))))))
      ;; TODO: check that meta-var is present only once
      (let* ((rules (mapcar #'%parse-rule rules))
             (meta-var (find 'meta-var rules :key #'rule-name)))
        (assert meta-var)
        (make-grammar1
          :name name 
          :rules rules 
          :terminals terminals)
        ))))

(defun make-first-rule (grammar1)
  (let* ((rules (remove-if #'rule-supertype
                           (grammar1-rules grammar1)))
         (start (gensym "START"))
         (start-map (mapcar (lambda (rule)
                              (let ((name (rule-name rule)))
                                (cons name (gensym (mk-symbol-name "start-" name)))))
                            rules)))
    (new-grammar1 grammar1
       :rules (cons (make-rule
                      :name start
                      :alts (mapcar
                              (lambda (el)
                                (make-alt
                                  :constructor (cdr el)
                                  :symbols (list
                                             (make-symb :name (cdr el)
                                                        :is-ignored-in-ast t)
                                             (make-symb :name (car el)))
                                  :is-inline t))
                              start-map))
                      (grammar1-rules grammar1))
       :start start
       :start-map start-map)))

(defun insert-meta-vars (grammar1)
  (let ((meta-var-starts))
    (new-grammar1 grammar1
      :rules (mapcar (lambda (rule)
                       ;; add meta var only if rule has no references to inline rules
                       (if (some #'alt-is-inline (rule-alts rule))
                         rule
                         (new-rule
                           rule
                           :alts (cons (make-alt
                                         ;;:constructor (gensym (mk-symbol-name (rule-name rule) "-meta"))
                                         :is-inline t
                                         :symbols (progn
                                                    (let ((start (gensym (mk-symbol-name (rule-name rule) "-meta-start"))))
                                                      (push (cons (or (rule-supertype rule)
                                                                      (rule-name rule))
                                                                  start)
                                                            meta-var-starts)
                                                      (list
                                                        (make-symb :name start
                                                                   :is-ignored-in-ast t
                                                                   :string "")
                                                        (make-symb :name 'meta-var)))))
                                       (rule-alts rule)))))
                     (grammar1-rules grammar1))
      :meta-var-starts meta-var-starts)))

(defun make-yacc-alt-ast-function (alt)
  (let* ((pairs (mapcar (lambda (s)
                          (let ((arg (gensym "X")))
                            (cons arg
                                  (unless (symb-is-ignored-in-ast s)
                                    arg))))
                        (alt-symbols alt)))
         (args (mapcar #'car pairs))
         (vals (remove nil (mapcar #'cdr pairs))))
    `(lambda ,args
       ,(if (alt-is-inline alt)
          (car (last vals))
          `(list ',(alt-constructor alt)
                 ,@vals)))))

(defun make-yacc-alt (alt)
  `(,@(mapcar #'symb-name (alt-symbols alt))
     ,(make-yacc-alt-ast-function alt)))

(defun make-yacc-rule (rule)
  `(,(rule-name rule)
     ,@(mapcar #'make-yacc-alt (rule-alts rule))))

(defun make-yacc-parser-spec (grammar1 parser-name)
  `(yacc:define-parser ,parser-name
                       (:start-symbol ,(grammar1-start grammar1))
                       (:terminals ,(append
                                      (mapcar #'car (hash-values (grammar1-terminals grammar1)))
                                      (mapcar #'cdr (grammar1-start-map grammar1))
                                      (mapcar #'cdr (grammar1-meta-var-starts grammar1))))
                       ,@(mapcar #'make-yacc-rule (grammar1-rules grammar1))))

(defun make-lex-string-lexer (grammar1 lexer-name)
  (let ((lexer-rules nil))
    (maphash (lambda (regex pair)
               (let ((symb (car pair))
                     (transform (cdr pair)))
                 (push `(,regex (return (values ',symb (,transform $@))))
                       lexer-rules)))
             (grammar1-terminals grammar1))
    `(cl-lex:define-string-lexer ,lexer-name
                                 ,@lexer-rules)))

(defvar *meta-var-types* nil)

(defun lookup-meta-var-type (name)
  (cdr (assoc name *meta-var-types* :test #'string=)))

(defun make-mk-lexer (grammar1 mk-lexer-name string-lexer-name)
  (let ((str (gensym "STR"))
        (func (gensym "FUNC"))
        (start-symbol (gensym "START-SYMBOL"))
        (first-call (gensym "FIRST-CALL"))
        (next (gensym "NEXT"))
        (symb (gensym "SYMB"))
        (val (gensym "VAL"))
        (tmp (gensym "TMP")))
    `(defun ,mk-lexer-name (,start-symbol ,str)
       (let ((,func (,string-lexer-name ,str))
             (,next (cons ,start-symbol nil)))
         (lambda ()
           (if ,next
             (let ((,tmp ,next))
               (setf ,next nil)
               (values (car ,tmp) (cdr ,tmp)))
             (multiple-value-bind (,symb ,val) (funcall ,func)
               ;;(format t "lexer: next=~A symb=~A val=~A~%" ,next ,symb ,val)
               (if (eq ,symb 'meta-var-terminal)
                 (progn
                   (setf ,next (cons ,symb ,val))
                   (values
                     (case (lookup-meta-var-type ,val)
                       ,@(mapcar (lambda (el) `(,(car el) ',(cdr el)))
                                 (grammar1-meta-var-starts grammar1))
                        (t (error "unknown meta-var type: ~A"
                                  (lookup-meta-var-type ,val))))
                     nil))
                 (values ,symb ,val)))))
         ))))

(defun make-parse-string (parse-string-name parser-name mk-lexer-name start-map)
  (let ((str (gensym "STR"))
        (start-symbol (gensym "START-SYMBOL"))
        (kind (gensym "KIND")))
    `(defun ,parse-string-name (,kind ,str)
       (yacc:parse-with-lexer
         (let ((,start-symbol (cdr (assoc ,kind ,start-map))))
           (assert ,start-symbol () "Not start symbol found for ~A in ~A" ,kind ,start-map)
           (,mk-lexer-name ,start-symbol ,str))
         ,parser-name))))

;; TODO: assert that symb-is-ignored-in-ast can't be inline

(defun make-alt-printer-function (alt printer-name)
  (let ((term (gensym "TERM"))
        (stream (gensym "STREAM"))
        (number 0))
    (if (alt-is-inline alt)
      `(lambda (,stream ,term)
         (,printer-name ,stream (last ,term)))
      `(lambda (,stream ,term)
         ,@(mapcar (lambda (s)
                     (if (symb-is-ignored-in-ast s)
                       `(progn
                          (write-string ,(symb-string s) ,stream)
                          (write-string " " ,stream))
                       (prog1
                         `(,printer-name ,stream (nth ,number ,term))
                         (incf number))))
                   (alt-symbols alt))))))

(defun make-printer (printer-name print-table)
  (let ((stream (gensym "STREAM"))
        (term (gensym "TERM")))
    `(defun ,printer-name (,stream ,term)
       (print-term ,stream ,print-table ,term))))

(defun make-print-table-init (grammar1 print-table printer-name)
  `(progn
     ,@(mapcan (lambda (rule)
                 (mapcar (lambda (alt)
                           `(setf (gethash ',(alt-constructor alt)
                                           ,print-table)
                                  ,(make-alt-printer-function alt printer-name)))
                         (rule-alts rule)))
               (grammar1-rules grammar1))))

(defun make-grammar1-defs (grammar1)
  (let* ((gname (grammar1-name grammar1))
         (parser-name (gensym (mk-symbol-name gname "-parser")))
         (string-lexer-name (gensym (mk-symbol-name gname "-string-lexer")))
         (mk-lexer-name (gensym (mk-symbol-name "mk-" gname "-lexer")))
         (parse-string-name (intern (mk-symbol-name gname "-parse-string")))
         (start-map (gensym (mk-symbol-name gname "-start-map")))
         (printer-name (intern (mk-symbol-name gname "-print")))
         (print-table (gensym (mk-symbol-name gname "-print-table"))))
  `(progn
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (defvar ,start-map ',(grammar1-start-map grammar1))
      (defvar ,print-table (make-hash-table :test #'eq))
      ,(make-print-table-init grammar1 print-table printer-name))
    ,(make-yacc-parser-spec grammar1 parser-name)
    ,(make-lex-string-lexer grammar1 string-lexer-name)
    ,(make-mk-lexer grammar1 mk-lexer-name string-lexer-name)
    ,(make-parse-string parse-string-name parser-name mk-lexer-name start-map)
    ,(make-printer printer-name print-table)
    )))


(defmacro defg (name &rest rules)
  (let ((defs (make-grammar1-defs
                (make-first-rule
                  (insert-meta-vars
                    (parse-grammar1 (cons name rules)))))))
    (print "-- DEFS DUMP START --")
    ;;(print defs)
    (print "-- DEFS DUMP END --")
    (print "")
    defs
    ))

(defg zulu
        (expr (:inline expr1)
              (expr1 "+" expr)
              (expr1 "-" expr))

        (expr1 :super expr
               (:inline expr2)
               (expr2 "*" expr1)
               (expr2 "/" expr1))

        (expr2 :super expr
               ("(" expr ")")
               (id)
               (int))

        (id :value "[_a-z][_a-zA-Z0-9]*")
        (int :value "[0-9]+")
        (meta-var :value "\\?[_a-z][_a-zA-Z0-9]*")
        )
)
;;(let ((e (zulu-parse-string 'expr "1")))
;;(let ((e (zulu-parse-string 'expr "gg * 2 + 1 + ( 211 )")))
;;  (print e)
;;  (print "")
;;  (zulu-print *standard-output* e)
;;  (print ""))
;;
;;(let* ((*meta-var-types* (list (cons '|?e| 'expr)))
;;       (e (zulu-parse-string 'expr "gg * 2 + 1 + ( ?e )")))
;;  (print e)
;;  (print "")
;;  (zulu-print *standard-output* e)
;;  (print ""))

(defmacro z (kind str) `',(zulu-parse-string kind str))

(defmacro z1 (kind str)
  (labels ((%add-cons (lst)
             (if (and (consp lst)
                      (not (eq (car lst) 'variable)))
               (list* 'list (mapcar #'%add-cons lst))
               (if (and (consp lst)
                        (eq (car lst) 'variable))
                 lst
                 (list 'quote lst)))))
    `,(%add-cons (zulu-parse-string kind str))))

(defmacro match (term (&rest vars) &rest clauses)
  (let* ((*meta-var-types* (mapcar (lambda (v) (cons (first v) (second v)))
                                   vars))
         (clauses (mapcar (lambda (c) `(,(macroexpand-1 (car c)) ,@(cdr c)))
                          clauses))
         (res `(optima:match ,(macroexpand-1 term) ,@clauses)))
    (print "")
    (print "-- match --")
    (print res)
    (print "-- end match --")
    res))

    (match (z expr "gg * 2 + 1 + ( 211 )") ((|?e| expr))
           ((z1 expr "gg * 2 + ?e")
             (print "")
             (print "match on ?e")
             (print |?e|)
             (print "")
             t)
           (otherwise (print "no match")))
;; TODO
;;           (defun ,parse-stream-func (,stream)
;;             (yacc:parse-with-lexer (stream-lexer #'read-line #',lexer-name (lambda () nil) (lambda () nil) :stream ,stream)
