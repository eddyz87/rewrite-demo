(in-package :rw)
;; TO DO:
;; unit tests ?

;;(defmacro \ ((&rest args) &body body) (lambda ,args ,@body))
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
  
  ;;(defmacro define-string-lexer (&rest args) `'(define-string-lexer ,@args))
  ;;(defmacro define-parser (&rest args) `'(define-parser ,@args))
  
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
          (print term stream))))
)
;;(print (extract-options '(1 2) '(:f 2) ':g))
;;(print (extract-options '(1 2 :f 3 4 :g 5) '(:f 2) ':g))


(defmacro defg (name &rest rules)
  "rules:
    (rule-name [:subtype rule-name] alt*) *
   alt:
    (alt-el*)
   alt-el:
    rule-name | string"
  (assert (listp rules)) ;; TO DO add error messages
  (let ((terminals (make-hash-table :test #'equal)) ;; string -> symbol
        (printer-inits nil)
        (print-table (gensym (mk-symbol-name name "-print-table")))
        (print-func (intern (mk-symbol-name name "-print"))))
    (labels ((%terminal (str &optional (quote t))
               (let ((str (if quote
                            (cl-ppcre:quote-meta-chars str)
                            str)))
                 (or (gethash str terminals)
                     (setf (gethash str terminals) (gensym str)))))
             (%parse-rule (rule)
               "returns rule in the cl-yacc form:
                  (rule-name alt*)"
               (assert (listp rule)) ;; TO DO add error messages
               (assert (symbolp (first rule)))
               (let ((name (car rule)))
                 (multiple-value-bind (alts1 value subtype)
                                      (extract-options (cdr rule) ':value '(:subtype 1))
;;                    (format t "alts1=~A value=~A~%" alts1 terminal)
                    (let ((alts (mapcar (lambda (a) (%parse-alt (symbol-name name) a))
                                        (if value
                                          (progn
                                            (assert (stringp (car alts1)))
                                            (list (list (%terminal (car alts1) nil))))
                                          alts1))))
                      `(,name ,@alts)))))
             (%parse-alt (base-name alt)
               "returns rule alternative in the cl-yacc form:
                  (rule-name+ (lambda (x1 x2 x3 ...) (list 'constructor-name y1 y2 y3 ...)))
                where Ys are Xs that are not terminals"
               (let* ((constructor (gensym base-name)) ;; TO DO put it into grammar package
                      ;; list of (body-el x y)
                      (body (mapcar (lambda (el)
                                      (cond
                                        ((symbolp el)
                                           (let ((x (gensym "x")))
                                             (list el x x)))
                                        ((stringp el)
                                           (list (%terminal el) (gensym "_") nil))
                                        (t (error "Alternative element has to be symbol or string ~
                                                   but ~A of type ~A found, while parsing rule ~A"
                                                  el (type-of el) base-name))))
                                    alt))
                      (stream (gensym "stream"))
                      (term (gensym "term"))
                      (number 0)
                      (printer-statements (mapcar (lambda (non-terminal notation)
                                                    (if non-terminal
                                                      (prog1
                                                        `(,print-func ,stream (nth ,number ,term))
                                                        (incf number))
                                                      `(write-string ,notation ,stream)))
                                                  (mapcar #'third body)
                                                  alt))
                      (names (mapcar #'first body))
                      (xs (mapcar #'second body))
                      (ys (remove nil (mapcar #'third body))))
                 (push `(setf (gethash ',constructor ,print-table)
                              (lambda (,stream ,term)
                                (progn ,@printer-statements)))
                       printer-inits)
                 `(,@names (lambda ,xs (list ',constructor ,@ys))))))
      (let* ((rules (mapcar #'%parse-rule rules))
             (start (caar rules))
             (parser-name (gensym (mk-symbol-name name "-parser")))
             (lexer-name (gensym (mk-symbol-name name "-lexer")))
             (parse-string-func (intern (mk-symbol-name name "-parse-string")))
             (parse-stream-func (intern (mk-symbol-name name "-parse-stream")))
             (lexer-rules nil)
             (term (gensym "term"))
             (str (gensym "str"))
             (stream (gensym "stream"))
             (stream1 (gensym "stream")))
        (maphash (lambda (regex symb)
                   (push `(,regex (return (values ',symb $@)))
                         lexer-rules))
                 terminals)
        `(progn
           (cl-lex:define-string-lexer ,lexer-name
                                       ,@lexer-rules)
           (yacc:define-parser ,parser-name
                               (:start-symbol ,start)
                               (:terminals ,(hash-values terminals))
                               ,@rules)
           (defun ,parse-string-func (,str)
             (yacc:parse-with-lexer (,lexer-name ,str) ,parser-name))
           (defun ,parse-stream-func (,stream)
             (yacc:parse-with-lexer (stream-lexer #'read-line #',lexer-name (lambda () nil) (lambda () nil) :stream ,stream)
                                    ,parser-name))
           (defvar ,print-table (make-hash-table :test #'eq))
           (progn ,@printer-inits)
           (defun ,print-func (,stream1 ,term)
             (print-term ,stream1 ,print-table ,term)))))))


(defmacro print-and-expand (what)
  `(progn
     (pprint (macroexpand (quote ,what)))
     ,what))

(print-and-expand
      (defg zulu
            (expr (expr1)
                  (expr1 "+" expr)
                  (expr1 "-" expr))

            (expr1 :subtype expr
                   (expr2)
                   (expr2 "*" expr1)
                   (expr2 "/" expr1))

            (expr2 :subtype expr
                   ("(" expr ")")
                   (id)
                   (int))

            (id :value "[_a-z][_a-zA-Z0-9]*")
            (int :value "[0-9]+")))

;;(print (zulu-parse-string "22 * 1 + 33 * 2"))
(let ((e (zulu-parse-string "gg * 2 + 1 + ( 211 )")))
  (pprint e)
  (zulu-print *standard-output* e))
