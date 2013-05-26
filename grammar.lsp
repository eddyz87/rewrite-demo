;; TO DO:
;; unit tests ?

;;(defmacro \ ((&rest args) &body body) (lambda ,args ,@body))

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
  (let ((terminals (make-hash-table :test #'equal))) ;; string -> symbol
    (labels ((%terminal (str)
                (or (gethash str terminals)
                    (setf (gethash str terminals) (gensym str))))
             (%parse-rule (rule)
               "returns rule in the cl-yacc form:
                  (rule-name alt*)"
               (assert (listp rule)) ;; TO DO add error messages
               (assert (symbolp (first rule)))
               (let ((name (car rule)))
                 (multiple-value-bind (alts1 terminal subtype)
                                      (extract-options (cdr rule) ':terminal '(:subtype 1))
;;                    (format t "alts1=~A terminal=~A~%" alts1 terminal)
                    (let ((alts (mapcar (lambda (a) (%parse-alt (symbol-name name) a))
                                        (if terminal
                                          (progn
                                            (assert (stringp (car alts1)))
                                            (list (list (%terminal (car alts1)))))
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
                      (names (mapcar #'first body))
                      (xs (mapcar #'second body))
                      (ys (remove nil (mapcar #'third body))))
               `(,@names (lambda ,xs (list ',constructor ,@ys))))))
      (let* ((rules (mapcar #'%parse-rule rules))
             (start (caar rules))
             (parser-name (gensym (mk-symbol-name name "-parser")))
             (lexer-name (gensym (mk-symbol-name name "-lexer")))
             (parse-string-func-name (intern (mk-symbol-name name "-parse-string")))
             (parse-stream-func-name (intern (mk-symbol-name name "-parse-stream")))
             (lexer-rules nil)
             (str (gensym "str"))
             (stream (gensym "stream")))
        (maphash (lambda (regex symb)
                   (push `(,regex (return (values ,symb $@)))
                         lexer-rules))
                 terminals)
        `(progn
           (define-string-lexer ,lexer-name
                                ,@lexer-rules)
           (define-parser ,parser-name
                          (:start-symbol ,start)
                          (:terminals ,(hash-values terminals))
                          ,@rules)
           (defun ,parse-string-func-name (,str)
             (parse-with-lexer (,lexer-name ,str) ,parser-name))
           (defun ,parse-stream-func-name (,stream)
             (parse-with-lexer (stream-lexer #'read-line #',lexer-name (lambda () nil) (lambda () nil) :stream ,stream)
                               ,parser-name))
             )))))


(pprint
  (macroexpand
    (quote
      (defg zulu
            (expr (expr1 "+" expr1)
                  (expr1 "-" expr1))

            (expr1 :subtype expr
                   (expr2 "*" expr2)
                   (expr2 "/" expr2))

            (expr2 :subtype expr
                   ("(" expr ")")
                   (id)
                   (int))

            (id :terminal "[_a-z][_a-zA-Z0-9]*")
            (int :terminal "[0-9]+")))))

