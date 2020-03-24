(defun m-sum (m)
  "method summary"
  (let ((name (jmethod-name m))
        (ret (jclass-name (jmethod-return-type m)))
        (params (mapcar #'jclass-name
                        (map 'list #'identity
                             (jmethod-params m)))))
    (list name ret params)))
        

(defun c-sum (cname)
  "class summary -- given a class name (or java-object) print its methods with their sigs"
  (let* ((c (if (typep cname 'java:java-object)
                cname
                (jclass cname)))
         (methods (map 'list 
                       #'identity
                       (jclass-methods c)))
         (summarized-methods (mapcar #'m-sum
                                     methods)))
    (mapcar #'(lambda (method-summary)
                (format t "~S ~S~%  ~S~%~%" 
                        (car method-summary) ;name
                        (cadr method-summary) ;return
                        (caddr method-summary) ; args
                        ))
            summarized-methods)
    nil))

; e.g (c-sum "org.apache.jena.rdf.model.ModelFactory")

