;;;; -*- Mode: Lisp -*-

(in-package "CLAST")

(eval-when (:load-toplevel :compile-toplevel)
  (shadow '(capi:item capi:element))
  (use-package "CAPI")
  )


(define-interface clast-graph ()
  ((ast :accessor ast
        :initarg :ast)
   )
  (:panes
   (tree-pane graph-pane
              ; :roots (with-slots (ast) interface ast)
              :roots ast
              :children-function 'clast-element-subforms
              :edge-pinboard-class 'right-angle-line-pinboard-object
              :node-pane-function 'make-pane-for-node
              :layout-function :left-right
              :layout-x-adjust '(:left 10)
              ;; :layout-y-adjust '(:top 10)
              )
   )
  (:layouts
   (main simple-layout '(tree-pane))
   )
  (:default-initargs
   :title "CLAST Form Graph"
   :best-width 600
   :best-height 400)
  )


(define-interface clast-tree ()
  ((ast :accessor ast
        :initarg :ast)
   )
  (:panes
   (tree-pane tree-view
              ; :roots (with-slots (ast) interface ast)
              :roots ast
              :children-function 'clast-element-subforms
              :print-function (lambda (ce)
                                (typecase ce
                                  (form (as-string ce))
                                  (t_lambda-list (string-downcase (type-of ce)))
                                  (null "NIL")
                                  (list "list-of")
                                  (keyword (format nil "keyword :~A" ce))
                                  (symbol (format nil "symbol ~A" ce))
                                  (t (format nil "unhandled ~A" ce))))
              ; :print-function (lambda (ce) (type-of ce))
              ;; :edge-pinboard-class 'right-angle-line-pinboard-object
              ;; :expandp-function 'make-tree-node-for-element
              ;; :layout-function :left-right
              ;; :layout-x-adjust '(:left 10)
              ;; :layout-y-adjust '(:top 10)
              )
   )
  (:layouts
   (main simple-layout '(tree-pane))
   )
  (:default-initargs
   :title "CLAST Form Tree"
   :best-width 400
   :best-height 400)
  )


(defun make-pane-for-node (graph-pane clast-element)
  (declare (ignore graph-pane))
  (make-instance 'item-pinboard-object
                 :data (typecase clast-element
                         (form (as-string clast-element))
                         (t_lambda-list (type-of clast-element))
                         (null "NIL")
                         (list "list-of")
                         (keyword (format nil "keyword :~A" clast-element))
                         (symbol (format nil "symbol ~A" clast-element))
                         (t  (format nil "unhandled ~A" clast-element)))))


(defun display-ast-graph (&optional ast)
  (display (make-instance 'clast-graph :ast (list ast))))


(defun display-ast (&optional ast)
  (values ast
          (display (make-instance 'clast-tree :ast (list ast)))))


;;;; end of file - clast-capi-tree.lisp --
