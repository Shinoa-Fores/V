(in-package "CL-TOPOSORT")

(defclass directed-edge ()
  ((head :initarg :head :reader head)
   (tail :initarg :tail :reader tail))
  (:documentation "A directed edge of a directed graph."))

(defclass directed-graph ()
  ((vertices :initarg :vertices :reader vertices)
   (edges :initarg :edges :reader edges))
  (:documentation "A directed graph, consisting of vertices
and directed edges."))

(defun make-directed-graph (vertices edge-alist)
  (make-instance 'directed-graph
                 :vertices vertices
                 :edges (mapcar #'(lambda (edge-cons)
                                    (make-instance 'directed-edge
                                                   :head (car edge-cons)
                                                   :tail (cdr edge-cons)))
                                edge-alist)))

(defun rootp (vertex edges)
  "Determines if VERTEX is a root in the list of edges EDGES.

Returns t if VERTEX is not the tail of any edge in EDGES, and NIL otherwise."
  (notany #'(lambda (e) (eq (tail e) vertex))
          edges))

(defun leafp (vertex edges)
  "Determines if VERTEX is a leaf in the list of edges EDGES.

Returns t if VERTEX is not the head of any edge in EDGES, nil otherwise."
  (notany #'(lambda (e) (eq (head e) vertex))
          edges))

(defun partition (proposition list)
  "Given a unary function PROPOSITION and a list LIST, separates
those which satisfy PROPOSITION from those which do not.

Returns two values: the first contains exactly the elements of LIST
for which PROPOSITION does not return NIL; the second contains exactly those
elements of LIST for which PROPOSITION returns NIL."
  (let ((successes '())
        (failures '()))
    (mapcar #'(lambda (x) (if (funcall proposition x)
                              (push x successes)
                              (push x failures)))
            list)
    (values successes failures)))

(defmethod toposort ((dg directed-graph))
  (labels ((separate-roots (vertices edges)
             (partition #'(lambda (v) (rootp v edges))
                        vertices))
           (decapitate (vertices edges)
             "Removes all edges with head in vertices"
             (remove-if #'(lambda (edge) (member (head edge) vertices))
                        edges))
           (flatten-iter (remaining-vertices remaining-edges sorted-vertices)
             (if (null remaining-vertices)
                 sorted-vertices
                 (multiple-value-bind (roots non-roots)
                     (separate-roots remaining-vertices remaining-edges)
                   (if (null roots)
                       'cyclic
                       (flatten-iter non-roots
                                     (decapitate roots remaining-edges)
                                     (append sorted-vertices roots)))))))
    (flatten-iter (vertices dg) (edges dg) '())))
