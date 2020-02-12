
(in-package "USER")

; ********************************************************************
; *** functions for navigating in the ontology
; ********************************************************************

; ********************************************************************
; *** returns true if the argument is a basic ontological link (as range,
;     domain-of, instance, ...)
(defun is-a-structural-item (item)
   (member item
      '(range range-of domain domain-of has-subclass subclass-of restricts
        restricted-by instance relinstance argument arg-of value value-of)))

; ********************************************************************
; *** basic, untyped, movement
(defun get-link-val (link elem)
   (get elem link))

; ********************************************************************
; *** this returns the local class of an instance
(defun get-instance-class (instance-id)
   (cond ((null instance-id) nil)
         ((not (is-subclass-of instance-id '££entity)) ; *** this is a class, not an instance
            (get-link-val 'instance instance-id))))

; ********************************************************************
; *** this returns the local relation of a relation instance
(defun get-relinstance-rel (instance-id)
   (cond ((null instance-id) nil)
         ((not (is-subclass-of instance-id '££entity)) ; *** this is a class, not a relation
            (get-link-val 'relinstance instance-id))))

; ********************************************************************
; *** membership at more levels
; *** instance-id (e.g. £northern) can have more than one direct class
;     (e.g. ££cardinal-direction and ££it-area-spec)
; *** the function returns all direct classes that are subclasses of "class"
(defun is-instance-of (instance-id class)
  (let ((iclass (get-instance-class instance-id)) found)
       (do ((nxtdirectclass (first iclass) (first iclass))
            (iclass (rest iclass) (rest iclass)))
           ((null nxtdirectclass) found)
           (cond ((is-subclass-of nxtdirectclass class)
                    (setq found (cons nxtdirectclass found)))))))

; ********************************************************************
; *** relation instances
; *** relinstance-id (e.g. &moved-sea-intens-1)
;     relation (e.g. &intensification-arg)
; *** the function returns t if relinstance is an instance of relation
(defun is-relinstance-of (relinstance-id relation)
  (let ((iclass (get-relinstance-rel relinstance-id)) found)
       (do ((nxtdirectclass (first iclass) (first iclass))
            (iclass (rest iclass) (rest iclass)))
           ((null nxtdirectclass) found)
           (cond ((is-restriction-of nxtdirectclass relation)
                    (setq found (cons nxtdirectclass found)))))))

; ********************************************************************
; *** membership at more levels (unprotected)
;     As above, but this works also if the instance is unknown (returning nil)
(defun is-instance-or-subclass-of (ident class)
  (let ((down-class (inlist (cond ((is-an-instance ident)
                                     (get-instance-class ident))
                                  (t ident))))
  ; *** down-class (i.e. the direct class of the ident) can be a list, so that
  ;     I force lists in all cases
        found)
      (do ((nxtdown (first down-class) (first down-class))
           (down-class (rest down-class) (rest down-class)))
          ((or (null nxtdown) found) found)
          (cond ((is-subclass-of nxtdown class)
                   (setq found nxtdown))))))

; ********************************************************************
; ********************************************************************
; *** true if ident is an instance
(defun is-an-instance (ident)
  (not (null (get-link-val 'instance ident))))

; ********************************************************************
; *** true if ident is a class
(defun is-a-class (ident)
  (or (not (null (get-link-val 'subclass-of ident)))
      (eq ident '££entity)))

; ********************************************************************
; *** subsumption
(defun is-subclass-of (class up-class)
  (cond ((or (null class) (null up-class))
          (exception 'semantic-error 
                     "PROC/onto-reasoning: Attempt to use a NIL ontology class"
                      class up-class))
        ((eq class up-class) t)
        (t (int-is-subclass-of (get class 'subclass-of) up-class))))

(defun int-is-subclass-of (next-up-classes up-class)
  (cond ((null next-up-classes) nil)
        ((member up-class next-up-classes) t)
        (t (int-is-subclass-of
               (merge-flatten (mapcar #'(lambda (x) (get x 'subclass-of))
                                     next-up-classes))
               up-class))))

; ********************************************************************
; *** checks if any of the elements in "classes" (which could also be an atom)
;     is a subclass of up-class
(defun one-is-subclass-of (classes up-class)
  (let ((cl-list (inlist classes)) found)
      (do ((nxtcl (first cl-list) (first cl-list))
           (cl-list (rest cl-list) (rest cl-list)))
          ((or found (null nxtcl)) found)
          (setq found (is-subclass-of nxtcl up-class)))))

; ********************************************************************
; *** checks if "class" is a subclass of one of up-classes
(defun is-subclass-of-one-of (class up-classes)
  (cond ((null up-classes) nil)
        ((is-subclass-of class (first up-classes)) t)
        (t (is-subclass-of-one-of class (rest up-classes)))))

; ********************************************************************
; *** a relation must have at least a domain or a range
(defun is-relation (ident)
  (or (not (null (get ident 'domain)))
      (not (null (get ident 'range)))))

; ********************************************************************
; *** relation restrictions
(defun is-restriction-of (relation up-relation)
  (cond ((or (null relation) (null up-relation))
          (exception 'semantic-error 
                  "PROC/onto-reasoning: Attempt to use a NIL ontology relation" 
                     relation up-relation))
        ((eq relation up-relation) t)
        (t (int-is-restriction-of (get relation 'restricts) up-relation))))

(defun int-is-restriction-of (next-up-relations up-relation)
  (cond ((null next-up-relations) nil)
        ((member up-relation next-up-relations) t)
        (t (int-is-restriction-of
               (merge-flatten (mapcar #'(lambda (x) (get x 'restricts))
                                     next-up-relations))
               up-relation))))

; ********************************************************************
; *** some operations must be blocked for basic datatypes.
;     here, I assume that a basic datatype is a direct subclass of ££datatype
(defun is-basic-datatype (class)
   (member '££datatype (get class 'subclass-of)))

; ********************************************************************
(defun class-inherit ()
  (int-cl-inh '££entity))

(defun int-cl-inh (class)
  (let ((subclasses (get class 'has-subclass)))
        ()))

; ********************************************************************
; *** this finds the shortest path between ontology nodes
;     Both node1 and node2 can be sets of nodes, so that is actually
;     carried out is a parallel search 
; *** In case "constraint" is non-null, it is a list of nodes through
;     which the found path must pass
; *** the top-level function just invokes the "cached" one and
;     saves the result in a file
(defun find-shortest-path (node1 node2 &optional constraint)
  (declare (special *ONTO-TIME-START*))
  (let ((cache (find-in-cache node1 node2 constraint)))
      (setq *ONTO-TIME-START* (get-internal-run-time))
      (cond ((null cache)
              (setq cache
                (find-shortest-path-new node1 node2 constraint))
              (add-in-cache node1 node2 constraint cache)))
      cache))

; ********************************************************************
; *** an entry in the cache includes:
;     1. The first node (actually a list)
;     2. The second node (actually a list)
;     3. The constraint, i.e. a node through which the found path must
;        pass (actualy a list, possibly nil in case of absence of constraints)
;     4. The found shortest path
;     5. The number of accesses (for statistical purposes)
(defun find-in-cache (node1 node2 constraint)
  (declare (special *ONTOLOGY-CACHE*))
  (let (found)
  (do ((nxtitem (first *ONTOLOGY-CACHE*) (first tempcache))
       (tempcache (rest *ONTOLOGY-CACHE*) (rest tempcache))
       (newcache nil (cons nxtitem newcache)))
      ((or (null nxtitem) found)
   ; *** the next to update the statistics
         (cond (found (setq *ONTOLOGY-CACHE*
                        (append (reverse (rest newcache))
                                (list (list (first found) (second found) 
                                            (third found) (fourth found) 
                                            (1+ (fifth found))))
                                (cond ((null nxtitem) nil)
                                      (t (list nxtitem)))
                                tempcache))))
         (fourth found))
      (cond ((and (equal (first nxtitem) node1)
                  (equal (second nxtitem) node2)
                  (equal (third nxtitem) constraint))
               (setq found nxtitem))))))
     
; ********************************************************************
; *** this adds a new item in the cache of ontology paths
(defun add-in-cache (node1 node2 constraint result)
  (declare (special *ONTOLOGY-CACHE*))
  (setq *ONTOLOGY-CACHE*  
     (cons (list node1 node2 constraint result 1)
            *ONTOLOGY-CACHE*)))
  
; ********************************************************************
(defun find-shortest-path-new (node1 node2 &optional constraint check)
  (cond ((or (null node1) (null node2))
          (exception 'semantic-error
                "PROC/onto-reasoning: Looking for a path to a nil node" node1 node2)))
  (let ((lnode1 (inlist node1))
        (lnode2 (inlist node2))
        (lconstr (inlist constraint)))
  ; *** inlist puts atoms in a singleton lists, and leaves lists
  ;     unchanged
      (cond ((or (member nil (mapcar #'spl lnode1))
                 (member nil (mapcar #'spl lnode2)))
               (exception 'semantic-error 
                          "PROC/onto-reasoning: No property for an ontology concept" 
                             lnode1 lnode2))
            ((null constraint)
               (int-find-sh-path (mapcar #'list lnode1) lnode2 nil check))
            (t 
               ;(int-2-find-sh-path lnode1 lnode2
               ;           (mapcar #'list lconstr) '(0 nil))
               (int-2-find-sh-path lnode1 lnode2
                           (mapcar #'list lconstr) check nil nil 1000 1000)
                ))))

(defun fsp (a b &optional c check) 
  (declare (special *ONTO-TIME-START*))
   (setq *ONTO-TIME-START* (get-internal-run-time))
   (let ((result (find-shortest-path-new a b c check)))
      (format t "Result : ~s~% Time: ~s~%" result 
                 (- (get-internal-run-time) *ONTO-TIME-START*))
      result))

; ********************************************************************
(defun int-find-sh-path (prevpaths node2 backup-solution &optional check)
; *** prevpaths is a list of paths, each ending in the initial node
;     (node1 of find-shortest-path).
;     Each path is a sequence <nodeN linkN node[N-1] link[N-1] ... node1>
;     N.B in the implementation, the paths are kept reversed
; *** node2 is the node to be reached
; *** backup-solution is a not appealing solution already found,
;     which could be used in absence of better alternatives
  (declare (special *ONTO-TIME-START* *SYSTEM-CONTEXT*))
  ;(format t "Entering int-find-sh-path; ~% prevpaths ~a;~%  node2 ~a;~%  backup-solution ~a ~%"
  ;          prevpaths node2 backup-solution)
  ;(break "")
  (cond ((and (not (null *ONTO-TIME-START*))
              (> (- (get-internal-run-time) *ONTO-TIME-START*) 20000))
           (exception 'semantic-error 
                 "PROC/onto-reasoning: Find shortest path timed out")))
  (cond ((null prevpaths) 
; *** end of search: failure or semi-failure
          (cond ((null backup-solution) 
                  (exception 'semantic-error 
                             "PROC/onto-reasoning: No path found in find-shortest-path"))
                (t backup-solution))))
  (let (nxtsteps result (numbinst 0) prevcl)
   ; *** repeat the job for all open paths
      (do ((prevp (first prevpaths) (first prevpaths))
           (prevpaths (rest prevpaths) (rest prevpaths)))
          ((null prevp))
         (cond (check
                  (cond ((and (is-an-instance (first prevp))
                              (not (is-a-class (first prevp))))
                     ; *** the next to control the number of printed instances in 
                     ;     case of controlled execution
                           (cond ((eq (third prevp) prevcl)
                                    (setq numbinst (1+ numbinst))
                                    (cond ((= numbinst 10)
                                            (format t "    ...~%"))
                                          ((< numbinst 10)
                                            (format t "Cammino: ~a~%" prevp))))
                                 (t (setq numbinst 1)
                                    (setq prevcl (third prevp))
                                    (format t "Cammino: ~a~%" prevp))))
                        (t (setq numbinst 0)
                           (setq prevcl nil)
                           (format t "Cammino: ~a~%" prevp)))))
         (cond ((memq (first prevp) node2)
      ; *** along prevp, we have reached node2, return it as a
      ;     result
                 (cond ((or (are-there-inverses prevp)
                            (member '&has-description prevp)
				    (and (not (member (ult (first prevpaths))
                                              '(££information &related-to)))
                                 (or (member 'restricts prevp)
                                     (member 'restricted-by prevp))))
      ; *** the solution found includes two inverse steps or traverses
      ;     the &has-description relation, save it as a 
      ;     backup solution, since it is not a good one
      ; *** I'm trying to block the direct use of &has-description,
      ;     since it is too general and prevents the use of more
      ;     suitable descriptions. In fact, going up to ££entity
      ;     (which costs nothing) then using &has-description and
      ;     going down from ££datatype (which again costs nothing)
      ;     one always finds a low-cost path for any description
      ; *** also, restrictions are avoided, unless the start node is ££information,
      ;     since it forces the passage through more specific "have information"
      ;     relation
                         (cond ((null backup-solution)
                                 (setq backup-solution 
                                      (list (reverse prevp))))))
                       (t (setq result 
                               (append1 result (reverse prevp))))))
               ((and (member '££entity prevp)
                     (not (= 1 (length prevp))))
      ; *** stop paths passing from ££entity: too general!!! (unless prevp has
      ;     length 1, so that ££entity is the starting point)
                  nil)
              ; ((memq '££datatype (get (first prevp) 'subclass-of))
              ;    nil)
      ; *** stop paths passing from instances, since instances of relations are
      ;     not stored in the ontology
               ((and (neq *SYSTEM-CONTEXT* 'atlas)	; In ATLAS, relation instances are in the KB
                     (is-an-instance (first prevp))
                     (not (is-a-class (first prevp)))
                     (> (length prevp) 1))
                  nil)
      ; *** stop paths including both a relation restriction and an inverse
      ;     relation restriction
               ((check-rel-restr prevp)
                  nil)
      ; *** advance on prevp on all links
               (t (let ((all-links (cond ((symbolp (first prevp))
                                           (spl (first prevp)))
          ; *** if one arrives at a datum (i.e. a string), then stop
                                         (t nil))))
                     (do ((nxtlink (first all-links) (first all-links))
                          (nxtvalue (second all-links) (second all-links))
                          (all-links (rest (rest all-links))
                                     (rest (rest all-links))))
                         ((null nxtlink))
          ; *** special treatment for subclasses: the link is followed,
          ;     It is horrible to change the loop range (prevpaths) during the
          ;     operations, but here it's the simplest solution
          ; *** in practice, this means that the class is replaced by
          ;     its superclass(es), and that this movement is not
          ;     taken into account (inheritance)
          ; *** actually, the movement is recorded (in order to avoid up-down
          ;     movements, but it does not count as a step)
                        (cond ((and (eq nxtlink 'subclass-of)
                                    (neq (second prevp) 'has-subclass))
                ; *** ancestors are used immediately (in this step)
                                 (dolist (singval nxtvalue)
                ; *** the dolist for multiple parents
                                       (cond ((not (member singval prevp))
                                               (setq prevpaths 
                                                 (append1 prevpaths
                                                   (cons singval
                                                      (cons nxtlink
                                                            prevp))))))))
                              ((or (eq nxtlink 'funct)
                                ;   (and (eq nxtlink 'subclass-of)
                                ;        (eq (second prevp) 'has-subclass))
                                ;   (and (eq nxtlink 'has-subclass)
                                ;        (eq (second prevp) 'subclass-of))
                                   (and (eq nxtlink 'domain)
                                        (eq (second prevp) 'domain-of))
                                   (and (eq nxtlink 'range)
                                        (eq (second prevp) 'range-of)))
      ; *** the movement is blocked for:
      ;     1. funct (functionality information is not a link)
      ;     2. has-subclass, followed by subclass-of (siblings) !!!!!!!!! REMOVED FOR ATLAS
      ;     3. subclass-of, followed by has-subclass (co-parents) !!!!!!!!! REMOVED FOR ATLAS
      ;     4. domain followed by domain-of (domain-of is a function)
      ;     5. range followed by range-of (range-of is a function)
                                 nil)
      ; *** in all other cases, standard movement
                              (t (cond ((or (and (eq nxtlink 'subclass-of)
                                                 (eq (second prevp) 'has-subclass))
                                            (and (eq nxtlink 'has-subclass)
                                                 (eq (second prevp) 'subclass-of)))
                          ; this added in order to accept movements
                          ; as <££positive-eval-entity ££positive-evening ££evening>
                                          (setq nxtvalue (subtrl nxtvalue (list (third prevp))))))
                                (setq nxtvalue (merge-union nxtvalue))
      ; *** repeat for all single values
                                (dolist (singval nxtvalue)
      ; *** test for loops (but links - as 'domain' - may be repeated)
                                     (cond ((member singval prevp) nil)
         ; *** add an extended path
         ; !!!! here we must block the expansion of the same ancestor
         ;      reached via different paths
         ;      e.g. ££time-spec subclass-of ££entity vs.
         ;           ££time-spec subclass-of ££dialogue-topic 
         ;                       subclass-of ££entity
                                           (t (setq nxtsteps
                                                 (cons 
                                                   (cons singval 
                                                       (cons nxtlink prevp))
                                                   nxtsteps))))))))))))
 ; *** end of loop on all previous paths: if node2 has been reached, then
 ;     return the path, otherwise proceed on the ontology
      (cond ((null result)
              (let ((nxtsteps (prune-rels nxtsteps)))
                  (cond ((null nxtsteps) 
                           (cond ((null backup-solution) nil)
                                 (t backup-solution)))
                        (t (int-find-sh-path nxtsteps node2 backup-solution check)))))
            (t result))))

;; ********************************************************************
;; ** this aims at pruning some irrelevant paths. Something has already
;     been done on-line.
; *** Currently, this only aims at avoiding the use of too general
;     relations. For instance, in searching for the connection between
;     ££day and ££day-descr, all ancestors of ££day-descr (as ££time-descr
;     and ££entity) are reached. But, since there is the &day-description
;     relation, it is useless to expand the paths involving the more
;     general &time-description and &has-description
(defun prune-rels (paths)
  (let (rels useless-rels rempaths)
    (dolist (path paths)
       (cond ((and (is-an-ont-relation (first path))
                   (not (member (first path) rels)))
                (setq rels (cons (first path) rels)))))
   ; *** now in rels we have all the relations reached in the last step
   ;     check if any of them is a restriction of another one
    (dolist (rel rels)
       (cond ((and (more-general-rel '&has-description rel)
                   (member rel rels :test #'more-general-rel))
               (setq useless-rels (cons rel useless-rels)))))
   ; *** now in useless-rels we have all the too general relations
   ;    prune the paths
    (dolist (path paths rempaths)
       (cond ((not (member (first path) useless-rels))
               (setq rempaths (cons path rempaths)))))))
    
;; ********************************************************************
;  *** an ontology relation is something that has a domain
(defun is-an-ont-relation (node)
   (not (null (get node 'domain))))

;; ********************************************************************
;  *** an ontology relation is more general than another relation
(defun more-general-rel (relup reldown)
  (cond ((eq relup reldown) nil)
        (t (m-gen-r relup (get reldown 'restricts)))))

(defun m-gen-r (relup rellist)
  (cond ((null rellist) nil)
        ((member relup rellist) t)
        (t (m-gen-r relup (flatten (mapcar #'get-more-gen-rels rellist))))))

(defun get-more-gen-rels (rel)
  (get rel 'restricts))

; ********************************************************************
; *** this finds the shortest path from node1 to node2, passing from
;     "prevpaths", which, initially, is a list of nodes (the ones through
;     which the solution path is constrained to pass).
; *** It starts from each node in prevpaths (nodeX) and then moves one step.
;     When node1 or node2 are reached, there are three cases:
;     1. the found path is of the form <node1 node1a ... nodeX>
;        and there is, among the previously found paths (partial-sol)
;        one of the form <node2 node2a ... nodeX>. This means that a
;        complete solution has been found. However, this could not be
;        the best (shortest) one: it could be that <node1 node1a ... nodeX>
;        is of length N, <node2 node2a ... nodeX> is of length M, but a
;        shortest solution exists passing through the constraint nodeY. 
;        This cannot happen just in case B+K > N+M, where N and M are
;        as above, B is the currently best partial path, and K is the
;        current distance from the constraint nodes. So:
;        1a. If B+K >= N+M, close the search
;        1b. If B+K < N+M, proceed in the search, saving in complete-sol
;            the solution found
;     2. Neither node1 nor node2 have been reached:
;        Proceed in the search
; >>> INPUT:
;     node1: one of the nodes to connect
;     node2: the other node to connect
;     prevpaths: all the paths along which we are moving
;     partial-sol: a set of pairs, each of which includes a partial solution 
;            (half-path) currently found and its length
;     complete-sol: the complete solutions currently found
;     best-l: a pair including the length of the shortest path to node1
;             and the length of the shortest path to node2
(defun int-2-find-sh-path (node1 node2 prevpaths check 
                              partial-sol complete-sol best-1-l best-2-l)
; *** prevpaths is a list of paths, each starting from a central node
;     (constraint of find-shortest-path).
;     Each path is a sequence <nodeK linkK-1 nodeK-1 linkK-2 ... sodeX>,-ths wher
;     nodeX is one of the constraints
;     (N.B in the implementation, the paths are kept reversed)
  (declare (special *ONTO-TIME-START*))
  (cond ((> (- (get-internal-run-time) *ONTO-TIME-START*) 5000)
           (exception 'semantic-error 
                 "PROC/onto-reasoning: Find-shortest-path-2 timed out")))
  (cond ((null prevpaths) 
; *** end of search: failure
           (exception 'semantic-error 
                      "PROC/onto-reasoning: no path found in find-shortest-path-2")))
  (cond ((not (null check))
            (break "int-2-find-sh-path: Next Level")))
  (let (nxtsteps
        (new-best-1-l best-1-l)
        (new-best-2-l best-2-l)
        (new-complete-sol complete-sol)
        (new-partial-sol partial-sol)
        (best-prevp-l 1000))
; ****************** FIRST SECTION ***********************************
; *** check if any path in prevpaths satisfies the search conditions
; ********************************************************************
   ; *** repeat the job for all open paths
      (do* ((prevp (first prevpaths) (first prevpaths))
            (prevpaths (rest prevpaths) (rest prevpaths))
            (prevp-l (get-path-length prevp) (get-path-length prevp)))
      ; *** the search stops either when there are no more paths to follow
      ;     or when a result has been found
          ((null prevp))
         (cond ((not (null check))
                  (format t "cammini: ~a~%" prevp)
                  ))
         (cond ((< prevp-l best-prevp-l)
                 (setq best-prevp-l prevp-l)))
  ; *** node1 has been reached ******************************************
         (cond ((and (member-or-subcl prevp node1)
                     (is-a-good-sol prevp))
                 (let ((prev-partial
                         (find-partial-sol new-partial-sol node2 (ult prevp))))
      ; *** "ult prevp" is the constraint node from which this partial
      ;     solution has been found
                     (cond ((not (null prev-partial))
             ; *** save this complete solution
                             (let* ((prev-part-path (first prev-partial))
                                    (prev-part-l (second prev-partial))
                                    (new-sol-length (+ prev-part-l (1- prevp-l))))
                                 (setq new-complete-sol
                                    (append1 new-complete-sol
                                       (list (append (rev-links prevp)
                                                     (rest (reverse prev-part-path)))
                                             new-sol-length)))))
             ; *** this is not a complete solution
                           (t (setq new-partial-sol
                                   (append1 new-partial-sol (list prevp prevp-l)))))
                     (cond ((eq new-best-1-l 1000)
                               (setq new-best-1-l prevp-l))))))
  ; *** node2 has been reached ******************************************
         (cond ((and (member-or-subcl prevp node2)
                     (is-a-good-sol prevp))
                 (let ((prev-partial
                         (find-partial-sol new-partial-sol node1 (ult prevp))))
      ; *** "ult prevp" is the constraint node from which this partial
      ;     solution has been found
                     (cond ((not (null prev-partial))
             ; *** save this complete solution
                             (let* ((prev-part-path (first prev-partial))
                                    (prev-part-l (second prev-partial))
                                    (new-sol-length (+ prev-part-l (1- prevp-l))))
                                (setq new-complete-sol
                                   (append1 new-complete-sol
                                      (list (append (rev-links prev-part-path)
                                                    (rest (reverse prevp)))
                                             new-sol-length)))))
             ; *** this is not a complete solution; save it as partial
                           (t (setq new-partial-sol
                                   (append1 new-partial-sol (list prevp prevp-l)))))
                     (cond ((eq new-best-2-l 1000)
                                 (setq new-best-2-l prevp-l)))))))
; ****************** SECOND SECTION ***********************************
 ; *** now, we have possibly found a new complete solution; we must check if
 ;     it is the best one. Note hat even if in this step no new solution has
 ;     been found, the check is useful, since a previous complete solution
 ;     could have now become the best (after advancement in the ontology)
; *********************************************************************
         (let ((best-complete-sol (get-best-sol new-complete-sol)))
    ; *** best-complete-sol is the currently best complete solution;
    ;     N.B. get-best-sol returns <nil 3000> in case no complete solution
    ;          does currently exist
             (cond ((< (second best-complete-sol)
    ; *** new-best-1-l and new-best-2-l are the length of the shortest paths
    ;     leading to node1 and node2 (respectively) from any of the constraint
    ;     nodes
                       (+ (min new-best-1-l new-best-2-l) best-prevp-l))
                     (list (first best-complete-sol)))
    ; *** we have not found the (certainly) best solution, so advance on the
    ;     ontology
                  (t (do* ((prevp (first prevpaths) (first prevpaths))
                           (prevpaths (rest prevpaths) (rest prevpaths)))
      ; *** the advancement stops when there are no more paths to follow
                        ((null prevp))
                        (let ((all-links (cond ((symbolp (first prevp))
                                                  (spl (first prevp)))
          ; *** if one arrives at a datum (i.e. a string), then stop
                                               (t nil))))
                       (do ((nxtlink (first all-links) (first all-links))
                            (nxtvalue (second all-links) (second all-links))
                            (all-links (rest (rest all-links))
                                       (rest (rest all-links))))
                           ((null nxtlink))
          ; *** special treatment for subclasses: the link is followed,
          ;     It is horrible to change the loop range (prevpaths) during the
          ;     operations, but here it's the simplest solution
          ; *** in practice, this means that the class is replaced by
          ;     its superclass(es), and that this movement is not
          ;     taken into account (inheritance)
          ; *** actually, the movement is recorded (in order to avoid up-down
          ;     movements, but it does not count as a step)
                          (cond ((not (is-a-good-sol prevp)) nil)
             ;                   ((or (and (eq nxtlink 'subclass-of) 
             ;                             (eq (second prevp) 'has-subclass))
             ;                        (and (eq nxtlink 'has-subclass)
             ;                             (eq (second prevp) 'subclass-of)))
             ; *** immediately up-down or down-up links stop the search 
             ; ----- removed to allow for down-up in examples as:
             ; -----    ££positive-eval-entity ££positive-evening ££evening
             ;                      nil)
             ; *** otherwise, subclassing is recorded, but if one of the
             ;     end nodes is reached, the movement is stopped; in this
             ;     case the next branch is followed, which saves the path
             ;     until now
                                ((and (or (eq nxtlink 'subclass-of) 
                                          (eq nxtlink 'has-subclass))
                                      (not (intersection nxtvalue node1))
                                      (not (intersection nxtvalue node2)))
             ; *** the next cond removes from the down-up or up-down movements the cycles
             ;     for instance, in case we have 
             ;     (Z HAS-SUBCLASS (Y)) and (Y SUBCLASS-OF (X Z W))
             ;     we may have in prevp (Y HAS-SUBCLASS Z) [prevp is reversed],
             ;                 in nxtlink SUBCLASS-OF and in nxtvalue (X Z W)
             ;     The cond changes nxtvalue into (X W), so we cannot have in a prevp
             ;     (Z SUBCLASS-OF Y HAS-SUBCLASS Z)
                                  (cond ((or (and (eq nxtlink 'subclass-of) 
                                                  (eq (second prevp) 'has-subclass))
                                             (and (eq nxtlink 'has-subclass)
                                                  (eq (second prevp) 'subclass-of)))
                                          (setq nxtvalue 
                                               (subtrl nxtvalue (list (third prevp))))))
             ; *** the dolist for multiple parents
                                  (dolist (singval nxtvalue)
                                       (cond ((not (member singval prevp))
                                               (setq prevpaths 
                                                   (append1 prevpaths
                                                       (cons singval
                                                             (cons nxtlink prevp))))))))
      ; *** standard case: for all other links, except 'funct
                                ((neq nxtlink 'funct)
                                  (setq nxtvalue (merge-union nxtvalue))
      ; *** repeat for all single values
                                  (dolist (singval nxtvalue)
      ; *** test for loops (but links - as 'domain' - may be repeated)
                                       (cond ((member singval prevp) nil)
         ; *** add an extended path
                                      ((and (eq (first prevp) '&HAS-INFO-TOPIC) 
                                            (or (and (eq (second prevp) 'domain)
                                                     (eq nxtlink 'domain-of))
                                                (and (eq (second prevp) 'domain-of)
                                                     (eq nxtlink 'domain))))
                                         nil)
         ; *** the branch above aims at stopping paths linking two possible
         ;     topics of a dialogue, since everything can be such a topic, so
         ;     any two nodes could be connected in this way
                                      (t (setq nxtsteps
                                            (cons 
                                                  (cons singval 
                                                       (cons nxtlink prevp))
                                                  nxtsteps))))))))))
 ; *** end of loop on all previous paths: if result is non-null, then one of the
 ;     paths has been found: return it; otherwise make another step
                       (int-2-find-sh-path node1 node2 nxtsteps check 
                              new-partial-sol new-complete-sol
                              new-best-1-l new-best-2-l))))))

; ******************************************************************
(defun get-path-length (path)
; *** a path is assumed to start and end with a concept, relation, or
;     instance; 
  (cond ((null path) 0)
        ((eq 1 (length path)) 1)
        ((member (second path) 
            '(subclass-of has-subclass has-instance instance-of))
           (+ .1 (get-path-length (rest (rest path)))))
        (t (1+ (get-path-length (rest (rest path)))))))
 
; ******************************************************************
(defun get-best-sol (solutions)
; *** "solutions" is a list of pairs <path path-length>
  (int-get-best-s solutions '(nil 3000)))

(defun int-get-best-s (solutions best)
  (cond ((null solutions) best)
        ((< (second (first solutions)) (second best))
           (int-get-best-s (rest solutions) (first solutions)))
        (t (int-get-best-s (rest solutions) best))))
        
; ******************************************************************
; *** partial-sols is a list of partial solutions, each of which has the
;     form <path length>
;     where length is the conventional length of the path
;     (see get-path-length above)
; *** end-node is a list (as node1 and node2 in find-shortest-path),
;     while start-node is an atom (the constraint node actually found
;     in a path)
(defun find-partial-sol (partial-sols end-node start-node)
  (cond ((null partial-sols) nil)
        ((and (member (first (first (first partial-sols))) end-node)
              (eq start-node (ult (first (first partial-sols)))))
   ; *** (first partial-sols): the first solution in the list
   ;     (first (first partial-sols)): the path of this solution
   ;     (first (first (first partial-sols))): node1 or node2
   ;     (ult (first (first partial-sols))): the constraint node
           (first partial-sols))
        (t (find-partial-sol (rest partial-sols) end-node start-node))))
        
; ******************************************************************
(defun is-a-good-sol (path)
 (and (not (are-there-inverses path))
      (not (member 'restricts path))      ; *** this and the next avoid
      (not (member 'restricted-by path))  ;     also moving among roles
      (not (member '&has-description path))))

; ******************************************************************
; *** returns true if the first element of "path" or any following element
;     reachable only via subclass links is a member of nodelist
(defun member-or-subcl (path nodelist)
  (cond ((null path) nil)
        ((memq (first path) nodelist) t)
        ((memq (second path) '(subclass-of has-subclass))
           (member-or-subcl (rest (rest path)) nodelist))
        (t nil)))
  
; ********************************************************************
; *** this handles the 'union operator
(defun merge-union (vals)
  (cond ((null vals) nil)
        ((atom (first vals))
          (cons (first vals) (merge-union (rest vals))))
        ((eq 'union (first (first vals)))
          (append (rest (first vals)) (merge-union (rest vals))))
        (t (cons (first vals) (merge-union (rest vals))))))

; ********************************************************************
; *** is there any pair of adjacent inverse links in path?
(defun are-there-inverses (path)
  (cond ((null (fourth path)) nil)
        ((are-inverses (second path) (fourth path))
           (cond ((memq (second path) 
                     '(range range-of argument arg-of domain domain-of subclass-of))
     ; *** in case of (X range Y range-of Z), they are real inverses in case X=Z
                    (cond ((eq (first path) (fifth path)) t)
                          (t (are-there-inverses (rest (rest path))))))
                 (t t)))
        (t (are-there-inverses (rest (rest path))))))

; ********************************************************************
(defun are-inverses (rel1 rel2)
  (and (not (null rel1))
       (not (null rel2))
       (or (eq rel1 (get-inverse rel2))
           (eq rel2 (get-inverse rel1)))))

; ********************************************************************
; *** this "reverses" a path, by changing all links to their inverses.
;     path is assumed to start with a concept and to alternate concepts
;     and links
(defun rev-links (path)
  (cond ((null path) nil)
        ((null (rest path)) path)
        (t (cons (first path)
                 (cons (get-inverse (second path))
                       (rev-links (rest (rest path))))))))

; ********************************************************************
; *** this checks if a given syntactic dependent can play a given role
;     Usual matters of case-role mapping are not taken into account
;     In articular, VERB-SUBJ is mapped to "agent"
(defun check-complem-compatib (compl-mean verb-mean link)
 (let (deep-role local-role 
       (compl-lexmean (get-flatavm-feat-val compl-mean 'lexmean))
       (verb-lexmean (get-flatavm-feat-val verb-mean 'lexmean)))
  (cond ((eq link 'VERB-SUBJ)
          (cond ((eq verb-lexmean '££to-be) t)
      ; *** anything can be the subject of "to be"
      ;     I prefer this procedural solution instead of writing in the ontology
      ;     that "to be" has any entity as &agent, since it does not seem
      ;     to me that "to be" has an agent
                (t (setq deep-role '&agent)
                   (setq local-role 
                      (find-ont-relation verb-lexmean 'range deep-role))
                   (cond ((and (not (null local-role))
                           (is-subclass-of compl-lexmean 
                                 (first (get local-role 'domain)))) t)
                         (t nil)))))
        (t (exception 'semantic-error
                      "PROC/onto-reasoning: checking complement compatibility for a non-subject")))))

; ********************************************************************
; *** this finds any relation having "node" as its range or domain
;     and restricting the "upper-rel" relation
(defun find-ont-relation (sem range-or-domain upper-rel)
  (let ((relations (get sem (get-inverse range-or-domain)))
        found)
; *** if we want the agent of "££buy" (node), we must find the relation such that
;     ££buy is its range (so, find all "range-of", i.e. inverse of "range")
     (do ((nextrel (first relations) (first relations))
          (relations (rest relations) (rest relations)))
         ((or (null nextrel) found) found)
         (cond ((is-restriction-of nextrel upper-rel)
                  (setq found nextrel))))))

; ********************************************************************
; *** this looks for a sequence ... downr1 restricted-by rel restricts downr2 ...
;     Since paths are reversed, this refers to a more specific relation
;     (downr2) from which a sister (downr1) was reached. 
(defun check-rel-restr (path)
  (let ((restr (member 'restricted-by path)))
     (eq (third restr) 'restricts)))

