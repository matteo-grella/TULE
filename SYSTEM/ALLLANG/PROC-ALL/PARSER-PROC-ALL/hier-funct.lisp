(in-package "USER")

;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;			TYPES
;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

; ***
;     *SURFACE-REALIZATION*
;	(cat1 cat2 ... catN) [N > 0]
;     where each 'cati' is either
;	- a syntactic category (es. adj)
;	- a list including a syntactic category and one or more constraints
;	  (ex. (adj (number pl) (agree)))
; ***
;     *INTERNAL-CASE-DEF*
;	(surf-defs surf-labs deep-labs)
;     This is the description of the depending cases; it is given as three
;     lists, which must be of the same length
;       - surf-defs: a list of *SURFACE-REALIZATION*
;       - surf-labs: a list of surface labels, generated from the deep-labels
;                    by prefixing 's-' (e.g. s-subject)
;       - deep-labs: a list of input labels (e.g. subject)
; ***
;     *HIERARCHY-NODE-DEF*
;	(class-name parents surf-defs deep-labs)
;     This is the input description of a node in the subcat hierarchy
;	- class-name: the label of the node, i.e. the name of a subcat class
;	- synonyms: possible 'equivalent' classes
;	- parents: the labels of the parent nodes in the hierarchy
;	- surf-defs: a list of *SURFACE-REALIZATION*
;       - deep-labs: a list of input labels (e.g. subject)
;     N.B. The pair <surf-defs, deep-labs> forms a 'local-cf-def'
; *** 
;     *SUBCAT-PATTERN*
;       ((transf-cl-name appl-cond internal-prefs applied-transfs cases))
;   !! Notice the double list (a residual of previous implementation) !!!!!
;     where:
;     - transf-cl-name is the concatenation of the original class-name with
;       the name of the applied transformation.
;       Ex: class-name = 'trans'
;           transformation = 'passivization'
;                ---------->  transf-class-name = 'trans+passivization'
;     - appl-cond are the 'applicability conditions'; Ex. in case of
;	passivization, 'appl-cond' encodes the fact that the verb must be
;	passive
;     - internal-prefs are local preferences concerning the application of a 
;       transformation. For instance, impersonalization is preferred in case 
;       the head verb precedes the direct object. This is paired with the
;       opposite preference for reflexivization ("Qui SI lavano i piatti" vs
;       "Qui SI e' lavato Luigi") 
;     - applied-transfs is a list including the basic class name and all the
;       applied transformations. It is used to implement another kind of
;       preference: patterns obtained by means of few transformations are
;       preferred to patterns obtained by means of many trasformations; in
;	case of equal number of transformations, some of them are preferred to
;	others. The preference is implemented by considering the size of
;       the list (1 + the number of applied transformations), and the
;       explicit preferences stored in *TRANSF-PREFERENCES*  and
;       *CLASS-PREFERENCES* at the end of this file.
;     - cases is an *INTERNAL-CASE-DEF*
; *** 
;     *TRANSFORMATIONS*
;	(transf-name transf-type v-classes constraints order-cond modifications)
;     where
;	- transf-name is an atom
;	- transf-type is "replacing" if the original cf is substituted by
;	  the transformed one, "extending" is the new cf is added to the
;         existing one
;	- v-classes is the list of verbal classes to which the transformation
;         must be applied. Implicitly, the transf is applied to the all classes
;         in each subtree rooted in one of the listed classes
;	- constraints are conditions that must be verified in the input
;    	  sentence; for instance, passivization is consistent just when the
;  	  associated verb is passive
;	- order-cond are the ordering constraints. They apply not to the
;	  transformation, but to the resulting pattern. So they must be
;	  verified on the final role assignment (see the comments in
;	  SUBCATS/gr-interp: match-vpatt)
;	- modifications is a set of operations that change the input case
;	  frame in the transformed one

; ***********************
;   INTERNAL REPRESENTATION OF A NODE IN THE HIERARCHY
;
;   It is represented as the atom 'class-name' (basic or transformed) with
;   the following properties:
;	subcat-patterns --> full definition after inheritance (of the type
;			    *SUBCAT-PATTERN*)
;	cf-def 		--> full definition after inheritance (an
;     			    *INTERNAL-CASE-DEF*)
;	loc-cf-def 	--> local definition (in the format of an
;			    *INTERNAL-CASE-DEF*)
;       synonyms        --> classes having the same definition
;       syn-repr        --> if the node is just a synonym, it does not include
;                           the definition, but just a reference to its
;                           synonym-class representative
;	grclass-parents --> set of (immediate) parents in the hierarchy
;	grclass-deps    --> (immediate) daughters of the class in the hierarchy
;	transf-source   --> a list of pairs <transformation, class>;
;			    if the class comes from a transformation, it says 
;			    which is the original class and which is the
;			    transformation
;       transf-sons	--> a list of pairs <transformation, class>; 
;			    if the class gave origin (via a transformation) to
;			    a new class, it says which is the new class and 
;			    which was the transformation

;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
;     Basic data structure access functions
;$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

; ******* node definitions ******************
(defun get-surf-def (internal-case-def)
   (first internal-case-def))

(defun get-surf-lab (internal-case-def)
   (second internal-case-def))

(defun get-deep-lab (internal-case-def)
   (third internal-case-def))

; ******* hierarchy node definitions *********
(defun get-hn-name (hierarchy-node)
   (first hierarchy-node))

(defun get-hn-synonyms (hierarchy-node)
   (second hierarchy-node))

(defun get-hn-parents (hierarchy-node)
   (third hierarchy-node))

(defun get-hn-surf-def (hierarchy-node)
   (fourth hierarchy-node))

(defun get-hn-deep-lab (hierarchy-node)
   (fifth hierarchy-node))

; ******* hierarchy node implementations *****
(defun get-syn-repr (node)
  (let ((refnode (get node 'syn-repr)))
      (cond ((null refnode) node)
	    (t refnode))))

(defun is-inthn-dummy? (node)
   (get node 'dummy))

(defun get-inthn-synonyms (node)
   (get node 'synonyms))

(defun get-inthn-patterns (node)
   (get (get-syn-repr node) 'subcat-patterns))

(defun get-inthn-parents (node)
   (get (get-syn-repr node) 'grclass-parents))

(defun get-repr-inthn-parents (node)
   (mapcar #'get-syn-repr (get-inthn-parents node)))

(defun get-inthn-cf-def (node)
   (get (get-syn-repr node) 'cf-def))

(defun get-inthn-loc-cf-def (node)
   (get (get-syn-repr node) 'loc-cf-def))

(defun get-inthn-deps (node)
   (get (get-syn-repr node) 'grclass-deps))

(defun get-inthn-tr-source (node)
   (get (get-syn-repr node) 'transf-source))

(defun get-inthn-tr-sons (node)
   (get (get-syn-repr node) 'transf-sons))

(defun get-inthn-applc (node)
   (get (get-syn-repr node) 'appl-cond))

(defun create-inthn (cl-name synonyms parents surfdef surfc deepc)
    (put-inthn-parents cl-name parents)
    (put-inthn-synonyms cl-name synonyms)
; *** for a synonym, syn-repr is its representative
    (dolist (syn synonyms)
        (putprop syn cl-name 'syn-repr))
    (dolist (parent parents)
           (put-inthn-deps parent cl-name))
    (put-inthn-loc-cf-def cl-name (list surfdef surfc deepc)))

(defun put-inthn-dummy (node)
   (putprop node t 'dummy))

(defun put-inthn-synonyms (node syn)
   (putprop node syn 'synonyms))

(defun put-inthn-patterns (node patt)
  (cond ((null (second patt)) 
           (exception 'parse-error "PROC/hier-funct: Null condition" patt))
	((member #\+ (explode (first (fourth patt))))
	   (exception 'parse-error "PROC/hier-funct: Non basic transf origin" patt)))
   (putprop (get-syn-repr node) patt 'subcat-patterns))

(defun put-inthn-parents (node parents)
   (putprop (get-syn-repr node) 
	    (union parents (get-inthn-parents node))
	    'grclass-parents))

(defun put-inthn-cf-def (node cf)
   (cond ((null cf) (setq cf '(NIL NIL NIL))))
   (putprop (get-syn-repr node) cf 'cf-def))

(defun put-inthn-loc-cf-def (node loccf)
   (cond ((null loccf) (setq loccf '(NIL NIL NIL))))
   (putprop (get-syn-repr node) loccf 'loc-cf-def))

(defun put-inthn-deps (node deps)
   (add-prop-val (get-syn-repr node) 'grclass-deps deps))

(defun put-inthn-tr-source (node trs)
   (putprop (get-syn-repr node) trs 'transf-source))

(defun put-inthn-tr-sons (node trso)
   (add-prop-val (get-syn-repr node) 'transf-sons trso))

; ******* hierarchy predicates ********************
(defun is-inthn-dependent (down-classes up-classes)
   (int-is-inthn-dependent (inlist down-classes) (inlist up-classes)))

(defun int-is-inthn-dependent (down-cll up-cll)
  (let (found)
   (do ((nextdown (first down-cll) (first down-cll))
        (down-cll (rest down-cll) (rest down-cll)))
       ((or (null nextdown) found) found)
       (setq found (sing-is-inthn-dependent nextdown up-cll)))))
 
(defun sing-is-inthn-dependent (down-class up-cll)
    (cond ((null up-cll) nil)
          ((member down-class up-cll) t)
          (t (int-is-inthn-dependent (get-inthn-parents down-class)
                              up-cll))))

; ******* subcat patterns ********************
(defun get-subc-name (subcat-pattern)
   (first subcat-pattern))

(defun get-subc-applc (subcat-pattern)
   (second subcat-pattern))

(defun get-subc-prefs (subcat-pattern)
   (third subcat-pattern))

(defun get-subc-transfs (subcat-pattern)
   (fourth subcat-pattern))

(defun get-subc-cases (subcat-pattern)
   (fifth subcat-pattern))

; ******* transformations ********************
(defun get-transf-name (transformation)
   (first transformation))

(defun get-transf-type (transformation)
   (second transformation))

(defun get-transf-classes (transformation)
   (third transformation))

(defun get-transf-cond (transformation)
   (fourth transformation))

(defun get-transf-order (transformation)
   (fifth transformation))

(defun get-transf-modif (transformation)
   (nthcdr 5 transformation))

