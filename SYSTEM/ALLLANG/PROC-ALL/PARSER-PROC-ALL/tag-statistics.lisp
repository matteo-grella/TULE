(in-package "USER")

(defun collect-tag-stat ()
  (with-open-file (oport (build-file-name "../DATI/tagstats.dat")
                              :direction :output :if-does-not-exist :create
                              :if-exists :overwrite)
    (with-open-file (iport (build-file-name "../DATI/tagcorpus.dat")
                                :direction :input :if-does-not-exist :error)
      (let (all-rules prevrule prevok preverr blanks allok allerr)
          (dolist (filen (read iport))
                  (setq filen (change-extens (build-file-name filen) ".tag"))
                  (setq all-rules
                        (read-tag-result filen all-rules)))
          (setq all-rules
               (sort all-rules
                     #'(lambda (x y) (alphalessp (first x) (first y)))))
   ; *** final printing of the result
          (setq prevrule (first (first all-rules)))
          (setq prevok (second (first all-rules)))
          (setq preverr (third (first all-rules)))
          (setq allok prevok)
          (setq allerr preverr)
          (dolist (nxtrule all-rules)
             (cond ((eq (first nxtrule) prevrule)
      ; *** if the rule is the same as the previous one, increment the data
                      (setq prevok (+ prevok (second nxtrule)))
                      (setq allok (+ allok (second nxtrule)))
                      (setq preverr (+ preverr (third nxtrule)))
                      (setq allerr (+ allerr (third nxtrule))))
                   (t (setq blanks (-
                               (cond ((> prevok 99) 33)
                                     ((> prevok 9) 34)
                                     (t 35))
                               (length (explode prevrule))))
                      (format oport "~a" prevrule)
     ; **** print-blanks in PARSER/eval-parse
                      (print-blanks blanks oport)
                      (format oport "  ~a  ~a~%" prevok preverr)
                      (setq prevrule (first nxtrule))
                      (setq prevok (second nxtrule))
                      (setq allok (+ allok prevok))
                      (setq preverr (third nxtrule))
                      (setq allerr (+ allerr preverr)))))
      ; *** at the end of the loop, print the data of the last rule
          (setq blanks (-
                  (cond ((> prevok 99) 33)
                        ((> prevok 9) 34)
                        (t 35))
                  (length (explode prevrule))))
          (format oport "~a" prevrule)
          (print-blanks blanks oport)
          (format oport "  ~a  ~a~%" prevok preverr)
          (setq allok (+ allok prevok))
          (setq allerr (+ allerr preverr))
          (format oport "Total: Rule Applications ok: ~a; Errors: ~a~%"
                        allok allerr)))))

(defun read-tag-result (file data)
  (with-open-file (iport file
                            :direction :input :if-does-not-exist :error)
; **** skip the five heading lines
    (read-line iport)
    (read-line iport)
    (read-line iport)
    (read-line iport)
    (do ((nxtrule (read iport nil #\Escape)
                  (read iport nil #\Escape)))
        ((equal nxtrule #\Escape) data)
        (setq data
            (cons (list nxtrule (read iport) (read iport)) 
                  data)))))
    
    

