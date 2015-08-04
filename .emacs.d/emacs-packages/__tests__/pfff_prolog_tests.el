;; read symbol tests
(require 'pfff-prolog)
(with-temp-buffer
  (loop
   for i in '(("TimelineInHouseAppCollections::VIDEO_WATCH")
              ("fb:timeline:medley" . ":fb:timeline:medley"))
   do
   (insert (car i))
   (let*
       (
        (expected (or (cdr i) (car i)))
        (tester
         (lambda (ctxt)
           (unless (equal expected (pfff-prolog-symbol-at-point))
             (error (format "didn't match symbol %s at %s" expected ctxt))))))
     (beginning-of-line)
     (funcall tester "beginning")
     (end-of-line)
     (backward-char)
     (funcall tester "end")
     (erase-buffer))))