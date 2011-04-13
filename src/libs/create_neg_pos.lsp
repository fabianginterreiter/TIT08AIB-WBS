(defun get-positive-examples(examples)
	(cond 
		((null examples) nil)
		((equal (car (last (car examples))) "ja") 
			(cons 
				(delete-last-element (car examples)) 
				(get-positive-examples (cdr examples)))
			)
		(T (get-positive-examples (cdr examples)))
	)
)

(defun get-negative-examples(examples)
	(cond 
		((null examples) nil)
        ((equal (car (last (car examples))) "nein") 
			(cons
				(delete-last-element (car examples)) 
				(get-negative-examples (cdr examples)))
			)
		(T (get-negative-examples (cdr examples)))
	)
)
	
(defun delete-last-element (list)
	(reverse (cdr (reverse list)))
)
	

(setq examples (get-examples))
(setq pexamples (get-positive-examples examples))
(setq nexamples (get-negative-examples examples))
