; Testet ob "element" vom Konzept "k" angenommen wird:
; Parameter:
;	element : Das zu testende Element
;	k : Konzept
; Return:
; 	T für "Ja"
; 	nil für "Nein"
(defun is-element-good (element k)
	(cond
		((null k) nil)
		((isMoreGeneral (car k) element) T)
		(T (is-element-good element (cdr k)))
	)
)

#| ############################################################################
#																			  #
#						AQ Basis Algorithmus								  #
#																			  #
############################################################################ |#

; Erstellt ein Konzept mittels des AQ Verfahrens.
; Parameter:
; 	positives: Eine Liste von positiven Trainingsdaten
; 	negatives: Eine Liste von negativen Trainingsdaten
; Return:
;	Konzept
(defun do-AQ (positives negatives)
	(cond 
		((null positives) nil)
		(T 
			(let 
				(
					(s 
						; Wählt die Beste Generalisation
						(choose-best-generalisation 
							; Erstellt den Star
							(time (create-star (car positives) negatives))
						)
					)
				)
				
				(append 
					(list s)
					(do-AQ (remove-covered-elements s(cdr positives)) negatives)
				)
			)
			
		)
	)
)


; Sucht die beste Generalisation aus einem Stern
; Parameter:
;	star : Stern
; Return:
;	Eine Generalisation
(defun choose-best-generalisation (star) 
	(cond 
		((null star) nil)
		(T (choose-less-stars star))
	)
)

; Löscht die Elemente aus aus einer Liste, für die "element" allgemeiner ist
; Parameter:
;	element : Eine Generalisation
;	list : Liste von Listen
; Return:
;	Eine gefilterte Liste
(defun remove-covered-elements (element list)
	(remove-if 
		#'(lambda (x) 
			(isMoreGeneral element x)
		) 
		list
	)
)

#| ############################################################################
#																			  #
#						VersionSpace										  #
#																			  #
############################################################################ |#

; Spezielle Implementation der Versionsraummethode zur Stern Generierung
; Parameter:
;	positive : Positives Beispiel
;	negatives : Liste der negativen Beispiele
; Return:
;	Gibt den Star zurück
(defun create-star (positive negatives)
	(reduce
		#'(lambda (result g)
			(create-g 
				result
				g
				positive
			)
		)
		negatives
		:initial-value (list (make-list (length positive) :initial-element "*"))
	)
)

; Erstellt neues G
; Parameter:
;	g : altes G
;	neg : Negatives Beispiel
;	s : Positives Beispiel
; Return:
; 	Neues G
(defun create-g (g neg s)
	(reduce
		#'(lambda (result h)
			(cond					
				(
					; Ersetze alle H e G, die neg als Beispiel haben und hängt diese an Result
					(isMoreGeneral h neg)
					(special-append 
						(create-specialization h neg s)
						result 							
					)
				)
				(
					; Ansonsten wird H an das Result angehängt
					T 
					(special-append 
						(list h)
						result 							
					)
				)
			)
		)
		g
		:initial-value '() ; Result ist zuerst eine leere Liste
	)
)

(defun special-append (list1 list2)
	(cond 
		((null list1) list2)
		((some 
			#'(lambda (x)
				(isMoreGeneral x (car list1))
			)
			list2
		 ) (special-append (cdr list1) list2))
		(T 
			(append 
				(list (car list1)) 
				(special-append (cdr list1) list2)
			)
		)
	)
)

; Spezialisiert die Hypothese hyp mit dem negativen Beispiel neg in Abhängigkeit von s.
; Aus dieser Spezialisierung werden alle Hypothesen entfernt, die genereller sind als das 
; negative Beispiel und bei denen "s" nicht genereller ist als "neg"
; Parameter:
;	hyp : Hypothese
;	neg : negatives Beispiel
;	s : positives Beispiel
; Return:
;	Spezialisierung von H
(defun create-specialization (hyp neg s)
	; Löscht alle Spezialisierungen von H, die a nicht enthalten
	(remove-if 
		#'(lambda (h) 
			(and 
				; Überprüft ob a Enthält
				(isMoreGeneral h neg) 
				(not (isMoreGeneral s neg))
			)
		) 
		(specialize hyp neg s)
	)
)

; Entfernt aus einer Liste die Elemente, die bereits durch ein anderes Element abgedeckt sind
; Parameter:
; 	list : Liste von Listen
; Return:
;	Gefilterte Liste
(defun filter-already-contained(list)
	; Löscht Elemente aus einer Liste für die zutrifft,
	(remove-if
		#'(lambda (h)
			; (Geht hier die komplette Liste (list) durch bis lambda = T)
			(some 
				#'(lambda (g)
					; das ein anderes Element aus der Liste, welches nicht das selbe ist, genereller ist.
					(and 					
						(not (equal h g))
						(isMoreGeneral g h)
					)
				)
				list
			)
		)
		list
	)
)

#| ############################################################################
#																			  #
#						Helper												  #
#																			  #
############################################################################ |#

; Zählt die "Sternchen" (*) aus einer Liste
; Parameter:
;	list : Liste
; Return:
;	Anzahl der Sterne
(defun count-stars (list)
	(count-if 
		#'(lambda (x) 
			(equal "*" x)
		) 
		list
	)
)

; Gibt aus einer Liste mit Listen die Liste zurück, welche die wenigsten Sterne hat
; Parameter:
;	list : Liste von Listen
; Return:
;	Die Liste mit den wenigsten Sternen
(defun choose-less-stars (list)
	(reduce
		#'(lambda (result x)
			(cond ((< (count-stars x) (count-stars result)) x)
				(T result)
			)
		)
		list
		:initial-value (make-list (length (car list)) :initial-element "*")
	)
)