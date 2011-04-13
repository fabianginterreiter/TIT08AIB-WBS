(load "libs/vendor/versionspace.fas")

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
;	k: Konzept Liste welche am Ende zurück gegeben wird (dient der tail-recursion)
; Return:
;	Konzept
(defun do-AQ-helper (positives negatives k)
	(cond 
		((null positives) k)
		(T 
			(let 
				(
					(s 
						; Wählt die Beste Generalisation
						(choose-best-generalisation 
							; Erstellt den Star
							(create-star (car positives) negatives)
						)
					)
				)
				(do-AQ-helper (remove-covered-elements s(cdr positives)) negatives (append (list s) k))
			)
		)
	)
)

; Nur eine wrapper Funktion um den Algorithmus mit einer leeren Liste zu starten
(defun do-AQ (positives negatives)
	(do-AQ-helper positives negatives '())
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
	;(filter-already-contained 
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
	;)
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
	(time (remove-if
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
	))
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

; Nur eine wrapper Funktion um mit einer leeren liste zu starten
(defun special-append (list1 list2)
	(special-append-helper list1 list2 '())
)

; Fügt elemente der "list1" an "list2" nur dann wenn es kein element in "list2" gibt welches genereller ist als das element von "list1"
; Parameter:
;	list1: Liste von elemente welche an list2 angehangen werden sollen
;	list2: Liste an die angehangen werden soll
;	to-list: Resultierende Liste, welche am Ende zurückgegeben wird (dient dazu, um tail-recursion zu garantieren)
; Return:
;	Neue Liste mit den angefügten elementen
(defun special-append-helper (list1 list2 to-list)
	(cond 
		((null list1) (append to-list list2))
		((some 
			#'(lambda (x)
				(isMoreGeneral x (car list1))
			)
			list2
		 ) (special-append-helper (cdr list1) list2 to-list))
		(T (special-append-helper (cdr list1) list2 (append (list (car list1)) to-list)))
	)
)