#lang racket

; This function finds the stable matching from the two lists of pairs of preferences for males and females.

(define (stable-marriage maleList femaleList)
  (let* ((firstList (beginMatching maleList femaleList))
         (toVector (callOrganizePairs firstList maleList))
         (finalList (vector->list toVector)))
    finalList))

; This function recursively matches males and females based on the stable matching criteria.

(define (beginMatching maleList femaleList)
  (let ((freeMales (constructMales maleList))
         (returnList '()))
   (let maleLoop ((malesNotMatched freeMales))
      (cond ((null? malesNotMatched) returnList)
            (else      
             (let ((male (car malesNotMatched))
                   (females (cdr (car maleList))))
               (let femaleLoop ((listOfFemales females))
                 (cond ((null? listOfFemales) "Error!")
                       (else
                        (let ((female (car listOfFemales)))
                          (if (not (pairExists female returnList))
                              (begin
                                (let ((pair (cons male female)))
                                  (set! returnList (append returnList (list pair)))
                                  (maleLoop (cdr malesNotMatched))))
                              (begin
                                (let* ((oldPair (findPair female returnList))
                                       (oldGuy (car oldPair))
                                       (femalePreferredRankings (cdr (findPairInMainList female femaleList)))
                                       (currentGuyRanking (findIndex male femalePreferredRankings))
                                       (oldGuyRanking (findIndex oldGuy femalePreferredRankings)))
                                (if (< currentGuyRanking oldGuyRanking)
                                    (begin
                                       (set! malesNotMatched (removeElement male malesNotMatched))
                                       (set! malesNotMatched (append malesNotMatched (list (car oldPair))))
                                      (set! returnList (removePair oldPair returnList))
                                      (let ((newPair (cons male female)))
                                        (set! returnList (append returnList (list newPair)))
                                        (maleLoop malesNotMatched)))
                                    (femaleLoop (cdr listOfFemales))))))))))))))))

; This function sorts the final list based on the original order of the males in the inputted list of males.

(define (organizePairs completeList maleList interVector)
  (if (<= (length completeList) 0) interVector
      (begin (vector-set! interVector (findElementIndex (car (car completeList)) maleList) (car completeList))
      (organizePairs (cdr completeList) maleList interVector))))

(define (callOrganizePairs completeList maleList)
   (let ((interVector (make-vector (length completeList))))
    (organizePairs completeList maleList interVector)))

; This function finds the index of an element in a list.

(define (findElementIndex element lst)
  (define (helper lst index)
    (cond ((null? lst) #f)
          ((equal? element (car (car lst))) index)
          (else (helper (cdr lst) (+ index 1)))))
  (helper lst 0))

; This function removes a pair from a list

(define (removePair pair lst)
  (cond ((null? lst) '())
        ((equal? pair (car lst)) (cdr lst))
        (else (cons (car lst) (removePair pair (cdr lst))))))

; This function finds a pair in the original inputted list.

(define (findPairInMainList name mainList)
  (cond ((null? mainList) #f)
        ((equal? (caar mainList) name) (car mainList))
        (else (findPairInMainList name (cdr mainList)))))

; This function finds a pair in the matched pairs list.

(define (findPair element pairList)
  (cond ((null? pairList) #f)
        ((equal? (cdr (car pairList)) element) (car pairList))
        (else (findPair element (cdr pairList)))))

; This function constructs the list of males without their list of preferred females.

(define (constructMales maleList)
  (if (= (length maleList) 0)
       '()
   (cons (car (car maleList))
         (constructMales (cdr maleList)))))

; This function removes an element from a list.

(define (removeElement element lst)
  (filter (lambda (x) (not (equal? x element))) lst))

; This element finds the index of an element in the list.

(define (findIndex element lst)
  (define (findIndexHelper element lst index)
    (cond ((null? lst) #f)
          ((equal? element (car lst)) index)
          (else (findIndexHelper element (cdr lst) (+ index 1)))))
  (findIndexHelper element lst 0))

; This function checks to see in a pair exists in a list.

(define (pairExists element pairList)
  (cond ((null? pairList) #f)
        ((or (equal? element (caar pairList))
             (equal? element (cdar pairList)))
         #t)
        (else (pairExists element (cdr pairList)))))

(display "(stable-marriage '((\"Jack\" . (\"Jane\" \"Amanda\" \"Kelly\")) (\"John\" . (\"Amanda\" \"Jane\" \"Kelly\")) (\"Mike\" . (\"Amanda\" \"Jane\" \"Kelly\"))) '((\"Jane\" . (\"Mike\" \"Jack\" \"John\")) (\"Amanda\" . (\"John\" \"Mike\" \"Jack\")) (\"Kelly\" . (\"Mike\" \"John\" \"Jack\")))) ==> '")
(display (stable-marriage '(("Jack". ("Jane" "Amanda" "Kelly")) ("John". ("Amanda" "Jane" "Kelly")) ("Mike". ("Amanda" "Jane" "Kelly"))) '( ("Jane". ("Mike" "Jack" "John")) ("Amanda". ("John" "Mike" "Jack")) ("Kelly". ("Mike" "John" "Jack")))))