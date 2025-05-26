#lang racket

;; Функція для зчитування чисел з окремих рядків
(define (read-numbers)
  (let loop ([numbers '()])
    (define line (read-line))
    (cond [(or (eof-object? line) (string=? line ""))
           (reverse numbers)]
          [else
           (define num (string->number line))
           (if num
               (loop (cons num numbers))
               (error "Некоректне число у введенні"))])))

;; Функція для визначення індексу інтервалу для числа
(define (get-interval-index x min-val width alphabet-size)
  (if (zero? width)
      0
      (min (floor (/ (- x min-val) width)) (sub1 alphabet-size))))

;; Функція для відображення чисел на літери за рівними інтервалами
(define (map-to-letters numbers alphabet-size)
  (define min-val (apply min numbers))
  (define max-val (apply max numbers))
  (define width (if (= alphabet-size 1)
                    0
                    (/ (- max-val min-val) alphabet-size)))
  (for/list ([x numbers])
    (define index (inexact->exact (get-interval-index x min-val width alphabet-size)))
    (integer->char (+ 65 index)))) ; 65 — ASCII-код для 'A'

;; Функція для побудови матриці передування
(define (build-matrix chain alphabet-size)
  ;; Ініціалізація матриці з окремими векторами для кожного рядка
  (define matrix (for/vector ([i (in-range alphabet-size)])
                   (make-vector alphabet-size 0)))
  ;; Побудова матриці передування
  (for ([i (in-range (sub1 (string-length chain)))])
    (define prev (char->integer (string-ref chain i)))
    (define next (char->integer (string-ref chain (add1 i))))
    (define row (- prev 65))  ; 'A'=65 -> 0, 'B'=66 -> 1, 'C'=67 -> 2
    (define col (- next 65))
    (vector-set! (vector-ref matrix row) col
                 (add1 (vector-ref (vector-ref matrix row) col))))
  matrix)

;; Функція для виведення матриці з позначками літер
(define (print-matrix matrix alphabet-size)
  (printf "  ")
  (for ([i (in-range alphabet-size)])
    (printf "~a " (integer->char (+ 65 i))))
  (newline)
  (for ([i (in-range alphabet-size)])
    (printf "~a " (integer->char (+ 65 i)))
    (for ([j (in-range alphabet-size)])
      (printf "~a " (vector-ref (vector-ref matrix i) j)))
    (newline)))

;; Головна функція для координації програми
(define (main)
  (define alphabet-size (string->number (read-line)))
  (when (or (< alphabet-size 1) (> alphabet-size 26))
    (error "Потужність алфавіту має бути від 1 до 26"))
  (define numbers (read-numbers))
  (when (null? numbers)
    (error "Числа не введено"))
  (define chain (list->string (map-to-letters numbers alphabet-size)))
  (define matrix (build-matrix chain alphabet-size))
  (printf "Лінгвістичний ланцюжок:\n~a\n" chain)
  (printf "Матриця передування:\n")
  (print-matrix matrix alphabet-size))

;; Запуск програми
(main)