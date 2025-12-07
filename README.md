<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПІСКС</b></p>
<p align="center">
<b>Звіт лабораторної роботи 3</b><br/>
з дисципліни "Вступ до функціонального програмування"<br/>
Тема: "Функціональний і імперативний підходи до роботи зі списками"
</p>
<p align="right"><b>Студент:</b> КВ-23 Домущі Дмитро</p>
<p align="right"><b>Рік:</b> 2025</p>

## Загальне завдання
Реалізуйте алгоритм сортування чисел у списку двома способами: функціонально і
імперативно.
1. Функціональний варіант реалізації має базуватись на використанні рекурсії і
конструюванні нових списків щоразу, коли необхідно виконати зміну вхідного
списку. Не допускається використання: псевдо-функцій, деструктивних операцій,
циклів . Також реалізована функція не має бути функціоналом (тобто приймати на
вхід функції в якості аргументів).
2. Імперативний варіант реалізації має базуватись на використанні циклів і
деструктивних функцій (псевдофункцій). Не допускається використання функцій
вищого порядку або функцій для роботи зі списками/послідовностями, що
використовуються як функції вищого порядку. Тим не менш, оригінальний список
цей варіант реалізації також не має змінювати, тому перед виконанням
деструктивних змін варто застосувати функцію copy-list (в разі необхідності).
Також реалізована функція не має бути функціоналом (тобто приймати на вхід
функції в якості аргументів).

## Варіант 8(3)
Алгоритм сортування обміном №2 (із використанням прапорця) за незменшенням.

## Лістинг функції з використанням конструктивного підходу
```lisp
(defun bubble-pass-functional (lst)
  "Виконує один прохід сортування обміном.
   Повертає два значення: новий список і ознаку, чи був обмін."
  (cond
    ((or (null lst) (null (cdr lst)))
     (values lst nil))
    ((> (car lst) (cadr lst))
     (multiple-value-bind (tail-result tail-swapped)
         (bubble-pass-functional (cons (car lst) (cddr lst)))
       (values (cons (cadr lst) tail-result) t)))
    (t
     (multiple-value-bind (tail-result tail-swapped)
         (bubble-pass-functional (cdr lst))
       (values (cons (car lst) tail-result) tail-swapped)))))

(defun functional-sort (lst)
  (multiple-value-bind (new-list swapped) 
      (bubble-pass-functional lst)
    (if swapped
        (functional-sort new-list)
        new-list)))
```

### Тестові набори та утиліти
```lisp
(defun check-sort (name func input expected)
  (format t "~:[FAILED~;passed~] ~a~%"
          (equal (funcall func input) expected)
          name))

(defun test-sorting ()
  (format t "~%--- Testing Functional Sort ---~%")
  (check-sort "test 1 (mixed)" #'functional-sort '(3 1 4 1 5 9 2) '(1 1 2 3 4 5 9))
  (check-sort "test 2 (sorted)" #'functional-sort '(1 2 3 4 5) '(1 2 3 4 5))
  (check-sort "test 3 (reverse)" #'functional-sort '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort "test 4 (empty)" #'functional-sort nil nil)
  
  (format t "~%--- Testing Imperative Sort ---~%")
  (check-sort "test 1 (mixed)" #'imperative-sort '(3 1 4 1 5 9 2) '(1 1 2 3 4 5 9))
  (check-sort "test 2 (sorted)" #'imperative-sort '(1 2 3 4 5) '(1 2 3 4 5))
  (check-sort "test 3 (reverse)" #'imperative-sort '(5 4 3 2 1) '(1 2 3 4 5))
  (check-sort "test 4 (empty)" #'imperative-sort nil nil))
```

### Тестування
```lisp
CL-USER> (test-sorting)
--- Testing Functional Sort ---
passed test 1 (mixed)
passed test 2 (sorted)
passed test 3 (reverse)
passed test 4 (empty)
```

## Лістинг функції з використанням деструктивного підходу
```lisp
(defun imperative-sort (lst)
  (let ((result (copy-list lst))
        (len (length lst))
        (flag t))
    (do () ((not flag) result)
      (setf flag nil)
      (dotimes (i (- len 1))
        (let ((current (nth i result))
              (next (nth (1+ i) result)))
          (when (> current next)
            (rotatef (nth i result) (nth (1+ i) result))
            (setf flag t)))))
    result))
```

### Тестування
```lisp
CL-USER> (test-sorting)
--- Testing Imperative Sort ---
passed test 1 (mixed)
passed test 2 (sorted)
passed test 3 (reverse)
passed test 4 (empty)
NIL
```

