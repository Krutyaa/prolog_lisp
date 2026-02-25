Код на языке программирования Prolog

% Главный предикат, запускающий программу
start :-
    write('Введите 5 чисел по одному:'), nl,
    input_5_numbers(Numbers),
    format('Введенный список: ~w', [Numbers]), nl,
    analyze_numbers(Numbers).

% Ввод 5 чисел по одному
input_5_numbers([N1, N2, N3, N4, N5]) :-
    input_number(1, N1),
    input_number(2, N2),
    input_number(3, N3),
    input_number(4, N4),
    input_number(5, N5).

% Ввод одного числа с проверкой
input_number(Pos, Number) :-
    format('  Число ~w: ', [Pos]),
    read(Number),
    number(Number), !.  % Успех - введено число
input_number(Pos, Number) :-
    write('  Ошибка! Введите число.'), nl,
    input_number(Pos, Number).  % Повтор при ошибке

% Основной анализ чисел
analyze_numbers(Numbers) :-
    msort(Numbers, Sorted),  % Сортируем для группировки
    count_groups(Sorted, Groups),
    determine_result(Groups, Result),
    format('Результат: ~w', [Result]), nl.

% Подсчет групп одинаковых чисел в отсортированном списке
count_groups([], []).
count_groups([X|Rest], [Group|OtherGroups]) :-
    take_same(X, Rest, Group, Remaining),
    count_groups(Remaining, OtherGroups).

% Берем все одинаковые числа подряд
take_same(X, [X|Rest], [X|Group], Remaining) :-
    !, take_same(X, Rest, Group, Remaining).
take_same(X, Other, [X], Other).

% Определение результата по группам
determine_result(Groups, Result) :-
    maplist(length, Groups, Sizes),  % Длины групп
    sort(0, @>=, Sizes, SortedSizes),  % Сортируем по убыванию

   (SortedSizes = [5] -> Result = 1;          
     SortedSizes = [4,1] -> Result = 2;        
     SortedSizes = [3,2] -> Result = 3;        
     SortedSizes = [3,1,1] -> Result = 4;      
     SortedSizes = [2,2,1] -> Result = 5;      
     SortedSizes = [2,1,1,1] -> Result = 6;    
     Result = 7).


Код программы на языке Lisp

;; Проверка: содержится ли x в списке   
(defun contains (x lst)
  (cond
    ((null lst) nil)
    ((equal x (car lst)) t)
    (t (contains x (cdr lst)))))

;; Функция получения списка уникальных элементов
(defun my-unique (lst)
  (labels ((add-uniq (src acc)
             (cond
               ((null src) (nreverse acc))
               ((contains (car src) acc)
                (add-uniq (cdr src) acc))
               (t
                (add-uniq (cdr src) (cons (car src) acc))))))
    (add-uniq lst '())))

;; Основная функция программы
(defun classify (lst)
  (let* ((unique (my-unique lst))
         (freqs (mapcar (lambda (x)
                          (count x lst))
                        unique))
         (sorted (sort freqs #'>)))
    (cond
      ((equal sorted '(5)) 1)
      ((equal sorted '(4 1)) 2)
      ((equal sorted '(3 2)) 3)
      ((equal sorted '(3 1 1)) 4)
      ((equal sorted '(2 2 1)) 5)
      ((equal sorted '(2 1 1 1)) 6)
      ((equal sorted '(1 1 1 1 1)) 7)
      (t 'error))))


(defun run-and-print (lst)
  (format t "Список: ~a → результат: ~a~%" lst (classify lst)))

;; Примеры
(run-and-print '(3 3 3 3 3))
(run-and-print '(4 4 4 4 2))
(run-and-print '(7 7 7 5 5))
(run-and-print '(1 1 1 2 3))
(run-and-print '(8 8 9 9 1))
(run-and-print '(6 6 2 3 4))
(run-and-print '(1 2 3 4 5))
(run-and-print '())
(run-and-print '(1 2 3 4 5 6))
