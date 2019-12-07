(defun advent-of-code-1-add (sub1-i sub2-i res-i program)
  "Add numbers from sub1-i and sub2-i positions in program, and store them at position res-i."
  (aset program res-i (+ (aref program sub1-i) (aref program sub2-i))))

(defun advent-of-code-1-mul (sub1-i sub2-i res-i program)
  "Add numbers from sub1-i and sub2-i positions in program, and store them at position res-i."
  (aset program res-i (* (aref program sub1-i) (aref program sub2-i))))

(defun advent-of-code-1-run-program (program)
  (let* ((program-copy (copy-sequence program))
        (i 0)
        (i-val (aref program-copy i)))
    (while (not (= i-val 99))
      (cond ((= i-val 1)
             (advent-of-code-1-add (aref program-copy (+ 1 i))
                                   (aref program-copy (+ 2 i))
                                   (aref program-copy (+ 3 i))
                                   program-copy))
            ((= i-val 2)
             (advent-of-code-1-mul (aref program-copy (+ 1 i))
                                   (aref program-copy (+ 2 i))
                                   (aref program-copy (+ 3 i))
                                   program-copy))
            (t (throw nil (concat "Wrong code: " (number-to-string i-val)))))
      (setq i (+ i 4)
            i-val (aref program-copy i)))
    (aref program-copy 0)))

(advent-of-code-1-run-program '[1 0 0 3 99])
(advent-of-code-1-run-program '[2 3 0 3 99])
(advent-of-code-1-run-program '[2 4 4 5 99 0])
(advent-of-code-1-run-program '[1 1 1 4 99 5 6 0 99])
(advent-of-code-1-run-program '[1 9 10 3 2 3 11 0 99 30 40 50])

(advent-of-code-1-add 0 0 3 '[1 0 0 3 99])

(defun advent-of-code-1-parse-program (text)
  (apply 'vector (mapcar 'string-to-number (split-string text ","))))

(advent-of-code-1-parse-program "1,0,0,3,99")

(defun advent-of-code-1-for-file (file-name)
  (let* ((input-buffer (find-file file-name))
         (input-text (progn
                       (let ((old-buf (current-buffer)))
                         (set-buffer input-buffer)
                         (let ((contents (buffer-string)))
                           (set-buffer old-buf)
                           contents))))
         (program (advent-of-code-1-parse-program input-text)))
    (aset program 1 12)
    (aset program 2 2)
    (advent-of-code-1-run-program program)))

(advent-of-code-1-for-file "input")

(defun advent-of-code-2-add (sub1-i sub2-i res-i program)
  "Add numbers from sub1-i and sub2-i positions in program, and store them at position res-i. Return instruction length."
  (aset program res-i (+ (aref program sub1-i) (aref program sub2-i)))
  4)

(defun advent-of-code-2-mul (sub1-i sub2-i res-i program)
  "Add numbers from sub1-i and sub2-i positions in program, and store them at position res-i. Return instruction length."
  (aset program res-i (* (aref program sub1-i) (aref program sub2-i)))
  4)

(defun advent-of-code-2-run-program (program)
  (let* ((program-copy (copy-sequence program))
        (i 0)
        (i-val (aref program-copy i)))
    (while (not (= i-val 99))
      (cond ((= i-val 1)
             (setq i (+ i (advent-of-code-2-add (aref program-copy (+ 1 i))
                                                (aref program-copy (+ 2 i))
                                                (aref program-copy (+ 3 i))
                                                program-copy))))
            ((= i-val 2)
             (setq i (+ i (advent-of-code-2-mul (aref program-copy (+ 1 i))
                                                (aref program-copy (+ 2 i))
                                                (aref program-copy (+ 3 i))
                                                program-copy))))
            (t (throw nil (concat "Wrong code: " (number-to-string i-val)))))
      (setq i-val (aref program-copy i)))
    (aref program-copy 0)))

(advent-of-code-2-run-program '[1 0 0 3 99])
(advent-of-code-2-run-program '[2 3 0 3 99])
(advent-of-code-2-run-program '[2 4 4 5 99 0])
(advent-of-code-2-run-program '[1 1 1 4 99 5 6 0 99])
(advent-of-code-2-run-program '[1 9 10 3 2 3 11 0 99 30 40 50])

(defun advent-of-code-2-for-file (file-name noun verb)
  (let* ((input-buffer (find-file file-name))
         (input-text (progn
                       (let ((old-buf (current-buffer)))
                         (set-buffer input-buffer)
                         (let ((contents (buffer-string)))
                           (set-buffer old-buf)
                           contents))))
         (program (advent-of-code-1-parse-program input-text)))
    (aset program 1 noun)
    (aset program 2 verb)
    (advent-of-code-2-run-program program)))

(advent-of-code-2-for-file "input" 1 2)


