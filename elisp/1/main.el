;; --- Day 1: The Tyranny of the Rocket Equation ---

;; Santa has become stranded at the edge of the Solar System while delivering presents to other planets! To accurately calculate his position in space, safely align his warp drive, and return to Earth in time to save Christmas, he needs you to bring him measurements from fifty stars.

;; Collect stars by solving puzzles. Two puzzles will be made available on each day in the Advent calendar; the second puzzle is unlocked when you complete the first. Each puzzle grants one star. Good luck!

;; The Elves quickly load you into a spacecraft and prepare to launch.

;; At the first Go / No Go poll, every Elf is Go until the Fuel Counter-Upper. They haven't determined the amount of fuel required yet.

;; Fuel required to launch a given module is based on its mass. Specifically, to find the fuel required for a module, take its mass, divide by three, round down, and subtract 2.

;; For example:

;;     For a mass of 12, divide by 3 and round down to get 4, then subtract 2 to get 2.
;;     For a mass of 14, dividing by 3 and rounding down still yields 4, so the fuel required is also 2.
;;     For a mass of 1969, the fuel required is 654.
;;     For a mass of 100756, the fuel required is 33583.

;; The Fuel Counter-Upper needs to know the total fuel requirement. To find it, individually calculate the fuel needed for the mass of each module (your puzzle input), then add together all the fuel values.

;; What is the sum of the fuel requirements for all of the modules on your spacecraft?

(defun advent-of-code-1-calc-fuel (mass)
  "Calculate fuel needed for transporting mass"
  (- (/ mass 3) 2))

(defun advent-of-code-1-calc-sum-of-fuel (mass-list)
  "Calculate sum of fuel needed for all of masses"
  (apply '+ (mapcar 'advent-of-code-1-calc-fuel mass-list)))

(defun advent-of-code-1-for-file (file-name)
  "Calculate sum of fuel for input in file-name"
  (let* ((input-buffer (find-file file-name))
         (input-text (progn
                       (let ((old-buf (current-buffer)))
                         (set-buffer input-buffer)
                         (let ((contents (buffer-string)))
                           (set-buffer old-buf)
                           contents)))))
    (advent-of-code-1-calc-sum-of-fuel (mapcar 'string-to-number (split-string input-text "\n")))))

;; (advent-of-code-1-calc-sum-of-fuel '(142195 119326 57976 138834 132685 113092 88731 52063 122899 78681 117881 121912 112633 85163 145655 76668 92939 81941 62645 126482 114642 55588 95934 68172 62160 111109 141496 97453 83723 50309 82930 66124 142265 100066 147434 149708 77906 71147 76590 59528 67973 68187 135534 129331 147054 89062 63159 80990 103402 139627 87251 66561 102708 91307 121287 149077 142275 144917 98677 114912 102236 56147 130660 63523 112577 75086 136006 142090 80446 53900 144975 143195 138974 60145 132474 62640 62270 76275 62315 85065 99617 73579 97553 79715 81297 77342 142907 114001 137846 122398 71457 133929 110617 68928 56741 87754 53907 68322 85782 140916))

(print (advent-of-code-1-for-file "input"))

;; --- Part Two ---

;; During the second Go / No Go poll, the Elf in charge of the Rocket Equation Double-Checker stops the launch sequence. Apparently, you forgot to include additional fuel for the fuel you just added.

;; Fuel itself requires fuel just like a module - take its mass, divide by three, round down, and subtract 2. However, that fuel also requires fuel, and that fuel requires fuel, and so on. Any mass that would require negative fuel should instead be treated as if it requires zero fuel; the remaining mass, if any, is instead handled by wishing really hard, which has no mass and is outside the scope of this calculation.

;; So, for each module mass, calculate its fuel and add it to the total. Then, treat the fuel amount you just calculated as the input mass and repeat the process, continuing until a fuel requirement is zero or negative. For example:

;;     A module of mass 14 requires 2 fuel. This fuel requires no further fuel (2 divided by 3 and rounded down is 0, which would call for a negative fuel), so the total fuel required is still just 2.
;;     At first, a module of mass 1969 requires 654 fuel. Then, this fuel requires 216 more fuel (654 / 3 - 2). 216 then requires 70 more fuel, which requires 21 fuel, which requires 5 fuel, which requires no further fuel. So, the total fuel required for a module of mass 1969 is 654 + 216 + 70 + 21 + 5 = 966.
;;     The fuel required by a module of mass 100756 and its fuel is: 33583 + 11192 + 3728 + 1240 + 411 + 135 + 43 + 12 + 2 = 50346.

;; What is the sum of the fuel requirements for all of the modules on your spacecraft when also taking into account the mass of the added fuel? (Calculate the fuel requirements for each module separately, then add them all up at the end.)

(defun advent-of-code-2-calc-fuel (mass fuel-for-mass-map)
  "Calculate fuel for given mass, use fuel-for-mass-map for dynamic programming."
  (gethash mass fuel-for-mass-map (let ((fuel (advent-of-code-1-calc-fuel mass)))
                                    (puthash mass fuel fuel-for-mass-map)
                                    fuel)))

(defun advent-of-code-2-calc-fuel-total (mass fuel-for-mass-map)
  "Calculate full fuel for given mass(accounting for fuel needed to carry fuel)"
  (if (= mass 0)
      0
    (let ((initial-fuel (max 0 (advent-of-code-2-calc-fuel mass fuel-for-mass-map))))
      (+ initial-fuel (advent-of-code-2-calc-fuel-total initial-fuel fuel-for-mass-map)))))

(advent-of-code-2-calc-fuel-total 100756 (make-hash-table))

(defun advent-of-code-2-calc-sum-of-fuel (mass-list)
  "Calculate sum of fuel needed for all of masses"
  (let ((fuel-for-mass-map (make-hash-table)))
    (apply '+ (mapcar (lambda (mass) (funcall 'advent-of-code-2-calc-fuel-total mass fuel-for-mass-map)) mass-list))))

(advent-of-code-2-calc-sum-of-fuel '(14 1969))

(defun advent-of-code-2-for-file (file-name)
  "Calculate sum of fuel total for input in file-name"
  (let* ((input-buffer (find-file file-name))
         (input-text (progn
                       (let ((old-buf (current-buffer)))
                         (set-buffer input-buffer)
                         (let ((contents (buffer-string)))
                           (set-buffer old-buf)
                           contents)))))
    (advent-of-code-2-calc-sum-of-fuel (mapcar 'string-to-number (split-string input-text "\n")))))

(advent-of-code-2-for-file "input")
