;;;file: copy.cl
;;;purpose: compare 3 copying algorithms: AM, AC, & CIU
;;;programmer: Shuaibo Huang, Tom Shultz

;;;globals

(defstruct agent
  "Agent has id index, state and region."
  id
  state
  )

(defvar *agents1* nil
  "Array of agents in region One.")
"______________________________Extenstion Made___________________________"
(defvar *agents2* nil
  "Array of agents in region Two")

(defvar *array1-size* 100
  "Size of agent1 array.")
"______________________________Extenstion Made___________________________"
(defvar *array2-size* 100
  "Size of agent2 array")

(defvar *path* nil
  "Path for saving files.")

;;;random numbers

(defun call-random (m n)
  "(m n)
Call (random n) m times. Used by seed-random."
  (do ((i 0 (1+ i)))
      ((= i m) nil)
    (if (zerop n) 
        (random 1)
      (random n))))

(defun seed-random ()
  "Seed random from the last 4 digits of time. 
Useful for generating unique random sequences."
  (let* ((time (get-internal-real-time))
         (n (multiple-value-bind (x y) (floor time 100) (cadr (list x y))))
         (hundreds (multiple-value-bind (x y) (floor time 100) (car (list x y))))
         (m (multiple-value-bind (x y) (floor hundreds 100) (cadr (list x y)))))
    (call-random m n)))

;;;make agents

(defun initialize-array (nu nx ny)
  "(nu nx ny)
Set *array-size* to sum of numbers of agents.
Initialize *agents* array with nil elements."
  (setf *array-size* (+ nu nx ny)
    *agents* (make-array *array-size* :initial-element nil)))

;;;(initialize-array 4 1 0)

(defun make-1-agent (i s)
  "(i s)
Make or change state of agent i to s."
  (setf (aref *agents* i) 
    (make-agent :id i
                :state s)))

;;;(make-1-agent 2 'u)

(defun make-agent-array (nu nx ny)
  "(nu nx ny)
Make agent array."
  (do ((i 0 (1+ i)))
      ((= i nu))
    (make-1-agent i 'u))
  (do ((i nu (1+ i)))
      ((= i (+ nu nx)))
    (make-1-agent i 'x))
  (do ((i (+ nu nx) (1+ i)))
      ((= i (+ nu nx ny)))
    (make-1-agent i 'y)))

;;;(initialize-array 4 2 0)
;;;(make-agent-array 4 2 0)
;;;*agents*

;;;count states

(defun get-agent-state (a)
  "(a)
Return state of agent at a."
  (let ((agent (aref *agents* a)))
    (agent-state agent)))

;;;(get-agent-state 3)

(defun count-state (state)
  "(state)
Count state across agents."
  (do ((i 0 (1+ i))
       (n 0 (if (eq (get-agent-state i) state)
                (1+ n)
              n)))
      ((= i *array-size*) n)))

;;;(count-state 'u)
;;;(count-state 'x)

(defun count-states (states)
  "(states)
Count each state across agents."
  (do ((stts states (cdr stts))
       (counts nil (cons (count-state (car stts))
                         counts)))
      ((null stts) (reverse counts))))

;;;(count-states '(u x y))

(defun count-state-section (state start end)
  "(state start end)
Count state for array section from start up to but not including end index."
  (do ((i start (1+ i))
       (n 0 (if (eq (get-agent-state i) state)
                (1+ n)
              n)))
      ((= i end) n)))

;;;(count-state-section 'x 4 5)
;;;(count-state-section 'x 4 6)

;;;interact

(defun decide-agent (i r p)
  "(i r p)
Initiator i convinces undecided recipient r in AM."
  (let* ((r-agent (aref *agents* r))
         (i-state (get-agent-state i))
         (r-state (get-agent-state r)))
    (if (and (not (eq i-state 'u))
             (eq r-state 'u)
             (< (random 1.0) p))
        (setf (agent-state r-agent) i-state))))
            
;;;(decide-agent 4 2 1.0)

(defun confuse-agent (i r p)
  "(i r p)
Initiator i confuses decided recipient r in AM."
  (let* ((r-agent (aref *agents* r))
         (i-state (get-agent-state i))
         (r-state (get-agent-state r)))
    (if (and (not (eq i-state 'u))
             (not (eq r-state 'u))
             (not (eq i-state r-state))
             (< (random 1.0) p))
        (setf (agent-state r-agent) 'u))))

;;;(confuse-agent 6 2 1.0)

(defun interact-am (agents p)
  "(agents p)
Initiator i & recipient r interact in AM."
  (let* ((i (first agents))
         (r (second agents))
         (r-state (get-agent-state r)))
    (if (eq r-state 'u)
        (decide-agent i r p)
      (confuse-agent i r p))))

(defun interact-ac (agents p)
  "(agents p)
Recipient imitates decided initiator in AC."
  (let* ((i (first agents))
         (r (second agents))
         (r-agent (aref *agents* r))
         (i-state (get-agent-state i)))
    (if (and (not (eq i-state 'u))
             (< (random 1.0) p))
        (setf (agent-state r-agent) i-state))))

;;;(interact-ac '(4 0) 1.0)

(defun interact-ciu (agents p)
  "(agents p)
Undecided recipient imitates decided initiator in CIU."
  (let* ((i (first agents))
         (r (second agents))
         (r-agent (aref *agents* r))
         (i-state (get-agent-state i))
         (r-state (get-agent-state r)))
    (if (and (not (eq i-state 'u))
             (eq r-state 'u)
             (< (random 1.0) p))
        (setf (agent-state r-agent) i-state))))

;;;(interact-ciu '(4 0) 1.0)

;;;get initiator & responder

(defun get-random-agent ()
  "()
Get 1 random agent."
  (random *array-size*))

;;;(get-random-agent)

(defun get-2-random-agents ()
  (let* ((demonstrator (get-random-agent))
         (observer (get-random-agent)))
    (if (eq demonstrator observer)
        (get-2-random-agents)
      (list demonstrator observer))))

;;;(get-2-random-agents)

;;;Check for consensus

(defun consensusp (counts)
  "(counts)
Consensus on x or y?"
  (let ((nu (first counts))
        (nx (second counts))
        (ny (third counts)))
    (and (= nu 0)
         (or (zerop nx)
             (zerop ny))
         (not (and (zerop nx)
                   (zerop ny))))))

;;;(consensusp '(1 1 2))
;;;(consensusp '(0 0 0))
;;;(consensusp '(0 1 0))
;;;(consensusp '(0 0 1))

;;;store lists in file

(defun lists->file (lst file &optional (separator " "))
  "(lst file &optional (separator " "))
Save reverse lst in file. Items in flat lst are printed 1 per line.
Separator is used to separate items on a line for embedded lst."
  (with-open-file
      (output-stream file :direction :output :if-exists :supersede)
    (do ((items (reverse lst) (cdr items)))
        ((null items))
      (let ((sub-item (car items)))
        (if (listp sub-item)
            (print-line sub-item separator output-stream)
          (format output-stream "~a ~%"
            sub-item))))))

(defun print-line (lst &optional (separator " ") output-stream)
  "(lst &optional (separator " ") output-stream)
Print each item in list on a line separated by separator. 
Then go to new line."
  (do ((lst lst (cdr lst)))
      ((null lst) (terpri output-stream))
    (princ (car lst) output-stream)
    (princ separator output-stream)))

;;; rotate list-of-lists

(defun rotate (lists)
  "(lists)
Rotate list of lists."
  (apply #'mapcar #'list lists))

;;;(rotate '((1 2 3)
;;;          (4 5 6)))

(defun winners (finishes)
  "(finishes)
Compute winners from finishing counts of x & y."
  (do ((fnshs finishes (cdr fnshs))
       (xwins 0 (if (> (first (car fnshs)) (second (car fnshs)))
                    (1+ xwins)
                  xwins))
       (ywins 0 (if (< (first (car fnshs)) (second (car fnshs)))
                    (1+ ywins)
                  ywins)))
      ((null fnshs) (list xwins ywins))))

;;;(winners '((0 20) (20 0) (20 0) (20 0) (20 0) (0 20) (20 0) (20 0) (20 0) (20 0) (20 0) (20 0) (20 0)
;;;           (20 0) (20 0) (20 0) (20 0) (20 0) (20 0) (20 0)))


;;;call algorithm

(defun call-algorithm (algo p)
  "(algo p)
Call a particular algorithm.
p is probability of copying.
Algorithm is 'am, 'ac, or 'ciu."
  (case algo
    (am (interact-am (get-2-random-agents) p))
    (ac (interact-ac (get-2-random-agents) p))
    (ciu (interact-ciu (get-2-random-agents) p))
    (otherwise (error "Unknown algorithm"))))

;;;run simulation

(defun run1-saturation (nu nx ny p replication algo)
  "(nu nx ny p replication algo)
Run simulation until consensus, recording counts at each cycle.
p is probability of copying.
Algorithm is 'am, 'ac, or 'ciu."
  (initialize-array nu nx ny)
  (make-agent-array nu nx ny)
  (do ((cycles 0 (1+ cycles))
       (ucount (list nu) (cons (count-state 'u) 
                               ucount))
       (xcount (list nx) (cons (count-state 'x) 
                               xcount))
       (ycount (list ny) (cons (count-state 'y) 
                               ycount)))
      ((consensusp (list (first ucount) (first xcount) (first ycount)))
       (progn 
         (lists->file ucount 
                      (concatenate 'string *path* (princ-to-string replication) "ucount"))
         (lists->file xcount 
                      (concatenate 'string *path* (princ-to-string replication) "xcount"))
         (lists->file ycount 
                      (concatenate 'string *path* (princ-to-string replication) "ycount"))
         (list (reverse ucount) 
               (reverse xcount) 
               (reverse ycount) 
               cycles
               (list (first xcount) (first ycount)))))
    (call-algorithm algo p)))

(defun run-saturation (n nu nx ny p algorithm)
  "(n nu nx ny p algorithm)
Run n simulations, returning list of cycles required for consensus.
p is probability of copying.
Algorithm is 'am, 'ac, or 'ciu."
  (seed-random)
  (setq *path* "~/Desktop/SocialImitationPhenomenna/results/")
  (do ((i 0 (1+ i))
       (cycles nil)
       (ucounts nil)
       (xcounts nil)
       (ycounts nil)
       (finishes nil))
      ((= i n) (progn
                 (lists->file 
                  cycles
                  (concatenate 'string *path* "cycles"))
                 (lists->file
                  (reverse (rotate (reverse ucounts)))
                  (concatenate 'string *path* "ucounts"))
                 (lists->file
                  (reverse (rotate (reverse xcounts)))
                  (concatenate 'string *path* "xcounts"))
                 (lists->file
                  (reverse (rotate (reverse ycounts)))
                  (concatenate 'string *path* "ycounts"))
                 (lists->file finishes (concatenate 'string *path* "finishes"))
                 (let ((winners (winners finishes)))
                   (with-open-file
                       (output-stream (concatenate 'string *path* "winners") 
                                      :direction :output)
                     (format output-stream "~a x  ~a y"
                       (first winners) (second winners))))))
    (let ((counts (run1-saturation nu nx ny p i algorithm)))
      (setf 
       ucounts (cons (first counts)
                     ucounts)
       xcounts (cons (second counts)
                     xcounts)
       ycounts (cons (third counts)
                     ycounts)
       cycles (cons (fourth counts)
                    cycles)
       finishes (cons (fifth counts)
                      finishes)))))

;;;(run-saturation 20 13 6 1 1.0 'ac)
;;;(run-saturation 20 13 6 1 1.0 'ciu)  out of memory

;;;(run-saturation 20 98 2 0 1.0 'ac)
;;;(run-saturation 20 98 2 0 1.0 'ciu)

;;;run ac to 570 cycles

(defun run1-to-cycle (nu nx ny max p replication algo)
  "(nu nx ny max p replication algo)
Run simulation until max cycle, record state counts per cycle, & return state counts at max cycle.
p is probability of copying.
Algorithm is 'am, 'ac, or 'ciu."
  (initialize-array nu nx ny)
  (make-agent-array nu nx ny)
  (do ((i 0 (1+ i))
       (ucount (list nu) (cons (count-state 'u) 
                               ucount))
       (xcount (list nx) (cons (count-state 'x) 
                               xcount))
       (ycount (list ny) (cons (count-state 'y) 
                               ycount)))
      ((= i max) (progn 
                   (lists->file ucount 
                                (concatenate 'string *path* (princ-to-string replication) "ucount"))
                   (lists->file xcount 
                                (concatenate 'string *path* (princ-to-string replication) "xcount"))
                   (lists->file ycount 
                                (concatenate 'string *path* (princ-to-string replication) "ycount"))
                   (list (reverse ucount) 
                         (reverse xcount) 
                         (reverse ycount) 
                         (list (first xcount) (first ycount)))))
    (call-algorithm algo p)))

(defun run-to-cycle (n nu nx ny max p algorithm)
  "(n nu nx ny max p algorithm)
Run n simulations to max cycle, recording cycle lists of nx for each simulation 
& simulation x cycle table of nx at end.
p is probability of copying.
Algorithm is 'am, 'ac, or 'ciu."
  (seed-random)
  (setq *path* "~/Desktop/SocialImitationPhenomenna/results/")
  (do ((i 0 (1+ i))
       (ucounts nil)
       (xcounts nil)
       (ycounts nil)
       (finishes nil))
      ((= i n) (progn
                 (lists->file
                  (reverse (rotate (reverse ucounts)))
                  (concatenate 'string *path* "ucounts"))
                 (lists->file
                  (reverse (rotate (reverse xcounts)))
                  (concatenate 'string *path* "xcounts"))
                 (lists->file
                  (reverse (rotate (reverse ycounts)))
                  (concatenate 'string *path* "ycounts"))
                 (lists->file finishes (concatenate 'string *path* "finishes"))
                 (let ((winners (winners finishes)))
                   (with-open-file
                       (output-stream (concatenate 'string *path* "winners") 
                                      :direction :output)
                     (format output-stream "~a x  ~a y"
                       (first winners) (second winners))))))
    (let ((counts (run1-to-cycle nu nx ny max p i algorithm)))
      (setf 
       ucounts (cons (first counts)
                     ucounts)
       xcounts (cons (second counts)
                     xcounts)
       ycounts (cons (third counts)
                     ycounts)
       finishes (cons (fourth counts)
                      finishes)))))

;;;(run-to-cycle 20 13 6 0 17 1.0 'ac)

;;;(run-to-cycle 20 98 2 0 570 1.0 'ac)
;;;(run-to-cycle 20 99 1 0 430 1.0 'ac)

;;;(run-to-cycle 20 98 2 0 500 1.0 'ciu)
;;;(run-to-cycle 20 99 1 0 375 1.0 'ciu)

(defun run1-to-cycle-switch (nu nx ny max p algo)
  "(nu nx ny max p algo)
Run n simulations to max cycle, recording final xcounts for newbie y section.
p is probability of copying.
Algorithm is 'am, 'ac, or 'ciu."
  (initialize-array nu nx ny)
  (make-agent-array nu nx ny)
  (do ((i 0 (1+ i)))
      ((= i max) (list (count-state-section 'u 100 114)
                       (count-state-section 'x 100 114)
                       (count-state-section 'y 100 114)))
    (call-algorithm algo p)))

(defun run-to-cycle-switch (n nu nx ny max p algorithm)
  "(n nu nx ny max p algorithm)
Run n simulations to max cycle, recording xcounts at ends for newbie y section.
p is probability of copying.
Algorithm is 'am, 'ac, or 'ciu."
  (seed-random)
  (setq *path* "~/Desktop/SocialImitationPhenomenna/results/")
  (do ((i 0 (1+ i))
       (counts nil (cons (run1-to-cycle-switch nu nx ny max p algorithm)
                         counts)))
      ((= i n) (lists->file 
                counts
                (concatenate 'string *path* "counts")))))

;;;(run-to-cycle-switch 20 25 75 14 570 1.0 'ac)
;;;(run-to-cycle-switch 20 25 75 14 500 1.0 'ciu)

;;;persist over years

(defun run1-to-cycle-persist (nu nx ny max p replication algo)
  "(nu nx ny max p replication algo)
Run simulation until max cycle, record state counts per cycle, & return state counts at max cycle.
p is probability of copying.
Algorithm is 'am, 'ac, or 'ciu."
  (initialize-array nu nx ny)
  (make-agent-array nu nx ny)
  (do ((i 0 (1+ i))
       (countx-newbies nil (cons (count-state-section 'x 0 70)
                                 countx-newbies))
       (countx-vets nil (cons (count-state-section 'x 70 100) 
                              countx-vets)))
      ((= i max) (progn 
                   (lists->file 
                    countx-newbies
                    (concatenate 'string *path* (princ-to-string replication) "countx-newbies"))
                   (lists->file 
                    countx-vets
                    (concatenate 'string *path* (princ-to-string replication) "countx-vets"))
                   (list (reverse countx-newbies) (reverse countx-vets))))
    (call-algorithm algo p)))

(defun run-to-cycle-persist (n nu nx ny max p algorithm)
  "(n nu nx ny max p algorithm)
Run n simulations to max cycle, recording cycle lists of nx for newbies & vets 
for each simulation & simulation x cycle table of nx at end for newbies & vets.
p is probability of copying. Algorithm is 'am, 'ac, or 'ciu."
  (seed-random)
  (setq *path* "~/Desktop/SocialImitationPhenomenna/results/")
  (do ((i 0 (1+ i))
       (countx-newbies nil)
       (countx-vets nil))
      ((= i n) (progn
                 (lists->file 
                  (reverse (rotate (reverse countx-newbies)))
                  (concatenate 'string *path* "countx-newbies"))
                 (lists->file 
                  (reverse (rotate (reverse countx-vets)))
                  (concatenate 'string *path* "countx-vets"))))
    (let ((countx-both (run1-to-cycle-persist nu nx ny max p i algorithm)))
      (setf 
       countx-newbies (cons (first countx-both)
                            countx-newbies)
       countx-vets (cons (second countx-both)
                         countx-vets)))))
                                 
;;;(run-to-cycle-persist 20 70 30 0 142 1.0 'ac)

;;;run to cycle conform

(defun run1-to-cycle-conform (nu nx ny max p replication algo)
  "(nu nx ny max p replication algo)
Run simulation until max cycle, record state counts per cycle, & return state counts at max cycle.
p is probability of copying.
Algorithm is 'am, 'ac, or 'ciu."
  (initialize-array nu nx ny)
  (make-agent-array nu nx ny)
  (do ((i 0 (1+ i))
       (ucount (list nu) (cons (count-state 'u) 
                               ucount))
       (xcount (list nx) (cons (count-state 'x) 
                               xcount))
       (ycount (list ny) (cons (count-state 'y) 
                               ycount)))
      ((= i max) (progn 
                   (lists->file ucount 
                                (concatenate 'string *path* (princ-to-string replication) "ucount"))
                   (lists->file xcount 
                                (concatenate 'string *path* (princ-to-string replication) "xcount"))
                   (lists->file ycount 
                                (concatenate 'string *path* (princ-to-string replication) "ycount"))
                   (list (reverse ucount) 
                         (reverse xcount) 
                         (reverse ycount) 
                         (list (first xcount) (first ycount)))))
    (call-algorithm algo p)))

(defun run-to-cycle-conform (n nu nx ny max p algorithm)
  "(n nu nx ny max algorithm)
Run n simulations to max cycle, recording cycle lists of state counts for each simulation 
& simulation by cycle table of state counts at end.
p is probability of copying.
Algorithm is 'am, 'ac, or 'ciu."
  (seed-random)
  (setq *path* "~/Desktop/SocialImitationPhenomenna/results/")
  (do ((i 0 (1+ i))
       (ucounts nil)
       (xcounts nil)
       (ycounts nil)
       (finishes nil))
      ((= i n) (progn
                 (lists->file
                  (reverse (rotate (reverse ucounts)))
                  (concatenate 'string *path* "ucounts"))
                 (lists->file
                  (reverse (rotate (reverse xcounts)))
                  (concatenate 'string *path* "xcounts"))
                 (lists->file
                  (reverse (rotate (reverse ycounts)))
                  (concatenate 'string *path* "ycounts"))
                 (lists->file finishes (concatenate 'string *path* "finishes"))
                 (let ((winners (winners finishes)))
                   (with-open-file
                       (output-stream (concatenate 'string *path* "winners") 
                                      :direction :output)
                     (format output-stream "~a x  ~a y"
                       (first winners) (second winners))))))
    (let ((counts (run1-to-cycle-conform nu nx ny max p i algorithm)))
      (setf 
       ucounts (cons (first counts)
                     ucounts)
       xcounts (cons (second counts)
                     xcounts)
       ycounts (cons (third counts)
                     ycounts)
       finishes (cons (fourth counts)
                      finishes)))))

;;;did use
;;;(run-to-cycle-conform 20 91 8 1 400 1.0 'am)
;;;(run-to-cycle-conform 20 91 8 1 400 1.0 'ac)
;;;(run-to-cycle-conform 20 91 8 1 500 1.0 'ciu)

;;;should use
;;;(run-to-cycle-conform 20 91 8 1 400 1.0 'am)
;;;(run-to-cycle-conform 20 91 8 1 430 1.0 'ac)
;;;(run-to-cycle-conform 20 91 8 1 375 1.0 'ciu)
