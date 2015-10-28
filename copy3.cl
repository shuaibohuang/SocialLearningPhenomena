;;;file: copy.cl
;;;purpose: compare 3 copying algorithms: AM, AC, & CIU
;;;programmer: Shuaibo Huang, Tom Shultz

;;;globals

"______________getting region property of agents_________"
(defstruct agent
  "Agent has id index region & state."
  id
  state
  region)

(defvar *agents* nil
  "Array of agents.")

(defvar *array-size* 100
  "Size of agent array.")

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

"_______________Parameters and the way of initiating array are changed_____________________"
"_____nu1 denotes #uncertain in region 1 and nu2 denotes #uncertain in region 2____"
(defun initialize-array (nu1 nx1 ny1 nu2 nx2 ny2)
  "(nu1 nx1 ny1 nu2 nx2 ny2)
Set *array-size* to sum of numbers of agents.
Initialize *agents* array with nil elements."
  (setf *array-size* (+ nu1 nu2 nx1 nx2 ny1 ny2)
    *agents* (make-array *array-size* :initial-element nil)))

;;;(initialize-array 4 1 0)

(defun make-1-agent (i s r)
  "(i s r)
Make or change state of agent i to s in region r."
  (setf (aref *agents* i) 
    (make-agent :id i
                :state s
                :region r)))

;;;(make-1-agent 2 'u)

"________________changes are made on how to initiate the array_________________"
(defun make-agent-array (nu1 nx1 ny1 nu2 nx2 ny2)
  "(nu1 nx1 ny1 nu2 nx2 ny2)
Make agent array."
  (do ((i 0 (1+ i)))
      ((= i nu1))
    (make-1-agent i 'u 1))
  (do ((i nu1 (1+ i)))
      ((= i (+ nu1 nu2)))
    (make-1-agent i 'u 2))
  (do ((i (+ nu1 nu2) (1+ i)))
      ((= i (+ nu1 nu2 nx1)))
    (make-1-agent i 'x 1))
  (do ((i (+ nu1 nu2 nx1) (1+ i)))
      ((= i (+ nu1 nu2 nx1 nx2)))
    (make-1-agent i 'x 2))
  (do ((i (+ nu1 nu2 nx1 nx2) (1+ i)))
      ((= i (+ nu1 nu2 nx1 nx2 ny1)))
    (make-1-agent i 'y 1))
  (do ((i (+ nu1 nu2 nx1 nx2 ny1)(1+ i)))
      ((= i (+ nu1 nu2 nx1 nx2 ny1 ny2)))
    (make-1-agent i 'y 2)))
"_______The returning array will be (nu1 nu2 nx1 nx2 ny1 ny2)_________"
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
"__________________________New function added___________________________________"
(defun get-agent-region (a)
  "(a)
Return region of agent at a"
  (let ((agent (aref *agents* a)))
    (agent-region agent)))


(defun count-state (state region)
  "(state)
Count state across agents."
  (do ((i 0 (1+ i))
       (n 0 (if (and (eq (get-agent-state i) state) (= (get-agent-region i) region))
                (1+ n)
              n)))
      ((= i *array-size*) n)))

;;;(count-state 'u)
;;;(count-state 'x)

"____________this function now counts state in certain region(1 or 2)________"
(defun count-states (states region)
  "(states)
Count each state across agents."
  (do ((stts states (cdr stts))
       (counts nil (cons (count-state (car stts) region)
                         counts)))
      ((null stts) (reverse counts))))

;;;(count-states '(u x y) 1)

"________count in certain region_________________"
(defun count-state-section (state start end region)
  "(state start end region)
Count state for array section from start up to but not including end index."
  (do ((i start (1+ i))
       (n 0 (if (and (eq (get-agent-state i) state) (= (get-agent-region i) region))
                (1+ n)
              n)))
      ((= i end) n)))

;;;(count-state-section 'x 4 5 1)
;;;(count-state-section 'x 4 6 1)
"______________NEW FUNCTION_______________________"
(defun travel (p i)
  "(p i)
Agent i Travel from region one to region two,
Or travel from region two to region one.
P is the probability of traveling"
  (setf i-agent (aref *agents* i))
  (if (< (random 1.0) p)
      (if (= 2 (get-agent-region i))
          (setf (agent-region i-agent) 1)
        (setf (agent-region i-agent) 2)))
  )

;;;interact
"_____only agent from the same region can communicate______"
(defun decide-agent (i r p)
  "(i r p)
Initiator i convinces undecided recipient r in AM."
  (let* ((r-agent (aref *agents* r))
         (i-state (get-agent-state i))
         (r-state (get-agent-state r))
         (i-region (get-agent-region i))
         (r-region (get-agent-region r)))
    (if (and (not (eq i-state 'u))
             (eq r-state 'u)
             (< (random 1.0) p)
             (= i-region r-region))
        (setf (agent-state r-agent) i-state))))
            
;;;(decide-agent 4 2 1.0)
"_____only agent from the same region can communicate______"
(defun confuse-agent (i r p)
  "(i r p)
Initiator i confuses decided recipient r in AM."
  (let* ((r-agent (aref *agents* r))
         (i-state (get-agent-state i))
         (r-state (get-agent-state r))
         (r-region (get-agent-region r))
         (i-region (get-agent-region i)))
    (if (and (not (eq i-state 'u))
             (not (eq r-state 'u))
             (not (eq i-state r-state))
             (< (random 1.0) p)
             (= i-region r-region))
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

"_____only agent from the same region can communicate______"
(defun interact-ac (agents p)
  "(agents p)
Recipient imitates decided initiator in AC."
  (let* ((i (first agents))
         (r (second agents))
         (r-agent (aref *agents* r))
         (i-state (get-agent-state i))
         (r-region (get-agent-region r))
         (i-region (get-agent-region i)))
    (if (and (not (eq i-state 'u))
             (< (random 1.0) p)
             (= i-region r-region))
        (setf (agent-state r-agent) i-state))))

;;;(interact-ac '(4 0) 1.0)

"_____only agent from the same region can communicate______"
(defun interact-ciu (agents p)
  "(agents p)
Undecided recipient imitates decided initiator in CIU."
  (let* ((i (first agents))
         (r (second agents))
         (r-agent (aref *agents* r))
         (i-state (get-agent-state i))
         (r-state (get-agent-state r))
         (r-region (get-agent-region r))
         (i-region (get-agent-region i)))
    (if (and (not (eq i-state 'u))
             (eq r-state 'u)
             (< (random 1.0) p)
             (= r-region i-region))
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
       (xwins1 0 (if (> (first (car fnshs)) (second (car fnshs)))
                    (1+ xwins1)
                  xwins1))
       (ywins1 0 (if (< (first (car fnshs)) (second (car fnshs)))
                    (1+ ywins1)
                   ywins1))
       (xwins2 0 (if (> (third (car fnshs)) (fourth (car fnshs)))
                    (1+ xwins2)
                  xwins2))
       (ywins2 0 (if (< (third (car fnshs)) (fourth (car fnshs)))
                    (1+ ywins2)
                  ywins2)))
      ((null fnshs) (list xwins1 ywins1 xwins2 ywins2))))

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
"_______Because there will be traveling between agents, so it will less likely to be saturations__________"


(defun run1-to-cycle (nu1 nx1 ny1 nu2 nx2 ny2 max p replication algo)
  "(nu nx ny max p replication algo)
Run simulation until max cycle, record state counts per cycle, & return state counts at max cycle.
p is probability of copying.
Algorithm is 'am, 'ac, or 'ciu."
  (initialize-array nu1 nx1 ny1 nu2 nx2 ny2)
  (make-agent-array nu1 nx1 ny1 nu2 nx2 ny2)
  (do ((i 0 (1+ i))
       (ucount1 (list nu1) (cons (count-state 'u 1) 
                               ucount1))
       (xcount1 (list nx1) (cons (count-state 'x 1) 
                               xcount1))
       (ycount1 (list ny1) (cons (count-state 'y 1) 
                                 ycount1))
       (ucount2 (list nu2) (cons (count-state 'u 2) 
                               ucount2))
       (xcount2 (list nx2) (cons (count-state 'x 2) 
                               xcount2))
       (ycount2 (list ny2) (cons (count-state 'y 2) 
                                 ycount2)))
      ((= i max) (progn 
                   (lists->file ucount1 
                                (concatenate 'string *path* (princ-to-string replication) "ucount1"))
                   (lists->file xcount1 
                                (concatenate 'string *path* (princ-to-string replication) "xcount1"))
                   (lists->file ycount1 
                                (concatenate 'string *path* (princ-to-string replication) "ycount1"))
                   (lists->file ucount2 
                                (concatenate 'string *path* (princ-to-string replication) "ucount2"))
                   (lists->file xcount2 
                                (concatenate 'string *path* (princ-to-string replication) "xcount2"))
                   (lists->file ycount2 
                                (concatenate 'string *path* (princ-to-string replication) "ycount2"))
                   (list (reverse ucount1) 
                         (reverse xcount1) 
                         (reverse ycount1)
                         (reverse ucount2) 
                         (reverse xcount2) 
                         (reverse ycount2) 
                         (list (first xcount1) (first ycount1) (first xcount2) (first ycount2)))))
    (travel 1 (get-random-agent))
    (call-algorithm algo p)))

(defun run-to-cycle (n nu1 nx1 ny1 nu2 nx2 ny2 max p algorithm)
  "(n nu nx ny max p algorithm)
Run n simulations to max cycle, recording cycle lists of nx for each simulation 
& simulation x cycle table of nx at end.
p is probability of copying.
Algorithm is 'am, 'ac, or 'ciu."
  (seed-random)
  (setq *path* "~/Desktop/SocialImitationPhenomenna/results/")
  (do ((i 0 (1+ i))
       (ucounts1 nil)
       (xcounts1 nil)
       (ycounts1 nil)
       (ucounts2 nil)
       (xcounts2 nil)
       (ycounts2 nil)
       (finishes nil))
      ((= i n) (progn
                 (lists->file
                  (reverse (rotate (reverse ucounts1)))
                  (concatenate 'string *path* "ucounts1"))
                 (lists->file
                  (reverse (rotate (reverse xcounts1)))
                  (concatenate 'string *path* "xcounts1"))
                 (lists->file
                  (reverse (rotate (reverse ycounts1)))
                  (concatenate 'string *path* "ycounts1"))
                 (lists->file
                  (reverse (rotate (reverse ucounts2)))
                  (concatenate 'string *path* "ucounts2"))
                 (lists->file
                  (reverse (rotate (reverse xcounts2)))
                  (concatenate 'string *path* "xcounts2"))
                 (lists->file
                  (reverse (rotate (reverse ycounts2)))
                  (concatenate 'string *path* "ycounts2"))
                 (lists->file finishes (concatenate 'string *path* "finishes"))
                 (let ((winners (winners finishes)))
                   (with-open-file
                       (output-stream (concatenate 'string *path* "winners") 
                                      :direction :output)
                     (format output-stream "~a x1  ~a y1 ~a x2 ~a y2"
                       (first winners) (second winners) (third winners) (fourth winners))))))
    (let ((counts (run1-to-cycle nu1 nx1 ny1 nu2 nx2 ny2 max p i algorithm)))
      (setf 
       ucounts1 (cons (first counts)
                     ucounts1)
       xcounts1 (cons (second counts)
                     xcounts1)
       ycounts1 (cons (third counts)
                     ycounts1)
       ucounts2 (cons (fourth counts)
                     ucounts2)
       xcounts2 (cons (fifth counts)
                     xcounts2)
       ycounts2 (cons (sixth counts)
                     ycounts2)
       finishes (cons (seventh counts)
                      finishes)))))

;;;(run-to-cycle 20 13 6 0 17 1.0 'ac)

;;;(run-to-cycle 20 98 2 0 570 1.0 'ac)
;;;(run-to-cycle 20 99 1 0 430 1.0 'ac)

;;;(run-to-cycle 20 98 2 0 500 1.0 'ciu)
;;;(run-to-cycle 20 99 1 0 375 1.0 'ciu)
