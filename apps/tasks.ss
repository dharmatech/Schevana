;;; apps/tasks.ss — a small jira-style task manager: a board and a list of
;;; tasks, their details, a search and filters.
;;;
;;; The tasks live on the heap (no files yet) and are live Scheme values;
;;; the open window shows every change at once:
;;;
;;;   (tasks-list 'status 'doing)        (find-task 12)
;;;   (add-task! "Fix the menu" 'priority 'high 'tags '("ui"))
;;;   (set-task! 12 'status 'review)     (move-task! 12 'done)
;;;   (delete-task! 12)                  (tasks-reset!)
;;;
;;; Keys, with no field focused: n new task, / search, v Board or List,
;;; arrows move the selection and Shift+arrows the task, Enter edits the
;;; title, Delete deletes, Escape leaves a field, then the task.

;;; ------------------------------------------------------------------ data

(define-record-type task
  (nongenerative schevana-task-1)
  (fields id                       ; 12, shown as TASK-12
          (mutable title)          ; one line; "" shows as "Untitled"
          (mutable description)    ; may hold newlines
          (mutable status)         ; todo doing review done
          (mutable kind)           ; bug feature chore
          (mutable priority)       ; urgent high normal low
          (mutable assignee)       ; a name, or #f
          (mutable tags)           ; tasks-tag strings as added, at most 64
          (mutable due)            ; (year month day), or #f
          (mutable checklist)      ; (("Write the test" . #f) ...)
          created                  ; seconds since the epoch
          (mutable updated)))

(record-writer (record-type-descriptor task)
  (lambda (t p wr)
    (fprintf p "#<task ~a ~a ~a ~s>" (task-label t) (task-status t)
             (task-priority t) (task-title t))))

;; In reading order; sorting "up" follows it.
(define tasks-statuses
  '((todo . "To do") (doing . "In progress") (review . "Review")
    (done . "Done")))
(define tasks-kinds '((bug . "Bug") (feature . "Feature") (chore . "Chore")))
(define tasks-priorities
  '((urgent . "Urgent") (high . "High") (normal . "Normal") (low . "Low")))

(defvar *tasks* #f)          ; every task, in board order
(defvar *tasks-next-id* 1)   ; ids are never reused

(define (task-label t) (format "TASK-~a" (task-id t)))

(define (tasks-now) (time-second (current-time)))

;; s without white space at its ends, or only at its start: (tasks-trim s
;; 'left).
(define (tasks-trim s . left)
  (let loop ([i 0] [j (string-length s)])
    (cond [(and (< i j) (char-whitespace? (string-ref s i))) (loop (+ i 1) j)]
          [(and (null? left) (< i j)
                (char-whitespace? (string-ref s (- j 1))))
           (loop i (- j 1))]
          [else (substring s i j)])))

(define (tasks-date? d)
  (and (list? d) (= 3 (length d)) (for-all fixnum? d)
       (<= 1 (cadr d) 12) (<= 1 (caddr d) (ui-days-in-month (car d) (cadr d)))))

;; The fields that the REPL and the window set, and their setters.
(define tasks-setters
  `((title . ,task-title-set!) (description . ,task-description-set!)
    (status . ,task-status-set!) (kind . ,task-kind-set!)
    (priority . ,task-priority-set!) (assignee . ,task-assignee-set!)
    (tags . ,task-tags-set!) (due . ,task-due-set!)
    (checklist . ,task-checklist-set!)))

;; value checked and made what field key holds, or an error that lists
;; the valid choices.
(define (tasks-normalize who key value)
  (define (names alist)
    (string-join (map (lambda (c) (symbol->string (car c))) alist) " "))
  (define (one-of choices)
    (if (assq value choices)
        value
        (errorf who "~a must be one of ~a, not ~s" key (names choices) value)))
  (define (text)
    (if (string? value) value (errorf who "~a must be a string, not ~s" key
                                      value)))
  (case key
    [(title) (ui-text-clean (text) #f)]
    [(description) (ui-text-clean (text) #t)]
    [(status) (one-of tasks-statuses)]
    [(kind) (one-of tasks-kinds)]
    [(priority) (one-of tasks-priorities)]
    [(assignee)
     (unless (or (not value) (string? value))
       (errorf who "assignee must be a name or #f, not ~s" value))
     (let ([s (and value (ui-text-clean value #f))])
       (and s (not (string=? (tasks-trim s) ""))
            ;; the details' field types a name a letter at a time, so a
            ;; space after it stays there, for the next word; at most 64
            ;; characters, so the draws that list the names stay cheap
            (let ([t (if (eq? who 'task-set!)
                         (tasks-trim s 'left)
                         (tasks-trim s))])
              (substring t 0 (min 64 (string-length t))))))]
    [(tags)
     (unless (and (list? value) (for-all string? value))
       (errorf who "tags must be a list of strings, not ~s" value))
     ;; each once, in order, at most 64: a pasted log makes a few tags,
     ;; not thousands for every draw to lay out
     (let loop ([l value] [acc '()] [n 0])
       (cond [(or (null? l) (= n 64)) (reverse acc)]
             [(tasks-tag (car l))
              => (lambda (g)
                   (if (member g acc)
                       (loop (cdr l) acc n)
                       (loop (cdr l) (cons g acc) (+ n 1))))]
             [else (loop (cdr l) acc n)]))]
    [(due)
     (cond [(not value) #f]
           [(tasks-date? value) value]
           [else (errorf who "due must be (year month day) or #f, not ~s"
                         value)])]
    [(checklist)
     (unless (list? value)
       (errorf who "checklist must be a list, not ~s" value))
     (map (lambda (i)
            (cond [(string? i) (cons i #f)]
                  [(and (pair? i) (string? (car i)))
                   (cons (car i) (and (cdr i) #t))]
                  [else (errorf who "a checklist item is a string or \
                                     (text . done?), not ~s" i)]))
          value)]
    [else (errorf who "~s is not a field; the fields are ~a" key
                  (names tasks-setters))]))

;; Sets fields, a property list, of t (all checked first) and stamps it.
;; A new status moves it to the end of that column.
(define (tasks-update! who t fields)
  (let ([pairs (let loop ([f fields] [acc '()])
                 (cond [(null? f) (reverse acc)]
                       [(null? (cdr f)) (errorf who "~s has no value" (car f))]
                       [else (loop (cddr f)
                                   (cons (cons (car f)
                                               (tasks-normalize who (car f)
                                                                (cadr f)))
                                         acc))]))])
    (for-each
     (lambda (p)
       (let ([key (car p)] [v (cdr p)])
         (when (and (eq? key 'status) (not (eq? v (task-status t)))
                    (memq t *tasks*))
           (set! *tasks* (tasks-move *tasks* t v #f)))
         ((cdr (assq key tasks-setters)) t v)))
     pairs)
    (task-updated-set! t (tasks-now))
    (tasks-changed!)
    t))

;; The window's edits: one field of t, whose card is then kept in view.
(define (task-set! v t key value)
  (tasks-update! 'task-set! t (list key value))
  (tasks-view-reveal?-set! v #t))

;; A task with the next id, at the end of its column.
(define (tasks-new! . fields)
  (let* ([now (tasks-now)]
         [t (make-task *tasks-next-id* "" "" 'todo 'feature 'normal #f '() #f
                       '() now now)])
    (tasks-update! 'add-task! t fields)
    (set! *tasks-next-id* (+ *tasks-next-id* 1))
    (set! *tasks* (tasks-move *tasks* t (task-status t) #f))
    (tasks-changed!)
    t))

;; t to status, before task before (#f: the column's end).
(define (tasks-place! t status before)
  (set! *tasks* (tasks-move *tasks* t status before))
  (task-status-set! t status)
  (task-updated-set! t (tasks-now))
  (tasks-changed!))

;; A copy with a new id, right after t.
(define (tasks-duplicate! t)
  (let* ([now (tasks-now)]
         [c (make-task *tasks-next-id* (string-append (task-title t) " (copy)")
                       (task-description t) (task-status t) (task-kind t)
                       (task-priority t) (task-assignee t) (task-tags t)
                       (task-due t) (task-checklist t) now now)])
    (set! *tasks-next-id* (+ *tasks-next-id* 1))
    (set! *tasks* (let loop ([l *tasks*])
                    (cond [(null? l) (list c)]
                          [(eq? (car l) t) (cons* t c (cdr l))]
                          [else (cons (car l) (loop (cdr l)))])))
    (tasks-changed!)
    c))

(define (tasks-remove! t)
  (set! *tasks* (remq t *tasks*))
  (tasks-changed!))

;; The open window shows the heap as it is now; what it held of a task
;; that is gone, it lets go.
(define (tasks-changed!)
  (let ([win (find-window 'tasks)]
        [gone? (lambda (t) (and t (not (memq t *tasks*))))])
    (when win
      (let ([v (window-ref win 'state #f)])
        (when (tasks-view? v)
          (when (gone? (tasks-view-selected v)) (tasks-view-selected-set! v #f))
          (when (gone? (tasks-view-confirm v)) (tasks-view-confirm-set! v #f))
          (let ([d (tasks-view-drag v)])
            (when (and d (gone? (tasks-drag-task d)))
              (tasks-view-drag-set! v #f)))
          (ui-stale! (tasks-view-ui v))))
      (invalidate! win))))

;;; ------------------------------------------------------------------ pure

(define (tasks-rank choices v)
  (list-index (lambda (c) (eq? (car c) v)) choices))

;; Where t is in ts, or #f.
(define (tasks-index t ts) (list-index (lambda (x) (eq? x t)) ts))

;; The tasks of ts in the column of status.
(define (tasks-in status ts)
  (filter (lambda (t) (eq? (task-status t) status)) ts))

;; A dropdown's choices ((label . value) ...) from an alist ((value .
;; label) ...).
(define (tasks-choices alist) (map (lambda (c) (cons (cdr c) (car c))) alist))

;; The search's lowercase words, each once, at most 16 and each cut to 64
;; characters: a pasted page searches by its first words, not by thousands
;; each over a long title, and no word costs a long title more than 64
;; compares a character.
(define (tasks-words query)
  (let loop ([ws (map (lambda (w) (substring w 0 (min 64 (string-length w))))
                      (string-split (string-downcase query) #\space))]
             [acc '()])
    (cond [(or (null? ws) (= (length acc) 16)) (reverse acc)]
          [(or (string=? (car ws) "") (member (car ws) acc))
           (loop (cdr ws) acc)]
          [else (loop (cdr ws) (cons (car ws) acc))])))

;; Is a the name b, ignoring case and spaces around it?
(define (tasks-same-name? a b) (string-ci=? (tasks-trim a) (tasks-trim b)))

;; The assignees of ts, each once (the first spelling, trimmed), sorted.
(define (tasks-names ts)
  (sort string-ci<?
        (fold-left (lambda (acc t)
                     (let ([a (task-assignee t)])
                       (if (and a (not (exists (lambda (b)
                                                 (tasks-same-name? a b))
                                               acc)))
                           (cons (tasks-trim a) acc)
                           acc)))
                   '() ts)))

;; The tags of ts, each once, sorted.
(define (tasks-tags ts)
  (sort string<?
        (fold-left (lambda (acc t)
                     (fold-left (lambda (acc g)
                                  (if (member g acc) acc (cons g acc)))
                                acc (task-tags t)))
                   '() ts)))

;; Every word is found, ignoring case, in "task-12", the title, the
;; description, the assignee or a tag; each filter is #f for any, and
;; assignee none means unassigned. Without done?, done tasks are out.
(define (tasks-match? t words kind priority assignee tag done?)
  (and (or done? (not (eq? (task-status t) 'done)))
       (or (not kind) (eq? (task-kind t) kind))
       (or (not priority) (eq? (task-priority t) priority))
       (cond [(not assignee) #t]
             [(eq? assignee 'none) (not (task-assignee t))]
             [else (and (task-assignee t)
                        (tasks-same-name? (task-assignee t) assignee))])
       (or (not tag) (member tag (task-tags t)))
       (let ([hay (cons* (task-label t) (task-title t) (task-description t)
                         (or (task-assignee t) "") (task-tags t))])
         (for-all (lambda (w)
                    (exists (lambda (s) (string-contains-ci? s w)) hay))
                  words))))

;; Stable. key: board id title status kind priority assignee due updated;
;; board is status order, keeping list order. Enums go in their lists'
;; order; no assignee or due date sorts last either way. dir: up or down.
(define (tasks-sorted ts key dir)
  (define (value t)
    (case key
      [(board status) (tasks-rank tasks-statuses (task-status t))]
      [(id) (task-id t)]
      [(title) (string-downcase (task-title t))]
      [(kind) (tasks-rank tasks-kinds (task-kind t))]
      [(priority) (tasks-rank tasks-priorities (task-priority t))]
      [(assignee) (and (task-assignee t) (string-downcase (task-assignee t)))]
      [(due) (and (task-due t) (ui-date->day (task-due t)))]
      [(updated) (task-updated t)]
      [else (errorf 'tasks-sorted "no key ~s" key)]))
  (define (before? a b) (if (string? a) (string<? a b) (< a b)))
  (list-sort (lambda (x y)
               (let ([a (value x)] [b (value y)])
                 (cond [(not a) #f]
                       [(not b) #t]
                       [(eq? dir 'up) (before? a b)]
                       [else (before? b a)])))
             ts))

;; ts with t before task before, or after the last task of status when
;; before is #f (at the end when there is none).
(define (tasks-move ts t status before)
  (let* ([rest (remq t ts)]
         [i (or (tasks-index before rest)
                (let loop ([l rest] [i 0] [last #f])
                  (cond [(null? l) (if last (+ last 1) i)]
                        [(eq? (task-status (car l)) status)
                         (loop (cdr l) (+ i 1) i)]
                        [else (loop (cdr l) (+ i 1) last)])))])
    (append (list-head rest i) (list t) (list-tail rest i))))

;; The drop slot: the first card whose middle is below py, or the count.
(define (tasks-slot tops heights py)
  (let loop ([ts tops] [hs heights] [i 0])
    (cond [(null? ts) i]
          [(> (+ (car ts) (/ (car hs) 2)) py) i]
          [else (loop (cdr ts) (cdr hs) (+ i 1))])))

;; The task after t in ts, else the one before, else #f.
(define (tasks-neighbor ts t)
  (let ([tail (memq t ts)])
    (cond [(not tail) #f]
          [(pair? (cdr tail)) (cadr tail)]
          [else (let loop ([l ts] [prev #f])
                  (if (eq? (car l) t) prev (loop (cdr l) (car l))))])))

;; Trimmed, lowercase, spaces become -, no commas, at most 24 characters;
;; #f when nothing is left.
(define (tasks-tag str)
  (let* ([s (list->string
             (filter-map (lambda (c)
                           (cond [(char=? c #\,) #f]
                                 [(char-whitespace? c) #\-]
                                 [else c]))
                         (string->list (string-downcase (tasks-trim str)))))]
         ;; at most 24 characters; a cut never ends in "-"
         [n (let cut ([n (min 24 (string-length s))])
              (if (and (< 0 n (string-length s))
                       (char=? (string-ref s (- n 1)) #\-))
                  (cut (- n 1))
                  n))])
    (and (> n 0) (substring s 0 n))))

;; "Pavel Uvarov" → "PU", "Claude" → "C".
(define (tasks-initials name)
  (let ([words (remp (lambda (w) (string=? w "")) (string-split name #\space))])
    (if (null? words)
        "?"
        (list->string
         (map (lambda (w) (char-upcase (string-ref w 0)))
              (if (null? (cdr words))
                  words
                  (list (car words) (car (last-pair words)))))))))

(define (tasks-overdue? t today)
  (and (task-due t) (not (eq? (task-status t) 'done))
       (< (ui-date->day (task-due t)) (ui-date->day today))))

;; "Today", "Tomorrow", "Yesterday", "Nov 15", or "Nov 15, 2027" in
;; another year.
(define (tasks-due-label due today)
  (case (- (ui-date->day due) (ui-date->day today))
    [(0) "Today"]
    [(1) "Tomorrow"]
    [(-1) "Yesterday"]
    [else (if (= (car due) (car today))
              (format "~a ~a" (vector-ref month-names (- (cadr due) 1))
                      (caddr due))
              (ui-date-label due))]))

;; "Oct 31, 14:02", local time.
(define (tasks-time-label secs)
  (let ([d (time-utc->date (make-time 'time-utc 0 secs))])
    (format "~a ~a, ~2,'0d:~2,'0d" (vector-ref month-names (- (date-month d) 1))
            (date-day d) (date-hour d) (date-minute d))))

;;; ------------------------------------------------------------------ REPL

;; The task x names: a task, an id (12), "TASK-12" or a piece of its title
;; (the first match); #f when none does, or x is blank.
(define (find-task x)
  (cond [(task? x) (and (memq x *tasks*) x)]
        [(fixnum? x) (find (lambda (t) (= (task-id t) x)) *tasks*)]
        [(string? x)
         (let ([n (and (string-prefix? "task-" (string-downcase x))
                       (string->number (substring x 5 (string-length x))))])
           (cond [(fixnum? n) (find-task n)]
                 [(string=? (tasks-trim x) "") #f]
                 [else (find (lambda (t) (string-contains-ci? (task-title t) x))
                             *tasks*)]))]
        [else #f]))

(define (tasks-get who x) (or (find-task x) (errorf who "no task ~s" x)))

;; Every task, in board order. filters: status kind priority assignee tag
;; search, e.g. (tasks-list 'status 'doing 'assignee "Pavel").
(define (tasks-list . filters)
  (let ([f (lambda (k) (plist-ref filters k #f))])
    (for-each (lambda (k)
                (unless (memq k '(status kind priority assignee tag search))
                  (errorf 'tasks-list "~s is not a filter; the filters are \
                                       status kind priority assignee tag \
                                       search" k)))
              (plist-keys filters))
    (unless (or (string? (f 'assignee)) (memq (f 'assignee) '(#f none)))
      (errorf 'tasks-list "assignee must be a name or none, not ~s"
              (f 'assignee)))
    (let* ([enum (lambda (k)
                   (and (f k) (tasks-normalize 'tasks-list k (f k))))]
           [status (enum 'status)] [kind (enum 'kind)]
           [priority (enum 'priority)]
           [tag (and (f 'tag) (tasks-tag (f 'tag)))]
           [words (tasks-words (or (f 'search) ""))])
      (filter (lambda (t)
                (and (or (not status) (eq? (task-status t) status))
                     (tasks-match? t words kind priority (f 'assignee) tag #t)))
              (tasks-sorted *tasks* 'board 'up)))))

;; (add-task! "Fix the menu" 'priority 'high 'kind 'bug 'tags '("ui")
;;            'due '(2026 11 15) 'assignee "Claude" 'status 'todo
;;            'description "..." 'checklist '("Reproduce" "Fix"))
(define (add-task! title . fields) (apply tasks-new! 'title title fields))

;; (set-task! 12 'status 'review 'priority 'urgent): any fields.
(define (set-task! x . fields)
  (tasks-update! 'set-task! (tasks-get 'set-task! x) fields))

;; (move-task! 12 'done): to the end of a column; (move-task! 12 'todo 7):
;; before TASK-7.
(define (move-task! x status . before)
  (let ([t (tasks-get 'move-task! x)]
        [s (tasks-normalize 'move-task! 'status status)]
        [b (and (pair? before) (tasks-get 'move-task! (car before)))])
    (tasks-place! t s b)
    t))

;; #t if it was there.
(define (delete-task! x)
  (let ([t (find-task x)]) (and t (begin (tasks-remove! t) #t))))

;; Schevana's own backlog, board order within each column: (id status
;; kind priority assignee tags due checklist title description); due in
;; days from today, a checklist item done when paired with #t.
(define tasks-seed
  '((16 todo bug high "Claude" ("perf" "render") -2 ()
     "Theme change takes 67 ms at 200%"
     "Every theme change paints the wallpaper again, so dragging a color \
      slider in Settings shows about 15 frames a second at 200%.")
    (11 todo feature high #f ("style" "kit") 21
     ("List every drawing procedure" "Parts and derived styles"
      "Port Settings" "Restyle Tasks under every style")
     "Style system: one look for windows, bars, menus and widgets"
     "Styles change only the windows today. One look should reach the top \
      bar, the taskbar, menus, toasts and every widget, and a derived style \
      should take a few lines.")
    (12 todo feature normal "Pavel" ("persistence") #f ()
     "Persistence: keep tasks, windows and app state across restarts"
     "No persistence yet: everything lives on the heap. fasl-write of app \
      state is a natural first step.")
    (13 todo feature normal #f ("render" "gpu") #f
     ("Each window in its own image" "Compose on the CPU" "Compose on the GPU"
      "(set-compositor! 'gpu) at the REPL")
     "GPU composer: per-window images composed on the GPU"
     "Every window drawn into an image of its own, only when it changes, and \
      the images composed: on the CPU first, then on the GPU.")
    (14 todo feature normal "Claude" ("apps") #f ()
     "Inspector app: browse any live value"
     "Click through records, hashtables, procedures and their sources, \
      windows and processes.")
    (22 todo feature normal #f ("apps") #f ()
     "AI agent app that reads window contents"
     "An assistant in a window that reads what the other windows show and \
      acts through the REPL.")
    (15 todo feature normal #f ("repl") #f ()
     "REPL: selection and copy"
     "The transcript cannot be selected or copied yet. The kit's text \
      editing could serve it.")
    (17 todo bug normal #f ("perf" "render") #f ()
     "Menus and toasts redraw the whole screen"
     "While a menu or a toast shows, every frame is the whole screen; their \
      own extents would be enough.")
    (18 todo bug low #f ("perf") #f ()
     "Tiled windows redraw with the clock every second"
     "Windows whose extent reaches into the top bar are drawn again with the \
      clock: 30 tiled Life windows take 7.5 ms a second at 200%.")
    (19 todo feature low #f ("apps" "threads") #f ()
     "Fractal explorer"
     "Deep zoom into the Mandelbrot set on every core, refined \
      progressively, with a flight mode and a Julia inset.")
    (20 todo feature low #f ("apps" "threads") #f ()
     "Smoke: stable fluids you push with the mouse"
     "A stable-fluids solver on all cores; drag to push the smoke around.")
    (21 todo feature low #f ("apps" "threads") #f ()
     "Galaxies on all cores"
     "Thousands of stars pulling at each other, the forces computed in \
      parallel.")
    (26 todo feature low #f ("apps") #f ()
     "A text editor app"
     "Open, edit and save files in a window, on the kit's text area.")
    (23 todo chore low #f ("portability") #f ()
     "Try the macOS and Windows libraries"
     "Their names are in ffi.ss but have not been tried. macOS needs the \
      loop on the main thread, which it already is.")
    (24 todo feature low #f ("repl") #f ()
     "Namespaces for apps"
     "Apps all share one environment. REPLs can be private or pure; apps \
      could be kept apart the same way.")
    ;; shadow{
    (25 todo chore low "Pavel" ("style") #f ()
     "Decide about window shadows"
     "Shadows are on probation, isolated and removable. Keep them, or take \
      them out?")
    ;; shadow}
    (9 doing feature high "Claude" ("apps" "kit") 5
     (("Focus and Tab" . #t) ("Text editing" . #t) "Dropdowns and the calendar"
      "Drag and drop" "Tests and screenshots")
     "Tasks app and its widget kit"
     "A task manager that gathers the widgets the style system must cover: \
      text fields, dropdowns, tables, chips, a calendar, dialogs and \
      tooltips.")
    (10 doing feature high "Pavel" ("threads") 14 ()
     "Threads spec: worker pool, jobs, parallel-for"
     "Jobs on a pool of workers, results handed back to the main thread, \
      pixmaps filled in parallel. Threads only when measured.")
    (7 review chore urgent "Pavel" ("sdl" "host") 1
     ("HiDPI at 200% on GNOME" "Typing with an IME"
      ("Clipboard both ways" . #t) "Full screen and back"
      ("Resize while drawing" . #t))
     "Check SDL3 in a real window"
     "Everything SDL3 does was tried against a stand-in. Try it on a real \
      desktop before trusting it.")
    (8 review chore low "Pavel" ("style" "genera") #f ()
     "Match Genera's margins to the 1986 manuals"
     "Check the margins and the label in the bottom-left corner against the \
      manuals' figures.")
    (1 done chore normal "Claude" ("sdl" "host") #f ()
     "Switch from SDL2 to SDL3"
     "Fractional wheel steps add up to whole ones, and the app id names the \
      window for the desktop.")
    (2 done feature normal "Claude" ("style") #f ()
     "Add nine window styles"
     "Cons, Curses, Draft, Fold, Halo, Lambda, Reticle, Slab and Spine, one \
      file each in styles/.")
    (3 done feature high "Claude" ("render" "perf") #f ()
     "Redraw only what changed"
     "A change says where it shows; a frame draws only that box, compares \
      its rows with the screen and uploads the ones that differ.")
    (4 done chore normal "Claude" ("structure") #f ()
     "Split the world into folders, at 80 columns"
     "Kits, styles, themes, wallpapers and apps are folders, one definition \
      per file, loaded in file-name order.")
    (5 done feature high "Claude" ("render") #f ()
     "HiDPI: draw from outlines at any scale"
     "The canvas is drawn at the scale with one cairo-scale, so text and \
      shapes stay sharp at 125, 150 or 200%.")
    (6 done feature normal "Claude" ("repl") #f ()
     "REPL completion with docs"
     "Tab lists the names that fit where the cursor is, with each one's \
      arguments and the comment above its definition.")))

;; The seed backlog again: created over the last three weeks, the done
;; ones first.
(define (tasks-reset!)
  (let ([now (tasks-now)] [today (ui-date->day (ui-today))] [day 86400])
    (set! *tasks*
      (map (lambda (s)
             (apply (lambda (id status kind priority who tags due checklist
                                title description)
                      (let ([created (+ (- now (* 21 day)) (* id 66000))])
                        (make-task id title description status kind priority
                                   who tags
                                   (and due (ui-day->date (+ today due)))
                                   (tasks-normalize 'tasks-reset! 'checklist
                                                    checklist)
                                   created
                                   (min now (+ created
                                               (* 3600
                                                  (+ 1 (mod (* id 7) 17))))))))
                    s))
           tasks-seed))
    (set! *tasks-next-id* (+ 1 (apply max (map car tasks-seed))))
    (tasks-changed!)))

;;; ------------------------------------------------------------------ view

(define-record-type tasks-view
  (nongenerative schevana-tasks-view-1)
  (fields ui
          (mutable mode)        ; board | list
          (mutable query)       ; the search text
          (mutable kind) (mutable priority) (mutable assignee)
          (mutable tag)         ; filters: a value, or #f for any
                                ; (assignee: a name, none, or #f)
          (mutable done?)       ; Show done
          (mutable sort)        ; (key . up|down), or #f: board order
          (mutable selected)    ; a task, or #f
          (mutable page?)       ; narrow windows: the details page shows
          (mutable confirm)     ; the task the dialog would delete, or #f
          (mutable drag)        ; a tasks-drag, or #f
          (mutable reveal?)     ; scroll the selection into view next draw
          (mutable tag-draft)   ; typed in the tags' field
          (mutable item-draft))) ; typed in "Add an item…"

(define-record-type tasks-drag
  (nongenerative schevana-tasks-drag-1)
  (fields task px py gx gy   ; pressed at (px, py), grabbed at (gx, gy)
          (mutable x) (mutable y)       ; the pointer now
          (mutable moving?)             ; moved more than 4 px
          (mutable status) (mutable before)))  ; the slot of the last draw;
                                               ; status #f: no target

(define (tasks-view-new ui)
  (make-tasks-view ui 'board "" #f #f #f #f #t #f #f #f #f #f #f "" ""))

(define (tasks-visible v)
  (let ([words (tasks-words (tasks-view-query v))])
    (filter (lambda (t)
              (tasks-match? t words (tasks-view-kind v) (tasks-view-priority v)
                            (tasks-view-assignee v) (tasks-view-tag v)
                            (tasks-view-done? v)))
            *tasks*)))

(define (tasks-shown-statuses v)
  (if (tasks-view-done? v)
      tasks-statuses
      (remp (lambda (s) (eq? (car s) 'done)) tasks-statuses)))

;; ((status . tasks) ...) of the board.
(define (tasks-columns v visible)
  (map (lambda (s) (cons (car s) (tasks-in (car s) visible)))
       (tasks-shown-statuses v)))

;; The visible tasks in the order shown: the List's sort, else board order.
(define (tasks-rows v visible)
  (let ([sort (tasks-view-sort v)])
    (if (and sort (eq? (tasks-view-mode v) 'list))
        (tasks-sorted visible (car sort) (cdr sort))
        (tasks-sorted visible 'board 'up))))

(define (tasks-filtered? v)
  (or (not (string=? (tasks-view-query v) "")) (tasks-view-kind v)
      (tasks-view-priority v) (tasks-view-assignee v) (tasks-view-tag v)
      (not (tasks-view-done? v))))

(define (tasks-clear-filters! v)
  (tasks-view-query-set! v "")
  (tasks-view-kind-set! v #f)
  (tasks-view-priority-set! v #f)
  (tasks-view-assignee-set! v #f)
  (tasks-view-tag-set! v #f)
  (tasks-view-done?-set! v #t)
  (tasks-view-reveal?-set! v #t))

(define (tasks-select! v t)
  (let ([ui (tasks-view-ui v)])
    (unless (eq? t (tasks-view-selected v))   ; another task, from its top
      (tasks-view-tag-draft-set! v "")
      (tasks-view-item-draft-set! v "")
      (for-each (lambda (id) (ui-forget! ui id)) '(details title description)))
    (tasks-view-selected-set! v t)
    (tasks-view-reveal?-set! v #t)
    (ui-focus! ui #f)))

;; In a window too narrow for the details beside the view?
(define (tasks-narrow? v)
  (let-values ([(w h) (window-content-size (current-window))])
    (let-values ([(mode view details) (tasks-layout v w h 0)])
      (eq? mode 'narrow))))

(define (tasks-edit-title! v)
  (when (tasks-narrow? v) (tasks-view-page?-set! v #t))
  (ui-focus! (tasks-view-ui v) 'title))

;; A new untitled task at the end of a column, with the filters' values,
;; selected and its title focused; the search is cleared so it shows.
(define (tasks-new-task! v status)
  (let ([t (tasks-new! 'status status
                       'kind (or (tasks-view-kind v) 'feature)
                       'priority (or (tasks-view-priority v) 'normal)
                       'assignee (let ([a (tasks-view-assignee v)])
                                   (and (string? a) a))
                       'tags (let ([g (tasks-view-tag v)])
                               (if g (list g) '())))])
    (tasks-view-query-set! v "")
    (tasks-select! v t)
    (tasks-edit-title! v)
    t))

;; The tasks shown beside t: its column on the Board, the rows of the List.
(define (tasks-peers v t)
  (let ([visible (tasks-visible v)])
    (if (eq? (tasks-view-mode v) 'board)
        (tasks-in (task-status t) visible)
        (tasks-rows v visible))))

;; Deletes t and selects the task next to it.
(define (tasks-delete! v t)
  (let ([next (tasks-neighbor (tasks-peers v t) t)])
    (tasks-view-confirm-set! v #f)
    (tasks-remove! t)
    (tasks-select! v next)))

(define (tasks-menu v t x y)
  (ui-open-menu! (tasks-view-ui v) x y
    (append
     (list (cons "Open" (lambda () (tasks-select! v t) (tasks-edit-title! v)))
           'separator)
     (map (lambda (s)
            (cons (string-append "Move to " (cdr s))
                  (lambda () (tasks-move-to! v t (car s) #f))))
          (remp (lambda (s) (eq? (car s) (task-status t))) tasks-statuses))
     (list 'separator
           (cons "Duplicate" (lambda () (tasks-select! v (tasks-duplicate! t))))
           'separator
           (cons "Delete…" (lambda () (tasks-view-confirm-set! v t)))))))

;;; ------------------------------------------------------------------ sizes

(define (tasks-details-width) (max 340 (round (* 23 (font-line-height 'ui)))))
(define (tasks-column-min-width)
  (max 160 (round (* 11 (font-line-height 'ui)))))
(define (tasks-column-width) (max 200 (round (* 27/2 (font-line-height 'ui)))))
(define (tasks-column-header-height) (round (+ (font-line-height 'ui) 14)))
;; How far the details, or the keys when nothing is selected, are from
;; the panel's edges.
(define (tasks-details-pad) 16)
;; Inside a column: its cards' margin at either side, the room above the
;; first card and between two, and the + button's from the header's end.
(define (tasks-column-insets) (values 10 8 8 6))
(define (tasks-label-width)
  (max 72 (+ 16 (widest-text '("Status" "Priority" "Kind" "Assignee" "Due"
                               "Tags")
                             'ui))))
(define (tasks-list-min-width) (* 36 (font-line-height 'ui)))
(define (tasks-page-width) (* 40 (font-line-height 'ui)))   ; at most
(define (tasks-avatar-size) (round (+ (font-line-height 'ui-small) 7)))
(define (tasks-mark-size) (round (* (font-line-height 'ui) 0.8)))

;; 4 columns and the details, tall enough for most of a task.
(define (tasks-natural-size)
  (values (+ (* 4 (tasks-column-width)) (* 3 (ui-gap)) (* 2 (ui-pad))
             (tasks-details-width))
          (max 640 (round (* 44 (font-line-height 'ui))))))

(define (tasks-min-size)
  (values (+ (* 4 (tasks-column-min-width)) (* 3 (ui-gap)) (* 2 (ui-pad)))
          (max 420 (* 28 (font-line-height 'ui)))))

;; (values mode view-rect details-rect) under a toolbar top high: wide
;; puts the details at the right; narrow gives the view the whole width,
;; or to the details page when it shows (details-rect #f otherwise).
(define (tasks-layout v w h top)
  (let* ([gap (ui-gap)] [dw (tasks-details-width)]
         [n (length (tasks-shown-statuses v))]
         [view-min (if (eq? (tasks-view-mode v) 'board)
                       (+ (* n (tasks-column-min-width)) (* (- n 1) gap)
                          (* 2 (ui-pad)))
                       (tasks-list-min-width))]
         [vh (- h top)])
    (if (>= w (+ view-min dw))
        (values 'wide (list 0 top (- w dw) vh) (list (- w dw) top dw vh))
        (values 'narrow (list 0 top w vh)
                (and (tasks-view-page? v) (tasks-view-selected v)
                     (let ([pw (min w (tasks-page-width))])
                       (list (round (/ (- w pw) 2)) top pw vh)))))))

;; What t's title shows: "Untitled" when it is empty.
(define (tasks-shown-title t)
  (if (string=? (task-title t) "") "Untitled" (task-title t)))

;; s as a menu entry or a message quotes it: at most 20 line heights wide,
;; so a long name keeps a menu on the screen, and a long title leaves room
;; for what the message says after it.
(define (tasks-quoted s) (fit-text s (* 20 (font-line-height 'ui)) 'ui))

;; The title's lines on a card w wide, wrapped as the details wrap it (a
;; long word breaks): at most 3, the last ending in "…" when there are
;; more. => (values lines cut?)
(define (tasks-card-title w t)
  (ui-wrap-lines (tasks-shown-title t) (- w 20) 'ui 3))

;; The tag chips that fit on a card w wide, the first always (cut to the
;; room), and how many more a "+k" after them counts. => (values widths k)
(define (tasks-card-tags w t)
  (let ([room (- w 20)]
        [more (lambda (k)   ; "+k" and its gap
                (if (zero? k)
                    0
                    (+ 4 (text-width (format "+~a" k) 'ui-small))))])
    (let loop ([tags (task-tags t)] [used 0] [ws '()])
      (if (null? tags)
          (values (reverse ws) 0)
          (let*-values ([(cw ch) (ui-chip-size (car tags) #f)]
                        [(u) (+ used (if (null? ws) 0 4) cw)]
                        [(k) (- (length tags) 1)])
            (cond
              [(<= (+ u (more k)) room) (loop (cdr tags) u (cons cw ws))]
              [(null? ws) (values (list (max 0 (- room (more k)))) k)]
              [else (values (reverse ws) (length tags))]))))))

(define (tasks-card-size w t)
  (let-values ([(lines cut?) (tasks-card-title w t)]
               [(cw ch) (ui-chip-size "" #f)])
    (values w (round (+ 10 (tasks-avatar-size) 6
                        (* (length lines) (font-line-height 'ui))
                        (if (pair? (task-tags t)) (+ 6 ch) 0)
                        (if (or (task-due t) (pair? (task-checklist t)))
                            (+ 6 (font-line-height 'ui-small) 3)
                            0)
                        10)))))

;; How far a List cell's text is from either side of the cell.
(define (tasks-cell-inset) 10)

;; Is t's title cut in a Title cell w wide? Its row's tooltip shows it.
(define (tasks-title-cut? t w)
  (> (text-width (task-title t) 'ui) (- w (* 2 (tasks-cell-inset)))))

;; The List's columns at width w: ((key title width) ...). Updated, Kind,
;; Due, Assignee, Priority and Status drop, in that order, as the width
;; runs out; the title takes the rest.
(define (tasks-list-columns w)
  (let* ([lh (font-line-height 'ui)] [m (tasks-mark-size)]
         [names (filter-map task-assignee *tasks*)]
         [cols `((id "ID" ,(+ 20 (text-width "TASK-000" 'mono-small)))
                 (status "Status" ,(+ 28 m (widest-text (map cdr tasks-statuses)
                                                        'ui)))
                 (priority "Priority"
                           ,(+ 28 m (widest-text (map cdr tasks-priorities)
                                                 'ui)))
                 (kind "Kind" ,(+ 28 m (widest-text (map cdr tasks-kinds) 'ui)))
                 (assignee "Assignee"
                           ,(min (* 9 lh)
                                 (+ 44 (widest-text (cons "Assignee" names)
                                                    'ui))))
                 (due "Due" ,(+ 20 (text-width "Nov 15, 2026" 'ui)))
                 (updated "Updated"
                          ,(+ 20 (text-width "Oct 31, 14:02" 'ui-small))))])
    (let loop ([cols cols] [drops '(updated kind due assignee priority status)])
      (let ([fixed (apply + (map caddr cols))])
        (if (or (null? drops) (<= (+ fixed (* 13 lh)) w))
            (cons* (car cols) (list 'title "Title" (- w fixed)) (cdr cols))
            (loop (remp (lambda (c) (eq? (car c) (car drops))) cols)
                  (cdr drops)))))))

;;; ------------------------------------------------------------------
;;; the app's own drawing: card, column, slot, marks, cells, empty states

;; 1–4 bars: low, normal, high, urgent.
(define (tasks-draw-priority x y s priority)
  (let* ([level (- 4 (tasks-rank tasks-priorities priority))]
         [color (case priority
                  [(urgent) 'red] [(high) 'orange] [(normal) 'text-dim]
                  [else 'text-faint])]
         [bw (/ s 5)])
    (do ([i 0 (+ i 1)]) ((= i 4))
      (let ([bh (* s (/ (+ i 2) 5))])
        (fill-round-rect (+ x (* i (+ bw (/ s 15)))) (+ y (- s bh)) bw bh 1
                         (if (< i level) color (alpha 'text-faint 0.35)))))))

;; Bug a circle, feature a diamond, chore a square: form and color both.
(define (tasks-draw-kind x y s kind)
  (let ([c (/ s 2)])
    (case kind
      [(bug) (fill-circle (+ x c) (+ y c) (* c 0.85) 'red)]
      [(feature)
       (fill-polygon (list (cons (+ x c) y) (cons (+ x s) (+ y c))
                           (cons (+ x c) (+ y s)) (cons x (+ y c)))
                     'accent)]
      [else (fill-round-rect (+ x 1) (+ y 1) (- s 2) (- s 2) 2 'text-dim)])))

;; One color per person, however the name was typed.
(define (tasks-draw-avatar x y s name)
  (let ([color (app-color
                (string->symbol (string-downcase (tasks-trim name))))])
    (fill-circle (+ x (/ s 2)) (+ y (/ s 2)) (/ s 2) color)
    (draw-text-centered x y s s (tasks-initials name) (readable-on color)
                        'ui-small)))

;; A line of t's title at (x, y): faint for "Untitled", dim when t is
;; done. Returns the x after it.
(define (tasks-draw-title x y line t)
  (draw-text x y line (cond [(string=? (task-title t) "") 'text-faint]
                            [(eq? (task-status t) 'done) 'text-dim]
                            [else 'text])
             'ui))

;; t's due date at (x, y), cut to w: red when overdue, orange today.
(define (tasks-draw-due x y w t today font)
  (draw-text x y (fit-text (tasks-due-label (task-due t) today) w font)
             (cond [(tasks-overdue? t today) 'red]
                   [(and (equal? (task-due t) today)
                         (not (eq? (task-status t) 'done)))
                    'orange]
                   [else 'text-faint])
             font))

;; A status's dot, in the s-square at (x, y), as the other marks are.
(define (tasks-draw-dot x y s status)
  (fill-circle (+ x (/ s 2)) (+ y (/ s 2)) (/ s 3)
               (case status
                 [(doing) 'accent] [(review) 'magenta] [(done) 'green]
                 [else 'text-faint])))

;; flags: hover, selected, floating (being dragged).
(define (tasks-draw-card x y w h flags t today)
  (let* ([on? (lambda (f) (memq f flags))]
         [lh (font-line-height 'ui)] [sh (font-line-height 'ui-small)]
         [a (tasks-avatar-size)] [m (tasks-mark-size)]
         [y1 (+ y 10)] [right (- (+ x w) 10)])
    (fill-round-rect x y w h 8 'bg-2)   ; bg-3 would hide the progress track
    (stroke-round-rect x y w h 8
                       (cond [(or (on? 'selected) (on? 'floating)) 'accent]
                             [(on? 'hover) 'text-faint]
                             [else 'window-border])
                       (if (on? 'selected) 2 1))
    (tasks-draw-priority (+ x 10) (+ y1 (/ (- a m) 2)) m (task-priority t))
    (tasks-draw-kind (+ x 16 m) (+ y1 (/ (- a m) 2)) m (task-kind t))
    (draw-text (+ x 24 m m) (+ y1 (/ (- a sh) 2)) (task-label t) 'text-faint
               'ui-small)
    (when (task-assignee t)
      (tasks-draw-avatar (- right a) y1 a (task-assignee t)))
    (let*-values ([(lines cut?) (tasks-card-title w t)]
                  [(ty) (+ y1 a 6)])
      (for-each (lambda (line i)
                  (tasks-draw-title (+ x 10) (+ ty (* i lh)) line t))
                lines (iota (length lines)))
      (let* ([cy (+ ty (* lh (length lines)) 6)]
             [fy (if (pair? (task-tags t))
                     (let*-values ([(ws k) (tasks-card-tags w t)]
                                   [(c0 ch) (ui-chip-size "" #f)]
                                   [(end)
                                    (fold-left (lambda (cx g cw)
                                                 (ui-draw-chip cx cy cw ch
                                                               '() g)
                                                 (+ cx cw 4))
                                               (+ x 10)
                                               (list-head (task-tags t)
                                                          (length ws))
                                               ws)])
                       (when (> k 0)
                         (draw-text end (+ cy (/ (- ch sh) 2))
                                    (format "+~a" k) 'text-faint 'ui-small))
                       (+ cy ch 6))
                     cy)]
             [items (task-checklist t)]
             [done (length (filter cdr items))] [n (length items)]
             [label (format "~a/~a" done n)] [bar-w (round (* 2 lh))]
             [ph (ui-progress-height)]
             [lx (- right bar-w 6 (text-width label 'ui-small))]
             [check-x (- lx sh 2)])   ; the checklist's left, when it shows
        (when (task-due t)
          (tasks-draw-due (+ x 10) fy
                          (- (if (> n 0) (- check-x 6) right) x 10)
                          t today 'ui-small))
        (when (> n 0)
          (ui-draw-glyph check-x fy sh sh '(quiet) 'check)
          (draw-text lx fy label 'text-faint 'ui-small)
          (ui-draw-progress (- right bar-w) (+ fy (/ (- sh ph) 2)) bar-w ph
                            '() (/ done n)))))))

;; A column's well and header: the status dot, its name and the count.
;; flags: empty (says so in its middle).
(define (tasks-draw-column x y w h flags status label count)
  (let ([hh (tasks-column-header-height)] [lh (font-line-height 'ui)]
        [sh (font-line-height 'ui-small)] [m (tasks-mark-size)])
    (ui-draw-surface x y w h '(well))
    (tasks-draw-dot (+ x 10) (+ y (/ (- hh m) 2)) m status)
    (let ([end (draw-text (+ x 16 m) (+ y (/ (- hh lh) 2)) label 'text
                          'ui-bold)])
      (draw-text (+ end 8) (+ y (/ (- hh sh) 2)) (number->string count)
                 'text-faint 'ui-small))
    (when (memq 'empty flags)
      (draw-text-centered x (+ y hh) w (min 120 (- h hh)) "No tasks"
                          'text-faint 'ui))))

;; Where a dragged card would land.
(define (tasks-draw-slot x y w h)
  (fill-round-rect x y w h 8 (alpha 'accent 0.15))
  (stroke-round-rect x y w h 8 'accent))

(define (tasks-draw-cell x y w h flags t key today)
  (let* ([lh (font-line-height 'ui)] [sh (font-line-height 'ui-small)]
         [m (tasks-mark-size)] [in (tasks-cell-inset)] [px (+ x in)]
         [ty (+ y (/ (- h lh) 2))] [my (+ y (/ (- h m) 2))] [cw (- w in in)]
         [text (lambda (x s color) (draw-text x ty (fit-text s (- (+ px cw) x)
                                                             'ui)
                                              color 'ui))])
    (case key
      [(id) (draw-text px (+ y (/ (- h (font-line-height 'mono-small)) 2))
                       (task-label t) 'text-faint 'mono-small)]
      [(title)   ; the title first; up to two tags in the room it leaves
       (let ([end (tasks-draw-title
                   px ty (fit-text (tasks-shown-title t) cw 'ui) t)])
         (let loop ([gs (list-head (task-tags t)
                                   (min 2 (length (task-tags t))))]
                    [cx (+ end 8)])
           (when (pair? gs)
             (let-values ([(gw gh) (ui-chip-size (car gs) #f)])
               (when (<= (+ cx gw) (+ px cw))
                 (ui-draw-chip cx (+ y (/ (- h gh) 2)) gw gh '() (car gs))
                 (loop (cdr gs) (+ cx gw 4)))))))]
      [(status)
       (tasks-draw-dot px my m (task-status t))
       (text (+ px m 8) (cdr (assq (task-status t) tasks-statuses)) 'text-dim)]
      [(priority)
       (tasks-draw-priority px my m (task-priority t))
       (text (+ px m 8) (cdr (assq (task-priority t) tasks-priorities))
             'text-dim)]
      [(kind)
       (tasks-draw-kind px my m (task-kind t))
       (text (+ px m 8) (cdr (assq (task-kind t) tasks-kinds)) 'text-dim)]
      [(assignee)
       (let ([a (tasks-avatar-size)] [who (task-assignee t)])
         (if who
             (begin (tasks-draw-avatar px (+ y (/ (- h a) 2)) a who)
                    (text (+ px a 8) who 'text-dim))
             (text px "—" 'text-faint)))]
      [(due)
       (when (task-due t) (tasks-draw-due px ty cw t today 'ui))]
      [(updated)
       (draw-text px (+ y (/ (- h sh) 2)) (tasks-time-label (task-updated t))
                  'text-faint 'ui-small)])))

;; The details panel when nothing is selected: the keys.
(define (tasks-draw-cheat-sheet x y w h flags)
  (let* ([lh (font-line-height 'ui)]
         [kw (+ 16 (text-width "Shift+arrows" 'mono-small))]
         [y1 (+ y (font-line-height 'heading) 6)]
         [y2 (+ y1 lh 18)])
    (draw-text x y "No task selected" 'text 'heading)
    (draw-text x y1 (fit-text "Click a card, or use the arrow keys." w 'ui)
               'text-dim 'ui)
    (let ([y3 (fold-left
               (lambda (ky row)
                 (draw-text x (+ ky 1) (car row) 'accent 'mono-small)
                 (draw-text (+ x kw) ky (fit-text (cdr row) (- w kw) 'ui)
                            'text-dim 'ui)
                 (+ ky lh 7))
               y2
               '(("n" . "New task") ("/" . "Search") ("v" . "Board or List")
                 ("↑ ↓ ← →" . "Move the selection")
                 ("Shift+arrows" . "Move the task")
                 ("Enter" . "Edit the title") ("Delete" . "Delete the task")
                 ("Tab" . "Next field")
                 ("Esc" . "Leave a field, then the task")))])
      (draw-text x (+ y3 14) "FROM A REPL" 'text-faint 'ui-small)
      (fold-left (lambda (ly line)
                   (draw-text x ly (fit-text line w 'mono-small) 'text-dim
                              'mono-small)
                   (+ ly (font-line-height 'mono-small) 6))
                 (+ y3 14 (font-line-height 'ui-small) 10)
                 '("(add-task! \"Fix the menu\" 'priority 'high)"
                   "(tasks-list 'status 'doing)"
                   "(set-task! 12 'status 'review)")))))

;;; ------------------------------------------------------------------
;;; the screens

;; A filter's dropdown in the toolbar: fixed and more are its choices
;; ((label . value) ...). A value no task has any more (the last one lost
;; the tag) stays a choice, so the box shows it until the filter changes.
(define (tasks-filter v x y w h key label value set fixed more)
  (let ([more (if (exists (lambda (c) (equal? (cdr c) value))
                          (append fixed more))
                  more
                  (append more (list (cons (tasks-quoted value) value))))])
    (apply ui-dropdown (tasks-view-ui v) (list 'filter key) x y w h
           (append fixed more) value
           (lambda (new) (set v new) (tasks-view-reveal?-set! v #t))
           (if value '(set #t) (list 'label label)))))

;; The toolbar's items, left to right: ((w h draw) ...), draw a (lambda
;; (x y w h) ...). The search's w is #f: it takes what the others leave.
;; What the tasks hold never moves an item: the filters are as wide as
;; their fixed labels need (a long name or tag is cut).
(define (tasks-toolbar-items v visible)
  (let*-values
      ([(ui) (tasks-view-ui v)]
       [(quoted) (lambda (l) (map (lambda (s) (cons (tasks-quoted s) s)) l))]
       ;; tasks-filter's arguments after the rect: key label value set
       ;; fixed more
       [(filters)
        (list (list 'kind "Kind" (tasks-view-kind v) tasks-view-kind-set!
                    (cons '("Any kind" . #f) (tasks-choices tasks-kinds)) '())
              (list 'priority "Priority" (tasks-view-priority v)
                    tasks-view-priority-set!
                    (cons '("Any priority" . #f)
                          (tasks-choices tasks-priorities))
                    '())
              (list 'assignee "Assignee" (tasks-view-assignee v)
                    tasks-view-assignee-set!
                    '(("Anyone" . #f) ("Nobody" . none))
                    (quoted (tasks-names *tasks*)))
              (list 'tag "Tag" (tasks-view-tag v) tasks-view-tag-set!
                    '(("Any tag" . #f)) (quoted (tasks-tags *tasks*))))]
       [(nw nh) (ui-button-size "New task" 'plus)]
       [(tw th) (ui-tabs-size '("Board" "List"))]
       [(sw sh) (ui-text-box-size '(search) 0 1)]   ; one row high
       [(fw fh) (ui-dropdown-size   ; each label and fixed choice
                 (apply append (map (lambda (f)
                                      (cons (cadr f) (map car (list-ref f 4))))
                                    filters)))]
       [(dw dh) (ui-toggle-size "Show done")]
       [(cw ch) (ui-button-size "Clear" #f)])
    (append
     (list
      (list nw nh (lambda (x y w h)
                    (ui-button ui 'new x y w h "New task"
                               (lambda () (tasks-new-task! v 'todo))
                               'glyph 'plus 'primary #t 'tip "New task  N")))
      (list tw th (lambda (x y w h)
                    (ui-tabs ui 'view x y h '(("Board" . board) ("List" . list))
                             (tasks-view-mode v)
                             (lambda (m)
                               (tasks-view-mode-set! v m)
                               (tasks-view-reveal?-set! v #t)))))
      (list #f sh (lambda (x y w h)
                    (ui-text-field
                     ui 'search x y w h (tasks-view-query v)
                     (lambda (s) (tasks-view-query-set! v s))
                     'search #t 'placeholder "Search" 'tip "Search  /"
                     'on-enter (lambda ()
                                 (ui-focus! ui #f)
                                 (let ([rows (tasks-rows v visible)])
                                   (when (pair? rows)
                                     (tasks-select! v (car rows)))))))))
     (map (lambda (f)
            (list fw fh (lambda (x y w h) (apply tasks-filter v x y w h f))))
          filters)
     (list
      (list dw dh (lambda (x y w h)
                    (ui-toggle ui 'done x y w h "Show done" (tasks-view-done? v)
                               (lambda (on?) (tasks-view-done?-set! v on?))
                               'tip "Hide or show finished tasks")))
      (list cw ch (lambda (x y w h)
                    (ui-button ui 'clear x y w h "Clear"
                               (lambda () (tasks-clear-filters! v))
                               'quiet #t 'disabled (not (tasks-filtered? v))
                               'tip "Clear the search and filters")))))))

;; The toolbar across the top; returns its height. The count has room for
;; any count of this many tasks, and the search takes what the other items
;; leave of the row, within 180 to 300.
(define (tasks-toolbar v w visible)
  (let*-values
      ([(pad gap) (values (ui-pad) (ui-gap))]
       [(total) (length *tasks*)]
       [(count) (if (and (= (length visible) total) (not (tasks-filtered? v)))
                    (format "~a tasks" total)
                    (format "~a of ~a" (length visible) total))]
       [(count-w) (apply max (map (lambda (s)
                                    (let-values ([(lw lh) (ui-label-size '(dim)
                                                                         s 0)])
                                      lw))
                                  (list (format "~a tasks" total)
                                        (format "~a of ~a" total total))))]
       [(room) (- w pad pad count-w gap)]
       [(items) (tasks-toolbar-items v visible)]
       ;; whole pixels, so the row it fills exactly still fits
       [(sw) (floor (clamp (- room (apply + (filter-map car items))
                              (* gap (- (length items) 1)))
                           180 300))]
       [(sizes) (map (lambda (i) (cons (or (car i) sw) (cadr i))) items)]
       [(at height) (ui-flow pad gap room gap sizes)])
    (ui-draw-surface 0 0 w (+ height gap gap) '(toolbar))
    (for-each (lambda (i s p) ((caddr i) (car p) (cdr p) (car s) (cdr s)))
              items sizes at)
    ;; on the first row, level with New task
    (ui-draw-label (- w pad count-w) (cdar at) count-w (cdar sizes) '(dim right)
                   count)
    (+ height gap gap)))

;; The columns of the board in x y w h.
(define (tasks-board v visible today x y w h)
  (let* ([ui (tasks-view-ui v)] [pad (ui-pad)] [gap (ui-gap)]
         [cols (tasks-columns v visible)] [n (length cols)]
         [cw (/ (- w pad pad (* (- n 1) gap)) n)]
         [d (tasks-view-drag v)]
         [moving (and d (tasks-drag-moving? d) (tasks-drag-task d))]
         [target (and moving
                      (point-in? (tasks-drag-x d) (tasks-drag-y d) x y w h)
                      (clamp (exact (floor (/ (- (tasks-drag-x d) x pad
                                                 (- (/ gap 2)))
                                              (+ cw gap))))
                             0 (- n 1)))])
    (when d
      (tasks-drag-status-set! d #f)
      (tasks-drag-before-set! d #f))
    (for-each (lambda (col i)
                (tasks-column v (car col) (remq moving (cdr col)) today
                              (+ x pad (* i (+ cw gap))) (+ y pad) cw
                              (- h pad pad) (eqv? i target)))
              cols (iota n))
    (when moving   ; as wide as the cards in a column
      (let-values ([(side first between edge) (tasks-column-insets)])
        (ui-later! ui
          (lambda (ww wh)
            (let-values ([(w h) (tasks-card-size (- cw side side) moving)])
              (tasks-draw-card (- (tasks-drag-x d) (tasks-drag-gx d))
                               (- (tasks-drag-y d) (tasks-drag-gy d))
                               w h '(floating) moving today))))))))

;; One column: its well and header, a + button, and the cards, which
;; scroll. target?: a dragged card is over it, so it shows the slot.
(define (tasks-column v status tasks today x y w h target?)
  (let*-values
      ([(ui) (tasks-view-ui v)] [(hh) (tasks-column-header-height)]
       [(side first between edge) (tasks-column-insets)]
       [(label) (cdr (assq status tasks-statuses))]
       [(card-w) (- w side side)] [(sid) (list 'column status)]
       [(heights) (map (lambda (t)
                         (let-values ([(cw ch) (tasks-card-size card-w t)]) ch))
                       tasks)]
       [(tops) (let loop ([hs heights] [cy first] [acc '()])
                 (if (null? hs)
                     (reverse acc)
                     (loop (cdr hs) (+ cy (car hs) between) (cons cy acc))))]
       [(bw bh) (ui-button-size #f 'plus)]
       [(d) (tasks-view-drag v)]
       [(sel) (tasks-view-selected v)])
    (tasks-draw-column x y w h
                       (if (and (null? tasks) (not target?)) '(empty) '())
                       status label (length tasks))
    (ui-button ui (list 'add status) (- (+ x w) edge bw) (+ y (/ (- hh bh) 2))
               bw bh #f (lambda () (tasks-new-task! v status))
               'glyph 'plus 'quiet #t 'focus 'keep
               'tip (string-append "New task in " label))
    (let ([i (and (tasks-view-reveal? v) (tasks-index sel tasks))])
      (when i
        (ui-reveal! ui sid (list-ref tops i) (+ (list-ref tops i)
                                                (list-ref heights i) between)
                    (- h hh))))
    (ui-scroll ui sid x (+ y hh) w (- h hh)
      (lambda (top)
        (let ([slot (and target?
                         (let ([i (tasks-slot tops heights
                                              (- (tasks-drag-y d) top))])
                           (tasks-drag-status-set! d status)
                           (tasks-drag-before-set! d (and (< i (length tasks))
                                                          (list-ref tasks i)))
                           i))]
              [dh (and target?
                       (let-values ([(cw ch) (tasks-card-size
                                              card-w (tasks-drag-task d))])
                         ch))])
          (let loop ([ts tasks] [hs heights] [i 0] [cy (+ top first)])
            (let ([cy (if (eqv? i slot)
                          (begin (tasks-draw-slot (+ x side) cy card-w dh)
                                 (+ cy dh between))
                          cy)])
              (if (null? ts)
                  cy
                  (begin
                    ;; only the cards in view: a long column stays cheap
                    (when (and (< cy (+ y h)) (> (+ cy (car hs)) (+ y hh)))
                      (tasks-card v (car ts) (+ x side) cy card-w (car hs)
                                  today))
                    (loop (cdr ts) (cdr hs) (+ i 1)
                          (+ cy (car hs) between)))))))))))

;; A card: pressed it selects (a double click edits the title) and may be
;; dragged to another column or place; right-pressed, the menu. Another
;; button pressed during a drag ends it, the card back where it was.
(define (tasks-card v t x y w h today)
  (let* ([ui (tasks-view-ui v)] [id (list 'card (task-id t))])
    (let-values ([(lines cut?) (tasks-card-title w t)])
      (ui-region! ui id x y w h 'tip (and cut? (task-title t))
        'on (lambda (ev)
              (case (car ev)
                [(press)
                 (let ([px (cadr ev)] [py (caddr ev)] [button (list-ref ev 3)])
                   (cond
                     [(eq? button 'left)
                      (tasks-select! v t)
                      (when (>= (list-ref ev 4) 2) (tasks-edit-title! v))
                      (tasks-view-drag-set!
                       v (make-tasks-drag t px py (- px x) (- py y) px py
                                          #f #f #f))
                      #t]
                     [(tasks-view-drag v) (tasks-view-drag-set! v #f) #t]
                     [(eq? button 'right)
                      (tasks-select! v t)
                      (tasks-menu v t px py)
                      #t]
                     [else #f]))]
                [(drag)
                 (let ([d (tasks-view-drag v)])
                   (and d
                        (begin
                          (tasks-drag-x-set! d (cadr ev))
                          (tasks-drag-y-set! d (caddr ev))
                          (when (> (+ (abs (- (cadr ev) (tasks-drag-px d)))
                                      (abs (- (caddr ev) (tasks-drag-py d))))
                                   4)
                            (tasks-drag-moving?-set! d #t))
                          (tasks-drag-moving? d))))]
                [(release)
                 (let ([d (tasks-view-drag v)])
                   (tasks-view-drag-set! v #f)
                   (cond
                     [(not d) #f]
                     [(tasks-drag-moving? d)
                      (when (tasks-drag-status d)
                        (tasks-place! t (tasks-drag-status d)
                                      (tasks-drag-before d)))
                      #t]
                     [else
                      (when (tasks-narrow? v) (tasks-view-page?-set! v #t))
                      #t]))]
                [else #f]))))
    (tasks-draw-card x y w h
                     (apply ui-flags ui id
                            (if (eq? t (tasks-view-selected v))
                                '(selected)
                                '()))
                     t today)))

;; The List: a table of the visible tasks.
(define (tasks-table v visible today x y w h)
  (let* ([ui (tasks-view-ui v)] [pad (ui-pad)]
         [columns (tasks-list-columns (- w pad pad))]
         [title-w (caddr (assq 'title columns))])
    (ui-draw-surface (+ x pad) (+ y pad) (- w pad pad) (- h pad pad) '(well))
    (ui-table ui 'table (+ x pad) (+ y pad 4) (- w pad pad) (- h pad pad 8)
              columns (tasks-rows v visible)
              (tasks-view-selected v)
              (lambda (t)
                (tasks-select! v t)
                (when (tasks-narrow? v) (tasks-view-page?-set! v #t)))
              (lambda (t key cx cy cw ch flags)
                (tasks-draw-cell cx cy cw ch flags t key today))
              'sort (tasks-view-sort v) 'reveal (tasks-view-reveal? v)
              'on-sort (lambda (key)
                         (let ([s (tasks-view-sort v)])
                           (tasks-view-sort-set!
                            v (cond [(not (and s (eq? (car s) key)))
                                     (cons key 'up)]
                                    [(eq? (cdr s) 'up) (cons key 'down)]
                                    [else #f]))))
              'on-open (lambda (t) (tasks-edit-title! v))
              'tip (lambda (t)
                     (and (tasks-title-cut? t title-w) (task-title t)))
              'on-menu (lambda (t mx my)
                         (tasks-select! v t)
                         (tasks-menu v t mx my)))))

;; The details of the selected task in x y w h (wide? beside the view,
;; else a page), or the keys when nothing is selected.
(define (tasks-details v today wide? x y w h)
  (let ([t (tasks-view-selected v)] [ui (tasks-view-ui v)]
        [p (tasks-details-pad)])
    (when wide? (ui-draw-surface x y w h '(side)))
    (if t
        (ui-scroll ui 'details x y w h
          (lambda (top)
            (+ p (tasks-details-body v t today wide? (+ x p) (+ top p)
                                     (- w p p)))))
        (tasks-draw-cheat-sheet (+ x p) (+ y p) (- w p p) (- h p p) '()))))

;; A list without its i-th element.
(define (tasks-without l i) (append (list-head l i) (list-tail l (+ i 1))))

;; The details' first row: Back (narrow), the id and kind, and Close
;; (wide). Returns the y below it.
(define (tasks-details-head v t wide? x y cw)
  (let*-values ([(ui) (tasks-view-ui v)] [(m) (tasks-mark-size)]
                [(back) (if (eq? (tasks-view-mode v) 'board) "Board" "List")]
                [(bw bh) (ui-button-size (and (not wide?) back)
                                         (if wide? 'cross 'chevron-left))]
                [(id) (task-label t)]
                [(iw ih) (ui-label-size '(mono) id 0)]
                [(lx) (if wide? x (+ x bw (ui-gap)))])
    (if wide?
        (ui-button ui 'close (- (+ x cw) bw) y bw bh #f
                   (lambda () (tasks-select! v #f))
                   'glyph 'cross 'quiet #t 'tip "Close  Esc")
        (ui-button ui 'back x y bw bh back
                   (lambda () (tasks-view-page?-set! v #f))
                   'glyph 'chevron-left 'quiet #t 'tip "Back  Esc"))
    (ui-draw-label lx y iw bh '(mono) id)
    (tasks-draw-kind (+ lx iw 10) (+ y (/ (- bh m) 2)) m (task-kind t))
    (ui-draw-label (+ lx iw 16 m) y (- (+ x cw) lx iw 16 m bw) bh '(dim)
                   (cdr (assq (task-kind t) tasks-kinds)))
    (+ y bh (ui-gap))))

;; Status, priority, kind, assignee, due date and tags, each beside its
;; label. Returns the y below them.
(define (tasks-details-fields v t today x y cw)
  (let*-values ([(ui) (tasks-view-ui v)] [(gap) (ui-gap)]
                [(lw) (tasks-label-width)] [(fx) (+ x lw)] [(fw) (- cw lw)]
                [(dw dh) (ui-dropdown-size '())]
                [(ew eh) (ui-text-box-size '() fw 1)])
    ;; the label beside a control h high at (fx, y), whose bottom is bottom
    ;; (a chip input's first row is h high)
    (define (row y label h bottom)
      (ui-draw-label x y lw h '(dim) label)
      (+ gap bottom))
    ;; A status picked here keeps the task open, even in a column the
    ;; board hides: the details are where it is being edited.
    (define (dropdown y id alist value)
      (ui-dropdown ui id fx y fw dh (tasks-choices alist) value
                   (lambda (new) (task-set! v t id new)))
      (+ y dh))
    (let* ([y (row y "Status" dh
                   (dropdown y 'status tasks-statuses (task-status t)))]
           [y (row y "Priority" dh
                   (dropdown y 'priority tasks-priorities (task-priority t)))]
           [y (row y "Kind" dh (dropdown y 'kind tasks-kinds (task-kind t)))]
           [y (row y "Assignee" eh
                   (begin
                     (ui-text-field ui 'assignee fx y fw eh
                                    (or (task-assignee t) "")
                                    (lambda (s) (task-set! v t 'assignee s))
                                    'placeholder "Nobody"
                                    'on-enter (lambda () (ui-focus! ui #f)))
                     (+ y eh)))]
           [y (row y "Due" dh
                   (begin
                     (ui-date-field ui 'due fx y fw dh (task-due t)
                                    (lambda (d) (task-set! v t 'due d))
                                    'label (lambda (d)
                                             (tasks-due-label d today)))
                     (+ y dh)))])
      (row y "Tags" (ui-chip-input-size fw '())
           (ui-chip-input ui 'tags fx y fw (task-tags t)
                          (tasks-view-tag-draft v)
                          (lambda (s) (tasks-view-tag-draft-set! v s))
                          (lambda (ss)
                            (when (exists tasks-tag ss)
                              (task-set! v t 'tags (append (task-tags t) ss))))
                          (lambda (i)
                            (task-set! v t 'tags
                                       (tasks-without (task-tags t) i)))
                          'placeholder "Add a tag…")))))

;; The checklist: its progress, the items, and a field that adds one.
;; Returns the y below it.
(define (tasks-details-checklist v t x y cw)
  (let*-values ([(ui) (tasks-view-ui v)] [(gap) (ui-gap)]
                [(fw fh) (ui-text-box-size '() cw 1)]
                [(sw sh) (ui-label-size '(section) "Checklist" cw)]
                [(ph) (ui-progress-height)]
                [(items) (task-checklist t)] [(n) (length items)]
                [(done) (length (filter cdr items))]
                [(bw bh) (ui-button-size #f 'cross)])
    (ui-draw-label x y cw sh '(section) "Checklist")
    (when (> n 0)
      (ui-draw-label x y cw sh '(hint right) (format "~a of ~a" done n))
      (ui-draw-progress x (+ y sh 6) cw ph '() (/ done n)))
    (let loop ([is items] [i 0] [iy (+ y sh 6 (if (> n 0) (+ ph 6) 0))])
      (if (pair? is)
          (let-values ([(kw kh) (ui-checkbox-size (caar is))])
            (ui-checkbox ui (list 'item i) x iy (- cw bw gap) kh (caar is)
                         (cdar is)
                         (lambda (on?)
                           (task-set! v t 'checklist
                                      (map (lambda (it j)
                                             (if (= j i)
                                                 (cons (car it) on?)
                                                 it))
                                           items (iota n)))))
            (ui-button ui (list 'drop i) (- (+ x cw) bw) iy bw bh #f
                       (lambda ()
                         (task-set! v t 'checklist (tasks-without items i)))
                       'glyph 'cross 'quiet #t 'tip "Remove")
            (loop (cdr is) (+ i 1) (+ iy (max kh bh) 2)))
          (begin
            (ui-text-field ui 'new-item x (+ iy 4) cw fh
                           (tasks-view-item-draft v)
                           (lambda (s) (tasks-view-item-draft-set! v s))
                           'placeholder "Add an item…"
                           'on-enter
                           (lambda ()
                             (let ([s (tasks-trim (tasks-view-item-draft v))])
                               (unless (string=? s "")
                                 (task-set! v t 'checklist
                                            (append items (list s))))
                               (tasks-view-item-draft-set! v ""))))
            (+ iy 4 fh))))))

;; Returns the y below the details.
(define (tasks-details-body v t today wide? x y cw)
  (let*-values ([(ui) (tasks-view-ui v)] [(gap) (ui-gap)]
                [(y) (tasks-details-head v t wide? x y cw)]
                ;; the bare title's box starts left of x, so its text lines
                ;; up with the labels; its right edge is the fields'
                [(in) (ui-text-box-inset '(large bare single))]
                [(tw th) (ui-text-area-size '(large bare single) (+ cw in)
                                            (task-title t) 1 4)])
    (ui-text-area ui 'title (- x in) y (+ cw in) th (task-title t)
                  (lambda (s) (task-set! v t 'title s))
                  'single #t 'large #t 'bare #t 'placeholder "Title"
                  'on-enter (lambda () (ui-focus! ui #f)))
    ;; labels h 0: as high as their role's font, which the kit decides
    (let*-values ([(y) (tasks-details-fields v t today x (+ y th gap 8) cw)]
                  [(y) (ui-draw-label x (+ y 8) cw 0 '(section) "Description")]
                  [(dw dh) (ui-text-area-size '() cw (task-description t)
                                              4 14)])
      (ui-text-area ui 'description x (+ y 6) cw dh (task-description t)
                    (lambda (s) (task-set! v t 'description s))
                    'placeholder "Add a description…")
      (let*-values ([(y) (tasks-details-checklist v t x (+ y 6 dh 16) cw)]
                    [(y) (ui-draw-label
                          x (+ y 16) cw 0 '(hint)
                          (format "Created ~a  ·  Updated ~a"
                                  (tasks-time-label (task-created t))
                                  (tasks-time-label (task-updated t))))]
                    [(bw bh) (ui-button-size "Delete task…" #f)])
        (ui-button ui 'delete x (+ y 12) bw bh "Delete task…"
                   (lambda () (tasks-view-confirm-set! v t))
                   'danger #t 'tip "Delete  Del")
        (+ y 12 bh)))))

;; What a search and filters that show nothing are; centered in the view,
;; with a button that undoes them (or makes a task, when there is none).
(define (tasks-empty v x y w h)
  (let* ([ui (tasks-view-ui v)] [none? (null? *tasks*)] [bw (min (- w 32) 460)]
         [title (if none? "No tasks yet" "Nothing matches")]
         [named (lambda (what value alist)
                  (and value
                       (format "~a: ~a" what
                               (cond [(assq value alist) => cdr]
                                     [(eq? value 'none) "Nobody"]
                                     [else (tasks-quoted value)]))))]
         [parts (filter
                 values
                 (list (named "Kind" (tasks-view-kind v) tasks-kinds)
                       (named "Priority" (tasks-view-priority v)
                              tasks-priorities)
                       (named "Assignee" (tasks-view-assignee v) '())
                       (named "Tag" (tasks-view-tag v) '())
                       (and (not (tasks-view-done? v)) "Done hidden")))]
         [body (if none?
                   "Press N to add one, or from a REPL: \
                    (add-task! \"Fix the menu\")"
                   (string-append
                    "No task matches"
                    (if (string=? (tasks-view-query v) "")
                        ""
                        (format " “~a”" (tasks-quoted (tasks-view-query v))))
                    (if (null? parts)
                        ""
                        (string-append " with " (string-join parts ", ")))
                    "."))]
         [label (if none? "New task" "Clear search and filters")])
    (let*-values ([(tw th) (ui-label-size '(heading) title bw)]
                  [(bodyw bodyh) (ui-label-size '(wrap dim) body bw)]
                  [(btw bth) (ui-button-size label (and none? 'plus))])
      (let* ([eh (+ th 8 bodyh)] [ex (+ x (round (/ (- w bw) 2)))]
             [ey (+ y (max 24 (round (/ (- h eh 16 bth) 2))))]
             [bx (+ x (round (/ (- w btw) 2)))] [by (+ ey eh 16)])
        (ui-draw-label ex (+ (ui-draw-label ex ey bw 0 '(heading center) title)
                             8)
                       bw 0 '(wrap center dim) body)
        (if none?
            (ui-button ui 'empty-new bx by btw bth label
                       (lambda () (tasks-new-task! v 'todo))
                       'glyph 'plus 'primary #t)
            (ui-button ui 'empty-clear bx by btw bth label
                       (lambda () (tasks-clear-filters! v))))))))

(define (tasks-dialog v t)
  (let ([cancel (lambda () (tasks-view-confirm-set! v #f))])
    (ui-dialog (tasks-view-ui v) 'confirm (format "Delete ~a?" (task-label t))
               (format "“~a” and its checklist will be gone. This cannot be \
                        undone."
                       (tasks-quoted (tasks-shown-title t)))
               (list (list "Cancel" cancel)
                     (list "Delete" (lambda () (tasks-delete! v t)) 'danger))
               cancel)))

(define (tasks-draw v w h)
  (let ([ui (tasks-view-ui v)] [today (ui-today)] [visible (tasks-visible v)])
    (ui-begin! ui)
    (ui-draw-surface 0 0 w h '(page))
    (let ([top (tasks-toolbar v w visible)])
      (let-values ([(mode view details) (tasks-layout v w h top)])
        (cond
          [(and (eq? mode 'narrow) details)
           (apply tasks-details v today #f details)]
          [(null? visible) (apply tasks-empty v view)]
          [(eq? (tasks-view-mode v) 'board)
           (apply tasks-board v visible today view)]
          [else (apply tasks-table v visible today view)])
        (when (eq? mode 'wide)
          ;; page? follows whether the details have the keyboard, so a
          ;; window made narrow shows them as the page, the field focused
          (let* ([f (ui-focused ui)] [before? (and f (ui-find ui f))])
            (apply tasks-details v today #t details)
            (tasks-view-page?-set! v (and f (not before?) (ui-find ui f)
                                          #t))))))
    (let ([t (tasks-view-confirm v)]) (when t (tasks-dialog v t)))
    (ui-due! ui (ui-next-midnight))
    (tasks-view-reveal?-set! v #f)
    (ui-end! ui w h)))

;;; ------------------------------------------------------------------ keys
;;; They act only when no widget has the keyboard.

;; Arrows on the Board: up and down the column, left and right to the
;; same place in the next column that has cards.
(define (tasks-board-step! v key visible)
  (let* ([cols (remp (lambda (c) (null? (cdr c))) (tasks-columns v visible))]
         [sel (tasks-view-selected v)]
         [ci (and sel (list-index (lambda (c) (memq sel (cdr c))) cols))])
    (cond
      [(null? cols) #f]
      [(not ci) (tasks-select! v (cadar cols)) #t]
      [else
       (let* ([col (cdr (list-ref cols ci))] [n (length col)]
              [i (tasks-index sel col)]
              [pick (lambda (col j) (tasks-select! v (list-ref col j)) #t)])
         (case key
           [(up) (pick col (max 0 (- i 1)))]
           [(down) (pick col (min (- n 1) (+ i 1)))]
           [(home) (pick col 0)]
           [(end) (pick col (- n 1))]
           [(left right)
            (let ([j (+ ci (if (eq? key 'left) -1 1))])
              (and (< -1 j (length cols))
                   (let ([other (cdr (list-ref cols j))])
                     (pick other (min i (- (length other) 1))))))]
           [else #f]))])))

;; t, the selected task, to status, before task before (#f: the column's
;; end), kept in view; when that hides it (Show done is off), the task
;; next to it is selected, as after a delete.
(define (tasks-move-to! v t status before)
  (let ([peers (tasks-peers v t)])
    (tasks-place! t status before)
    (tasks-view-reveal?-set! v #t)
    (when (and (memq t peers) (not (memq t (tasks-visible v))))
      (tasks-select! v (tasks-neighbor peers t)))))

;; Shift+Left/Right: t to the previous or next status, at the same place.
(define (tasks-shift-status! v t dir visible)
  (let* ([i (tasks-rank tasks-statuses (task-status t))]
         [j (clamp (+ i dir) 0 (- (length tasks-statuses) 1))])
    (unless (= i j)
      (let* ([at (tasks-index t (tasks-in (task-status t) visible))]
             [status (car (list-ref tasks-statuses j))]
             [col (tasks-in status visible)])
        (tasks-move-to! v t status
                        (and at (< at (length col)) (list-ref col at)))))
    (tasks-view-reveal?-set! v #t)
    #t))

;; Shift+Up/Down on the Board: t up or down its column.
(define (tasks-shift-order! v t dir visible)
  (let* ([col (tasks-in (task-status t) visible)]
         [i (tasks-index t col)] [n (length col)])
    (cond
      [(not i) #f]
      [(and (< dir 0) (> i 0))
       (tasks-place! t (task-status t) (list-ref col (- i 1)))]
      [(and (> dir 0) (< i (- n 1)))
       (tasks-place! t (task-status t)
                     (and (< (+ i 2) n) (list-ref col (+ i 2))))])
    (tasks-view-reveal?-set! v #t)
    #t))

(define (tasks-key v key mods)
  (let* ([ui (tasks-view-ui v)] [sel (tasks-view-selected v)]
         [visible (tasks-visible v)]
         [board? (eq? (tasks-view-mode v) 'board)]
         [shift? (equal? mods '(shift))])
    (cond
      [(ui-focused ui) #f]
      [(tasks-view-drag v)
       (and (eq? key 'escape) (begin (tasks-view-drag-set! v #f) #t))]
      [(and shift? sel (memq key '(left right)))
       (tasks-shift-status! v sel (if (eq? key 'left) -1 1) visible)]
      [(and shift? sel board? (memq key '(up down)))
       (tasks-shift-order! v sel (if (eq? key 'up) -1 1) visible)]
      [(not (null? mods)) #f]
      [(memq key '(up down left right home end page-up page-down))
       (if board?
           (tasks-board-step! v key visible)
           (let* ([rows (tasks-rows v visible)]
                  [j (ui-table-step (tasks-index sel rows) (length rows)
                                    (ui-table-page ui 'table) key)])
             (and j (begin (tasks-select! v (list-ref rows j)) #t))))]
      [(and (eq? key 'return) sel) (tasks-edit-title! v) #t]
      [(and (memq key '(delete backspace)) sel)
       (tasks-view-confirm-set! v sel)
       #t]
      [(eq? key 'escape)
       (cond [(and sel (tasks-view-page? v) (tasks-narrow? v))
              (tasks-view-page?-set! v #f)
              #t]
             [sel (tasks-select! v #f) #t]
             [else #f])]
      [else #f])))

;; Letter commands arrive as text no widget took.
(define (tasks-text v str)
  (let ([ui (tasks-view-ui v)])
    (and (not (ui-focused ui))
         (cond
           [(member str '("n" "N")) (tasks-new-task! v 'todo) #t]
           [(string=? str "/") (ui-focus! ui 'search #t) #t]
           [(member str '("v" "V"))
            (tasks-view-mode-set! v (if (eq? (tasks-view-mode v) 'board)
                                        'list 'board))
            (tasks-view-reveal?-set! v #t)
            #t]
           [else #f]))))

(define-app (tasks) "Tasks"
  (let ([existing (find-window 'tasks)])
    (if (and existing (tasks-view? (window-ref existing 'state #f)))
        (begin (focus-window! existing) existing)
        (let* ([ui (make-ui)] [v (tasks-view-new ui)])
          ;; a window from before a reload that changed tasks-view
          (when existing (close-window! existing))
          (let-values ([(w h) (tasks-natural-size)]
                       [(mw mh) (tasks-min-size)])
            (apply open-window "Tasks"
              'size (cons w h) 'min-size (cons mw mh) 'state v
              'draw (lambda (win w h) (tasks-draw v w h))
              (ui-handlers ui
                'key (lambda (win key mods) (tasks-key v key mods))
                'text (lambda (win str) (tasks-text v str)))))))))

;; Seeds the heap on first load, and again after a change to task's
;; fields (a new uid) makes the old records strangers.
(unless (and (list? *tasks*) (for-all task? *tasks*)) (tasks-reset!))
