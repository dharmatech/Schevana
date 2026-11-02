;;; system.ss — the main loop, timers, boot and reload.

(defvar *running* #f)
(defvar *timers* '())     ; ((due-ms . thunk) ...) sorted by due time
(defvar *injected* '())   ; synthetic events waiting to be handled
;; events that came while a reload held the world, newest first
(defvar *held-events* '())
;; whether this step handled events: work that can wait (a text's rows
;; for its scroll bar) waits, so that input's frame comes first
(defvar *input-step?* #f)
(defvar *boot-ms* (now-ms))

;; (after 1000 (lambda () (notify "a second later")))
(define (after ms thunk)
  (let ([t (cons (+ (now-ms) ms) thunk)])
    (set! *timers*
      (let ins ([ts *timers*])
        (cond [(null? ts) (list t)]
              [(< (car t) (caar ts)) (cons t ts)]
              [else (cons (car ts) (ins (cdr ts)))])))
    t))

(define (run-timers!)
  (let loop ()
    (when (and (pair? *timers*) (<= (caar *timers*) (now-ms)))
      (let ([t (car *timers*)])
        (set! *timers* (cdr *timers*))
        (safe-call 'timer (cdr t) (lambda (c) (report-error! 'timer c)))
        (loop)))))

;; Feed a synthetic event into the world, exactly as if the host sent it:
;; (inject! '(mouse-down 100 100 left 1))
(define (inject! . events)
  (set! *injected* (append *injected* events)))

(define (handle-event! ev)
  (case (car ev)
    [(mouse-move) (apply wm-mouse-move (cdr ev))]
    [(mouse-down) (apply wm-mouse-down (cdr ev))]
    [(mouse-up) (apply wm-mouse-up (cdr ev))]
    [(wheel) (apply wm-wheel (cdr ev))]
    [(key-down) (apply wm-key-down (cdr ev))]
    [(text) (wm-text (cadr ev))]
    [(resize) (wm-screen-resized!)]
    [(expose) (set! *present-only?* #t)]   ; the texture still holds the screen
    [(render-reset) (invalidate-screen!)]   ; the texture's pixels were lost
    [(device-reset) (host-recreate-texture!) (invalidate-screen!)]
    [(mouse-leave) (set! *hover* #f) (request-frame!)]
    [(quit) (quit!)]))

;; A reload that failed or was killed: the world goes on as far as it got.
(define (reload-ended! msg)
  (set! *reloading* #f)
  (safely 'reload (lambda () (invalidate!) (notify msg 'red))))

;; While a process loads the sources (*reloading*, set by load-sources!),
;; the world holds still: no event, timer, tick, frame or other process
;; runs half-loaded code, and the screen stays as it was. A reload whose
;; process was killed ends here.
(define (reload-holding?)
  (when (and *reloading* (not (memq (car *reloading*) *procs*)))
    (reload-ended! (format "Reload stopped while loading ~a"
                           (cdr *reloading*))))
  (and *reloading* #t))

;; Ctrl+C kills the reload and quitting quits; other events wait for it.
(define (hold-event! ev)
  (cond
    [(and (eq? (car ev) 'key-down) (eqv? (cadr ev) #\c)
          (memq 'ctrl (caddr ev)))
     (kill! (car *reloading*))]
    [(eq? (car ev) 'quit) (quit!)]
    [else (set! *held-events* (cons ev *held-events*))]))

;; One turn of the world: events, timers, processes, ticks, and a frame
;; if something asked for one.
(define (schevana-step! wait-ms)
  (let* ([hold? (reload-holding?)]
         [evs (append (if hold? '() (reverse *held-events*))
                      (host-events (if (pair? *injected*) 0 wait-ms))
                      *injected*)])
    (unless hold? (set! *held-events* '()))
    (set! *injected* '())
    (set! *input-step?* (pair? evs))
    (for-each
     (lambda (ev)
       (safely 'event
               (lambda () (if hold? (hold-event! ev) (handle-event! ev)))))
     evs))
  (unless (reload-holding?) (safely 'timers run-timers!))
  (safely 'procs (lambda () (run-procs! 10)))
  (unless (reload-holding?)
    (safely 'tick wm-tick!)
    (let ([damage *dirty*])
      (set! *dirty* #f)
      (safely 'render (lambda () (render-frame! damage))))
    (safely 'present host-present-damage!)))

(define (schevana-run!)
  (set! *running* #t)
  (let loop ()
    (when *running*
      (schevana-step! (if (procs-ready?) 0 15))
      (loop)))
  (host-shutdown!))

(define (quit!) (set! *running* #f))

(define (uptime-ms) (- (now-ms) *boot-ms*))

;;; ------------------------------------------------------------------ boot

(define user-init-file
  (format "~a/.schevana.ss" (or (getenv "HOME") ".")))

(define (load-user-init!)
  (when (file-exists? user-init-file)
    (safely 'init (lambda () (load-file! user-init-file)))))

;; The user's own folder: lib for libraries, and kits, desktop, styles,
;; themes, wallpapers and apps like the tree's (see source-folders).
(define user-directory (format "~a/.schevana.d" (or (getenv "HOME") ".")))

;; (import (acme math)) also finds acme/math.sls here.
(define user-library-directory (string-append user-directory "/lib"))

;; Make import search dir before Chez's own library directories. It need
;; not exist yet.
(define (add-library-directory! dir)
  (let ([dir (if (string-prefix? "~/" dir)
                 (string-append (or (getenv "HOME") "")
                                (substring dir 1 (string-length dir)))
                 dir)])
    (unless (exists (lambda (e) (equal? (if (pair? e) (car e) e) dir))
                    (library-directories))
      (library-directories (cons (cons dir dir) (library-directories))))
    dir))

;; (schevana-boot! 'sdl 1280 800 #t) starts full screen.
(define (schevana-boot! mode w h . fullscreen?)
  (apply host-init! mode w h fullscreen?)
  (invalidate!))

;;; ------------------------------------------------------------------ sources
;;; The core is load.ss's list. After it come the folders, one definition
;;; per file, each folder's files in file-name order: drop a file in and it
;;; loads at the next boot or (reload!). A real session then loads the same
;;; folders from ~/.schevana.d.

;; The folders after the core, in load order.
(define source-folders
  '("kits" "desktop" "styles" "themes" "wallpapers" "apps"))

;; main.ss turns this on: a real session loads the user's folders too, at
;; boot and at every (reload!). The tests never do.
(defvar *user-folders?* #f)

;; A file name relative to the tree, or an absolute one (the user's).
(define (source-path f)
  (if (path-absolute? f) f (string-append *schevana-root* "/" f)))

;; The .ss files in the folder dir, by name; none if it does not exist.
;; Names starting with a dot (editors' lock files) are left out.
(define (folder-sources dir)
  (let ([path (source-path dir)])
    (if (file-directory? path)
        (map (lambda (f) (string-append dir "/" f))
             (sort string<?
                   (filter (lambda (f)
                             (and (string-suffix? ".ss" f)
                                  (not (string-prefix? "." f))))
                           (directory-list path))))
        '())))

(define (user-folder-files)
  (if *user-folders?*
      (apply append
             (map (lambda (d)
                    (folder-sources (string-append user-directory "/" d)))
                  source-folders))
      '()))

(define (folder-files)
  (append (apply append (map folder-sources source-folders))
          (user-folder-files)))

;; Every source file of the world, in load order. The REPL's docs and the
;; tests read this list.
(define (source-files) (append *schevana-sources* (folder-files)))

;; The text of every source as it was loaded (or, from a REPL, evaluated),
;; by its name: a file's path, or what the REPL calls its input. What reads
;; the sources (the REPL's descriptions) reads these, not the files as they
;; are now, so that it tells of the code that runs. (serial . text), the
;; serial counting the texts kept.
(defvar *source-texts* (make-hashtable string-hash string=?))
(defvar *source-serial* 0)

(define (keep-source-text! name text)
  (set! *source-serial* (+ *source-serial* 1))
  (hashtable-set! *source-texts* name (cons *source-serial* text)))

(define (file-text path) (call-with-input-file path get-string-all))

;; Loads a file, keeping its text (read just before load reads it).
(define (load-file! path)
  (keep-source-text! path (file-text path))
  (load path))

(define (load-source f) (load-file! (source-path f)))

;; Loads files into the world (from a private REPL too). Run as a process
;; (the launcher's "Reload sources", (reload!) in a REPL), it names itself
;; and the file in *reloading* until it ends, so the world holds still
;; meanwhile (schevana-step!), yet it is still preempted and Ctrl+C kills it.
;; Not with dynamic-wind: an engine runs its exits at every preemption.
(define (load-sources! files)
  (let* ([p (current-proc)]
         [outer? (and p (not (and *reloading* (eq? (car *reloading*) p))))])
    (parameterize ([interaction-environment *world-env*])
      (guard (c [outer? (reload-ended!
                         (format "Reload failed in ~a: ~a"
                                 (cdr *reloading*) (condition->string c)))
                        (raise c)])
        (for-each (lambda (f)
                    (when p (set! *reloading* (cons p f)))
                    (load-source f))
                  files)
        (when outer? (set! *reloading* #f))))))

;; The user's folders, after the built-in ones (main.ss, before the saved
;; theme, which may name a user's style or palette). One file at a time, so
;; that one which raises is named in its toast and the rest still load.
(define (load-user-folders!)
  (set! *user-folders?* #t)
  (add-library-directory! user-library-directory)
  (for-each (lambda (f) (safely f (lambda () (load-sources! (list f)))))
            (user-folder-files)))

;; The source files load.ss lists, read from the file itself so that a
;; reload picks up new ones.
(define (read-source-list)
  (guard (c [#t *schevana-sources*])
    (call-with-input-file (string-append *schevana-root* "/load.ss")
      (lambda (p)
        (let loop ()
          (let ([x (read p)])
            (cond
              [(eof-object? x) *schevana-sources*]
              [(and (list? x) (= (length x) 3) (eq? (car x) 'define)
                    (eq? (cadr x) '*schevana-sources*)
                    (pair? (caddr x)) (eq? (car (caddr x)) 'quote)
                    (list? (cadr (caddr x))))
               (cadr (caddr x))]
              [else (loop)])))))))

;; Re-read every source file. Code changes take effect; state declared
;; with defvar (windows, theme, processes...) survives.
(define (reload!)
  (let ([files (read-source-list)])
    (set! *schevana-sources* files)
    (load-sources! files))
  (invalidate!)
  (notify "Sources reloaded" 'green))

;; Loading this file (at boot, or in a reload) starts the renderer's
;; offscreen surfaces afresh; caches make them again when needed.
(free-owned-surfaces!)

;; load.ss loads the core before this file can keep its texts: kept now,
;; moments after (a reload keeps them as it loads them).
(for-each (lambda (f)
            (let ([path (source-path f)])
              (unless (hashtable-contains? *source-texts* path)
                (keep-source-text! path (file-text path)))))
          *schevana-sources*)

;; The folders load from here, the end of the last file in load.ss's list,
;; so that a reload by older code, which loads only that list, loads them
;; too. Their files may import the libraries shipped in lib/.
(add-library-directory! (string-append *schevana-root* "/lib"))
(load-sources! (folder-files))
