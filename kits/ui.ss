;;; kits/ui.ss — immediate-mode widgets.
;;;
;;; Widgets draw themselves during 'draw and record hit regions (with an
;;; id) in a ui object; the window's handlers ask the ui object what was
;;; hit. There is no widget tree: the draw procedure is the layout.
;;;
;;;   (define ui (make-ui))
;;;   (apply open-window "Demo"
;;;     'draw (lambda (win w h)
;;;             (ui-begin! ui)
;;;             (ui-button ui 'hi 20 20 120 28 "Say hi"
;;;                        (lambda () (notify "hi")))
;;;             (ui-end! ui w h))
;;;     (ui-handlers ui))
;;;
;;; The seam: each kind of widget draws through one ui-draw-KIND procedure,
;;; given its rectangle, its state as a list of flags (hover, pressed,
;;; focused, selected, ...) and its data. Only those decide colors, fonts,
;;; radii and paddings, and they read nothing else, so any state can be
;;; drawn from made-up flags. A widget's size comes from its ui-KIND-size
;;; procedure, which layout asks, and where its pieces are from its
;;; ui-KIND-areas, which its drawing and its clicks both read. Behavior
;;; (events, focus, editing) never draws. Each of those is a part (theme.ss,
;;; define-part): a style replaces it by its key, and the shared numbers
;;; are settings. A container draws its inside in its place (with-place):
;;; a text box in 'field, a table or tree in 'list, a dialog or calendar
;;; in 'dialog, a tooltip in 'tooltip. tests/sources.ss checks the seam.
;;;
;;; Keyboard focus: one focused widget per ui. A click focuses what takes
;;; typing; Tab and Shift+Tab go through every widget in draw order; Escape
;;; leaves. Text editing is pure (kits/edit.ss), so the tests need no
;;; window.

;;; ------------------------------------------------------------------ state

(define-record-type ui-region
  (nongenerative schevana-ui-region-3)
  (fields id x y w h   ; what can be hit: cut to the clip (may be 0 × 0)
          frame        ; (x y w h) before the cut, for scrolling into view
          on           ; #f, or (lambda (event) ...): true when handled
          focus        ; click, tab, keep or #f: see ui-mouse-down
          tip          ; tooltip text, or #f
          cursor       ; pointer shape over it ('ibeam), or #f
          modal?       ; covers the window: what lies under it is not live
          doc))        ; what the mouse buttons do on it: ((L . "Set") ...)

(define-record-type ui-state
  (nongenerative schevana-ui-state-3)
  (fields (mutable regions)  ; this draw's, newest (topmost) first
          (mutable active)   ; the region the left button holds, or #f
          (mutable hover-id) ; the topmost live region under the pointer
          (mutable mx) (mutable my)
          (mutable clicks)   ; click count of the press in progress
          (mutable rest)     ; (ms x y): when and where the pointer settled;
                             ; ms +inf.0 once a press, key, text or wheel
                             ; used the tooltip up
          (mutable clip)     ; (x y w h) new regions are cut to, or #f
          (mutable focus)    ; id of the focused widget, or #f
          (mutable edit)     ; the focused text's edit, or #f; all, or a
                             ; caret index, for a new focus
          (mutable blink-at) ; ms of the last caret change: phase 0
          (mutable reveal)   ; (id . span) to scroll into view at the next
                             ; draw, or #f; span: (top . bottom) within
                             ; it (a caret's row) for a view too short
                             ; for all of it, or #f
          (mutable popup)    ; (id . cursor-day) of an open calendar, or #f
          (mutable later)    ; overlays for ui-end!, newest first
          (mutable due)      ; ms when the window next looks different
          (mutable fresh?)   ; nothing handled since the last layout
          (mutable scans)    ; text layouts a scroll bar waits on: the
                             ; ticks compute their rows to the end
          memo))             ; per-id kit memory (scroll offsets, ...)

(define (make-ui)
  (make-ui-state '() #f #f -1 -1 1 (list 0 -1 -1) #f #f #f 0 #f #f '() #f #t
                 '() (make-hashtable equal-hash equal?)))

(define (ui-memo ui key default) (hashtable-ref (ui-state-memo ui) key default))
(define (ui-memo! ui key value) (hashtable-set! (ui-state-memo ui) key value))

;; Forgets what the kit keeps for widget id, a scroll offset and a caret:
;; given other data to show, it starts at its top again.
(define (ui-forget! ui id)
  (hashtable-delete! (ui-state-memo ui) id)
  (hashtable-delete! (ui-state-memo ui) (list id 'caret)))

;; How long the pointer rests before a tooltip shows, and a caret phase.
(define ui-tip-ms 600)
(define ui-blink-ms 530)

;; A rect cut to clip (x y w h), as (values x y w h); 0 wide or high, at
;; the clip's edge, when it lies outside.
(define (ui-cut x y w h clip)
  (if clip
      (let ([cx (car clip)] [cy (cadr clip)]
            [cx1 (+ (car clip) (caddr clip))]
            [cy1 (+ (cadr clip) (cadddr clip))])
        (let ([x0 (clamp x cx cx1)] [y0 (clamp y cy cy1)])
          (values x0 y0
                  (max 0 (- (clamp (+ x w) cx cx1) x0))
                  (max 0 (- (clamp (+ y h) cy cy1) y0)))))
      (values x y w h)))

;; The one way anything enters the hit list. opts: 'on 'focus 'tip
;; 'cursor 'modal 'doc. Inside a scroll view the rect is cut to the viewport;
;; one cut away entirely is still recorded, 0 × 0 at the edge, so it
;; stays a Tab stop (and can be revealed) but cannot be hit.
(define (ui-region! ui id x y w h . opts)
  (let-values ([(cx cy cw ch) (ui-cut x y w h (ui-state-clip ui))])
    (let ([r (make-ui-region id cx cy cw ch (list x y w h)
                             (plist-ref opts 'on #f) (plist-ref opts 'focus #f)
                             (plist-ref opts 'tip #f)
                             (plist-ref opts 'cursor #f)
                             (plist-ref opts 'modal #f)
                             (plist-ref opts 'doc '()))])
      (ui-state-regions-set! ui (cons r (ui-state-regions ui)))
      r)))

;; Settings' clickable area: on-press a thunk or #f; on-drag, if given,
;; receives the pointer position on press and on every move until release;
;; opts as ui-region!'s ('doc, say).
(define ui-area
  (case-lambda
    [(ui id x y w h on-press) (ui-area ui id x y w h on-press #f)]
    [(ui id x y w h on-press on-drag . opts)
     (apply ui-region! ui id x y w h 'on
       (and (or on-press on-drag)
            (lambda (ev)
              (case (car ev)
                [(press)
                 (and (eq? (list-ref ev 3) 'left)
                      (begin (when on-press (on-press))
                             (when on-drag (on-drag (cadr ev) (caddr ev)))
                             #t))]
                [(drag)
                 (and on-drag (begin (on-drag (cadr ev) (caddr ev)) #t))]
                [else #f])))
       opts)
     (void)]))

;; The live regions, newest first: those drawn above the newest modal
;; region, which is the last.
(define (ui-live ui)
  (let loop ([rs (ui-state-regions ui)] [acc '()])
    (cond [(null? rs) (reverse acc)]
          [(ui-region-modal? (car rs)) (reverse (cons (car rs) acc))]
          [else (loop (cdr rs) (cons (car rs) acc))])))

(define (ui-region-in? r x y)
  (point-in? x y (ui-region-x r) (ui-region-y r) (ui-region-w r)
             (ui-region-h r)))

(define (ui-region-at ui x y)
  (find (lambda (r) (ui-region-in? r x y)) (ui-live ui)))

(define (ui-find ui id)
  (find (lambda (r) (equal? (ui-region-id r) id)) (ui-state-regions ui)))

(define (ui-live-region ui id)
  (and id (find (lambda (r) (equal? (ui-region-id r) id)) (ui-live ui))))

(define (ui-hover? ui x y w h)
  (point-in? (ui-state-mx ui) (ui-state-my ui) x y w h))

(define (ui-active? ui id)
  (let ([a (ui-state-active ui)]) (and a (equal? (ui-region-id a) id))))

;; The symbols among keys whose option value is true: intents such as
;; 'primary #t become flags.
(define (ui-opt-flags opts keys)
  (filter (lambda (k) (plist-ref opts k #f)) keys))

;; The state flags the kit computes for id, then own. hover: the pointer
;; rests on it and no other is held; pressed: the left button holds it;
;; focused: it has the keyboard, in the focused window.
(define (ui-flags ui id . own)
  (let* ([win (current-window)]
         [r (ui-find ui id)]
         [pressed? (ui-active? ui id)]
         [hover? (and win (equal? (ui-state-hover-id ui) id)
                      (or pressed? (not (ui-state-active ui)))
                      (memq (window-hover win) '(content scrollbar))
                      (or (not r)
                          (ui-region-in? r (ui-state-mx ui) (ui-state-my ui))))]
         [focused? (and win (equal? (ui-state-focus ui) id)
                        (eq? win (focused-window)))])
    (append (if hover? '(hover) '()) (if pressed? '(pressed) '())
            (if focused? '(focused) '()) own)))

;;; ------------------------------------------------------------------ focus

;; Focuses id (#f: nothing), and scrolls it into view at the next draw. A
;; new focus starts its caret at the end; at, when given, is #t to select
;; everything, or the caret's index.
(define (ui-focus! ui id . at)
  (unless (equal? id (ui-state-focus ui))
    (ui-state-focus-set! ui id)
    (ui-state-edit-set! ui (and (pair? at) (if (eq? (car at) #t) 'all
                                               (car at))))
    (ui-state-blink-at-set! ui (now-ms)))
  (when id (ui-state-reveal-set! ui (cons id #f))))

(define (ui-focused ui) (ui-state-focus ui))

;; The edit of the text that has the keyboard, as its last draw left it,
;; or #f; and its caret.
(define (ui-edit ui)
  (let ([e (ui-state-edit ui)]) (and (edit? e) e)))

(define (ui-caret ui)
  (let ([e (ui-edit ui)]) (and e (edit-caret e))))

;; The focus moves to the next (dir 1) or previous (-1) live Tab stop in
;; draw order, wrapping; #f when there is none.
(define (ui-tab! ui dir)
  (let ([ids (map ui-region-id
                  (filter (lambda (r) (memq (ui-region-focus r) '(click tab)))
                          (reverse (ui-live ui))))])
    (and (pair? ids)
         (let* ([n (length ids)]
                [i (list-index (lambda (id) (equal? id (ui-state-focus ui)))
                               ids)]
                [j (cond [i (mod (+ i dir) n)] [(> dir 0) 0] [else (- n 1)])])
           (ui-focus! ui (list-ref ids j) #t)
           #t))))

;;; ------------------------------------------------------------------ time

;; The draw says when the window next looks different by itself (a caret
;; flip, a tooltip, midnight); the tick invalidates it then.
(define (ui-due! ui ms)
  (let ([d (ui-state-due ui)])
    (when (or (not d) (< ms d)) (ui-state-due-set! ui ms))))

;; Also, a scroll bar over a text whose rows are not all computed shows
;; an estimate: each tick computes some more of them, a millisecond's
;; worth, but not in a step that brought any window input (never on a
;; keystroke's frame); once they reach the end the window is drawn again,
;; so the bar settles.
(define (ui-tick ui win)
  (let ([d (ui-state-due ui)])
    (when (and d (>= (now-ms) d))
      (ui-state-due-set! ui #f)
      (invalidate! win)))
  (let ([scans (ui-state-scans ui)])
    (when (and (pair? scans) (not *input-step?*)
               (edit-layout-scan! (car scans) 1))
      (ui-state-scans-set! ui (cdr scans))
      (invalidate! win))))

;; Whether a focused caret shows now; asks for its next flip.
(define (ui-caret-on? ui)
  (let* ([t0 (ui-state-blink-at ui)]
         [k (quotient (max 0 (- (now-ms) t0)) ui-blink-ms)])
    (ui-due! ui (+ t0 (* (+ k 1) ui-blink-ms)))
    (even? k)))

;; The tooltip's wait starts at ms (+inf.0: not until the pointer moves),
;; for a pointer at (x, y), by default where it was.
(define (ui-rest! ui ms . at)
  (ui-state-rest-set! ui (cons ms
                               (if (pair? at) at (cdr (ui-state-rest ui))))))

;;; ------------------------------------------------------------------ events
;;; A region's on receives (press x y button clicks), (drag x y clicks),
;;; (release x y), (key key mods), (text str) and (wheel dy), and returns
;;; true when it handled the event.

(define (ui-stale! ui) (ui-state-fresh?-set! ui #f))

;; Before an event that needs the regions: when an event handled since the
;; last draw may have changed them, lay the window out again. Its draw
;; runs on the measuring context (between frames *cr* draws nowhere).
(define (ui-fresh! ui)
  (let ([win (current-window)])
    (unless (or (ui-state-fresh? ui) (not win))
      (let-values ([(w h) (window-content-size win)])
        ((hashtable-ref (window-handlers win) 'draw (lambda a #f)) win w h)
        (ui-state-fresh?-set! ui #t)))))

;; The held region as the latest layout drew it (as it was pressed, if
;; it is gone or no longer takes events: a button that disabled itself).
(define (ui-held ui)
  (let* ([a (ui-state-active ui)] [r (and a (ui-find ui (ui-region-id a)))])
    (if (and r (ui-region-on r)) r a)))

;; The hover goes to the topmost live region at (x, y). A change starts
;; the tooltip's wait at ms, sets the pointer's shape and redraws; then #t.
(define (ui-hover! ui win x y ms)
  (let* ([r (ui-region-at ui x y)] [id (and r (ui-region-id r))]
         [c (or (and r (ui-region-cursor r)) 'arrow)])
    (and (not (equal? id (ui-state-hover-id ui)))
         (begin
           (ui-state-hover-id-set! ui id)
           ;; the mouse documentation reads it; no redraw for it
           (hashtable-set! (window-props win) 'mouse-doc
                           (if r (ui-region-doc r) '()))
           (ui-rest! ui ms x y)
           (unless (eq? c (window-ref win 'cursor 'arrow))
             (window-set! win 'cursor c)
             (host-set-cursor! c))
           (invalidate! win)
           #t))))

;; A press: a left one focuses what takes typing (focus click), leaves the
;; focus to Tab stops and keep regions, and clears it elsewhere; then the
;; region gets it. While the left button holds a region, another button's
;; press goes to that region (a card ends its drag). button and clicks are
;; optional (Settings).
(define (ui-mouse-down ui x y . more)
  (let ([button (if (pair? more) (car more) 'left)]
        [clicks (if (and (pair? more) (pair? (cdr more))) (cadr more) 1)])
    (ui-state-mx-set! ui x)
    (ui-state-my-set! ui y)
    (let ([r (or (and (not (eq? button 'left)) (ui-held ui))
                 (ui-region-at ui x y))])
      (when (eq? button 'left)
        (case (and r (ui-region-focus r))
          [(click) (ui-focus! ui (ui-region-id r))]
          [(tab keep) (void)]
          [else (ui-focus! ui #f)])
        (ui-state-clicks-set! ui clicks)
        (ui-state-active-set! ui (and r (ui-region-on r) r)))
      (ui-state-hover-id-set! ui (and r (ui-region-id r)))
      (ui-rest! ui +inf.0 x y)
      (when (and r (ui-region-on r))
        ((ui-region-on r) (list 'press x y button clicks)))
      (ui-stale! ui)
      (invalidate! (current-window))
      (and r #t))))

(define (ui-mouse-move ui x y)
  (ui-state-mx-set! ui x)
  (ui-state-my-set! ui y)
  (let ([a (ui-held ui)] [win (current-window)])
    (if a
        (when ((ui-region-on a) (list 'drag x y (ui-state-clicks ui)))
          (ui-stale! ui)
          (invalidate! win))
        (unless (ui-hover! ui win x y (now-ms))
          ;; a tooltip not shown yet waits for the pointer to rest
          (when (< (now-ms) (+ (car (ui-state-rest ui)) ui-tip-ms))
            (ui-rest! ui (now-ms) x y))))))

;; x y button are optional (Settings): a left release at the pointer.
(define (ui-mouse-up ui . more)
  (let ([x (if (pair? more) (car more) (ui-state-mx ui))]
        [y (if (pair? more) (cadr more) (ui-state-my ui))]
        [button (if (pair? more) (caddr more) 'left)])
    (when (eq? button 'left)
      (let ([a (ui-held ui)])
        (ui-state-active-set! ui #f)
        (when (and a ((ui-region-on a) (list 'release x y)))
          (ui-stale! ui))))
    (invalidate! (current-window))))

;; Offered to each live region under the pointer, topmost first.
(define (ui-wheel ui dy)
  (ui-rest! ui +inf.0)
  (let ([x (ui-state-mx ui)] [y (ui-state-my ui)])
    (and (exists (lambda (r)
                   (and (ui-region-on r) (ui-region-in? r x y)
                        ((ui-region-on r) (list 'wheel dy))))
                 (ui-live ui))
         (begin (ui-stale! ui) #t))))

;; The key chain: the focused widget, Tab, a modal (it takes every key),
;; Escape leaving the focus, then the app's own key handler.
(define (ui-key ui key mods app-key)
  (ui-rest! ui +inf.0)
  (let* ([f (ui-live-region ui (ui-state-focus ui))]
         [modal (find ui-region-modal? (ui-live ui))]
         [done?
          (cond
            [(and f (ui-region-on f) ((ui-region-on f) (list 'key key mods)))]
            [(and (eq? key 'tab) (member mods '(() (shift)))
                  (ui-tab! ui (if (null? mods) 1 -1)))]
            [modal ((ui-region-on modal) (list 'key key mods)) #t]
            [(and (eq? key 'escape) (ui-state-focus ui)) (ui-focus! ui #f) #t]
            [app-key (app-key (current-window) key mods)]
            [else #f])])
    (when done? (ui-stale! ui))
    done?))

;; Typed text: the focused widget, else a modal, else the app's.
(define (ui-text ui str app-text)
  (ui-rest! ui +inf.0)
  (let* ([f (ui-live-region ui (ui-state-focus ui))]
         [modal (find ui-region-modal? (ui-live ui))]
         [done?
          (cond
            [(and f (ui-region-on f) ((ui-region-on f) (list 'text str)))]
            [modal #t]
            [app-text (app-text (current-window) str)]
            [else #f])])
    (when done? (ui-stale! ui))
    done?))

;; open-window options: the kit's mouse, wheel, key, text and tick
;; handlers, and 'ui ui. app: 'key (win key mods) and 'text (win str),
;; the app's own, called for what the kit did not take; they return true
;; when they acted.
(define (ui-handlers ui . app)
  (let ([key (plist-ref app 'key #f)] [text (plist-ref app 'text #f)])
    (list 'mouse-down (lambda (win x y button clicks)
                        (ui-fresh! ui)
                        (ui-mouse-down ui x y button clicks))
          'mouse-move (lambda (win x y) (ui-mouse-move ui x y))
          'mouse-up (lambda (win x y button)
                      (ui-fresh! ui)
                      (ui-mouse-up ui x y button))
          'wheel (lambda (win dx dy) (ui-fresh! ui) (ui-wheel ui dy))
          'key (lambda (win k mods) (ui-fresh! ui) (ui-key ui k mods key))
          'text (lambda (win str) (ui-fresh! ui) (ui-text ui str text))
          'tick (lambda (win) (ui-tick ui win))
          'ui ui)))

;; The region under an overlay: a press calls on-dismiss; every key is
;; taken, going to on-key first (Escape: on-dismiss); the wheel is taken.
(define (ui-modal-on on-dismiss on-key)
  (lambda (ev)
    (case (car ev)
      [(press) (on-dismiss) #t]
      [(key) (unless (on-key (cadr ev) (caddr ev))
               (when (eq? (cadr ev) 'escape) (on-dismiss)))
             #t]
      [(wheel) #t]
      [else #f])))

;; A region pressed like a button: a left press, or one of keys (no
;; modifiers) while it is focused, calls thunk. opts may give 'disabled
;; (no behavior), 'focus (tab by default), 'tip and 'doc (doc, if given,
;; by default).
(define (ui-pressable! ui id x y w h thunk keys opts . doc)
  (let ([disabled? (plist-ref opts 'disabled #f)])
    (ui-region! ui id x y w h
      'focus (if disabled? 'keep (plist-ref opts 'focus 'tab))
      'tip (plist-ref opts 'tip #f)
      'doc (plist-ref opts 'doc (if (pair? doc) (car doc) '()))
      'on (and (not disabled?)
               (lambda (ev)
                 (case (car ev)
                   [(press)
                    (and (eq? (list-ref ev 3) 'left) (begin (thunk) #t))]
                   [(key)
                    (and (null? (caddr ev)) (memv (cadr ev) keys)
                         (begin (thunk) #t))]
                   [else #f]))))))

;;; ------------------------------------------------------------------ a draw

(define (ui-begin! ui)
  (let ([win (current-window)])   ; an outside bar shows again if drawn
    (when win (hashtable-delete! (window-props win) 'scrollbar)))
  (ui-state-regions-set! ui '())
  (ui-state-clip-set! ui #f)
  (ui-state-later-set! ui '())
  (ui-state-due-set! ui #f)
  (ui-state-scans-set! ui '())
  (ui-state-fresh?-set! ui #t))

;; proc: (lambda (w h) ...), drawn by ui-end! above everything.
(define (ui-later! ui proc)
  (ui-state-later-set! ui (cons proc (ui-state-later ui))))

;; Ends a draw of a window w × h: the overlays, then what vanished loses
;; the focus, a reveal no scroll view took is dropped, the hover follows
;; rows that moved under a still pointer, and the tooltip.
(define (ui-end! ui w h)
  (let loop ()
    (let ([later (reverse (ui-state-later ui))])
      (unless (null? later)
        (ui-state-later-set! ui '())
        (for-each (lambda (p) (p w h)) later)
        (loop))))
  (ui-state-reveal-set! ui #f)
  (let ([f (ui-state-focus ui)])
    (when (and f (not (ui-find ui f)))
      (ui-state-focus-set! ui #f)
      (ui-state-edit-set! ui #f)))
  (let ([p (ui-state-popup ui)])
    (when (and p (not (ui-find ui (car p)))) (ui-state-popup-set! ui #f)))
  (let ([win (current-window)])
    (when (and win (memq (window-hover win) '(content scrollbar))
               (not (ui-state-active ui)))
      ;; rows moved under a still pointer: the tooltip's wait starts now,
      ;; unless a press, key or wheel used it up (+inf.0)
      (ui-hover! ui win (ui-state-mx ui) (ui-state-my ui)
                 (max (now-ms) (car (ui-state-rest ui))))
      ;; none over the widget that has the keyboard, where typing pauses
      (let* ([r (ui-live-region ui (ui-state-hover-id ui))]
             [tip (and r (not (equal? (ui-region-id r) (ui-state-focus ui)))
                       (ui-region-tip r))]
             [rest (ui-state-rest ui)]
             [at (+ (car rest) ui-tip-ms)])
        (cond
          [(not tip) (void)]
          [(< (now-ms) at) (ui-due! ui at)]
          [else
           (with-place 'tooltip
             (lambda ()
               (let*-values ([(px py) (values (cadr rest) (caddr rest))]
                             [(tw th) (ui-tooltip-size tip)]
                             [(tx ty) (ui-popup-place (+ px 12) (+ py 20)
                                                      (- py 8) tw th w h)])
                 (ui-draw-tooltip tx ty tw th '() tip))))])))))

;;; ------------------------------------------------------------------ menus

;; open-menu! at a point of the current window's content. Each entry's
;; thunk runs with current-window set to this window, then marks the ui
;; stale; once the window is closed, an entry does nothing. With 'filter,
;; entries may be (lambda (filter) entries): the menu's own filtering.
(define (ui-open-menu! ui x y entries . opts)
  (let ([win (current-window)])
    (define (wrap es)
      (map (lambda (e)
             (if (pair? e)
                 (cons (car e)
                       (lambda ()
                         (when (memq win (windows))
                           (parameterize ([current-window win]) ((cdr e)))
                           (ui-stale! ui)
                           (invalidate! win))))
                 e))
           es))
    (let-values ([(cx cy cw ch) (content-rect win)])
      (apply open-menu! (+ cx x) (+ cy y)
             (if (procedure? entries)
                 (lambda (f) (wrap (entries f)))
                 (wrap entries))
             opts))))

;;; ------------------------------------------------------------------
;;; sizes and layout

;; The shared sizes are settings, rules from the fonts: a style may give
;; its own pixels. A glyph's side beside 'ui text (a button's plus, a
;; dropdown's chevron), and beside 'ui-small text (a chip's ×, a sort
;; arrow).
(for-each theme-default!
  '(control-height glyph-size small-glyph-size gap pad)
  (list (theme-rule (max 28 (round (+ (font-line-height 'ui) 13))))
        (theme-rule (round (* 4/5 (font-line-height 'ui))))
        (theme-rule (round (* 4/5 (font-line-height 'ui-small))))
        (theme-rule (max 8 (round (/ (font-line-height 'ui) 2))))
        (theme-rule (max 12 (round (* (font-line-height 'ui) 4/5))))))
(define (ui-control-height) (theme 'control-height))
(define (ui-glyph-size) (theme 'glyph-size))
(define (ui-small-glyph-size) (theme 'small-glyph-size))
(define (ui-gap) (theme 'gap))
(define (ui-pad) (theme 'pad))

;; Where a popup w × h goes in a window ww × wh: at x, its top at below,
;; or its bottom at above when there is no room below; inside the window.
;; => (values x y)
(define (ui-popup-place x below above w h ww wh)
  (values (clamp x 0 (max 0 (- ww w)))
          (clamp (if (<= (+ below h) wh) below (- above h))
                 0 (max 0 (- wh h)))))

;; Places sizes ((w . h) ...) left to right from (x, y), wrapping before
;; an item that would pass x + w, gap apart both ways; items in a row are
;; centered on its tallest. Pure. => (values ((x . y) ...) height)
(define (ui-flow x y w gap sizes)
  (let ([rows (let loop ([ss sizes] [cx x] [row '()] [rows '()])
                (cond
                  [(null? ss)
                   (reverse (if (null? row) rows (cons (reverse row) rows)))]
                  [(or (null? row) (<= (+ cx (caar ss)) (+ x w)))
                   (loop (cdr ss) (+ cx (caar ss) gap)
                         (cons (cons cx (car ss)) row) rows)]
                  [else (loop ss x '() (cons (reverse row) rows))]))])
    (let loop ([rows rows] [ry y] [acc '()])
      (if (null? rows)
          (values (reverse acc) (max 0 (- ry y gap)))
          (let ([rh (apply max (map cddr (car rows)))])
            (loop (cdr rows) (+ ry rh gap)
                  (append (reverse
                           (map (lambda (it)
                                  (cons (car it)
                                        (+ ry (floor (/ (- rh (cddr it)) 2)))))
                                (car rows)))
                          acc)))))))

;;; ------------------------------------------------------------------
;;; dates, pure: (year month day), and day numbers since 1970-01-01

(define (ui-date->day d)
  (let* ([m (cadr d)] [y (if (<= m 2) (- (car d) 1) (car d))]
         [era (div y 400)] [yoe (- y (* era 400))]
         [doy (+ (quotient (+ (* 153 (mod (+ m 9) 12)) 2) 5) (- (caddr d) 1))]
         [doe (+ (* yoe 365) (quotient yoe 4) (- (quotient yoe 100)) doy)])
    (+ (* era 146097) doe -719468)))

(define (ui-day->date n)
  (let* ([z (+ n 719468)] [era (div z 146097)] [doe (- z (* era 146097))]
         [yoe (quotient (- doe (quotient doe 1460) (- (quotient doe 36524))
                           (quotient doe 146096))
                        365)]
         [doy (- doe (+ (* 365 yoe) (quotient yoe 4) (- (quotient yoe 100))))]
         [mp (quotient (+ (* 5 doy) 2) 153)]
         [d (+ (- doy (quotient (+ (* 153 mp) 2) 5)) 1)]
         [m (if (< mp 10) (+ mp 3) (- mp 9))])
    (list (+ yoe (* era 400) (if (<= m 2) 1 0)) m d)))

;; 0 is Monday.
(define (ui-week-day n) (mod (+ n 3) 7))

(define (ui-days-in-month y m)
  (- (ui-date->day (if (= m 12) (list (+ y 1) 1 1) (list y (+ m 1) 1)))
     (ui-date->day (list y m 1))))

;; Day n moved k months, its day of the month clamped to the new month.
(define (ui-add-months n k)
  (let* ([d (ui-day->date n)] [m0 (+ (cadr d) k -1)]
         [y (+ (car d) (div m0 12))] [m (+ 1 (mod m0 12))])
    (ui-date->day (list y m (min (caddr d) (ui-days-in-month y m))))))

(define (ui-today)
  (let ([d (current-date)]) (list (date-year d) (date-month d) (date-day d))))

;; "Nov 14, 2026"
(define (ui-date-label d)
  (format "~a ~a, ~a" (vector-ref month-names (- (cadr d) 1)) (caddr d)
          (car d)))

(define ui-month-titles
  '#("January" "February" "March" "April" "May" "June" "July" "August"
     "September" "October" "November" "December"))

;; The time, in ms, of the next local midnight.
(define (ui-next-midnight)
  (let ([d (current-date)])
    (+ (now-ms)
       (* 1000 (- 86400 (* 3600 (date-hour d)) (* 60 (date-minute d))
                  (date-second d))))))

;;; ------------------------------------------------------------------
;;; shared drawing

;; More font roles, from the font settings as the core's are: Genera's
;; pane headings and fixed italic, the CADR's current choice.
(theme-default! 'ui-bold-italic (theme-font-rule 'ui 0 'bold 'italic))
(theme-default! 'mono-italic (theme-font-rule 'mono 0 'italic))
(theme-default! 'ui-small-bold (theme-font-rule 'ui -2 'bold))

;; f's italic, bold kept: a style's 'faint-text-font where it has no gray
;; (Genera's hints and type descriptions).
(define (ui-italic-of f)
  (apply font (font-family f) (font-size f) 'italic
         (if (= (font-weight f) CAIRO_FONT_WEIGHT_BOLD) '(bold) '())))

;; The ring of a widget that has the keyboard from Tab.
(define-part (ui-draw-focus x y w h flags r)
  (stroke-round-rect (- x 3) (- y 3) (+ w 6) (+ h 6) (+ r 3)
                     (alpha 'accent 0.7) 2))

;; The box of what takes typing (a text box, a chip input): an accent
;; border while it has the keyboard.
(define-part (ui-draw-field x y w h flags)
  (fill-round-rect x y w h 6 'bg-0)
  (if (memq 'focused flags)
      (stroke-round-rect x y w h 6 'accent 1.5)
      (stroke-round-rect x y w h 6
                         (if (memq 'hover flags) 'text-faint 'window-border))))

;; Vector glyphs, no font: check cross plus chevron-down chevron-left
;; chevron-right search expand, the triangles up down left right (sort
;; marks, scroll arrows, submenus; sort-up and sort-down, their old names),
;; centered in the box; pin and undo (Settings' rows); and a window's
;; buttons, close minimize maximize restore shade, filling a square box
;; in lines of 1 pixel. flags may hold (ink . color) and (width . w),
;; the line's.
(define-part (ui-draw-glyph x y w h flags name)
  (let* ([s (min w h)] [ox (+ x (/ (- w s) 2))] [oy (+ y (/ (- h s) 2))]
         [p (lambda (fx fy) (cons (+ ox (* fx s)) (+ oy (* fy s))))]
         [given (lambda (key)
                  (let ([f (find (lambda (f) (and (pair? f) (eq? (car f) key)))
                                 flags)])
                    (and f (cdr f))))]
         [ink (cond [(given 'ink)]
                    [(memq 'primary flags) (readable-on 'accent)]
                    [(and (memq 'danger flags) (memq 'hover flags)) 'red]
                    [(memq 'hover flags) 'text]
                    [(memq 'quiet flags) 'text-faint]
                    [else 'text-dim])]
         [ink (if (memq 'disabled flags) (alpha ink 0.5) ink)]
         [lw (max 1.5 (* s 0.13))]
         [t (or (given 'width) 1)])
    (define (line . ps) (draw-polyline ps ink lw))
    (define (triangle . ps) (fill-polygon (map (lambda (q) (apply p q)) ps)
                                          ink))
    (case name
      [(check) (line (p 0.2 0.52) (p 0.42 0.74) (p 0.8 0.3))]
      [(cross) (line (p 0.28 0.28) (p 0.72 0.72))
               (line (p 0.72 0.28) (p 0.28 0.72))]
      [(plus) (line (p 0.5 0.2) (p 0.5 0.8)) (line (p 0.2 0.5) (p 0.8 0.5))]
      [(chevron-down) (line (p 0.25 0.38) (p 0.5 0.63) (p 0.75 0.38))]
      [(chevron-left) (line (p 0.62 0.25) (p 0.37 0.5) (p 0.62 0.75))]
      [(chevron-right) (line (p 0.38 0.25) (p 0.63 0.5) (p 0.38 0.75))]
      [(up sort-up) (triangle '(0.5 0.3) '(0.8 0.7) '(0.2 0.7))]
      [(down sort-down) (triangle '(0.5 0.7) '(0.8 0.3) '(0.2 0.3))]
      [(left) (triangle '(0.3 0.5) '(0.7 0.2) '(0.7 0.8))]
      [(right) (triangle '(0.7 0.5) '(0.3 0.2) '(0.3 0.8))]
      [(close) (draw-line ox oy (+ ox s) (+ oy s) ink t)
               (draw-line (+ ox s) oy ox (+ oy s) ink t)]
      [(minimize) (fill-rect ox (+ oy (ceiling (/ (- s t) 2))) s t ink)]
      [(maximize) (stroke-rect ox oy s s ink t)]
      [(restore)   ; a sheet in front of another
       (stroke-rect ox (+ oy 2) (- s 2) (- s 2) ink)
       (fill-rect (+ ox 2) oy (- s 2) 1 ink)
       (fill-rect (+ ox s -1) oy 1 (- s 2) ink)]
      [(shade) (stroke-rect ox oy s (ceiling (/ s 3)) ink t)]   ; a title
      [(pin)   ; a pushpin from the side: its head, its collar, its point
       (fill-circle (+ ox (* 0.5 s)) (+ oy (* 0.3 s)) (* 0.25 s) ink)
       (line (p 0.22 0.56) (p 0.78 0.56))
       (line (p 0.5 0.56) (p 0.5 1))]
      [(undo)   ; ↺: three quarters of a circle, an arrowhead at its top
       (apply line (map (lambda (i)
                          (let ([a (* pi (/ (- i 6) 12))])
                            (p (+ 0.5 (* 0.38 (cos a)))
                               (+ 0.56 (* 0.38 (sin a))))))
                        (iota 19)))
       (triangle '(0.26 0.18) '(0.56 0) '(0.56 0.36))]
      [(search)
       (stroke-circle (+ ox (* 0.43 s)) (+ oy (* 0.43 s)) (* 0.25 s) ink lw)
       (line (p 0.62 0.62) (p 0.82 0.82))]
      [(expand)   ; two arrows out to opposite corners
       (line (p 0.56 0.2) (p 0.8 0.2) (p 0.8 0.44))
       (line (p 0.8 0.2) (p 0.55 0.45))
       (line (p 0.44 0.8) (p 0.2 0.8) (p 0.2 0.56))
       (line (p 0.2 0.8) (p 0.45 0.55))])))

;; The glyph of win's frame button kind: a maximized window's maximize
;; button restores.
(define (ui-frame-glyph win kind)
  (if (and (eq? kind 'maximize) (maximized? win)) 'restore kind))

;;; ------------------------------------------------------------------ button

(define (ui-button ui id x y w h label on-press . opts)
  (ui-pressable! ui id x y w h on-press '(#\space return) opts
                 (list (cons 'L (or label (plist-ref opts 'tip #f) "Press"))))
  (ui-draw-button x y w h
                  (apply ui-flags ui id
                         (ui-opt-flags opts '(selected primary danger quiet
                                              disabled)))
                  label (plist-ref opts 'glyph #f)))

;; A primary button's fill while pressed, and under the pointer.
(color-default! 'primary-pressed (theme-rule (mix 'accent 'bg-0 0.15)))
(color-default! 'primary-hover (theme-rule (mix 'accent 'text 0.15)))

;; With hover and selected only it is the look Settings was built with.
(define-part (ui-draw-button x y w h flags label glyph)
  (let* ([flags (if (memq 'disabled flags)   ; the pointer does nothing
                    (remp (lambda (f) (memq f '(hover pressed))) flags)
                    flags)]
         [on? (lambda (f) (memq f flags))]
         [ink (lambda (c) (if (on? 'disabled) (alpha c 0.5) c))]
         [hot? (or (on? 'hover) (on? 'pressed))]
         [fill (cond [(on? 'primary)
                      (cond [(on? 'pressed) 'primary-pressed]
                            [(on? 'hover) 'primary-hover]
                            [else 'accent])]
                     [(on? 'selected) (alpha 'accent 0.25)]
                     [(on? 'pressed) (mix 'bg-3 'text 0.08)]
                     [(and (on? 'danger) (on? 'hover)) (alpha 'red 0.15)]
                     [(on? 'hover) 'bg-3]
                     [(on? 'quiet) #f]
                     [else 'bg-2])]
         [stroke (cond [(or (on? 'primary) (and (on? 'quiet) (not hot?))) #f]
                       [(on? 'selected) 'accent]
                       [(on? 'danger) 'red]
                       [else 'window-border])]
         [color (cond [(on? 'primary) (readable-on 'accent)]
                      [(on? 'danger) 'red]
                      [(or (on? 'hover) (on? 'selected)) 'text]
                      [else 'text-dim])]
         [lh (font-line-height 'ui)] [g (ui-glyph-size)]
         [gw (if glyph (* 3/2 g) 0)]   ; the glyph and its gap
         [label (and label (fit-text label (- w 12 gw) 'ui))]
         [cw (if label (+ gw (text-width label 'ui)) g)]
         [x0 (+ x (/ (- w cw) 2))])
    (when fill (fill-round-rect x y w h 6 (ink fill)))
    (when stroke (stroke-round-rect x y w h 6 (ink stroke)))
    (when glyph (ui-draw-glyph x0 (+ y (/ (- h g) 2)) g g flags glyph))
    (when label
      (draw-text (+ x0 gw) (+ y (/ (- h lh) 2)) label (ink color) 'ui))
    (when (on? 'focused) (ui-draw-focus x y w h flags 6))))

;; A glyph alone is a square button.
(define-part (ui-button-size label glyph)
  (let ([h (ui-control-height)])
    (values (if label
                (+ 24 (text-width label 'ui)
                   (if glyph (* 3/2 (ui-glyph-size)) 0))
                h)
            h)))

;; choices: ((label . value) ...); the one equal? to current is selected.
(define (ui-segmented ui id x y w h choices current on-select)
  (let ([bw (/ w (length choices))])
    (for-each (lambda (c i)
                (ui-button ui (list id (cdr c)) (+ x (* i bw)) y (- bw 6) h
                           (car c) (lambda () (on-select (cdr c)))
                           'selected (equal? current (cdr c))))
              choices (iota (length choices)))))

;;; ------------------------------------------------------------------ slider

;; A horizontal slider. on-change receives the new value in [lo, hi].
;; paint-track, if given, draws the track: (lambda (x y w h) ...).
(define ui-slider
  (case-lambda
    [(ui id x y w value lo hi on-change)
     (ui-slider ui id x y w value lo hi on-change #f)]
    [(ui id x y w value lo hi on-change paint-track)
     (let ([h (ui-slider-height)])
       (ui-area ui id (- x 8) y (+ w 16) h #f
                (lambda (mx my)
                  (on-change
                   (+ lo (* (- hi lo) (clamp (/ (- mx x) w) 0 1)))))
                'doc '((L . "Set")))
       (ui-draw-slider x y w h (ui-flags ui id)
                       (clamp (/ (- value lo) (- hi lo)) 0 1) paint-track))]))

(theme-default! 'slider-height 24)
(define (ui-slider-height) (theme 'slider-height))

(define-part (ui-draw-slider x y w h flags frac paint)
  (let* ([cy (+ y (/ h 2))] [kx (+ x (* frac w))]
         [r (if (or (memq 'hover flags) (memq 'pressed flags)) 8 7)])
    (if paint
        (call-with-round-clip x (- cy 4) w 8 4
                              (lambda () (paint x (- cy 4) w 8)))
        (begin
          (fill-round-rect x (- cy 3) w 6 3 'bg-3)
          (fill-round-rect x (- cy 3) (max 6 (- kx x)) 6 3 'accent)))
    (fill-circle kx cy r #xf2f2f5)
    (stroke-circle kx cy r (alpha #x000000 0.35) 1)))

;;; ------------------------------------------------------------------
;;; checkbox and toggle: one behavior, two looks

(define (ui-checkbox ui id x y w h label checked? on-change . opts)
  (let-values ([(kw kh) (ui-checkbox-size label)])   ; cut: a tooltip
    (ui-pressable! ui id x y w h (lambda () (on-change (not checked?)))
                   '(#\space)
                   (cons* 'tip (or (plist-ref opts 'tip #f)
                                   (and (> kw w) label))
                          opts)
                   '((L . "Toggle"))))
  (ui-draw-checkbox x y w h
                    (apply ui-flags ui id (if checked? '(checked) '()))
                    label))

(define (ui-toggle ui id x y w h label on? on-change . opts)
  (ui-pressable! ui id x y w h (lambda () (on-change (not on?)))
                 '(#\space) opts '((L . "Toggle")))
  (ui-draw-toggle x y w h (apply ui-flags ui id (if on? '(checked) '()))
                  label))

(define (ui-checkbox-box) (round (+ (font-line-height 'ui) 1)))

(define-part (ui-draw-checkbox x y w h flags label)
  (let* ([b (ui-checkbox-box)] [by (+ y (floor (/ (- h b) 2)))]
         [lh (font-line-height 'ui)] [checked? (memq 'checked flags)])
    (if checked?
        (begin (fill-round-rect x by b b 4 'accent)
               (ui-draw-glyph x by b b '(primary) 'check))
        (begin (fill-round-rect x by b b 4 'bg-0)
               (stroke-round-rect x by b b 4 (if (memq 'hover flags)
                                                 'accent
                                                 'text-faint))))
    (when label
      (draw-text (+ x b 8) (+ y (/ (- h lh) 2))
                 (fit-text label (- w b 8) 'ui)
                 (if checked? 'text-dim 'text) 'ui))
    (when (memq 'focused flags) (ui-draw-focus x by b b flags 4))))

(define-part (ui-checkbox-size label)
  (values (+ (ui-checkbox-box) 8 (text-width label 'ui)) (ui-control-height)))

;; A toggle's track, the pill its knob slides in: (values w h).
(define (ui-toggle-track)
  (let ([th (+ (ui-glyph-size) 6)]) (values (- (* 2 th) 4) th)))

(define-part (ui-draw-toggle x y w h flags label)
  (let*-values ([(tw th) (ui-toggle-track)]
                [(ty) (+ y (floor (/ (- h th) 2)))]
                [(on? r) (values (memq 'checked flags) (- (/ th 2) 2))]
                [(kx) (if on? (+ x tw (- (/ th 2))) (+ x (/ th 2)))]
                [(lh) (font-line-height 'ui)])
    (fill-round-rect x ty tw th (/ th 2)
                     (cond [on? 'accent] [(memq 'hover flags) 'text-faint]
                           [else 'bg-3]))
    (fill-circle kx (+ ty (/ th 2)) r #xf2f2f5)
    (stroke-circle kx (+ ty (/ th 2)) r (alpha #x000000 0.35) 1)
    (when label
      (draw-text (+ x tw 8) (+ y (/ (- h lh) 2))
                 (fit-text label (- w tw 8) 'ui)
                 (if (memq 'hover flags) 'text 'text-dim) 'ui))
    (when (memq 'focused flags) (ui-draw-focus x ty tw th flags (/ th 2)))))

(define-part (ui-toggle-size label)
  (let-values ([(tw th) (ui-toggle-track)])
    (values (+ tw 8 (text-width label 'ui)) (ui-control-height))))

;;; ------------------------------------------------------------------ tabs

;; choices: ((label . value) ...). Returns the x after the last tab.
(define (ui-tabs ui id x y h choices current on-select)
  (let loop ([cs choices] [tx x])
    (if (null? cs)
        tx
        (let ([label (caar cs)] [value (cdar cs)] [tid (list id (cdar cs))])
          (let-values ([(tw th) (ui-tabs-size (list label))])
            (ui-pressable! ui tid tx y tw h (lambda () (on-select value))
                           '(#\space return) '() (list (cons 'L label)))
            (ui-draw-tab tx y tw h
                         (apply ui-flags ui tid
                                (if (equal? value current) '(selected) '()))
                         label)
            (loop (cdr cs) (+ tx tw)))))))

(define-part (ui-draw-tab x y w h flags label)
  (let ([selected? (memq 'selected flags)] [lh (font-line-height 'ui-bold)])
    (draw-text-centered x y w h label
                        (if (or selected? (memq 'hover flags)) 'text 'text-dim)
                        'ui-bold)
    (when selected?
      (fill-round-rect (+ x 10) (+ y (/ (+ h lh) 2) 3) (- w 20) 2 1 'accent))
    (when (memq 'focused flags) (ui-draw-focus x y w h flags 6))))

;; Each tab is its label in 'ui-bold plus 24.
(define-part (ui-tabs-size labels)
  (values (fold-left (lambda (acc l) (+ acc 24 (text-width l 'ui-bold))) 0
                     labels)
          (round (+ (font-line-height 'ui) 15))))

;;; ------------------------------------------------------------------
;;; labels and surfaces: an app's text and strips, so it never names a
;;; font, a color or a fill

;; A section's label: its font, and whether it is in capitals (Genera
;; heads a pane in bold italic instead).
(theme-default! 'label-section-font 'ui-small)
(theme-default! 'label-section-upcase #t)

(define (ui-label-font flags)
  (cond [(memq 'heading flags) 'heading]
        [(memq 'section flags) (theme 'label-section-font)]
        [(memq 'hint flags) 'ui-small]
        [(memq 'mono flags) 'mono-small]
        [else 'ui]))

(define (ui-label-text flags text)
  (if (and (memq 'section flags) (theme 'label-section-upcase))
      (string-upcase text)
      text))

;; flags: a role (heading body dim hint section mono), wrap, center
;; or right. Without wrap the text is cut to w and centered in h; with it,
;; a paragraph from y. Returns the y below it.
(define-part (ui-draw-label x y w h flags text)
  (let* ([font (ui-label-font flags)]
         [color (cond [(memq 'dim flags) 'text-dim]
                      [(or (memq 'hint flags) (memq 'section flags)
                           (memq 'mono flags))
                       'text-faint]
                      [else 'text])]
         [text (ui-label-text flags text)]
         [lh (font-line-height font)]
         [at (lambda (s)
               (cond [(memq 'right flags) (- (+ x w) (text-width s font))]
                     [(memq 'center flags)
                      (+ x (/ (- w (text-width s font)) 2))]
                     [else x]))])
    (if (memq 'wrap flags)
        (fold-left (lambda (y line) (draw-text (at line) y line color font)
                     (+ y lh))
                   y (ui-label-lines text w font))
        (let ([s (fit-text text w font)])
          (draw-text (at s) (+ y (max 0 (/ (- h lh) 2))) s color font)
          (+ y (max h lh))))))

;; (values w h); w matters only with wrap.
(define-part (ui-label-size flags text w)
  (let* ([font (ui-label-font flags)] [lh (font-line-height font)]
         [text (ui-label-text flags text)])
    (if (memq 'wrap flags)
        (values w (* lh (length (ui-label-lines text w font))))
        (values (text-width text font) lh))))

;; A wrapped label's lines at w, broken as text boxes break them (a word
;; wider than w too), without the space a line ends in at a soft break.
(define (ui-label-lines text w font)
  (let ([n (string-length text)])
    (map (lambda (row)
           (let ([s (car row)] [e (cdr row)])
             (substring text s
                        (if (and (< s e n)
                                 (char=? (string-ref text (- e 1)) #\space)
                                 (not (char=? (string-ref text e) #\newline)))
                            (- e 1)
                            e))))
         (vector->list (edit-wrap text w (lambda (s) (text-width s font)))))))

;; text wrapped at w in font as text boxes wrap it (a long word breaks):
;; at most n lines, the last ending in "…" when more follow. Only the
;; rows shown are computed. => (values lines cut?)
(define (ui-wrap-lines text w font n)
  (let ([lay (edit-layout-of #f text w #f (lambda (s) (text-width s font))
                             #f)])
    (edit-layout-extend! lay n)
    (let* ([cut? (> (edit-layout-count lay) n)]
           [k (min n (edit-layout-count lay))])
      (values (map (lambda (r)
                     (let-values ([(s e) (edit-row lay r)])
                       (if (and cut? (= r (- k 1)))
                           (fit-text (string-append
                                      (substring text s (edit-row-last lay r))
                                      "…")
                                     w font)
                           (substring text s e))))
                   (iota k))
              cut?))))

;; flags: page, toolbar (a line below), side (a line at the left), nav
;; (a deeper column, a line at the right: Settings' tabs), well.
(define-part (ui-draw-surface x y w h flags)
  (cond
    [(memq 'well flags) (fill-round-rect x y w h 8 'bg-0)]
    [(memq 'nav flags)
     (fill-rect x y w h 'bg-0)
     (fill-rect (+ x w -1) y 1 h 'window-border)]
    [else
     (fill-rect x y w h 'bg-1)
     (cond [(memq 'toolbar flags)
            (fill-rect x (- (+ y h) 1) w 1 'window-border)]
           [(memq 'side flags) (fill-rect x y 1 h 'window-border)])]))

;; How wide a well's ring is, if its style draws one (Genera's 1-pixel
;; box): a view that scrolls in the well stops short of it, so that what
;; it cuts never covers it.
(theme-default! 'surface-well-ring 0)
(define (ui-surface-well-ring) (theme 'surface-well-ring))

;;; ------------------------------------------------------------------
;;; text field and text area: one box, a row or wrapped rows

;; The box's inner rect and font, and where an area's scroll bar goes:
;; over its right padding, its thumb a pixel clear of a full row's end.
;; ((edit tx ty tw th font) (bar x y w h))
(define-part (ui-text-box-areas x y w h flags)
  (let ([left (if (memq 'search flags) (+ (ui-glyph-size) 14) 8)]
        [bw (ui-scrollbar-width)])
    (list (list 'edit (+ x left) (+ y 6) (max 0 (- w left 8)) (max 0 (- h 12))
                (if (memq 'large flags) 'heading 'ui))
          (list 'bar (+ x w 1 (- bw)) (+ y 3) bw (max 0 (- h 6))))))

;; How far in from the box's left edge its text starts: a bare box set that
;; much to the left lines its text up with what is above and below it.
;; A box's sizes, areas and drawing are all its place's, field.
(define (ui-text-box-inset flags)
  (with-place 'field
    (lambda () (cadr (car (ui-text-box-areas 0 0 0 0 flags))))))

;; (values w h) for rows of text.
(define-part (ui-text-box-size flags w rows)
  (with-place 'field
    (lambda ()
      (let ([large? (memq 'large flags)])
        (values w
                (if (and (= rows 1) (not large?))
                    (ui-control-height)
                    (round (+ 12 (* rows (font-line-height
                                          (if large? 'heading 'ui)))))))))))

;; The rows of box id's text, tw wide in font (a field's: unwrapped). The
;; kit keeps them, so the layout pass and the frame share them, and a draw
;; computes only the rows it shows. The text the focused box's edit made
;; says where it changed, so that the rows need not compare the texts.
;; They measure in the font they were laid out in (a click comes outside
;; the box's place).
(define (ui-text-layout ui id text tw font area?)
  (let* ([e (ui-edit ui)] [f (get-font font)]
         [lay (edit-layout-of (ui-memo ui (list id 'rows) #f) text
                              (and area? tw) (cons f *scale*)
                              (lambda (s) (text-width s f))
                              (and e (equal? (ui-state-focus ui) id)
                                   (eq? (edit-text e) text)
                                   (edit-change e)))])
    (ui-memo! ui (list id 'rows) lay)
    lay))

;; The text area id as tall as text wrapped at w needs, within the row
;; counts: only rows up to max-rows are computed, which the box draws.
(define-part (ui-text-area-size ui id flags w text min-rows max-rows)
  (with-place 'field
    (lambda ()
      (let* ([a (car (ui-text-box-areas 0 0 w 0 flags))]
             [lay (ui-text-layout ui id text (list-ref a 3) (list-ref a 5)
                                  #t)])
        (edit-layout-extend! lay (- max-rows 1))
        (ui-text-box-size flags w
                          (clamp (edit-layout-count lay) min-rows
                                 max-rows))))))

(define (ui-text-field ui id x y w h text on-change . opts)
  (with-place 'field
    (lambda () (ui-text-box ui id x y w h text on-change #f opts))))

(define (ui-text-area ui id x y w h text on-change . opts)
  (with-place 'field
    (lambda () (ui-text-box ui id x y w h text on-change #t opts))))

;; The focused text's edit, of text: kept, or clamped to text when it
;; changed elsewhere. A new focus puts the caret at the end, or where
;; ui-focus! said.
(define (ui-text-edit ui text)
  (let* ([e (ui-state-edit ui)] [n (string-length text)]
         [e (cond [(edit? e)
                   (if (eq? (edit-text e) text)
                       e
                       (make-edit text (min (edit-caret e) n)
                                  (min (edit-anchor e) n) #f #f))]
                  [(eq? e 'all) (edit-all text)]
                  [(fixnum? e) (edit-at text e)]
                  [else (edit-at text n)])])
    (ui-state-edit-set! ui e)
    e))

;; opts: 'placeholder 'tip 'max (the most characters the text may hold),
;; 'on-enter (a thunk for Return), 'on-key ((lambda (key mods text) ...),
;; true when it acted), and the flags 'large 'bare 'search 'single (an
;; area whose Return is not a newline) 'inner (inside another widget's
;; box: never a box of its own). An area of lines taller than its box
;; has a scroll bar.
(define (ui-text-box ui id x y w h text on-change area? opts)
  (let* ([own (append (ui-opt-flags opts '(large bare search single inner))
                      ;; an area of lines: a box of its own, not a field
                      (if (and area? (not (plist-ref opts 'single #f)))
                          '(lines)
                          '()))]
         [areas (ui-text-box-areas x y w h own)]
         [a (car areas)] [tx (list-ref a 1)] [ty (list-ref a 2)]
         [tw (list-ref a 3)] [th (list-ref a 4)] [font (list-ref a 5)]
         [lh (font-line-height font)]
         [lines? (and area? (not (memq 'single own)))]
         [most (plist-ref opts 'max #f)]
         [on-key (plist-ref opts 'on-key #f)]
         [on-enter (plist-ref opts 'on-enter #f)]
         [lay (ui-text-layout ui id text tw font area?)]
         [e (and (equal? (ui-state-focus ui) id) (ui-text-edit ui text))]
         [page (max 1 (exact (floor (/ th lh))))]
         [dx (if (and e (not area?))
                 (max 0 (- (edit-caret-x lay (edit-caret e)) tw))
                 0)]
         [dy (if area?
                 (let* ([off (ui-memo ui id 0)]
                        [c (and e (edit-caret e))]
                        [moved? (and c (not (eqv? c (ui-memo ui (list id 'caret)
                                                             #f))))]
                        [off (cond
                               [moved?
                                (let ([top (* lh (edit-row-at lay c))])
                                  (min top (max off (- (+ top lh) th))))]
                               ;; the focus left: back to the start
                               [(and (not c) (ui-memo ui (list id 'caret) #f))
                                0]
                               [else off])])
                   ;; the rows in view, and one more
                   (edit-layout-extend! lay (+ (exact (floor (/ (+ off th) lh)))
                                               1))
                   (let ([off (clamp off 0 (max 0 (- (* lh (edit-layout-total
                                                            lay))
                                                     th)))])
                     (ui-memo! ui id off)
                     (ui-memo! ui (list id 'caret) c)
                     off))
                 (begin (edit-layout-extend! lay 0) 0))]
         [content (* lh (edit-layout-total lay))]
         ;; the bar shows an estimate: a scan is asked for even if the
         ;; caret's row, worked out below, brings the rows to the end, so
         ;; that the tick finding them done draws the bar again
         [estimate? (not (edit-layout-done? lay))]
         [max-dy (if area? (max 0 (- content th)) 0)]
         [bar? (and lines? (> max-dy 0))])
    (define (current) (ui-text-edit ui text))
    (define (index-at px py)
      (let ([r (max 0 (exact (floor (/ (- (+ py dy) ty) lh))))])
        (edit-layout-extend! lay r)
        (edit-index-at lay (min r (- (edit-layout-count lay) 1))
                       (- (+ px dx) tx))))
    ;; What moves the caret or changes the text, or acts on a key, shows:
    ;; the box asks to be scrolled into view at the next draw.
    (define (show!) (ui-state-reveal-set! ui (cons id #f)) #t)
    (define (apply! e2)
      (ui-state-edit-set! ui e2)
      (ui-state-blink-at-set! ui (now-ms))
      (unless (eq? (edit-text e2) text) (on-change (edit-text e2)))
      (show!))
    (define (key! key mods)
      (let ([e (current)])
        (cond
          [(and on-key (on-key key mods (edit-text e))) (show!)]
          [(and (equal? mods '(ctrl)) (memv key '(#\c #\x #\v)))
           (let ([s (edit-selection e)])
             (case key
               [(#\c) (unless (string=? s "") (clipboard-set! s)) #t]
               [(#\x) (unless (string=? s "")
                        (clipboard-set! s)
                        (apply! (edit-erase e 0)))
                      #t]
               [else (apply! (edit-insert
                              e (edit-clean (clipboard-get) lines?) most))]))]
          [(eq? key 'return)
           (cond [lines? (apply! (edit-insert e "\n" most))]
                 [on-enter (on-enter) (show!)]
                 [else #f])]
          [(edit-key e key mods lay page)
           => (lambda (e2)
                ;; Page Up and Down scroll the view as many rows as they
                ;; move the caret, which keeps its place in it
                (when (and area? (memq key '(page-up page-down)))
                  (ui-memo! ui id (max 0 (+ (ui-memo ui id 0)
                                            (* lh (if (eq? key 'page-up)
                                                      (- page)
                                                      page))))))
                (apply! e2))]
          [else #f])))
    ;; Asked to be shown, a focused box names its caret's row, where the
    ;; typing goes, for a scroll view too short for all of it; its first
    ;; and last rows bring the box's edges along.
    (let ([rv (ui-state-reveal ui)])
      (when (and e rv (equal? (car rv) id))
        (let* ([r (edit-row-at lay (edit-caret e))]
               [top (- (+ (- ty y) (* r lh)) dy)]
               [last? (begin (edit-layout-extend! lay (+ r 1))
                             (>= (+ r 1) (edit-layout-count lay)))])
          (ui-state-reveal-set!
           ui (cons id (cons (if (= r 0) 0 top) (if last? h (+ top lh))))))))
    (ui-region! ui id x y w h 'focus 'click 'cursor 'ibeam
      'tip (plist-ref opts 'tip #f)
      'doc (plist-ref opts 'doc '((L . "Place the caret")
                                   (L2 . "Select a word")))
      'on (lambda (ev)
            (case (car ev)
              [(press)
               (and (eq? (list-ref ev 3) 'left)
                    (let ([i (index-at (cadr ev) (caddr ev))])
                      (apply!
                       (case (list-ref ev 4)
                         [(1) (edit-at text i)]
                         [(2) (let-values ([(lo hi) (edit-word-bounds text i)])
                                (edit-select (current) lo hi))]
                         [else (let-values ([(lo hi) (edit-line-bounds text i)])
                                 (edit-select (current) lo hi))]))))]
              [(drag)
               (and (eqv? (cadddr ev) 1)
                    (apply! (edit-move (current)
                                       (index-at (cadr ev) (caddr ev)) #t)))]
              [(key) (key! (cadr ev) (caddr ev))]
              [(text) (apply! (edit-insert
                               (current) (edit-clean (cadr ev) lines?) most))]
              [(wheel)
               (and (> max-dy 0)
                    (let* ([old (ui-memo ui id 0)]
                           [new (clamp (- old (* (cadr ev) (ui-scroll-step)))
                                       0 max-dy)])
                      (and (not (= new old)) (begin (ui-memo! ui id new) #t))))]
              [else #f])))
    (let* ([base (apply ui-flags ui id own)]
           [flags (append base
                          (if (and (memq 'focused base) (ui-caret-on? ui))
                              '(caret)
                              '())
                          (if (string=? text "") '(placeholder) '()))])
      (let-values ([(lo hi) (if (and e (memq 'focused base))
                                (edit-range e)
                                (values 0 0))])
        (ui-draw-text-box x y w h flags lay lo hi (if e (edit-caret e) 0) dx dy
                          (plist-ref opts 'placeholder #f))))
    ;; the bar's total is an estimate until the ticks finish the rows
    (when bar?
      (when estimate?
        (ui-state-scans-set! ui (cons lay (ui-state-scans ui))))
      (apply ui-scrollbar ui (list id 'bar)
             (append (cdr (cadr areas))
                     (list dy content th
                           (lambda (off) (ui-memo! ui id off))))))))

;; lay: the text's rows (an edit-layout), those in view computed; [lo, hi)
;; the selection; dx and dy how far the text is scrolled.
(define-part (ui-draw-text-box x y w h flags lay lo hi caret dx dy placeholder)
  (let* ([a (car (ui-text-box-areas x y w h flags))]
         [tx (list-ref a 1)] [ty (list-ref a 2)] [tw (list-ref a 3)]
         [th (list-ref a 4)] [font (list-ref a 5)]
         [lh (font-line-height font)] [text (edit-layout-text lay)]
         [measure (lambda (s) (text-width s font))]
         [focused? (memq 'focused flags)] [hover? (memq 'hover flags)])
    (unless (or (memq 'inner flags)
                (and (memq 'bare flags) (not hover?) (not focused?)))
      (ui-draw-field x y w h flags))
    (when (memq 'search flags)
      (let ([s (+ (ui-glyph-size) 2)])
        (ui-draw-glyph (+ x 7) (+ y (/ (- h s) 2)) s s '(quiet) 'search)))
    ;; The edit rect, and 2 px either side for the caret: a scrolled row
    ;; never shows in the padding. Scrolled sideways, the caret is at the
    ;; right, so the left margin goes.
    (call-with-clip (if (> dx 0) tx (- tx 2)) ty (+ tw (if (> dx 0) 2 4)) th
      (lambda ()
        (if (memq 'placeholder flags)
            (when placeholder
              (draw-text tx ty (fit-text placeholder tw font) 'text-faint font))
            ;; only the rows in view
            (let loop ([r (max 0 (- (exact (floor (/ dy lh))) 1))])
              (let ([ry (- (+ ty (* r lh)) dy)] [rx (- tx dx)])
                (when (and (< r (edit-layout-count lay)) (< (+ ry 1) (+ ty th)))
                  ;; a row a float's width in shows only its descenders' tips
                  (when (> (+ ry lh) (+ ty 1))
                    (let-values ([(s e) (edit-row lay r)])
                      (when (and (< lo hi) (< lo (max e (+ s 1))) (> hi s))
                        (let* ([x0 (measure (substring text s (max s lo)))]
                               [x1 (if (> hi e)
                                       (+ (measure (substring text s e)) 4)
                                       (measure (substring text s hi)))])
                          (if (eq? (theme 'text-selection) 'underline)
                              (px-fill (+ rx x0) (+ ry lh -1) (- x1 x0) 1
                                       'text)
                              (fill-rect (+ rx x0) ry (- x1 x0) lh
                                         'selection))))
                      ;; unfocused at its start, the last row in view says
                      ;; that more follows
                      (draw-text rx ry
                                 (if (and (not focused?) (= dy 0)
                                          (or (< (+ r 1)
                                                 (edit-layout-count lay))
                                              (not (edit-layout-done? lay)))
                                          (> (+ ry lh lh) (+ ty th 1)))
                                     (fit-text
                                      (string-append
                                       (substring text s (edit-row-last lay r))
                                       "…")
                                      tw font)
                                     (substring text s e))
                                 'text font)))
                  (loop (+ r 1))))))
        (when (memq 'caret flags)
          (let ([cx (+ (- tx dx) (edit-caret-x lay caret))]
                [cy (- (+ ty (* lh (edit-row-at lay caret))) dy)])
            (if (eq? (theme 'text-caret) 'block)
                (px-invert cx cy (ui-caret-block-width text caret font) lh)
                (fill-rect (- cx 1) cy 2 lh 'accent))))))))

;; How the text's caret and selection show: a bar and a fill, or as the
;; Lisp machines showed them, a block in reverse video over the character
;; and an underline (block, underline).
(theme-default! 'text-caret 'bar)
(theme-default! 'text-selection 'fill)

;; A block caret is as wide as the character under it; at the end of the
;; text or of a line, the font's own width.
(define (ui-caret-block-width text i font)
  (if (and (< i (string-length text))
           (not (char=? (string-ref text i) #\newline)))
      (text-width (string (string-ref text i)) font)
      (font-char-width font)))

;;; ------------------------------------------------------------------ scroll

;; A scroll view: body is (lambda (top) ... bottom), drawing its content
;; from top and returning where it ends; it is clipped to the view, and
;; the wheel and a scroll bar over its edge move it. The style places the
;; bar (scrollbar-side right, left, or outside: over all of its frame's
;; region of kind scrollbar, for a view against the window's left edge;
;; on the right where there is none) and shows it
;; always or while the pointer is over the view (scrollbar-show hover;
;; outside, over the margin); it lies over the content, which never
;; reflows for it: content that keeps clear of it asks how far
;; (ui-scrollbar-inset).
(define (ui-scroll ui id x y w h body)
  (let ([off (car (ui-memo ui id '(0 . 0)))]
        [clip (ui-state-clip ui)])
    (ui-region! ui id x y w h 'on
      (lambda (ev)
        (and (eq? (car ev) 'wheel)
             (let* ([m (ui-memo ui id '(0 . 0))]
                    [new (clamp (- (car m) (* (cadr ev) (ui-scroll-step)))
                                0 (cdr m))])
               (and (not (= new (car m)))
                    (begin (ui-memo! ui id (cons new (cdr m))) #t))))))
    (let ([before (ui-state-regions ui)])
      (ui-state-clip-set! ui (call-with-values
                               (lambda () (ui-cut x y w h clip))
                               list))
      (let* ([bottom (ui-call-with-pixel-clip x y w h
                                              (lambda () (body (- y off))))]
             [content (- bottom (- y off))]
             [most (max 0 (- content h))]
             [now (car (ui-memo ui id '(0 . 0)))]
             [new (clamp now 0 most)])
        (ui-state-clip-set! ui clip)
        (ui-memo! ui id (cons new most))
        (unless (= new off) (invalidate! (current-window)))
        ;; drawn in it: shown, a gap clear of the edges, where a focus
        ;; ring drawn around it shows whole
        (let ([rv (ui-state-reveal ui)])
          (when rv
            (let loop ([rs (ui-state-regions ui)])
              (unless (eq? rs before)
                (if (equal? (ui-region-id (car rs)) (car rv))
                    ;; all of it when it fits, else the span it asks for
                    ;; (a text box's caret row)
                    (let* ([f (ui-region-frame (car rs))]
                           [top (- (cadr f) (- y off))]
                           [span (if (and (cdr rv)
                                          (> (+ (cadddr f) (* 2 (ui-gap))) h))
                                     (cdr rv)
                                     (cons 0 (cadddr f)))])
                      (ui-state-reveal-set! ui #f)
                      (ui-reveal! ui id (- (+ top (car span)) (ui-gap))
                                  (+ top (cdr span) (ui-gap)) h))
                    (loop (cdr rs)))))))
        (let* ([win (current-window)] [bid (list id 'bar)]
               [side (theme 'scrollbar-side)]
               [r (and win (eq? side 'outside) (zero? x)
                       (assq 'scrollbar (frame-regions win)))]
               [out? (and r #t)]   ; all of the region its frame leaves
               [bw (if out? (list-ref r 3) (ui-scrollbar-width))]
               [hover (and win (window-hover win))])
          (when (and (> content h)
                     (or (eq? (theme 'scrollbar-show) 'always)
                         (ui-active? ui bid)
                         (if out?
                             (eq? hover 'scrollbar)
                             (and (eq? hover 'content)
                                  (ui-hover? ui x y w h)))))
            ;; outside, all of the region (in the content's coordinates)
            (let-values ([(bx by bh)
                          (if out?
                              (let-values ([(l t . rb) (frame-insets win)])
                                (values (- (cadr r) l) (- (caddr r) t)
                                        (list-ref r 4)))
                              (values (if (eq? side 'left) x (- (+ x w) bw))
                                      y h))])
              (ui-scrollbar ui bid bx by bw bh new content h
                            (lambda (off) (ui-memo! ui id (cons off most))))
              (when out?   ; for the frame's over pass: ui-frame-scrollbar
                (hashtable-set! (window-props win) 'scrollbar
                                (list bx by bw bh (ui-flags ui bid)
                                      (/ new content) (/ h content)))))))))))

;; thunk drawn clipped to x y w h, its edges on whole device pixels, so
;; that what it cuts ends in a crisp edge at any scale: every scroll
;; view's clip, the kit's and an app's own.
(define (ui-call-with-pixel-clip x y w h thunk)
  (let-values ([(x0 y0) (px-snap x y)] [(x1 y1) (px-snap (+ x w) (+ y h))])
    (call-with-clip x0 y0 (- x1 x0) (- y1 y0) thunk)))

;; A scroll bar at x y w h, for a view h high showing content from off on:
;; pressing it, on its thumb or beside it, and dragging set the offset by
;; (set-off! off).
(define (ui-scrollbar ui id x y w h off content view set-off!)
  (let* ([most (max 0 (- content view))]
         [start (/ off content)] [size (/ view content)]
         [thumb (cdr (assq 'thumb (ui-scrollbar-areas x y w h start size)))])
    (define (thumb-to! ty)
      (set-off! (clamp (* most (/ (- ty y) (max 1 (- h (cadddr thumb)))))
                       0 most))
      #t)
    (ui-region! ui id x y w h 'focus 'keep 'doc '((L . "Scroll")) 'on
      (lambda (ev)
        (case (car ev)
          [(press)
           (and (eq? (list-ref ev 3) 'left)
                (let* ([py (caddr ev)] [ty (cadr thumb)]
                       [grab (if (<= ty py (+ ty (cadddr thumb)))
                                 (- py ty)
                                 (/ (cadddr thumb) 2))])
                  (ui-memo! ui (list id 'grab) grab)
                  (thumb-to! (- py grab))))]
          [(drag) (thumb-to! (- (caddr ev) (ui-memo ui (list id 'grab) 0)))]
          ;; as its view does: an outside bar lies over no view
          [(wheel) (let ([new (clamp (- off (* (cadr ev) (ui-scroll-step)))
                                     0 most)])
                     (and (not (= new off)) (begin (set-off! new) #t)))]
          [else #f])))
    (ui-draw-scrollbar x y w h (ui-flags ui id) start size)))

;; Scrolls view id so that [top, bottom) of its content is in view (its
;; top first); invalidates when the offset moved. Not clamped to the last
;; draw's content, which may not hold [top, bottom) yet (a card just
;; added): the scroll view clamps as it draws.
(define (ui-reveal! ui id top bottom view-h)
  (let* ([m (ui-memo ui id '(0 . 0))]
         [new (max 0 (min top (max (car m) (- bottom view-h))))])
    (unless (= new (car m))
      (ui-memo! ui id (cons new (cdr m)))
      (invalidate! (current-window)))))

(theme-default! 'scrollbar-width 10)
(theme-default! 'scrollbar-side 'right)
(theme-default! 'scrollbar-show 'always)
;; The room content keeps clear past a bar always shown at its left or
;; right: #f, none (the bar lies over the content), or a margin past the
;; bar's width (Genera's).
(theme-default! 'scrollbar-margin #f)
(define (ui-scrollbar-width) (theme 'scrollbar-width))

;; What a scroll view's content keeps clear for its bar on side (left or
;; right): the kit's tables and trees keep it, and an app's own content
;; may (Tasks' cards).
(define (ui-scrollbar-inset side)
  (let ([m (theme 'scrollbar-margin)])
    (if (and m
             (eq? side (theme 'scrollbar-side))
             (eq? (theme 'scrollbar-show) 'always))
        (+ (ui-scrollbar-width) m)
        0)))

;; The window's outside scroll bar, which its main scroll view leaves in
;; its 'scrollbar while it shows: a style that leaves a margin for it
;; (scrollbar-side outside, a region of kind scrollbar there) draws it so
;; in its frame's over pass, after the content.
(define (ui-frame-scrollbar win)
  (let ([b (window-ref win 'scrollbar)])
    (when b
      (let-values ([(cx cy cw ch) (content-rect win)])
        (apply (lambda (x y w h flags start size)
                 (ui-draw-scrollbar (+ cx x) (+ cy y) w h flags start size))
               b)))))

;; How far one wheel step scrolls: three lines.
(define (ui-scroll-step) (* 3 (font-line-height 'ui)))

;; start and size: the view's top and height as fractions of the content.
(define-part (ui-scrollbar-areas x y w h start size)
  (let ([th (min h (max (* 2 (font-line-height 'ui)) (* size h)))])
    (list (list 'thumb x
                (+ y (if (< size 1) (* (- h th) (/ start (- 1 size))) 0))
                w th))))

(define-part (ui-draw-scrollbar x y w h flags start size)
  (let ([t (cdr (assq 'thumb (ui-scrollbar-areas x y w h start size)))])
    (fill-round-rect (+ x 2) (+ (cadr t) 2) (- w 5) (- (cadddr t) 4) 2.5
                     (alpha 'text (if (or (memq 'hover flags)
                                          (memq 'pressed flags))
                                      0.4 0.2)))))

;;; ------------------------------------------------------------------ table

;; columns: ((key title width) ...); rows: the app's values, selected the
;; one eq? to it; a left press calls on-select. cell draws a cell:
;; (lambda (row key x y w h flags) ...). opts: 'sort ((key . up|down) or
;; #f), 'on-sort (key), 'on-open (row) on a double click, 'on-menu (row x
;; y) on a right press, 'tip (row): a row's tooltip, or #f, and 'reveal:
;; true scrolls the selected row into view.
(define (ui-table ui id x y w h columns rows selected on-select cell . opts)
  (with-place 'list
    (lambda ()
      (let* (;; the scroll view's, its bar in a margin the content keeps
             [sx x] [sw w]
             [x (+ sx (ui-scrollbar-inset 'left))]
             [w (- sw (ui-scrollbar-inset 'left) (ui-scrollbar-inset 'right))]
             [hh (ui-table-header-height)] [rh (ui-table-row-height)]
             [sort (plist-ref opts 'sort #f)]
             [tip (plist-ref opts 'tip (lambda (r) #f))]
             [on-sort (plist-ref opts 'on-sort (lambda (k) #f))]
             [on-open (plist-ref opts 'on-open (lambda (r) #f))]
             [on-menu (plist-ref opts 'on-menu (lambda (r x y) #f))]
             [body (list id 'body)] [view-h (max 0 (- h hh))]
             [i (and (plist-ref opts 'reveal #f)
                     (list-index (lambda (r) (eq? r selected)) rows))]
             [rows (list->vector rows)] [n (vector-length rows)])
        (let loop ([cs columns] [cx x])
          (unless (null? cs)
            (let* ([key (caar cs)] [cw (caddr (car cs))]
                   [hid (list id 'header key)])
              (ui-pressable! ui hid cx y cw hh (lambda () (on-sort key)) '()
                             '(focus keep) '((L . "Sort")))
              (ui-draw-table-header cx y cw hh (ui-flags ui hid) (cadar cs)
                                    (and sort (eq? (car sort) key) (cdr sort)))
              (loop (cdr cs) (+ cx cw)))))
        (when i (ui-reveal! ui body (* i rh) (* (+ i 1) rh) view-h))
        (ui-memo! ui (list id 'page) (max 1 (exact (floor (/ view-h rh)))))
        (ui-scroll ui body sx (+ y hh) sw view-h
          (lambda (top)
            (let ([vy (+ y hh)])
              (do ([i (max 0 (exact (floor (/ (- vy top) rh)))) (+ i 1)])
                  ((or (>= i n) (>= (+ top (* i rh)) (+ vy view-h))))
                (let* ([row (vector-ref rows i)] [ry (+ top (* i rh))]
                       [rid (list id 'row i)])
                  (ui-region! ui rid x ry w rh 'tip (tip row)
                    'doc '((L . "Select")) 'on
                    (lambda (ev)
                      (and (eq? (car ev) 'press)
                           (case (list-ref ev 3)
                             [(left) (on-select row)
                                     (when (>= (list-ref ev 4) 2) (on-open row))
                                     #t]
                             [(right) (on-menu row (cadr ev) (caddr ev)) #t]
                             [else #f]))))
                  (let ([flags (apply ui-flags ui rid
                                      (if (eq? row selected) '(selected) '()))])
                    (ui-draw-table-row x ry w rh flags)
                    (let cells ([cs columns] [cx x])
                      (unless (null? cs)
                        (cell row (caar cs) cx ry (caddr (car cs)) rh flags)
                        (cells (cdr cs) (+ cx (caddr (car cs))))))
                    (when (eq? row selected)
                      (ui-draw-highlight-over x ry w rh '()))))))
            (+ top (* n rh))))))))

;; The new row index for a key, #f for any other: Up/Down ±1,
;; PageUp/PageDown ±page, Home 0, End n−1; from i #f, Down, PageDown and
;; Home give 0, the others n−1; #f when n is 0.
(define (ui-table-step i n page key)
  (and (> n 0)
       (case key
         [(up) (if i (max 0 (- i 1)) (- n 1))]
         [(down) (if i (min (- n 1) (+ i 1)) 0)]
         [(page-up) (if i (max 0 (- i page)) (- n 1))]
         [(page-down) (if i (min (- n 1) (+ i page)) 0)]
         [(home) 0]
         [(end) (- n 1)]
         [else #f])))

;; Rows per page of table id at its last draw.
(define (ui-table-page ui id) (ui-memo ui (list id 'page) 1))

(theme-default! 'table-header-height
                (theme-rule (round (+ (font-line-height 'ui-small) 13))))
(theme-default! 'table-row-height
                (theme-rule (round (+ (font-line-height 'ui) 13))))
(define (ui-table-header-height) (theme 'table-header-height))
(define (ui-table-row-height) (theme 'table-row-height))

;; dir: up, down or #f (not sorted by it).
(define-part (ui-draw-table-header x y w h flags label dir)
  (let ([sh (font-line-height 'ui-small)] [g (ui-small-glyph-size)])
    (fill-rect x (- (+ y h) 1) w 1 'window-border)
    (let ([end (draw-text (+ x 10) (+ y (/ (- h sh) 2))
                          (fit-text label (max 0 (- w 16 g)) 'ui-small)
                          (if (or dir (memq 'hover flags)) 'text 'text-faint)
                          'ui-small)])
      (when dir
        (ui-draw-glyph (+ end 4) (+ y (/ (- h g) 2)) g g '(hover) dir)))))

(define-part (ui-draw-table-row x y w h flags)
  (cond [(memq 'selected flags) (fill-rect x y w h 'selection)]
        [(memq 'hover flags) (fill-rect x y w h 'bg-2)])
  (fill-rect x (- (+ y h) 1) w 1 (alpha 'window-border 0.6)))

;;; ------------------------------------------------------------------ tree
;;; A tree of the app's values (nodes): its roots and a children
;;; procedure, shown as rows with columns, as a table's, the first column
;;; indented by depth, with a twisty that expands and collapses a node
;;; that has children. The kit keeps which nodes are expanded; the app,
;;; which is selected. Only expanded nodes' children are asked for, and
;;; only the rows in view are drawn. Like the table it takes no keyboard
;;; focus: the app passes it keys (ui-tree-key).

;; A row as shown: its node, depth (0: a root), its parent's row (#f for
;; a root), whether the node has children, and whether they show.
(define-record-type ui-tree-row
  (nongenerative schevana-ui-tree-row-1)
  (fields node depth parent kids? expanded?))

;; The rows shown: each root and, under each expanded node, its children,
;; depth first. children: (lambda (node) ...) a list, empty for a leaf;
;; expanded?: (lambda (node) ...). Pure. => a vector of ui-tree-row
(define (ui-tree-rows roots children expanded?)
  (let ([acc '()] [n 0])
    (let walk ([nodes roots] [depth 0] [parent #f])
      (for-each (lambda (node)
                  (let* ([kids (children node)]
                         [open? (and (pair? kids) (expanded? node) #t)]
                         [i n])
                    (set! acc (cons (make-ui-tree-row node depth parent
                                                      (pair? kids) open?)
                                    acc))
                    (set! n (+ n 1))
                    (when open? (walk kids (+ depth 1) i))))
                nodes))
    (list->vector (reverse acc))))

;; The row of rows whose node is equal? to node, or #f.
(define (ui-tree-index rows node)
  (let loop ([i 0])
    (cond [(= i (vector-length rows)) #f]
          [(equal? (ui-tree-row-node (vector-ref rows i)) node) i]
          [else (loop (+ i 1))])))

;; Is row j of rows (#f: none) under row i, at any depth? (A row's parent
;; comes before it.)
(define (ui-tree-under? rows j i)
  (let up ([p (and j (ui-tree-row-parent (vector-ref rows j)))])
    (and p (>= p i)
         (or (= p i) (up (ui-tree-row-parent (vector-ref rows p)))))))

;; What key does to rows (as ui-tree-rows makes them), row i selected (#f:
;; none), page rows a page: (select . j), (expand . i), (collapse . i),
;; (open . i), or #f. Up, Down, Page Up and Down, Home and End move as in
;; a table; Right expands, or goes to the first child; Left collapses, or
;; goes to the parent; Space expands or collapses; Return opens. Pure.
(define (ui-tree-step rows i page key)
  (let ([r (and i (vector-ref rows i))])
    (case key
      [(right) (and r (ui-tree-row-kids? r)
                    (if (ui-tree-row-expanded? r)
                        (cons 'select (+ i 1))
                        (cons 'expand i)))]
      [(left) (and r (if (ui-tree-row-expanded? r)
                         (cons 'collapse i)
                         (let ([p (ui-tree-row-parent r)])
                           (and p (cons 'select p)))))]
      [(#\space) (and r (ui-tree-row-kids? r)
                      (cons (if (ui-tree-row-expanded? r) 'collapse 'expand)
                            i))]
      [(return) (and r (cons 'open i))]
      [else (let ([j (ui-table-step i (vector-length rows) page key)])
              (and j (cons 'select j)))])))

;; Which nodes of tree id are expanded or collapsed, where it differs
;; from how they start: a hashtable kept by the kit.
(define (ui-tree-states ui id)
  (or (ui-memo ui (list id 'expanded) #f)
      (let ([states (make-hashtable equal-hash equal?)])
        (ui-memo! ui (list id 'expanded) states)
        states)))

;; Expands node in tree id (on? #t) or collapses it, from its next draw.
(define (ui-tree-expand! ui id node on?)
  (let ([states (ui-tree-states ui id)])
    (unless (eq? on? (hashtable-ref states node 'unset))
      (hashtable-set! states node on?)
      (ui-memo! ui (list id 'generation) #f)   ; its rows change
      (invalidate! (current-window)))))

;; A key the app's key handler passes to tree id, as its last draw showed
;; it (see ui-tree-step): the selection moves by (on-select node), Return
;; calls (on-open node); true when it acted. A tree the last draw did not
;; show (the app showed something else instead) takes none.
(define (ui-tree-key ui id key mods selected on-select on-open)
  (let* ([rows (if (ui-find ui (list id 'body))
                   (ui-memo ui (list id 'rows) '#())
                   '#())]
         [act (and (null? mods)
                   (ui-tree-step rows (ui-tree-index rows selected)
                                 (ui-memo ui (list id 'page) 1) key))])
    (and act
         (let ([node (ui-tree-row-node (vector-ref rows (cdr act)))])
           (case (car act)
             [(select) (on-select node)]
             [(open) (on-open node)]
             [else (ui-tree-expand! ui id node (eq? (car act) 'expand))])
           #t))))

;; roots: the top nodes; children: (lambda (node) ...), a list. columns:
;; ((key title width) ...), as a table's, under a header; the first holds
;; the tree. selected: the node equal? to it is selected. A left press
;; calls on-select, or on a twisty expands or collapses. cell draws a
;; cell: (lambda (node key x y w h flags) ...), the first column's after
;; the indentation and twisty. opts: 'expanded (nodes start expanded;
;; else collapsed), 'on-open (node) on a double click, 'on-menu (node x y)
;; on a right press, 'on-hide (node) when a collapse hides the selected
;; row: node, the collapsed one, is to be selected, as Left would (by
;; default on-select), 'tip (node w): its tooltip, or #f, w its first
;; cell's width, 'reveal: true scrolls the selected row into view, 'dim
;; (node): true dims its row (the flag dim), and 'generation: a value that
;; changes (eqv?) whenever roots or children would give other nodes, so
;; the rows are kept until it does or a node is expanded or collapsed
;; (without it, they are made at every draw).
(define (ui-tree ui id x y w h columns roots children selected on-select cell
                 . opts)
  (with-place 'list
    (lambda ()
      (let* (;; the scroll view's, its bar in a margin the content keeps
             [sx x] [sw w]
             [x (+ sx (ui-scrollbar-inset 'left))]
             [w (- sw (ui-scrollbar-inset 'left) (ui-scrollbar-inset 'right))]
             [hh (ui-table-header-height)] [rh (ui-tree-row-height)]
             [tip (plist-ref opts 'tip (lambda (node w) #f))]
             [on-open (plist-ref opts 'on-open (lambda (node) #f))]
             [on-menu (plist-ref opts 'on-menu (lambda (node x y) #f))]
             [on-hide (plist-ref opts 'on-hide on-select)]
             [dim? (plist-ref opts 'dim (lambda (node) #f))]
             [start (plist-ref opts 'expanded #f)]
             [generation (plist-ref opts 'generation #f)]
             [states (ui-tree-states ui id)]
             [rows (if (and generation
                            (eqv? generation
                                  (ui-memo ui (list id 'generation) #f)))
                       (ui-memo ui (list id 'rows) #f)
                       (ui-tree-rows roots children
                                     (lambda (node)
                                       (hashtable-ref states node start))))]
             [n (vector-length rows)] [body (list id 'body)]
             [view-h (max 0 (- h hh))] [w0 (caddr (car columns))]
             [c0 (cdr (assq 'cell (ui-tree-areas x y w0 rh 0)))])
        (ui-memo! ui (list id 'rows) rows)
        (ui-memo! ui (list id 'generation) generation)
        ;; the first title over a root's cell
        (ui-draw-table-header x y (- (car c0) x) hh '() "" #f)
        (let loop ([cs columns] [cx (car c0)] [cw (- w0 (- (car c0) x))])
          (unless (null? cs)
            (ui-draw-table-header cx y cw hh '() (cadar cs) #f)
            (unless (null? (cdr cs))
              (loop (cdr cs) (+ cx cw) (caddr (cadr cs))))))
        (when (plist-ref opts 'reveal #f)
          (let ([i (ui-tree-index rows selected)])
            (when i (ui-reveal! ui body (* i rh) (* (+ i 1) rh) view-h))))
        (ui-memo! ui (list id 'page) (max 1 (exact (floor (/ view-h rh)))))
        (ui-scroll ui body sx (+ y hh) sw view-h
          (lambda (top)
            (let ([vy (+ y hh)])
              (do ([i (max 0 (exact (floor (/ (- vy top) rh)))) (+ i 1)])
                  ((or (>= i n) (>= (+ top (* i rh)) (+ vy view-h))))
                (let* ([r (vector-ref rows i)] [node (ui-tree-row-node r)]
                       [ry (+ top (* i rh))] [rid (list id 'row i)]
                       [areas (ui-tree-areas x ry w0 rh (ui-tree-row-depth r))]
                       [twisty (cdr (assq 'twisty areas))]
                       [c (cdr (assq 'cell areas))])
                  (ui-region! ui rid x ry w rh 'tip (tip node (caddr c))
                    'doc '((L . "Select or open")) 'on
                    (lambda (ev)
                      (and (eq? (car ev) 'press)
                           (case (list-ref ev 3)
                             [(left)
                              (if (and (ui-tree-row-kids? r)
                                       (apply point-in? (cadr ev) (caddr ev)
                                              twisty))
                                  (let ([open? (ui-tree-row-expanded? r)])
                                    (ui-tree-expand! ui id node (not open?))
                                    (when (and open?
                                               (ui-tree-under?
                                                rows
                                                (ui-tree-index rows selected)
                                                i))
                                      (on-hide node)))
                                  (begin
                                    (on-select node)
                                    (when (>= (list-ref ev 4) 2)
                                      (on-open node))))
                              #t]
                             [(right) (on-menu node (cadr ev) (caddr ev)) #t]
                             [else #f]))))
                  (let ([flags (apply ui-flags ui rid
                                      (append
                                       (if (equal? node selected)
                                           '(selected)
                                           '())
                                       (if (dim? node) '(dim) '())
                                       (if (ui-tree-row-kids? r) '(kids) '())
                                       (if (ui-tree-row-expanded? r)
                                           '(expanded)
                                           '())))])
                    (ui-draw-tree-row x ry w rh flags w0 (ui-tree-row-depth r))
                    (cell node (caar columns) (car c) ry (caddr c) rh flags)
                    (let cells ([cs (cdr columns)] [cx (+ x w0)])
                      (unless (null? cs)
                        (cell node (caar cs) cx ry (caddr (car cs)) rh flags)
                        (cells (cdr cs) (+ cx (caddr (car cs))))))
                    (when (memq 'selected flags)
                      (ui-draw-highlight-over x ry w rh '())))))
              (+ top (* n rh)))))))))

;; A tree's rows are a table's height.
(define (ui-tree-row-height) (ui-table-row-height))

;; Where a row at depth puts its twisty and its first cell, in its first
;; column x y w h: indented a twisty's width a level, but by at most half
;; the column, so that a deep row keeps room for its cell.
;; ((twisty x y w h) (cell x y w h))
(define-part (ui-tree-areas x y w h depth)
  (let* ([s (+ (ui-glyph-size) 8)]
         [in (min (* depth s) (max 0 (floor (/ (- w s) 2))))])
    (list (list 'twisty (+ x 4 in) y s h)
          (list 'cell (+ x 4 in s) y (max 0 (- w 4 in s)) h))))

;; A row's look: a table row's, and the twisty of a node with children
;; (flags kids, expanded) at its depth in its first column, w0 wide; dim,
;; all of it left of its first cell, as the cells dim themselves.
(define-part (ui-draw-tree-row x y w h flags w0 depth)
  (let ([areas (ui-tree-areas x y w0 h depth)] [g (ui-glyph-size)])
    (ui-draw-table-row x y w h flags)
    (when (memq 'kids flags)
      (let ([t (cdr (assq 'twisty areas))])
        (ui-draw-glyph (+ (car t) (/ (- (caddr t) g) 2)) (+ y (/ (- h g) 2))
                       g g (if (memq 'hover flags) '(hover) '(quiet))
                       (if (memq 'expanded flags)
                           'chevron-down
                           'chevron-right))))
    (ui-draw-dim x y (- (cadr (assq 'cell areas)) x) h flags)))

;; A row a view shows but leaves out (flags: dim; the app's search, say),
;; or a part of it, x y w h: shaded, unless it is selected.
(define-part (ui-draw-dim x y w h flags)
  (when (and (memq 'dim flags) (not (memq 'selected flags)))
    (fill-rect x y w h (alpha 'bg-0 0.6))))

;;; ------------------------------------------------------------------ chips

;; A tag: a pill whose hue comes from its label, so a tag keeps its color
;; everywhere; removable adds a ×, which hover turns red.
(define-part (ui-draw-chip x y w h flags label)
  (let* ([sh (font-line-height 'ui-small)] [r (/ h 2)]
         [g (ui-small-glyph-size)] [removable? (memq 'removable flags)]
         [hue (app-color (string->symbol label))])
    (fill-round-rect x y w h r (alpha hue 0.18))
    (draw-text (+ x 8) (+ y (/ (- h sh) 2))
               (fit-text label (- w 16 (if removable? (+ g 4) 0)) 'ui-small)
               'text 'ui-small)
    (when removable?
      (let ([a (cdr (assq 'remove (ui-chip-areas x y w h)))])
        (ui-draw-glyph (+ (car a) (/ (- (caddr a) g) 2))
                       (+ (cadr a) (/ (- (cadddr a) g) 2)) g g
                       (if (memq 'hover flags) '(danger hover) '())
                       'cross)))))

;; The text 8 in from either end; removable, a × after it.
(define-part (ui-chip-size label removable?)
  (values (+ 16 (text-width label 'ui-small)
             (if removable? (+ (ui-small-glyph-size) 4) 0))
          (round (+ (font-line-height 'ui-small) 8))))

(define-part (ui-chip-areas x y w h)
  (let ([g (ui-small-glyph-size)])
    (list (list 'remove (- (+ x w) g 10) y (+ g 6) h))))

;; Chips (removable) and a field to type more, wrapped in one box. The
;; draft is the field's text; a comma or Return adds it, Backspace in an
;; empty field removes the last chip. on-add gets a list: the draft, or a
;; paste's comma-separated parts all at once. Returns the box's bottom.
(define (ui-chip-input ui id x y w chips draft on-draft on-add on-remove
                       . opts)
  (with-place 'field
    (lambda ()
      (let* ([areas (ui-chip-input-areas x y w chips)]
             [field (cdr (assq 'field areas))]
             [bottom (+ (cadr field) (cadddr field))]
             [fid (list id 'field)])
        (ui-draw-field x y w (- bottom y) (ui-flags ui fid))
        (for-each
         (lambda (chip a i)
           (let ([rid (list id 'remove i)]
                 [r (cdr (assq 'remove (apply ui-chip-areas a)))])
             (ui-pressable! ui rid (car r) (cadr r) (caddr r) (cadddr r)
                            (lambda () (on-remove i)) '()
                            '(focus keep tip "Remove") '((L . "Remove")))
             (apply ui-draw-chip (append a (list (apply ui-flags ui rid
                                                        '(removable))
                                                 chip)))))
         chips (map cdr (filter (lambda (a) (eq? (car a) 'chip)) areas))
         (iota (length chips)))
        (apply ui-text-field ui fid
               (append field
                       (list draft
                             (lambda (s)
                               (let ([parts (string-split s #\,)])
                                 (when (pair? (cdr parts))
                                   (on-add (list-head parts
                                                      (- (length parts) 1))))
                                 (on-draft (car (last-pair parts)))))
                             'inner #t
                             'placeholder (plist-ref opts 'placeholder #f)
                             'on-key
                             (lambda (key mods text)
                               (cond
                                 [(and (eq? key 'return) (null? mods))
                                  (on-add (list text)) (on-draft "") #t]
                                 [(and (eq? key 'backspace) (string=? text "")
                                       (pair? chips))
                                  (on-remove (- (length chips) 1)) #t]
                                 [else #f])))))
        bottom))))

;; Where a chip input at (x, y), w wide, puts its chips and its field:
;; ((chip x y w h) ... (field x y w h)), flowed in rows of a control's
;; height, 4 apart and 4 in from the sides; the field takes the rest of
;; the last row, or a row of its own when less than 8 lines wide is left.
(define-part (ui-chip-input-areas x y w chips)
  (let*-values ([(pad ch) (values 4 (ui-control-height))]
                [(sizes) (map (lambda (c)
                                (call-with-values
                                  (lambda () (ui-chip-size c #t))
                                  cons))
                              chips)]
                [(at height)
                 (ui-flow (+ x pad) y (- w pad pad) pad
                          (append (map (lambda (s) (cons (car s) ch)) sizes)
                                  (list (cons (min (- w pad pad)
                                                   (* 8 (font-line-height 'ui)))
                                              ch))))])
    (let ([f (list-ref at (length chips))])
      (append (map (lambda (s p)
                     (list 'chip (car p)
                           (+ (cdr p) (floor (/ (- ch (cdr s)) 2)))
                           (car s) (cdr s)))
                   sizes (list-head at (length chips)))
              (list (list 'field (car f) (cdr f) (- (+ x w) pad (car f))
                          ch))))))

;; The box's height for chips at width w.
(define-part (ui-chip-input-size w chips)
  (let ([f (cdr (assq 'field (ui-chip-input-areas 0 0 w chips)))])
    (+ (cadr f) (cadddr f))))

;;; ------------------------------------------------------------------
;;; avatar

;; "Pavel Uvarov" → "PU", "Claude" → "C".
(define (ui-initials name)
  (let ([words (remp (lambda (w) (string=? w "")) (string-split name #\space))])
    (if (null? words)
        "?"
        (list->string
         (map (lambda (w) (char-upcase (string-ref w 0)))
              (if (null? (cdr words))
                  words
                  (list (car words) (car (last-pair words)))))))))

;; A person, by name: flags may hold named (the name is written beside
;; it, so a style may leave it out: (values 0 0)). => (values w h)
(define-part (ui-avatar-size flags name)
  (let ([s (round (+ (font-line-height 'ui-small) 7))]) (values s s)))

;; A circle of one color per person, however the name was typed (spaces
;; around it, capitals), and the initials.
(define-part (ui-draw-avatar x y w h flags name)
  (let ([color (let trim ([i 0] [j (string-length name)])
                 (cond [(and (< i j) (char-whitespace? (string-ref name i)))
                        (trim (+ i 1) j)]
                       [(and (< i j)
                             (char-whitespace? (string-ref name (- j 1))))
                        (trim i (- j 1))]
                       [else (app-color (string->symbol
                                         (string-downcase
                                          (substring name i j))))]))])
    (fill-circle (+ x (/ w 2)) (+ y (/ w 2)) (/ w 2) color)
    (draw-text-centered x y w w (ui-initials name) (readable-on color)
                        'ui-small)))

;;; ------------------------------------------------------------------
;;; progress, tooltip

(define-part (ui-draw-progress x y w h flags frac)
  (fill-round-rect x y w h (/ h 2) 'bg-3)
  (when (> frac 0)
    (fill-round-rect x y (max h (* w (min 1 frac))) h (/ h 2)
                     (if (>= frac 1) 'green 'accent))))

(theme-default! 'progress-height
                (theme-rule (max 4 (round (* (font-line-height 'ui) 0.4)))))
(define (ui-progress-height) (theme 'progress-height))

;;; ------------------------------------------------------------------
;;; popups: what menus, toasts, dialogs, calendars, tooltips and the
;;; completion list share, so that a style restyles them all at once

;; A popup's box. Flags: menu (the desktop's menus and the completion
;; list: a soft edge), toast (a plain border, a smaller soft edge),
;; dialog (rounder), tooltip (the panels' color, no border); none: flat,
;; as the calendar.
(define-part (ui-draw-panel x y w h flags)
  (let* ([on? (lambda (f) (memq f flags))]
         [r (cond [(on? 'dialog) 10] [(on? 'tooltip) 5] [else 8])]
         [edge (cond [(on? 'menu) 12] [(on? 'toast) 10] [else 0])])
    (draw-shadow x y w h r edge)   ; shadow
    (fill-round-rect x y w h r (if (on? 'tooltip) (alpha 'panel 0.96) 'bg-2))
    (unless (on? 'tooltip)
      (stroke-round-rect x y w h r (if (on? 'toast)
                                       'window-border
                                       'window-border-focus)))))

;; The entry chosen in a menu or a list; with outline (a menu's
;; menu-hover), a 1-pixel box, and pressed, filled: inverted. With hover,
;; one under the pointer, not chosen: filled (Settings' list), or with
;; popup, on a popup's panel, a 1-pixel box, as a card or a field under
;; the pointer (the completion list).
(define-part (ui-draw-highlight x y w h flags)
  (cond [(memq 'hover flags)
         (if (memq 'popup flags)
             (stroke-round-rect x y w h 5 'text-faint)
             (fill-round-rect x y w h 5 'bg-2))]
        [(not (memq 'outline flags))
         (fill-round-rect x y w h 5 (alpha 'accent 0.22))]
        [(memq 'pressed flags) (px-fill x y w h 'text)]
        [else (px-ring x y w h 'text)]))

;; The chosen entry or row again, after what is drawn on it: nothing
;; here; a style that inverts what is chosen does it now (Genera's
;; inverse video: the text on it must turn too).
(define-part (ui-draw-highlight-over x y w h flags) (void))

;; A line across the middle of the box, between a menu's groups.
(define-part (ui-draw-separator x y w h flags)
  (let ([my (+ y (/ h 2))]) (draw-line x my (+ x w) my 'window-border)))

;; A card's outline (a task, a style): flags selected (or floating, while
;; dragged), hover.
(define-part (ui-draw-card x y w h flags)
  (let ([selected? (memq 'selected flags)])
    (stroke-round-rect x y w h 8
                       (cond [(or selected? (memq 'floating flags)) 'accent]
                             [(memq 'hover flags) 'text-faint]
                             [else 'window-border])
                       (if selected? 2 1))))

;; A tooltip's lines: wrapped at 280, at most 6.
(define (ui-tooltip-lines text)
  (let-values ([(lines cut?) (ui-wrap-lines text 280 'ui-small 6)]) lines))

(define-part (ui-tooltip-size text)
  (let ([lines (ui-tooltip-lines text)])
    (values (+ 16 (min 280 (widest-text lines 'ui-small)))
            (+ 10 (* (length lines) (font-line-height 'ui-small))))))

(define-part (ui-draw-tooltip x y w h flags text)
  (ui-draw-panel x y w h '(tooltip))
  (fold-left (lambda (ty line) (draw-text (+ x 8) ty line 'panel-text 'ui-small)
               (+ ty (font-line-height 'ui-small)))
             (+ y 5) (ui-tooltip-lines text)))

;;; ------------------------------------------------------------------
;;; dropdown, date field and calendar

;; choices: ((label . value) ...). Opens the desktop's menu under it, the
;; current choice selected. opts: 'label (shown instead of the current
;; choice's), 'set (the choice matters: a filter in use), 'tip.
(define (ui-dropdown ui id x y w h choices current on-select . opts)
  (let ([i (list-index (lambda (c) (equal? (cdr c) current)) choices)]
        [open? (and *menu* (equal? (menu-tag *menu*) (cons ui id)))])
    (ui-pressable! ui id x y w h
      (lambda ()
        (ui-open-menu! ui x (+ y h)
          (map (lambda (c)
                 (cons (if (equal? (cdr c) current)
                           (string-append (car c) "  ✓")
                           (car c))
                       (lambda () (on-select (cdr c)))))
               choices)
          'selected i 'tag (cons ui id) 'filter (> (length choices) 8)))
      '(#\space return down) opts '((L . "Choose")))
    (ui-draw-dropdown x y w h
                      (apply ui-flags ui id
                             (append (if open? '(open) '())
                                     (ui-opt-flags opts '(set label))))
                      (or (plist-ref opts 'label #f)
                          (if i (car (list-ref choices i)) "")))))

;; flags: open, set (accent border and text), placeholder, label (it
;; shows its 'label, not a choice).
(define-part (ui-draw-dropdown x y w h flags label)
  (let ([lh (font-line-height 'ui)] [hot? (memq 'hover flags)]
        [set? (memq 'set flags)] [s (+ (ui-glyph-size) 2)])
    (fill-round-rect x y w h 6 (if hot? 'bg-3 'bg-2))
    (stroke-round-rect x y w h 6
                       (if (or set? (memq 'open flags)) 'accent 'window-border))
    (draw-text (+ x 10) (+ y (/ (- h lh) 2)) (fit-text label (- w 22 s) 'ui)
               (cond [set? 'accent] [(memq 'placeholder flags) 'text-faint]
                     [hot? 'text] [else 'text-dim])
               'ui)
    ;; the chevron 6 from the text's room and from the edge
    (ui-draw-glyph (- (+ x w) 6 s) (+ y (/ (- h s) 2)) s s flags
                   'chevron-down)
    (when (memq 'focused flags) (ui-draw-focus x y w h flags 6))))

;; The text, 10 in, then 6, the chevron and 6.
(define-part (ui-dropdown-size labels)
  (values (min (* 10 (font-line-height 'ui))
               (+ 24 (ui-glyph-size) (widest-text labels 'ui)))
          (ui-control-height)))

;; A date (year month day) or #f. Opens a calendar under it; Delete and
;; Backspace clear it. opts: 'label (a procedure from the date to what is
;; shown; ui-date-label by default), 'placeholder, 'tip.
(define (ui-date-field ui id x y w h date on-select . opts)
  (let* ([p (ui-state-popup ui)] [open? (and p (equal? (car p) id))]
         [open! (lambda ()
                  (ui-state-popup-set! ui (cons id (ui-date->day
                                                    (or date (ui-today))))))])
    (ui-region! ui id x y w h 'focus 'tab 'tip (plist-ref opts 'tip #f)
      'doc (plist-ref opts 'doc '((L . "Choose"))) 'on (lambda (ev)
            (case (car ev)
              [(press) (and (eq? (list-ref ev 3) 'left) (begin (open!) #t))]
              [(key)
               (and (null? (caddr ev))
                    (cond [(memv (cadr ev) '(#\space return down)) (open!) #t]
                          [(memq (cadr ev) '(delete backspace))
                           (on-select #f) #t]
                          [else #f]))]
              [else #f])))
    (ui-draw-dropdown x y w h
                      (apply ui-flags ui id
                             (append (if open? '(open) '())
                                     (if date '() '(placeholder))))
                      (if date
                          ((plist-ref opts 'label ui-date-label) date)
                          (plist-ref opts 'placeholder "No date")))
    (when open?
      (ui-later! ui (lambda (ww wh)
                      (ui-calendar ui id x y h date on-select ww wh))))))

;; The calendar of the field id at (fx, fy), fh high, in a window ww × wh:
;; under the field, or above it when there is no room, over a modal
;; region that closes it on a press outside.
(define (ui-calendar ui id fx fy fh date on-select ww wh)
  (with-place 'dialog
    (lambda ()
      (let*-values ([(cw ch) (ui-calendar-size)]
                    [(x y) (ui-popup-place fx (+ fy fh 4) (- fy 4) cw ch ww
                                           wh)])
        (let* ([cursor (cdr (ui-state-popup ui))]
               [close (lambda () (ui-state-popup-set! ui #f))]
               [pick (lambda (n) (close) (on-select (ui-day->date n)))]
               [move (lambda (n) (ui-state-popup-set! ui (cons id n)) #t)]
               [today (ui-date->day (ui-today))]
               [cd (ui-day->date cursor)]
               [first (ui-date->day (list (car cd) (cadr cd) 1))]
               [start (- first (ui-week-day first))]
               [chosen (and date (ui-date->day date))]
               [areas (ui-calendar-areas x y cw ch)]
               [area (lambda (k) (cdr (assq k areas)))]
               [button (lambda (k label thunk . opts)
                         (apply ui-button ui (list id k)
                                (append (area k) (list label thunk 'quiet #t
                                                       'focus 'keep)
                                        opts)))])
          (ui-region! ui (list id 'calendar) 0 0 ww wh 'modal #t 'focus 'keep
            'on (ui-modal-on close
                  (lambda (key mods)
                    (case key
                      [(left) (move (- cursor 1))]
                      [(right) (move (+ cursor 1))]
                      [(up) (move (- cursor 7))]
                      [(down) (move (+ cursor 7))]
                      [(page-up) (move (ui-add-months cursor -1))]
                      [(page-down) (move (ui-add-months cursor 1))]
                      [(home) (move today)]
                      [(return) (pick cursor) #t]
                      [else #f]))))
          (ui-draw-calendar x y cw ch '()
                            (format "~a ~a" (vector-ref ui-month-titles
                                                        (- (cadr cd) 1))
                                    (car cd)))
          (button 'prev (and (theme 'calendar-step-words) "Previous")
                  (lambda () (move (ui-add-months cursor -1)))
                  'glyph 'chevron-left)
          (button 'next (and (theme 'calendar-step-words) "Next")
                  (lambda () (move (ui-add-months cursor 1)))
                  'glyph 'chevron-right)
          (let* ([g (area 'grid)] [gx (car g)] [gy (cadr g)] [cell (caddr g)])
            (do ([i 0 (+ i 1)]) ((= i 42))
              (let* ([n (+ start i)] [did (list id 'day n)]
                     [dx (+ gx (* cell (mod i 7)))]
                     [dy (+ gy (* cell (div i 7)))])
                (ui-pressable! ui did dx dy cell cell (lambda () (pick n)) '()
                               '(focus keep) '((L . "Choose")))
                (ui-draw-day dx dy cell cell
                             (apply ui-flags ui did
                                    (append (if (= n today) '(today) '())
                                            (if (eqv? n chosen) '(selected) '())
                                            (if (= n cursor) '(cursor) '())
                                            (if (= (cadr (ui-day->date n))
                                                   (cadr cd))
                                                '()
                                                '(outside))))
                             (number->string (caddr (ui-day->date n)))))))
          (button 'today "Today" (lambda () (pick today)))
          (button 'clear "Clear" (lambda () (close) (on-select #f))))))))

(define (ui-calendar-cell) (max 28 (round (* (font-line-height 'ui) 1.9))))

;; The month's steps as glyphs, or as the words Previous and Next.
(theme-default! 'calendar-step-words #f)

(define-part (ui-calendar-size)
  (let* ([w (+ 20 (* 7 (ui-calendar-cell)))]
         [t (cdr (assq 'today (ui-calendar-areas 0 0 w 0)))])
    (values w (round (+ (cadr t) (cadddr t) 8)))))

;; ((prev x y w h) (next ..) (title ..) (week x y w h) (grid gx gy cell)
;; (today ..) (clear ..)); day i of the grid lies at gx + (i mod 7)·cell,
;; gy + (i div 7)·cell.
(define-part (ui-calendar-areas x y w h)
  (let* ([cell (ui-calendar-cell)] [sh (font-line-height 'ui-small)]
         [b (ui-control-height)] [gy (+ y 4 b sh 6)]
         [fy (+ gy (* 6 cell) 8)])
    `((prev ,(+ x 8) ,(+ y 4) ,b ,b)
      (next ,(- (+ x w) 8 b) ,(+ y 4) ,b ,b)
      (title ,(+ x 8 b) ,(+ y 4) ,(- w 16 b b) ,b)
      (week ,(+ x 10) ,(+ y 4 b) ,(* 7 cell) ,(+ sh 6))
      (grid ,(+ x 10) ,gy ,cell)
      (today ,(+ x 8) ,fy ,(/ (- w 24) 2) ,b)
      (clear ,(+ x 16 (/ (- w 24) 2)) ,fy ,(/ (- w 24) 2) ,b))))

;; The panel, its title and the weekday initials.
(define-part (ui-draw-calendar x y w h flags title)
  (let* ([areas (ui-calendar-areas x y w h)]
         [t (cdr (assq 'title areas))] [wk (cdr (assq 'week areas))]
         [cell (/ (caddr wk) 7)])
    (ui-draw-panel x y w h '())
    (apply draw-text-centered (append t (list title 'text 'ui-bold)))
    (do ([i 0 (+ i 1)]) ((= i 7))
      (draw-text-centered (+ (car wk) (* i cell)) (cadr wk) cell (cadddr wk)
                          (string (string-ref "MTWTFSS" i)) 'text-faint
                          'ui-small))))

;; flags: hover, today (a ring), selected (filled), cursor (the keyboard's
;; day), outside (another month).
(define-part (ui-draw-day x y w h flags label)
  (let* ([on? (lambda (f) (memq f flags))] [s (- (min w h) 4)]
         [cx (+ x (/ (- w s) 2))] [cy (+ y (/ (- h s) 2))])
    (cond [(on? 'selected) (fill-round-rect cx cy s s 6 'accent)]
          [(on? 'hover) (fill-round-rect cx cy s s 6 'bg-3)])
    (when (on? 'today)
      (stroke-round-rect cx cy s s 6 (if (on? 'selected) 'text 'accent) 1.5))
    (when (on? 'cursor) (stroke-round-rect (- cx 1) (- cy 1) (+ s 2) (+ s 2) 7
                                           'text 1))
    (draw-text-centered x y w h label
                        (cond [(on? 'selected) (readable-on 'accent)]
                              [(on? 'outside) 'text-faint]
                              [else 'text])
                        'ui)))

;;; ------------------------------------------------------------------ dialog

;; A modal dialog over the window, drawn at ui-end!. buttons: ((label
;; thunk flag ...) ...), right-aligned; the last is the default (Return).
;; Escape and a press outside call on-dismiss. A field under it lets go of
;; the keyboard.
(define (ui-dialog ui id title message buttons on-dismiss)
  (ui-later! ui
    (lambda (w h)
      (with-place 'dialog
        (lambda ()
          (let*-values ([(labels) (map car buttons)]
                        [(dw dh) (ui-dialog-size title message labels)]
                        [(dx dy) (values (max 0 (round (/ (- w dw) 2)))
                                         (max 0 (round (/ (- h dh) 2))))]
                        [(f) (ui-state-focus ui)])
            (unless (or (not f) (and (pair? f) (equal? (car f) id)))
              (ui-focus! ui #f)
              (invalidate! (current-window)))
            (ui-draw-scrim 0 0 w h '())
            (ui-region! ui id 0 0 w h 'modal #t 'focus 'keep
              'on (ui-modal-on on-dismiss
                    (lambda (key mods)
                      (and (eq? key 'return) (null? mods)
                           (begin ((cadr (car (last-pair buttons)))) #t)))))
            (ui-draw-dialog dx dy dw dh '() title message)
            (for-each (lambda (b a i)
                        (apply ui-button ui (list id i)
                               (append a (list (car b) (cadr b))
                                       (apply append
                                              (map (lambda (f) (list f #t))
                                                   (cddr b))))))
                      buttons
                      (map cdr (filter (lambda (a) (eq? (car a) 'button))
                                       (ui-dialog-areas dx dy dw dh labels)))
                      (iota (length buttons)))))))))

;; Each button is at least 88 wide.
(define (ui-dialog-button-widths labels)
  (map (lambda (l) (let-values ([(bw bh) (ui-button-size l #f)]) (max bw 88)))
       labels))

;; A dialog's message lines at width w: at most 6, so a long message
;; keeps the dialog and its buttons small.
(define (ui-dialog-lines message w)
  (let-values ([(lines cut?) (ui-wrap-lines message w 'ui 6)]) lines))

;; labels: the buttons'.
(define-part (ui-dialog-size title message labels)
  (let* ([lh (font-line-height 'ui)]
         [bw (+ (apply + (ui-dialog-button-widths labels))
                (* (ui-gap) (max 0 (- (length labels) 1))))]
         [w (clamp (+ 48 (max (text-width title 'heading)
                              (text-width message 'ui) bw))
                   360 (* 32 lh))])
    (values w
            (round (+ 24 (font-line-height 'heading) 8
                      (* lh (length (ui-dialog-lines message (- w 48))))
                      24 (ui-control-height) 20)))))

;; ((title x y w h) (message ..) (button x y w h) ...): one button per
;; label, right-aligned in the bottom row, a gap apart.
(define-part (ui-dialog-areas x y w h labels)
  (let* ([hh (font-line-height 'heading)] [b (ui-control-height)]
         [gap (ui-gap)] [widths (ui-dialog-button-widths labels)]
         [by (- (+ y h) 20 b)])
    `((title ,(+ x 24) ,(+ y 24) ,(- w 48) ,hh)
      (message ,(+ x 24) ,(+ y 24 hh 8) ,(- w 48) ,(- h 24 hh 8 24 b 20))
      ,@(let loop ([ws widths]
                   [bx (- (+ x w -24) (apply + widths)
                          (* gap (max 0 (- (length widths) 1))))])
          (if (null? ws)
              '()
              (cons (list 'button bx by (car ws) b)
                    (loop (cdr ws) (+ bx (car ws) gap))))))))

(define-part (ui-draw-scrim x y w h flags)
  (fill-rect x y w h (alpha 'bg-0 0.55)))

(define-part (ui-draw-dialog x y w h flags title message)
  (let* ([areas (ui-dialog-areas x y w h '())] [t (cdr (assq 'title areas))]
         [m (cdr (assq 'message areas))])
    (ui-draw-panel x y w h '(dialog))
    (draw-text (car t) (cadr t) (fit-text title (caddr t) 'heading) 'text
               'heading)
    (fold-left (lambda (ly line) (draw-text (car m) ly line 'text-dim 'ui)
                 (+ ly (font-line-height 'ui)))
               (cadr m) (ui-dialog-lines message (caddr m)))))

;;; ------------------------------------------------------------------ reload

;; Every window's regions are its next draw's (an older kit's are records
;; of another type).
(for-each (lambda (win)
            (let ([ui (window-ref win 'ui #f)])
              (when (ui-state? ui)
                (ui-state-regions-set! ui '())
                (ui-state-active-set! ui #f)
                (ui-stale! ui))))
          (windows))

;; A window opened under an older kit holds that kit's ui, a record this
;; code cannot read: its app opens again in its place, once the reload
;; has loaded the apps too. One that no app opened (Tasks' editor) closes.
(for-each
 (lambda (win)
   (let ([ui (window-ref win 'ui #f)] [app (window-app win)])
     (when (and ui (not (ui-state? ui)) (not app))
       (close-window! win))
     (when (and ui (not (ui-state? ui)) app)
       (let-values ([(cx cy cw ch) (content-rect win)])
         (let ([x (window-x win)] [y (window-y win)])
           (close-window! win)
           (after 0
             (lambda ()
               (let ([new (parameterize ([new-window-position (cons x y)])
                            (launch-app app))])
                 (when (window? new) (resize-window! new cw ch))))))))))
 (windows))
