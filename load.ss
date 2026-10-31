;;; load.ss — loads the whole Schevana world into the interaction environment.
;;; Everything is top-level on purpose: any definition can be redefined live
;;; from the REPL, and the running system picks it up immediately.

;; The script that loads this file sets *schevana-root* first.
(define *schevana-root*
  (if (top-level-bound? '*schevana-root*) (top-level-value '*schevana-root*) "."))

(define *schevana-sources*
  '("src/ffi.ss" "src/base.ss" "src/theme.ss" "src/gfx.ss"
    "src/shadows.ss"   ; shadow
    "src/proc.ss" "src/host.ss" "src/wm.ss" "src/styles.ss" "src/shell.ss" "src/ui.ss"
    "src/system.ss"
    "apps/repl.ss" "apps/clock.ss" "apps/life.ss" "apps/sketch.ss" "apps/monitor.ss"
    "apps/settings.ss"))

(for-each (lambda (f) (load (string-append *schevana-root* "/" f))) *schevana-sources*)
