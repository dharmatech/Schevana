;;; styles/cadr.ss — the MIT CADR's TV windows (System 99), read from
;;; MIT's sources (BSD; .work/style/research/lispm.md part 2): the window
;;; system Genera grew from. Genera's frame, who line and feel, with what
;;; the CADR did otherwise: a 1-pixel border and a 1-pixel white margin
;;; (baswin.lisp:864-871, :1012-1014), and the label upright in the code
;;; font, CPTFONT on the CADR, at the top of its strip, which lies under
;;; the content, the margin and the border under it (baswin.lisp:1104-1119,
;;; :1180, :1215-1226). No gray over windows not selected: the CADR grayed
;;; only covered windows (lispm.md 2.5-2.6), and Schevana covers none.

(define-style cadr "MIT CADR"
  'parent 'genera
  'genera-border 1  'genera-margin 1
  'genera-label-font 'mono  'genera-label-align 'top
  'genera-label-under 'content  'frame-over 'frame-over*)
