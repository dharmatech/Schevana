;;; palettes/open-look.ss — the OPEN LOOK palette.
;;; OpenWindows' own colors: windows #cccccc with black ink, panes and
;;; fields white, the workspace #40a0c0 (olwm's resources.c:235-250,
;;; XView's cms_pblc.c:232-234). bg-2 is BG2, the 3D shade OLGX takes
;;; from the window color (styles/open-look.ss); bg-3 is a step past it,
;;; for the kits' hovers, which OPEN LOOK lacks.

(define-palette open-look "OPEN LOOK"
  '(bg-0 #xffffff
    bg-1 #xcccccc
    bg-2 #xb7b7b7
    bg-3 #xa5a5a5
    text #x000000
    text-dim #x333333
    text-faint #x666666
    accent #x000000
    accent-2 #x666666
    red #xb0202c
    orange #xa04e00
    yellow #x7a5c00
    green #x2a6e27
    cyan #x145f78
    teal #x166459
    magenta #x86266f
    desktop-top #x40a0c0
    desktop-bottom #x40a0c0
    watermark #xffffff
    panel #xcccccc
    panel-text #x000000
    panel-dim #x666666
    panel-hover #xb7b7b7
    window-border #x000000
    window-border-focus #x000000))
