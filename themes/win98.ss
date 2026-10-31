;;; themes/win98.ss — the Windows 98 palette.
;;; Windows 98's standard scheme: gray faces, navy to blue captions,
;;; the teal desktop, and the 16-color palette's darker hues.

(define-theme win98 "Windows 98"
  '(bg-0 #xffffff bg-1 #xc0c0c0 bg-2 #xc0c0c0 bg-3 #xa8a8a8
    text #x000000 text-dim #x202020 text-faint #x808080
    accent #x000080 accent-2 #x1084d0
    red #x800000 orange #x804000 yellow #x808000
    green #x008000 cyan #x008080 teal #x008080 magenta #x800080
    desktop-top #x008080 desktop-bottom #x008080 watermark #xffffff
    panel #xc0c0c0
    desktop-pattern flat))
