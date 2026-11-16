;;; tests/units/olgl.ss — OPEN LOOK's glyph fonts (kits/olgl.ss).

(check "the glyph kit has OLGX's four fonts: resize corners 10, 11, 12 and \
        14 square, the window button 14, 16, 18 and 22 wide, the pushpin \
        two sizes, 10's as 12's and 14's as 19's"
       (let ([wh (lambda (size name)
                   (let ([g (olgl-glyph size name)]) (list (car g) (cadr g))))])
         (and (equal? (map (lambda (s) (wh s 'ul-resize-outline))
                           '(10 12 14 19))
                      '((10 10) (11 11) (12 12) (14 14)))
              (equal? (map (lambda (s) (car (wh s 'abbrev-menu-button)))
                           '(10 12 14 19))
                      '(14 16 18 22))
              (equal? (olgl-glyph 10 'menu-pin-out)
                      (olgl-glyph 12 'menu-pin-out))
              (equal? (olgl-glyph 14 'menu-pin-in) (olgl-glyph 19 'menu-pin-in))
              (not (equal? (olgl-glyph 12 'menu-pin-in)
                           (olgl-glyph 14 'menu-pin-in)))
              (not (olgl-glyph 12 'no-such-glyph))
              (equal? (map olgl-size '(9 12 13 15 20)) '(10 12 12 14 19)))))
