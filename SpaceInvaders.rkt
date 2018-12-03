#lang racket
(require 2htdp/image)
(require 2htdp/universe)
; Joao Barboza Rodrigues 11811ECP005
; Rodrigo Henrique Alves Ferreira 11811ECP001
; Gabriel Cardoso Mendes de Ataide 11811ECP008
;(require rsound))
(define MOTHER-IMG (scale 0.1 (bitmap "./Sprites/mother.png")))
(define NUM0 (scale 0.2 (bitmap "./Sprites/0.png")))
(define NUM1 (scale 0.2 (bitmap "./Sprites/num1.png")))
(define NUM2 (scale 0.2 (bitmap "./Sprites/num2.png")))
(define NUM3 (scale 0.2 (bitmap "./Sprites/num3.png")))
(define NUM4 (scale 0.2 (bitmap "./Sprites/num4.png")))
(define NUM5 (scale 0.2 (bitmap "./Sprites/num5.png")))
(define NUM6 (scale 0.2 (bitmap "./Sprites/num6.png")))
(define NUM7 (scale 0.2 (bitmap "./Sprites/num7.png")))
(define NUM8 (scale 0.2 (bitmap "./Sprites/num8.png")))
(define NUM9 (scale 0.2 (bitmap "./Sprites/num9.png")))
(define BACKGROUND (scale 1.4 (bitmap "./Sprites/background.gif")))
(define enemy-speed 3)
(define TICK 1)
(define CAR (scale 0.6 (bitmap "./Sprites/delorean.png")))
(define WIN (scale 0.6 (bitmap "./Sprites/WIN.png")))
(define GAMEOVER (scale 0.6 (bitmap "./Sprites/GAMEOVER.png")))
(define POLICE-1 (scale 0.3 (rotate 180 (bitmap "./Sprites/1.png"))))
(define POLICE-2 (scale 0.3 (rotate 180 (bitmap "./Sprites/2.png"))))
(define POLICE-3 (scale 0.3 (rotate 180 (bitmap "./Sprites/3.png"))))
(define TITLE (bitmap "./Sprites/title.jpg"))
(define START (scale 0.5 (bitmap "./Sprites/start.png")))
(define PROJECTILE (rotate 90 (scale 0.8 . )))
;(define MUSIC (rs-read "./music.wav"))
;(define WIN-S (rs-read "./WIN-S.wav"))

(define-struct posn (x y))
(define-struct projectile ([x #:mutable]  [y #:mutable] [flag #:mutable] [image #:mutable]))
(define-struct enemy ([image #:mutable] id [x #:mutable] [y #:mutable] [alive? #:mutable]))
(define-struct player ([x #:mutable]  [y #:mutable] [alive? #:mutable]))
(define-struct score ([image #:mutable] [value #:mutable]))
(define-struct mother ([x #:mutable]  [y #:mutable] [image #:mutable] [alive? #:mutable] [spawned? #:mutable]))
(define PLAYER (make-player 0 (+ (* (/ (image-height BACKGROUND) 2) -1) (/ (image-height CAR) 2)) 1))
(define P-PROJ (make-projectile 0 -297 0 empty-image))
(define E-PROJ (make-projectile 0 0 0 empty-image))
(define MOTHER (make-mother 555 260 empty-image 1 0))
(define SCORE1 (make-score NUM0 0))
(define SCORE2 (make-score NUM0 0))
(define SCORE3 (make-score NUM0 0))
(define SCORE4 (make-score NUM0 0))

(define ENEMY-1-1 (make-enemy POLICE-1 11 -445 274 1))
(define ENEMY-1-2 (make-enemy POLICE-1 12  -277 274 1))
(define ENEMY-1-3 (make-enemy POLICE-1 13 -149 274 1))
(define ENEMY-1-4 (make-enemy POLICE-1 14 0 274 1))
(define ENEMY-1-5 (make-enemy POLICE-1 15 149 274 1))
(define ENEMY-1-6 (make-enemy POLICE-1 16 277 274 1))
(define ENEMY-1-7 (make-enemy POLICE-1 17 445 274 1))
(define ENEMY-2-1 (make-enemy POLICE-1 21 -445 156 1))
(define ENEMY-2-2 (make-enemy POLICE-1 22 -277 156 1))
(define ENEMY-2-3 (make-enemy POLICE-1 23 -149 156 1))
(define ENEMY-2-4 (make-enemy POLICE-1 24 0 156 1))
(define ENEMY-2-5 (make-enemy POLICE-1 25 149 156 1))
(define ENEMY-2-6 (make-enemy POLICE-1 26 277 156 1))
(define ENEMY-2-7 (make-enemy POLICE-1 27 445 156 1))
(define ENEMY-3-1 (make-enemy POLICE-1 31 -445 38 1))
(define ENEMY-3-2 (make-enemy POLICE-1 32 -277 38 1))
(define ENEMY-3-3 (make-enemy POLICE-1 33 -149 38 1))
(define ENEMY-3-4 (make-enemy POLICE-1 34 0 38 1))
(define ENEMY-3-5 (make-enemy POLICE-1 35 149 38 1))
(define ENEMY-3-6 (make-enemy POLICE-1 36 277 38 1))
(define ENEMY-3-7 (make-enemy POLICE-1 37 445 38 1))
(define ENEMY-4-1 (make-enemy POLICE-1 41 -445 -80 1))
(define ENEMY-4-2 (make-enemy POLICE-1 42 -277 -80 1))
(define ENEMY-4-3 (make-enemy POLICE-1 43 -149 -80 1))
(define ENEMY-4-4 (make-enemy POLICE-1 44 0 -80 1))
(define ENEMY-4-5 (make-enemy POLICE-1 45 149 -80 1))
(define ENEMY-4-6 (make-enemy POLICE-1 46 277 -80 1))
(define ENEMY-4-7 (make-enemy POLICE-1 47 445 -80 1))

(define enemy-list (list ENEMY-1-1 ENEMY-1-2 ENEMY-1-3 ENEMY-1-4 ENEMY-1-5 ENEMY-1-6 ENEMY-1-7
                                    ENEMY-2-1 ENEMY-2-2 ENEMY-2-3 ENEMY-2-4 ENEMY-2-5 ENEMY-2-6 ENEMY-2-7
                                    ENEMY-3-1 ENEMY-3-2 ENEMY-3-3 ENEMY-3-4 ENEMY-3-5 ENEMY-3-6 ENEMY-3-7
                                    ENEMY-4-1 ENEMY-4-2 ENEMY-4-3 ENEMY-4-4 ENEMY-4-5 ENEMY-4-6 ENEMY-4-7))



(define (enemy-shoot list)
  (define k (random 0 25))
  (cond
    [(equal? (enemy-alive? (list-ref list k)) 1)
     (cond
       [(equal? (projectile-flag E-PROJ) 0) (begin (set-projectile-image! E-PROJ (rotate 180 PROJECTILE)) (set-projectile-y! E-PROJ (enemy-y (list-ref list k))) (set-projectile-x! E-PROJ (enemy-x (list-ref list k)))  (set-projectile-flag! E-PROJ 1))]
       [(equal? (projectile-flag E-PROJ) 1) 1])]
     [(equal? (enemy-alive? (list-ref list k)) 0) 1]))

(define (check-win ws list)
  (cond
   [(equal? (player-alive? PLAYER) 0) (+ ws 1)]
   [(empty? list) (- ws 1)]
   [else
    (cond
      [(equal? (enemy-alive? (car list)) 1) ws]
      [else (check-win ws (cdr list))])]))

(define (move ws key)
  (cond
   [(equal? ws 1) 
    (cond
      [(key=? key "right") (begin (move-player-right) ws) ]
      [(key=? key "left") (begin (move-player-left) ws) ]
      [(key=? key " ") (cond
                         [(equal? (projectile-flag P-PROJ) 0)
                          (begin (set-projectile-image! P-PROJ PROJECTILE) (set-projectile-flag! P-PROJ 1) (set-projectile-x! P-PROJ (player-x PLAYER)) ws)]
                         [else ws]) ]
      [else ws])]
   [(equal? ws 3)
     (cond
       [(key=? key "up") (- ws 2)]
       [else ws])]
   [else ws]))

(define (move-e-proj)
  (cond
    [(equal? (projectile-flag E-PROJ) 0) 1]
    [(equal? (projectile-flag E-PROJ) 1)
     (cond
       [(< (projectile-y E-PROJ) -290) (begin (set-projectile-flag! E-PROJ 0) (set-projectile-image! E-PROJ empty-image) (set-projectile-y! E-PROJ 0))]
       [else (set-projectile-y! E-PROJ (- (projectile-y E-PROJ) 10))])]))

(define (move-proj)
  (cond
    [(equal? (projectile-flag P-PROJ) 0) 1]
    [(equal? (projectile-flag P-PROJ) 1)
     (cond
       [(> (projectile-y P-PROJ) 290) (begin (set-projectile-flag! P-PROJ 0) (set-projectile-image! P-PROJ empty-image) (set-projectile-y! P-PROJ (player-y PLAYER)))]
       [else (set-projectile-y! P-PROJ (+ (projectile-y P-PROJ) 50))])]))


(define (move-player-left)
  (cond
    [(equal? (player-x PLAYER) 555) 1]
    [else (begin (set-player-x! PLAYER (+ (player-x PLAYER) 15)) (cond
                                                                   [(equal? (projectile-flag P-PROJ) 0)
                                                                                     (set-projectile-x! P-PROJ (+ (projectile-x P-PROJ) 15))]
                                                                   [else 1]) 1)]))

(define (move-player-right)
  (cond
    [(equal? (player-x PLAYER) -555) 1]
    [else (begin (set-player-x! PLAYER (- (player-x PLAYER) 15)) (cond
                                                                   [(equal? (projectile-flag P-PROJ) 0)
                                                                                     (set-projectile-x! P-PROJ (- (projectile-x P-PROJ) 15))]
                                                                   [else 1]) 1)]))

 (define (render ws)
 (cond
  [(equal? ws 1)
   (overlay/offset (mother-image MOTHER) (mother-x MOTHER) (mother-y MOTHER)
   (overlay/offset (score-image SCORE1) 0 300
   (overlay/offset (score-image SCORE2) -40 300                
   (overlay/offset (score-image SCORE3) -80 300
   (overlay/offset (score-image SCORE4) -120 300                             
   (overlay/offset (enemy-image ENEMY-4-7) (enemy-x ENEMY-4-7) (enemy-y ENEMY-4-7)
   (overlay/offset (enemy-image ENEMY-4-6) (enemy-x ENEMY-4-6) (enemy-y ENEMY-4-6)
   (overlay/offset (enemy-image ENEMY-4-5) (enemy-x ENEMY-4-5) (enemy-y ENEMY-4-5)
   (overlay/offset (enemy-image ENEMY-4-4) (enemy-x ENEMY-4-4) (enemy-y ENEMY-4-4)
   (overlay/offset (enemy-image ENEMY-4-3) (enemy-x ENEMY-4-3) (enemy-y ENEMY-4-3)
   (overlay/offset (enemy-image ENEMY-4-2) (enemy-x ENEMY-4-2) (enemy-y ENEMY-4-2)
   (overlay/offset (enemy-image ENEMY-4-1) (enemy-x ENEMY-4-1) (enemy-y ENEMY-4-1)
   (overlay/offset (enemy-image ENEMY-3-7) (enemy-x ENEMY-3-7) (enemy-y ENEMY-3-7)
   (overlay/offset (enemy-image ENEMY-3-6) (enemy-x ENEMY-3-6) (enemy-y ENEMY-3-6)
   (overlay/offset (enemy-image ENEMY-3-5) (enemy-x ENEMY-3-5) (enemy-y ENEMY-3-5)
   (overlay/offset (enemy-image ENEMY-3-4) (enemy-x ENEMY-3-4) (enemy-y ENEMY-3-4)
   (overlay/offset (enemy-image ENEMY-3-3) (enemy-x ENEMY-3-3) (enemy-y ENEMY-3-3)
   (overlay/offset (enemy-image ENEMY-3-2) (enemy-x ENEMY-3-2) (enemy-y ENEMY-3-2)
   (overlay/offset (enemy-image ENEMY-3-1) (enemy-x ENEMY-3-1) (enemy-y ENEMY-3-1)
   (overlay/offset (enemy-image ENEMY-2-7) (enemy-x ENEMY-2-7) (enemy-y ENEMY-2-7)
   (overlay/offset (enemy-image ENEMY-2-6) (enemy-x ENEMY-2-6) (enemy-y ENEMY-2-6)
   (overlay/offset (enemy-image ENEMY-2-5) (enemy-x ENEMY-2-5) (enemy-y ENEMY-2-5)
   (overlay/offset (enemy-image ENEMY-2-4) (enemy-x ENEMY-2-4) (enemy-y ENEMY-2-4)
   (overlay/offset (enemy-image ENEMY-2-3) (enemy-x ENEMY-2-3) (enemy-y ENEMY-2-3)
   (overlay/offset (enemy-image ENEMY-2-2) (enemy-x ENEMY-2-2) (enemy-y ENEMY-2-2)
   (overlay/offset (enemy-image ENEMY-2-1) (enemy-x ENEMY-2-1) (enemy-y ENEMY-2-1)
   (overlay/offset (enemy-image ENEMY-1-7) (enemy-x ENEMY-1-7) (enemy-y ENEMY-1-7)
   (overlay/offset (enemy-image ENEMY-1-6) (enemy-x ENEMY-1-6) (enemy-y ENEMY-1-6)
   (overlay/offset (enemy-image ENEMY-1-5) (enemy-x ENEMY-1-5) (enemy-y ENEMY-1-5)
   (overlay/offset (enemy-image ENEMY-1-4) (enemy-x ENEMY-1-4) (enemy-y ENEMY-1-4)
   (overlay/offset (enemy-image ENEMY-1-3) (enemy-x ENEMY-1-3) (enemy-y ENEMY-1-3)
   (overlay/offset (enemy-image ENEMY-1-2) (enemy-x ENEMY-1-2) (enemy-y ENEMY-1-2)                                                   
   (overlay/offset (enemy-image ENEMY-1-1) (enemy-x ENEMY-1-1) (enemy-y ENEMY-1-1)
   (overlay/offset (projectile-image E-PROJ) (projectile-x E-PROJ) (projectile-y E-PROJ)
                                             (overlay/offset CAR
                              (player-x PLAYER ) (player-y PLAYER) (overlay/offset (projectile-image P-PROJ) (projectile-x P-PROJ) (projectile-y P-PROJ) BACKGROUND))))))))))))))))))))))))))))))))))))]
  [(equal? ws 0) (overlay/xy WIN 0 0 BACKGROUND)]
  [(equal? ws 2) (overlay/xy GAMEOVER 0 0 BACKGROUND)]
  [(equal? ws 3) (overlay/offset START 0 0 TITLE)]))

(define (change-score ws list)
  (cond
    [(empty? list) ws]
    [else
     (cond
       [(equal? (score-value (car list)) 0) (set-score-image! (car list) NUM0)]
       [(equal? (score-value (car list)) 1) (set-score-image! (car list) NUM1)]
       [(equal? (score-value (car list)) 2) (set-score-image! (car list) NUM2)]
       [(equal? (score-value (car list)) 3) (set-score-image! (car list) NUM3)]
       [(equal? (score-value (car list)) 4) (set-score-image! (car list) NUM4)]
       [(equal? (score-value (car list)) 5) (set-score-image! (car list) NUM5)]
       [(equal? (score-value (car list)) 6) (set-score-image! (car list) NUM6)]
       [(equal? (score-value (car list)) 7) (set-score-image! (car list) NUM7)]
       [(equal? (score-value (car list)) 8) (set-score-image! (car list) NUM8)]
       [(equal? (score-value (car list)) 9) (set-score-image! (car list) NUM9)])
       (change-score ws (cdr list))]))

(define SCOREBOARD (list SCORE1 SCORE2 SCORE3 SCORE4))

(define (move-enemies list)
  (cond
    [(empty? list) 0]
    [else (begin (set-enemy-x! (car list) (+ (enemy-x (car list)) enemy-speed)) (move-enemies (cdr list)))]))

(define (enemy-down list)
  (cond
    [(empty? list) 0]
    [else (begin (set-enemy-y! (car list) (+ (enemy-y (car list)) -15)) (enemy-down (cdr list)))]))

(define (check-colide-enemy ws player-y list)
  (cond
    [(empty? list) (check-win ws enemy-list)]
    [else (cond
            [(equal? (enemy-alive? (car list)) 1)
             (cond
               [(> (+ player-y 77) (enemy-y (car list))) (begin (enemy-down list) (+ ws 1))]
               [else (check-colide-enemy ws player-y (cdr list))])]
            [else (check-colide-enemy ws player-y (cdr list))])])
)
    


(define (kill-enemy enemy)
 (set-enemy-alive?! enemy 0)
 (set-enemy-image! enemy empty-image)
  (cond
  [(equal? (score-value SCORE3) 0) (set-score-value! SCORE3 5)]
  [(equal? (score-value SCORE3) 5)
   (cond
     [(equal? (score-value SCORE2) 9) (begin (set-score-value! SCORE3 0) (set-score-value! SCORE2 0) (set-score-value! SCORE1 (+ (score-value SCORE1) 1)))]
     [else (begin (set-score-value! SCORE3 0) (set-score-value! SCORE2 (+ (score-value SCORE2) 1)))])]))

(define (kill-mother ws)
 (set-mother-alive?! MOTHER 0)
 (set-mother-image! MOTHER empty-image)
   (cond
     [(equal? (score-value SCORE2) 9) (set-score-value! SCORE2 0) (set-score-value! SCORE1 (+ (score-value SCORE1) 5))]
     [else (begin (set-score-value! SCORE2 (+ (score-value SCORE2) 5)))]))

(define (move-mother ws)
  (cond
   [(equal? (mother-spawned? MOTHER) 1) (set-mother-x! MOTHER (+ (mother-x MOTHER) -8))]
   [else ws]))

(define (spawn-mother ws)
  (cond
   [(equal? TICK 1024) (begin (set-mother-spawned?! MOTHER 1) (set-mother-image! MOTHER MOTHER-IMG))]
   [else ws]))

(define (colide-projectile proj-y list)
  (cond
    [(empty? list) 0]
    [(equal? proj-y (enemy-y (car list)))
     (cond
       [(equal? (enemy-alive? (car list)) 1) (kill-enemy (car list))] 
       [else (colide-projectile proj-y (cdr list))])]
     [else (colide-projectile proj-y (cdr list))]))

(define (colide-player)
  (cond
    [(< (projectile-y E-PROJ) (+ (player-y PLAYER) 50))
    (cond [(and (< (projectile-x E-PROJ) (+ (player-x PLAYER) 10)) (> (projectile-x E-PROJ) (- (player-x PLAYER) 10))) (set-player-alive?! PLAYER 0)]
          [else 1])]
   [else 1]))

(define (colide-mother proj)
    (cond
    [(and (< (projectile-x proj) (+ 15 (mother-x MOTHER))) (> (projectile-x proj) (- (mother-x MOTHER) 15)))
     (cond
       [(and (< (projectile-y proj) (+ 32 (mother-y MOTHER))) (> (projectile-y proj) (- (mother-y MOTHER) 32)))
        (cond
          [(equal? (mother-alive? MOTHER) 1) (begin (set-projectile-flag! P-PROJ 0) (set-projectile-image! P-PROJ empty-image) (set-projectile-y! P-PROJ (player-y PLAYER)) (kill-mother 1))]
          [else 1])])]))

(define (change-sprite list)
  (cond
    [(empty? list) 0]
    [else (begin
            (cond [(equal? (enemy-alive? (car list)) 1)
             (cond
                      [(even? TICK) (set-enemy-image! (car list) POLICE-3) (change-sprite (cdr list))]
                      [(odd? TICK) (set-enemy-image! (car list) POLICE-1) (change-sprite (cdr list))]
                   )]
                  [else (change-sprite (cdr list))]))]))

(define (colide-enemy proj list)
 (cond
   [(empty? list) 0]
   [else
    (cond
    [(and (< (projectile-x proj) (+ 15 (enemy-x (car list)))) (> (projectile-x proj) (- (enemy-x (car list)) 15)))
     (cond
       [(and (< (projectile-y proj) (+ 32 (enemy-y (car list)))) (> (projectile-y proj) (- (enemy-y (car list)) 32)))
        (cond
          [(equal? (enemy-alive? (car list)) 1) (begin (set-projectile-flag! P-PROJ 0) (set-projectile-image! P-PROJ empty-image) (set-projectile-y! P-PROJ (player-y PLAYER)) (kill-enemy (car list)) (colide-enemy proj (cdr list)))]
          [else (colide-enemy proj (cdr list))])]
          [else (colide-enemy proj (cdr list))])]
    [else (colide-enemy proj (cdr list))])]))

(define (check-border-enemy list)
  (cond
  [(> (enemy-x (list-ref enemy-list 1)) -148) (begin (set! enemy-speed (* enemy-speed -1)) (enemy-down list))]
  [(< (enemy-x (list-ref enemy-list 27)) 318) (begin (set! enemy-speed (* enemy-speed -1)) (enemy-down list))]
  [else 0]))


(define (tock ws)
  (cond
    [(equal? ws 1) (begin (colide-player) (enemy-shoot enemy-list) (move-e-proj) (colide-mother P-PROJ) (spawn-mother ws) (move-mother ws) (change-score ws SCOREBOARD) (colide-enemy P-PROJ enemy-list) (move-proj) (change-sprite enemy-list) (set! TICK (+ TICK 1)) (move-enemies enemy-list) (check-border-enemy enemy-list) (check-colide-enemy ws (player-y PLAYER) enemy-list))]
    [else ws]))


(define (wain ws)
;  (cond
;    [(equal? ws 3) (play MUSIC)]
;    [(equal? ws 0) (begin (stop) (play WIN-S))])
  (begin (big-bang ws
   [to-draw render]
   [on-key move]
   [on-tick tock]
;   [display-mode 'fullscreen]
   )))
