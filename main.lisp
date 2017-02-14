;;(ql:quickload :lispbuilder-sdl)

(defmacro define-class (name superclasses slots form)
  `(defclass ,name ,superclasses
    ,(mapcar (lambda (slot)
               (let ((keyword (intern (symbol-name slot) :keyword)))
               `(,slot :initarg ,keyword :initform ,form :accessor ,slot)))
              slots)))
;; --------------------------------------------
(defun load-png-image (source-file)
  (sdl:convert-to-display-format :surface (sdl:load-image source-file)
                                 :enable-alpha t
                                 :pixel-alpha t))
;; --------------------------------------------
;;(defparameter *gpin-image* (load-png-image "/home/aria/game/gpin/gpin.png"))
;; --------------------------------------------
(define-class object ()
  (id x y width height) 0)
; id      graphic id in imageid
; x       upper left corner
; y       upper left corner
; width   from upper left corner
; height  from upper left corner

(define-class entity (object)
  (dx dy dir dir-time update-frame state hp move-type
      kusa-x kusa-y explode-cnt big)  0)     ; + ship explosion revival
; dx          x direction speed
; dy          y direction speed
;;dir 向き
;;dir-time 向きを変更するタイミング
;;update-frame キャラの時間
;; state 0 生存 1 死亡 2 爆発
;;move-type 1 草を食べに行く  2 キャラを追いかける
;; --------------------------------------------
(define-class tama (object)
  (dx dy rad state) 0)
;; --------------------------------------------
(define-class chara-manager ()
  (gpin-list mukku-list kusa-list kinoko-list unk-list tama-list) nil)
;; --------------------------------------------
(define-class keystate ()
  (right left up down z lshift) nil)
 ; right  right-key
 ; left   left-key
 ; up     up-key
 ; down   down-key
 ; z      z-key
 ; lshift lshift-key
;; -----------------------------------------------------------------------------------------------
(define-class stage ()
  (stage-flag stage-number title-loop ending-loop start) t)
 ; stage-flag        on-stage or not
 ; stage-number      map change
 ; title-loop        waiting for input-key
 ; ending-loop       waiting for input-key
 ; start             game start

;; -----------------------------------------------------------------------------------------------
(defparameter *path-explodes-sound* "./sound/Explosion.wav")
(defparameter *path-hit-sound* "./sound/Hit.wav")
(defparameter *path-hit-unk-sound* "./sound/Hit_unk.wav")
(defparameter *path-hit-kusa-sound* "./sound/kusa.wav")

(defvar *explode*)
(defvar *hit*)
(defvar *hit-unk*)
(defvar *hit-kusa*)

(defun open-sound ()
  (sdl-mixer:open-audio :chunksize 1024 :channels 2)
  (sdl-mixer:allocate-channels 16)
  (setf *explode* (sdl-mixer:load-sample *path-explodes-sound*)
        *hit*     (sdl-mixer:load-sample *path-hit-sound*)
        *hit-unk* (sdl-mixer:load-sample *path-hit-unk-sound*)
        *hit-kusa* (sdl-mixer:load-sample *path-hit-kusa-sound*)))

(defun stop-sound ()
  (when (sdl-mixer:music-playing-p)
    (sdl-mixer:halt-music))
  (when (sdl-mixer:sample-playing-p nil)
    (sdl-mixer:halt-sample)))

(defun close-sound ()
  (sdl-mixer:free *explode*)
  (sdl-mixer:free *hit*)
  (sdl-mixer:free *hit-unk*)
  (sdl-mixer:free *hit-kusa*)
  (sdl-mixer:close-audio))

(defun play-sample (sample)
  (sdl-mixer:play-sample sample))
;; -----------------------------------------------------------------------------------------------
#|
(defun Stage-start-message (stage keystate)            ; stage start message
  "Draw stage start message and set game parameters"
  (when (eql (stage-flag stage) t)
    (setf (stage-flag stage) nil
          (z keystate) nil                                 ; z-key reset
	  (lshift keystate) nil)                            ; lshift-key reset
    (incf (stage-number stage) 1)
    (case (stage-number stage)
      (1 (setf *atlas* *map1*
               ;;*BGM* *samplebgm1*
               *enemymap* *enemy-map1*))
      (2 (setf *atlas* *map2*
               ;;*BGM* *samplebgm2*
               *enemymap* *enemy-map2*))
      (3 (setf *atlas* *map3*
               ;;*BGM* *samplebgm3*
               *enemymap* *enemy-map3*)))
    (sdl:clear-display sdl:*black*)
    (sdl:draw-string-solid-*
         (format nil "S T A G E  ~d" (stage-number stage)) 272 208 :color sdl:*white* :font *menu-font*)
    (sdl:update-display)
    (sleep 3)))
|#

;; ---------------------------------------------------------------------------------------------
;;フォント設定
(defparameter *path-font16* "./font/mplus-1mn-regular.ttf")
(defparameter *font16* (make-instance 'sdl:ttf-font-definition
                                :size 16
                                :filename (sdl:create-path *path-font16*)))
(defvar *menu-font*)                          ; menu font
;;(defparameter *path-font80* "./font/Pacifico.ttf")
(defparameter *font80* (make-instance 'sdl:ttf-font-definition
                                :size 80
                                :filename (sdl:create-path *path-font16*)))
(defvar *title-font*)                          ; menu font
(defparameter *font60* (make-instance 'sdl:ttf-font-definition
                                :size 60
                                :filename (sdl:create-path *path-font16*)))
(defvar *60-font*)                          ; menu font



(defun Set-font ()
  (setf *menu-font*  (sdl:initialise-font *font16*)
        *title-font* (sdl:initialise-font *font80*)
        *60-font* (sdl:initialise-font *font60*)))


;; -----------------------------------------------------------------------------------------------

(defun Game-over-message (stage gpin-p chara-manager keystate)
  "Draw game ending"
  (cond ((/= (state gpin-p) 0)
         ; game over message
         (sdl:draw-string-solid-* "GAME OVER" 184 200  :color sdl:*white* :font *title-font*)
         (sdl:update-display))
        ((null (mukku-list chara-manager))
         (sdl:clear-display sdl:*black*)
         ; congratulations message
         ;;(sdl:draw-string-solid-* "君の活躍によりアボガドロ軍は撤退した。"
         ;;                                    178 64  :color sdl:*white* :font *menu-font*)
         (sdl:draw-string-solid-* "V I C T O R Y !!"
                                  130 64  :color sdl:*white* :font *title-font*)
         (sdl:draw-string-solid-* "C O N G R A T U L A T I O N S"
                                  20 196  :color sdl:*white* :font *60-font*)
         (sdl:update-display)))

    (when (z keystate)                       ; push z key
      (setf (title-loop stage) t                 ; GAME TITLE flag   ON
            (ending-loop stage) nil              ; GAME OVER flag    OFF
            (z keystate) nil)
      (init-charas gpin-p chara-manager)))
;; -----------------------------------------------------------------------------------------------

(defun Game-start-message (pointer stage keystate)   ; game start message
  "Draw game opening message"
  (sdl:clear-display sdl:*black*)
  ;;(Stop-sound)                                                     ; stop ending BGM if playing
 ; title

  (sdl:draw-string-solid-* "Gピン vs Mック" 120 100 :color sdl:*white* :font *title-font*) ; show menu
 ; memu
  (sdl:draw-string-solid-* "S T A R T" 374 328 :color sdl:*white* :font *menu-font*) ; show menu
  (sdl:draw-string-solid-* "E X I T" 374 360 :color sdl:*white* :font *menu-font*)
 ; select menu
  (cond ((up keystate)                                            ; select menu
         (decf (y pointer) 32)
         (setf (up keystate) nil)
         (when (<= (y pointer) 328)                               ; y:328 is START menu position
                (setf (y pointer) 328
                      (start stage) t)))
        ((down keystate)
          (incf (y pointer) 32)
          (setf (down keystate) nil)
          (when (>= (y pointer) 360)                              ; y:392 is EXIT menu position
            (setf (y pointer) 360
                  (start stage) nil))))
 ; show pointer
  (sdl:draw-string-solid-* ">" (x pointer) (y pointer) :color sdl:*white* :font *menu-font*)
 ; game start or exit
  (cond ((and (z keystate) (eql (start stage) t) (= (y pointer) 328))              ; input z-key on start menu
          (setf (title-loop stage) nil
                (z keystate) nil))                                                 ; z key state reset
        ((and (z keystate) (eql (start stage) nil) (= (y pointer) 360))            ; input z-key on exit menu
          (sdl:push-quit-event)))
  (sdl:update-display))
;;-----------------------------------------------------------------
(declaim (inline degree-radian))
(defun degree-radian (degree)                       ; convert from radian to degree
  (/ (* degree pi) 180))                            ; degree -> radian

(declaim (inline radian-degree))
(defun radian-degree (radian)                       ; convert from radian to degree
  (/ (* radian 180) pi)) 
;; --------------------------------------------
(defgeneric Update-keystate (key boolean keystate))
(defmethod Update-keystate (key boolean keystate)
  (cond ((sdl:key= key :SDL-KEY-RIGHT)  (setf (right  keystate) boolean))
        ((sdl:key= key :SDL-KEY-LEFT)   (setf (left   keystate) boolean))
        ((sdl:key= key :SDL-KEY-UP)     (setf (up     keystate) boolean))
        ((sdl:key= key :SDL-KEY-DOWN)   (setf (down   keystate) boolean))
        ((sdl:key= key :SDL-KEY-Z)      (setf (z      keystate) boolean))
        ((sdl:key= key :SDL-KEY-LSHIFT) (setf (lshift keystate) boolean))))


;; ----------------------------------------------------------------------------
(defvar *mukku32*)
(defvar *mukku-boss*)
(defvar *mukku->player*)
(defvar *mukku64*)
(defvar *mukku->gpin*)
(defvar *gpin32-p*)
(defvar *gpin32*)
(defvar *gpin64-p*)
(defvar *gpin64*)
(defvar *gpin-explode*)
(defvar *mukku-explode*)
(defvar *unk*)
(defvar *tama*)
(defvar *kusa*)
(defvar *kinoko*)
(defun set-image ()
  (setf *mukku32* (load-png-image "./image/mukku32.png")
	*mukku-boss* (load-png-image "./image/mukku-kusa.png")
	*mukku->player* (load-png-image "./image/mukku-player.png")
	*mukku64* (load-png-image "./image/mukku64.png")
	*mukku->gpin* (load-png-image "./image/mukku-chara.png")
	*gpin32-p* (load-png-image "./image/gpin-p.png")
	*gpin32* (load-png-image "./image/gpin.png")
	*gpin64-p* (load-png-image "./image/gpin64-p.png")
	*gpin64* (load-png-image "./image/gpin64.png")
	*gpin-explode* (load-png-image "./image/gpin-explode.png")
	*mukku-explode* (load-png-image "./image/mukku-explode.png")
	*unk* (load-png-image "./image/unk.png")
        *kusa* (load-png-image "./image/bokusou16.png")
        *kinoko* (load-png-image "./image/kinoko.png")
	*tama* (load-png-image "./image/tama.png")))
  
;; ----------------------------------------------------------------------------
(defun draw-status (chara)
  (sdl:draw-surface-at-* *gpin32-p* 650 190)
  (sdl:draw-string-solid-*
   (format nil "H P  ~d" (hp chara)) 690 200
   :color sdl:*white* :font *menu-font*))

(defun draw-kusamukku (chara)
  (cond
    ((= (move-type chara) 100)
     (sdl:draw-surface-at-* *mukku-boss* 650 290)
     (sdl:draw-string-solid-*
      (format nil "H P  ~d" (hp chara)) 690 300
      :color sdl:*white* :font *menu-font*))))
;; --------------------------------------------

(defun Draw (obj)
  "character draw"
  (sdl:draw-surface-at-* (id obj) (round (x obj)) (round (y obj))))
  
  ;;(sdl:draw-string-solid-*
  ;; (format nil " ~d" (hp obj)) (+ (x obj) 1) (- (y obj) 15)
  ;; :color sdl:*white* :font *menu-font*))

(defgeneric Draw-chara (chara-manager))
(defmethod Draw-chara (chara-manager)
  (dolist (kusa (kusa-list chara-manager))
    (Draw kusa))
  (dolist (kinoko (kinoko-list chara-manager))
    (draw kinoko))
  (dolist (unk (unk-list chara-manager))
    (draw unk))
  (dolist (tama (tama-list chara-manager))
    (when (= 0 (state tama))
      (draw tama)))
  (dolist (gpin (gpin-list chara-manager))
    (when (= 0 (state gpin))
      (Draw gpin)))
  (dolist (mukku (mukku-list chara-manager))
    (when (= 0 (state mukku))
      ;;(cond
	;;((= 100 (move-type mukku))
	 ;;(setf (id mukku) *mukku-boss*)
	 ;;(draw-kusamukku mukku))
	;;((= 2 (move-type mukku))
	;; (setf (id mukku) *mukku->gpin*))
	;;((= 3 (move-type mukku))
	 ;;(setf (id mukku) *mukku->player*)))
      (Draw mukku))))
;; ----------------------------------------------------------
;; うんこ最初は当たり判定なし
(defun unk-counter (chara-manager)
  (dolist (unk (unk-list chara-manager))
    (when (= (state unk) 2)
      (incf (explode-cnt unk))
      (when (= (explode-cnt unk) 100)
	(setf (state unk) 0)))))
;; --------------------------------------------
(defgeneric Explode-counter (chara chara-manager  game-field))
(defmethod Explode-counter (chara chara-manager  game-field)
  (when (= (state chara) 2)                    ; ship is explode
    (incf (explode-cnt chara) 1)
    (when (= (explode-cnt chara) 100)
      (setf (state chara) 1)))
  (dolist (gpin (gpin-list chara-manager))
    (when (= (state gpin) 2)
      (incf (explode-cnt gpin) 1)
      (when (= (explode-cnt gpin) 100)
	(setf (state gpin) 1))))
  (dolist (mukku (mukku-list chara-manager))
    (when (= (state mukku) 2)
      (incf (explode-cnt mukku) 1)
      (when (= (explode-cnt mukku) 100)
	(setf (state mukku) 1)))))
;; --------------------------------------------
(defgeneric Draw-chara-explosion (chara chara-manager explosion))
(defmethod Draw-chara-explosion (chara chara-manager explosion)
  (when (and (= (state chara) 2)
	     (< (explode-cnt chara) 100))
    (if (= 0 (big chara))
	(setf (x explosion) (- (x chara) 32)           ; x : center of explosion
	      (y explosion) (- (y chara) 32)
	      (id explosion) *gpin-explode*)     ; y : center of explosion
	(setf (x explosion) (x chara) 
	      (y explosion) (y chara)
	      (id explosion) *gpin-explode*))
    (play-sample *explode*)
    (Draw explosion))
  (dolist (gpin (gpin-list chara-manager))
    (when (and (= (state gpin) 2)
	       (< (explode-cnt gpin) 30))
      (if (= 0 (big gpin))
	(setf (x explosion) (- (x gpin) 32)           ; x : center of explosion
	      (y explosion) (- (y gpin) 32)
	      (id explosion) *gpin-explode*)     ; y : center of explosion
	(setf (x explosion) (x gpin) 
	      (y explosion) (y gpin)
	      (id explosion) *gpin-explode*))
      (play-sample *explode*)
      (Draw explosion)))
  (dolist (mukku (mukku-list chara-manager))
    (when (and (= (state mukku) 2)
	       (< (explode-cnt mukku) 30))
      (if (= 0 (big mukku))
	(setf (x explosion) (- (x mukku) 32)           ; x : center of explosion
	      (y explosion) (- (y mukku) 32)
	      (id explosion) *mukku-explode*)     ; y : center of explosion
	(setf (x explosion) (x mukku) 
	      (y explosion) (y mukku)
	      (id explosion) *mukku-explode*))
      (play-sample *explode*)
      (Draw explosion))))
;; --------------------------------------------
(defun Initialize ()
  "graphics initialize"
  (setf (sdl:frame-rate) 60)                      ; frame rate set
  (setf *random-state* (make-random-state t)))     ; random set
  ;;(sdl:show-cursor nil))                          ; cursor not show
;; --------------------------------------------
(define-class game-field ()
  (field-x field-y width height) 0)
; field-x  game field upper left x
; field-y  game field upper left y
; width    game field width
; height   game field height
(defparameter *gamen-w* 640)
(defparameter *gamen-h* 480)
(defgeneric Fix-chara-position (chara game-field))
(defmethod Fix-chara-position (chara game-field)
  "chara always inside game-field"
  (when (< (x chara) (field-x game-field))
    (setf (x chara) (field-x game-field)
          (dir chara) 1))
  (when (< (y chara) (field-y game-field))
    (setf (y chara) (field-y game-field)
          (dir chara) 0))
  (cond
    ((= 0 (big chara))
     (when (> (x chara) (- (width game-field) 32))
       (setf (x chara) (- (width game-field) 32)
	     (dir chara) 2))
     (when (> (y chara) (- (height game-field) 32))
       (setf (y chara) (- (height game-field) 32)
	     (dir chara) 3)))
    ((= 1 (big chara))
     (when (> (x chara) (- (width game-field) 64))
       (setf (x chara) (- (width game-field) 64)
	     (dir chara) 2))
     (when (> (y chara) (- (height game-field) 64))
       (setf (y chara) (- (height game-field) 64)
	     (dir chara) 3)))))
;;-----------------------------------------------------------------
;;画面外に出た玉のstateを2にする
(defun delete-tama-position (chara game-field)
  "chara always inside game-field"
  (when (< (+ 16 (x chara)) (field-x game-field))
    (setf (state chara) 1))
  (when (< (+ 16 (y chara)) (field-y game-field))
    (setf (state chara) 1))
  (when (> (x chara) (width game-field))
    (setf (state chara) 1))
  (when (> (y chara) (height game-field))
    (setf (state chara) 1)))
;; --------------------------------------------
(defun add-chara (type chara-manager chara)
  (with-slots (mukku-list gpin-list kusa-list kinoko-list unk-list tama-list) chara-manager
    (cond
      ((eq type 'mukku)
       (push chara mukku-list))
      ((eq type 'gpin)
       (push chara gpin-list))
      ((eq type 'kusa)
       (push chara kusa-list))
      ((eq type 'unk)
       (push chara unk-list))
      ((eq type 'tama)
       (push chara tama-list))
      ((eq type 'kinoko)
       (push chara kinoko-list)))))
;; --------------------------------------------
(defgeneric Move-gpin (gpin keystate))
(defmethod Move-gpin (gpin keystate)
  (when (= (state gpin) 0)                       
    (cond ((right keystate) (incf (x gpin) (dx gpin)))
          ((left  keystate) (decf (x gpin) (dx gpin)))
          ((up    keystate)  (decf (y gpin) (dy gpin)))
          ((down  keystate)  (incf (y gpin) (dy gpin))))))
;; --------------------------------------------------------
(defun update-gpin (gpin stage)
  (cond ((>= 0 (hp gpin))
         (play-sample *explode*)
         (setf (state gpin) 2
               (ending-loop stage) t))
         ((/= (state gpin) 0)
              (setf (ending-loop stage) t))))

;; --------------------------------------------------------
(defun init-charas (gpin-p chara-manager)
  (setf (x gpin-p) (random *gamen-w*)
        (y gpin-p) (random *gamen-h*)
        (state gpin-p) 0
        (hp gpin-p) 50)
  (setf (mukku-list chara-manager) nil
        (kusa-list chara-manager) nil
        (tama-list chara-manager) nil
        (unk-list chara-manager) nil
        (gpin-list chara-manager) nil)
  (add-chara 'mukku chara-manager
             (make-instance
               'entity
               :id *mukku32* :width 32 :height 32 :state 0
               :x (random *gamen-w*) :y (random *gamen-h*) :dx 1 :dy 1 :hp 50
               :move-type 100
               :dir (random 4) :dir-time (+ 100 (random 80)))))

  
;; --------------------------------------------------------
(defun square (x)
  (* x x))
(defun 2tenkyori (x1 y1 x2 y2)
  (sqrt (+ (square  (- x1 x2)) (square (- y1 y2)))))

(defun manhatan (x1 y1 x2 y2)
  (+ (abs (- x1 x2)) (abs (- y1 y2)))) 
	   
(defun push-last (lst y)
  (append lst (list y)))
;; ----------------------------------------------------------------------------
(defun 32-16-hit (chara kinoko)
  (and (> (+ (x chara) 25) (+ (x kinoko) 3))
       (< (+ (x chara) 8) (+ (x kinoko) 13))
       (> (+ (y chara) 26) (+ (y kinoko) 3))
       (< (+ (y chara) 6) (+ (y kinoko) 13))))
(defun 64-16-hit (chara kinoko)
  (and (> (+ (x chara) 52) (+ (x kinoko) 3))
       (< (+ (x chara) 16) (+ (x kinoko) 13))
       (> (+ (y chara) 52) (+ (y kinoko) 3))
       (< (+ (y chara) 12) (+ (y kinoko) 13))))
;; --------------------------------------------------------
(defun move-tama (tama game-field)
  (incf (x tama) (dx tama))
  (incf (y tama) (dy tama))
  (delete-tama-position tama game-field))
;; --------------------------------------------------------
(defgeneric move-chara (chara game-field))
(defmethod move-chara (chara game-field)
  (with-slots (x y dx dy dir dir-time update-frame) chara
    (when (zerop (mod update-frame dir-time))
      (setf dir (random 4)))
    (cond
     	((eq dir 0) (incf y dy))
     	((eq dir 1) (incf x dx))
     	((eq dir 2) (decf x dx))
     	((eq dir 3) (decf y dy)))
    (fix-chara-position chara game-field)
    (if (> update-frame 10000)
	(setf update-frame 0))
    (incf update-frame)))
;; dir=0 下向き dir=1 右向き dir=2 左向き dir=3 上向き
;; --------------------------------------------------------
(defun move->kusa (chara chara-manager)
  (let ((kyori 2000)
	(tmp 0)
	(kusa-x 0)
	(kusa-y 0))
    (if (kinoko-list chara-manager)
	(dolist (kinoko (kinoko-list chara-manager))
	  (setf tmp (manhatan (x chara) (y chara)  (x kinoko) (y kinoko)))
	  (if (> kyori tmp)
	      (setf kyori tmp
		    kusa-x (x kinoko)
		    kusa-y (y kinoko))))
	(dolist (kusa (kusa-list chara-manager))
	  (setf tmp (manhatan (x chara) (y chara)  (x kusa) (y kusa)))
	  (if (> kyori tmp)
	      (setf kyori tmp
		    kusa-x (x kusa)
		    kusa-y (y kusa)))))
    (if (= 1 (big chara))
	(cond
	  ((> (+ (x chara) 22)  kusa-x)
	   (decf (x chara)))
	  ((< (+ (x chara) 22)  kusa-x)
	   (incf (x chara)))
	  ((> (y chara)  kusa-y)
	   (decf (y chara)))
	  ((< (y chara) kusa-y)
	   (incf (y chara))))
	(cond
	  ((> (x chara)  kusa-x)
	   (decf (x chara)))
	  ((< (x chara)  kusa-x)
	   (incf (x chara)))
	  ((> (y chara)  kusa-y)
	   (decf (y chara)))
	  ((< (y chara)  kusa-y)
	   (incf (y chara)))))))
    ;;(fix-chara-position chara game-field)))
;; --------------------------------------------------------
(defun move->300kusa (chara chara-manager game-field)
  (let ((kusa? nil))
    ;;狙った草がまだあるか調べる
    (loop for kusa in (kusa-list chara-manager)
       do (cond
	    ;;300px以内に草がないか調べる
	    ((> 400 (manhatan (x chara) (x kusa) (y chara) (y kusa)))
	     (setf (kusa-x chara) (x kusa)
		   (kusa-y chara) (y kusa)
		   kusa? t)
	     (return))))
    (cond
      ((not kusa?)
       (move-chara chara game-field))
      ((> (x chara)  (kusa-x chara))
       (decf (x chara)))
      ((< (x chara)  (kusa-x chara))
       (incf (x chara)))
      ((> (y chara)  (kusa-y chara))
       (decf (y chara)))
      ((< (y chara)  (kusa-y chara))
       (incf (y chara))))))
;; --------------------------------------------------------
(defun move->kusa-kai (chara chara-manager game-field)
  (let ((kusa  (car (kusa-list chara-manager))))
    (if (or (and kusa (= 0 (kusa-x chara)) (= 0 (kusa-y chara)))
	    (and kusa (= (kusa-x chara) (x chara)) (= (kusa-y chara) (y chara))))
	  (setf (kusa-x chara) (x kusa)
		(kusa-y chara) (y kusa)))
    (cond
      ((not kusa)
       (move-chara chara game-field))
      ((> (x chara)  (kusa-x chara))
       (decf (x chara)))
      ((< (x chara)  (kusa-x chara))
       (incf (x chara)))
      ((> (y chara)  (kusa-y chara))
       (decf (y chara)))
      ((< (y chara)  (kusa-y chara))
       (incf (y chara))))))
;; --------------------------------------------------------
(defun move->player (chara mukku game-field)
  (cond
    ((> (x chara)  (x mukku))
     (incf (x mukku)))
    ((< (x chara)  (x mukku))
     (decf (x mukku)))
    ((> (y chara)  (y mukku))
     (incf (y mukku)))
    ((< (y chara)  (y mukku))
     (decf (y mukku))))
  (fix-chara-position mukku game-field))
;; --------------------------------------------------------
;;Gピンに向かっていく
(defun move->gpin (mukku chara-manager game-field)
  (let ((gpin1 (car (gpin-list chara-manager))))
    (cond
      ((not gpin1)
       (move-chara mukku game-field))
      ((> (x mukku)  (x gpin1))
       (decf (x mukku)))
      ((< (x mukku)  (x gpin1))
       (incf (x mukku)))
      ((> (y mukku)  (y gpin1))
       (decf (y mukku)))
      ((< (y mukku) (y gpin1))
       (incf (y mukku))))))
;; --------------------------------------------
;; update-chara
(defun move-charas (chara chara-manager game-field)
  (dolist (gpin (gpin-list chara-manager))
    (when (= 0 (state gpin))
      (move-chara gpin game-field)))
  (dolist (tama (tama-list chara-manager))
    (when (= 0 (state tama))
      (move-tama tama game-field)))
  (dolist (mukku (mukku-list chara-manager))
    (when (= 0 (state mukku))
      (cond
	((= 100 (move-type mukku))
	 (move->kusa  mukku chara-manager))
	((and (= 2 (move-type mukku)) (>= 20 (hp mukku)))
	 (move->kusa-kai mukku chara-manager game-field))
	((= 2 (move-type mukku))
	 (move->gpin mukku chara-manager game-field))
	((= 3 (move-type mukku))
	 (move->player  chara mukku  game-field))
	(t
	 (move-chara mukku game-field))))))
;;-----------------------------------------------------
(defun add-unks (x chara-manager)
  (dolist (mukku (mukku-list chara-manager))
    (when (zerop (mod x (+ (random 800) 700)))
      (add-unk mukku chara-manager))))

;;-----------------------------------------------------
;;弾発射
(defun add-tamas (x chara-manager gpin)
  (dolist (mukku (mukku-list chara-manager))
    (when (and (= (state mukku) 0)
               (zerop (mod x (+ (random 700) 300))))
      (add-tama mukku chara-manager gpin))))

;; --------------------------------------------
(defun chara-hp-down (chara chara-manager time)
  (when (zerop (mod time 60))
    (decf (hp chara))
    (if (>= 0 (hp chara))
	(setf (state chara) 1))
    (dolist (mukku (mukku-list chara-manager))
      (decf (hp mukku))
      (if (<= (hp mukku) 0)
	  (setf (state mukku) 1)))
    (dolist (gpin (gpin-list chara-manager))
      (decf (hp gpin))
      (if (<= (hp gpin) 0)
	  (setf (state gpin) 1)))))
;; --------------------------------------------------------------------
(defun normarl-gpin-mukku-hit (gpin mukku)
  (if (= 0 (big mukku))
      (when (and (> (+ (x gpin) 26) (+ (x mukku) 8))
		 (< (+ (x gpin) 8) (+ (x mukku) 24))
		 (> (+ (y gpin) 26) (+ (y mukku) 6))
		 (< (+ (y gpin) 6) (+ (y mukku) 26)))
	(cond
	  ((> (hp gpin) (hp mukku))
	   (setf (hp gpin) (- (hp gpin) (hp mukku))
		 (state mukku) 2))
	  ((< (hp gpin) (hp mukku))
	   (setf (hp mukku) (- (hp mukku) (hp gpin))
		 (state gpin) 2))
	  ((= (hp gpin) (hp mukku))
	   (setf (state gpin) 2
		 (state mukku) 2))))
      (when (and (> (+ (x gpin) 26) (+ (x mukku) 16))
		 (< (+ (x gpin) 8) (+ (x mukku) 48))
		 (> (+ (y gpin) 26) (+ (y mukku) 12))
		 (< (+ (y gpin) 6) (+ (y mukku) 52)))
        (cond
	  ((> (hp gpin) (hp mukku))
	   (setf (hp gpin) (- (hp gpin) (hp mukku))
		 (state mukku) 2))
	  ((< (hp gpin) (hp mukku))
	   (setf (hp mukku) (- (hp mukku) (hp gpin))
		 (state gpin) 2))
	  ((= (hp gpin) (hp mukku))
	   (setf (state gpin) 2
		 (state mukku) 2))))))
;; --------------------------------------------------------------------
(defun big-gpin-mukku-hit (gpin mukku)
  (if (= 0 (big mukku))
      (when (and (> (+ (x gpin) 52) (+ (x mukku) 8))
		 (< (+ (x gpin) 16) (+ (x mukku) 24))
		 (> (+ (y gpin) 52) (+ (y mukku) 6))
		 (< (+ (y gpin) 12) (+ (y mukku) 26)))
        (cond
	  ((> (hp gpin) (hp mukku))
	   (setf (hp gpin) (- (hp gpin) (hp mukku))
		 (state mukku) 2))
	  ((< (hp gpin) (hp mukku))
	   (setf (hp mukku) (- (hp mukku) (hp gpin))
		 (state gpin) 2))
	  ((= (hp gpin) (hp mukku))
	   (setf (state gpin) 2
		 (state mukku) 2))))
      (when (and (> (+ (x gpin) 52) (+ (x mukku) 16))
		 (< (+ (x gpin) 16) (+ (x mukku) 48))
		 (> (+ (y gpin) 52) (+ (y mukku) 12))
		 (< (+ (y gpin) 12) (+ (y mukku) 52)))
        (cond
	  ((> (hp gpin) (hp mukku))
	   (setf (hp gpin) (- (hp gpin) (hp mukku))
		 (state mukku) 2))
	  ((< (hp gpin) (hp mukku))
	   (setf (hp mukku) (- (hp mukku) (hp gpin))
		 (state gpin) 2))
	  ((= (hp gpin) (hp mukku))
	   (setf (state gpin) 2
		 (state mukku) 2))))))
;; --------------------------------------------------------------------
(defun normal-chara-kusa-hit (chara kusa type chara-manager)
  (when (32-16-hit chara kusa)
    (play-sample *hit-kusa*)
    (setf (state kusa) 1)
    (incf (hp chara) 10)
    (cond
      ((eq type 'gpin)
       (add-chara  'gpin chara-manager
		   (make-instance 'entity :id *gpin32*
				  :x (x chara) :y (y chara) :width 32 :height 32
				  :dx 1 :dy 1 :hp 40
				  :dir (random 4) :dir-time (+ 100 (random 80)))))
      ((eq type 'mukku)
       (setf (kusa-x chara) 0
	     (kusa-y chara) 0)
       (add-chara 'mukku chara-manager
		   (make-instance 'entity :id *mukku32*
				  :x (x chara) :y (y chara) :width 32 :height 32
				  :dx 1 :dy 1 :hp 40 :move-type (random 10)
				  :dir (random 4) :dir-time (+ 100 (random 80))))))))
;; --------------------------------------------------------------------
(defun big-chara-kusa-hit (chara kusa type chara-manager)
  (when (64-16-hit chara kusa)
    (play-sample *hit-kusa*)
    (setf (state kusa) 1)
    (incf (hp chara) 10)
    (cond
      ((eq type 'gpin)
       (add-chara  'gpin chara-manager
		   (make-instance 'entity :id *gpin32*
				  :x (x chara) :y (y chara) :width 32 :height 32
				  :dx 1 :dy 1 :hp 40
				  :dir (random 4) :dir-time (+ 100 (random 80)))))
      ((eq type 'mukku)
       (setf (kusa-x chara) 0
	     (kusa-y chara) 0)
       (add-chara  'mukku chara-manager
		   (make-instance 'entity :id *mukku32*
				  :x (x chara) :y (y chara) :width 32 :height 32
				  :dx 1 :dy 1 :hp 40 :move-type (random 10)
				  :dir (random 4) :dir-time (+ 100 (random 80))))))))
;; --------------------------------------------------------------------
(defun normal-chara-unk-hit (chara unk)
  (when (32-16-hit chara unk)
    (play-sample *hit-unk*)
    (setf (state unk) 1)
    (decf (hp chara) 5)))
(defun big-chara-unk-hit (chara unk)
  (when (64-16-hit chara unk)
    (play-sample *hit-unk*)
    (setf (state unk) 1)
    (decf (hp chara) 5)))
;; -----------------------------------------------------------------------
(defun normal-chara-tama-hit (chara tama)
  (when (32-16-hit chara tama)
    (play-sample *hit*)
    (setf (state tama) 1)
    (decf (hp chara) 10)))
(defun big-chara-tama-hit (chara tama)
  (when (64-16-hit chara tama)
    (play-sample *hit*)
    (setf (state tama) 1)
    (decf (hp chara) 10)))

;; --------------------------------------------
(defun Hit-gpin-mukku (chara-manager)
  (dolist (gpin (gpin-list chara-manager))
    (if (= 0 (big gpin))
	(when (= 0 (state gpin))
	  (dolist (mukku (mukku-list chara-manager))
	    (when (= 0 (state mukku))
	      (normarl-gpin-mukku-hit gpin mukku))))
	(when (= 0 (state gpin))
	  (dolist (mukku (mukku-list chara-manager))
	    (when (= 0 (state mukku))
	      (big-gpin-mukku-hit gpin mukku)))))))
	  
;;草とキャラの当たり判定
(defun Hit-kusa (chara chara-manager)
  (dolist (gpin (gpin-list chara-manager))
    (when (= 0 (state gpin))
      (dolist (kusa (kusa-list chara-manager))
	(cond
	  ((= 0 (big gpin))
	   (normal-chara-kusa-hit gpin kusa 'gpin chara-manager))
	  ((= 1 (big gpin))
	   (big-chara-kusa-hit gpin kusa 'gpin chara-manager))))))
  ;;操作キャラとの当たり判定
  (dolist (kusa (kusa-list chara-manager))
    (cond
      ((= 0 (big chara))
       (normal-chara-kusa-hit chara kusa 'gpin chara-manager))
      ((= 1 (big chara))
       (big-chara-kusa-hit chara kusa 'gpin chara-manager))))
  ;;ムックと草の当たり判定
  (dolist (mukku (mukku-list chara-manager))
    (when (= 0 (state mukku))
      (dolist (kusa (kusa-list chara-manager))
	(cond
	  ((= 0 (big mukku))
	   (normal-chara-kusa-hit mukku kusa 'mukku chara-manager))
	   ((= 1 (big mukku))
	   (big-chara-kusa-hit mukku kusa 'mukku chara-manager)))))))
;;操作キャラとムックの当たり判定
(defun hit-gpin-p (chara chara-manager)
  (if (= 0 (big chara))
      (dolist (mukku (mukku-list chara-manager))
	(when (= 0 (state mukku))
	  (normarl-gpin-mukku-hit chara mukku)))
      (dolist (mukku (mukku-list chara-manager))
	(when (= 0 (state mukku))
	  (big-gpin-mukku-hit chara mukku)))))
;; ----------------------------------------------------------------------------
;;うんこと弾とキャラの当たり判定
(defun Hit-unk-and-tama (chara chara-manager)
  (dolist (gpin (gpin-list chara-manager))
    (when (= 0 (state gpin))
      (dolist (unk (unk-list chara-manager))
	(cond
	  ((= 0 (big gpin))
	   (normal-chara-unk-hit gpin unk))
	  ((= 1 (big gpin))
           (big-chara-unk-hit gpin unk))))
      (dolist (tama (tama-list chara-manager))
        (cond
          ((= 0 (big gpin))
           (normal-chara-tama-hit gpin tama))
          ((= 1 (big gpin))
           (big-chara-tama-hit gpin tama))))))

  ;;操作キャラとの当たり判定
  (dolist (unk (unk-list chara-manager))
    (cond
      ((= 0 (big chara))
       (normal-chara-unk-hit chara unk))
      ((= 1 (big chara))
       (big-chara-unk-hit chara unk))))
  ;;操作キャラとtamaの当たり判定
  (dolist (tama (tama-list chara-manager))
    (cond
      ((= 0 (big chara))
       (normal-chara-tama-hit chara tama))
      ((= 1 (big chara))
       (big-chara-tama-hit chara tama))))

  ;;ムックとunkの当たり判定
  (dolist (mukku (mukku-list chara-manager))
    (when (= 0 (state mukku))
      (dolist (unk (unk-list chara-manager))
	(when (= (state unk) 0)
	  (cond
	    ((= 0 (big mukku))
	     (normal-chara-unk-hit mukku unk))
	    ((= 1 (big mukku))
	     (big-chara-unk-hit mukku unk))))))))
;; ----------------------------------------------------------------------------
(defun Hit-kinoko (chara chara-manager)
  (dolist (gpin (gpin-list chara-manager))
    (when (= 0 (state gpin))
      (dolist (kinoko (kinoko-list chara-manager))
	(if (= 0 (big gpin))
	    (when (32-16-hit gpin kinoko)
	      (setf (state kinoko) 1)
	      (incf (hp gpin) 50)
	      (setf (id gpin) *gpin64*
		    (width gpin) 64 (big gpin) 1
		    (height gpin) 64))
	    (when (64-16-hit gpin kinoko)
	      (setf (state kinoko) 1)
	      (incf (hp gpin) 50))))))
  ;;操作キャラとの当たり判定
  (dolist (kinoko (kinoko-list chara-manager))
    (if (= 0 (big chara))
	(when (32-16-hit chara kinoko)
	  (incf (hp chara) 50)
	  (setf (state kinoko) 1)
	  (setf (id chara) *gpin64-p*
		(width chara)  64 (big chara) 1
		(height chara) 64))
	(when (64-16-hit chara kinoko)
	  (setf (state kinoko) 1)
	  (incf (hp chara) 50))))
  (dolist (mukku (mukku-list chara-manager))
    (when (= 0 (state mukku))
      (dolist (kinoko (kinoko-list chara-manager))
	(if (= 0 (big mukku))
	    (when (32-16-hit mukku kinoko)
	      (setf (state kinoko) 1)
	      (incf (hp mukku) 50)
	      (setf (id mukku) *mukku64*
		    (width mukku)  64 (big mukku) 1
		    (kusa-x mukku) 0 (kusa-y mukku) 0
		    (height mukku) 64))
	    (when (64-16-hit mukku kinoko)
	      (setf (state kinoko) 1)
	      (setf (kusa-x mukku) 0
		    (kusa-y mukku) 0)
	      (incf (hp mukku) 50)))))))
;; --------------------------------------------
(defun gc-charas (chara-manager)
  (setf (gpin-list chara-manager)
        (delete-if #'(lambda (gpin)
                             (= (state gpin) 1))
                   (gpin-list chara-manager))
        (mukku-list chara-manager)
        (delete-if #'(lambda (mukku)
                             (= (state mukku) 1))
                   (mukku-list chara-manager))
	(kinoko-list chara-manager)
        (delete-if #'(lambda (kinoko)
                             (= (state kinoko) 1))
                   (kinoko-list chara-manager))
	(unk-list chara-manager)
        (delete-if #'(lambda (unk)
                             (= (state unk) 1))
                   (unk-list chara-manager))
	(tama-list chara-manager)
        (delete-if #'(lambda (tama)
                             (= (state tama) 1))
                   (tama-list chara-manager))
        (kusa-list chara-manager)
        (delete-if #'(lambda (kusa)
                             (= (state kusa) 1))
                   (kusa-list chara-manager))
        (tama-list chara-manager)
        (delete-if #'(lambda (tama)
                       (= (state tama) 1))
                   (tama-list chara-manager))))
;; --------------------------------------------
(defun add-kusa (x chara-manager)
  (when (zerop (mod x 120))
    (add-chara 'kusa chara-manager
               (make-instance 'entity :id *kusa*
                              :x (random (- *gamen-w* 16)) :y (random (- *gamen-h* 16))
                              :width 16 :height 16 :state 0))))
;; --------------------------------------------
(defun add-kinoko (x chara-manager)
  (when  (zerop (mod x 1720))
    (add-chara 'kinoko chara-manager
               (make-instance 'entity :id *kinoko*
                              :x (random (- *gamen-w* 16)) :y (random (- *gamen-h* 16))
                              :width 16 :height 16 :state 0))))
;; ----------------------------------------------------------------------------
;; うんこ追加
(defun add-unk (chara chara-manager)
  (add-chara 'unk chara-manager
	     (make-instance 'entity :id *unk*
			    :x (+ 8 (x chara)) :y (+ 8 (y chara)) :state 2
			    :width 16 :height 16)))
;; ----------------------------------------------------------------------------
;; 弾追加
(defun add-tama (chara chara-manager gpin)
  (let ((angle (atan (- (y gpin) (y chara)) (- (x gpin) (x chara)))))
  (add-chara 'tama chara-manager
	     (make-instance 'tama :id *tama*
			    :x (+ 8 (x chara)) :y (+ 8 (y chara)) :state 0
			    :dx (* 2 (cos angle)) :dy (* 2 (sin angle))
			    :width 16 :height 16))))
;; ----------------------------------------------------------------------------
(defun victory? (chara-manager stage)
  (if (null (mukku-list chara-manager))
    (setf (ending-loop stage) t)))
;; ----------------------------------------------------------------------------
(defun draw-lines ()
  (sdl:draw-line-* 640 0 640 480))
;; --------------------------------------------
(defun GPIN ()
  "main routine"
  (sdl:with-init
   (sdl:sdl-init-video sdl:sdl-init-audio) ; use video and audio
   (sdl:window (+ 200 *gamen-w*) *gamen-h* ;;:position 'center ;size 640*480, position center
               :position #(192 50)              ; position x(192) y(50)
               :title-caption "GPIN"
               :icon-caption  "GPIN"
               :flags '(sdl:sdl-doublebuf sdl:sdl-sw-surface))
   (initialize)
   (set-font)
   (open-sound)
   (set-image)
   (let ((chara-manager (make-instance 'chara-manager))
         (kusa-time 0)
         (keystate (make-instance 'keystate))
         (pointer (make-instance 'object :x 358 :y 328))
         (stage (make-instance 'stage :title-loop t :ending-loop nil))
	 (explosion (make-instance 'object :id *gpin-explode*))
         (gpin-p (make-instance
                 'entity
                 :id *gpin32-p* :width 32 :height 32
                 :x (random *gamen-w*) :y (random *gamen-h*) :dx 1 :dy 1 :hp 50
                 :dir (random 4) :dir-time (+ 100 (random 80))))
         (game-field (make-instance 'game-field :field-x 0 :field-y 0 
                                    :width *gamen-w* :height *gamen-h*)))
     #|
     (dotimes (i 20)
              (add-chara 'gpin chara-manager
                         (make-instance
                          'entity
                          :id *gpin32* :width 32 :height 32
                          :x (random *gamen-w*) :y (random *gamen-h*) :dx 1 :dy 1 :hp 50
                          :dir (random 4) :dir-time (+ 100 (random 80)))))
     |#
    ;;初期ムック
     (dotimes (i 1)
       (add-chara 'mukku chara-manager
		  (make-instance
		   'entity
		   :id *mukku32* :width 32 :height 32 :state 0
		   :x (random *gamen-w*) :y (random *gamen-h*) :dx 1 :dy 1 :hp 50
		   :move-type 100
		   :dir (random 4) :dir-time (+ 100 (random 80)))))
     (sdl:update-display)
     (sdl:with-events
      ()
      (:quit-event ()
       (stop-sound)
       (close-sound)
       t)
      (:key-down-event (:key key)
       (if (sdl:key= key :SDL-KEY-ESCAPE)
         (sdl:push-quit-event)
         (Update-keystate key t keystate)))
      (:key-up-event (:key key)
       (Update-keystate key nil keystate))
      (:idle
        ()
        ;<Title Loop>
        (when (and (eql (title-loop stage) t)               ; GAME TITLE flag   ON
                   (eql (ending-loop stage) nil))
          (Game-start-message pointer stage keystate))
        (when (and (eql (title-loop stage) nil)
                   (eql (ending-loop stage) t))
          (Game-over-message stage gpin-p chara-manager keystate))
        (when (and (eql (title-loop stage) nil)
                   (eql (ending-loop stage) nil))
          (sdl:clear-display sdl:*black*)
            ;;キャラクター描画
            (sdl:clear-display sdl:*black*)
            (draw-chara chara-manager)
            ;;プレイヤー画像表示
            (when (= (state gpin-p) 0)
              (draw gpin-p))
            ;;HP表示
            (draw-status gpin-p)
            ;;キャラクター移動
            (move-charas gpin-p chara-manager game-field)
            (move-gpin gpin-p keystate)
            ;;プレイヤーの生死判定
            (update-gpin gpin-p stage)
            ;;草追加
            (add-kusa kusa-time chara-manager)
            ;;キノコ追加
            (add-kinoko kusa-time chara-manager)
            ;;玉発射
            (add-tamas kusa-time chara-manager gpin-p)
            ;;unk追加

            (add-unks kusa-time chara-manager)
            ;;操作キャラとムックの当たり判定
            (hit-gpin-p gpin-p chara-manager)
            ;; ガチャピンとムックの当たり判定
            (hit-gpin-mukku chara-manager)
            ;;キャラと草の当たり判定
            (hit-kusa gpin-p chara-manager)
            ;;キャラとキノコの当たり判定
            (hit-kinoko gpin-p chara-manager)
            ;;キャラとうんこの当たり判定
            (hit-unk-and-tama gpin-p chara-manager)
            ;;unk無敵時間
            (unk-counter chara-manager)
            ;;爆発描写
            (explode-counter gpin-p chara-manager game-field)
            (draw-chara-explosion gpin-p chara-manager explosion)
            ;;キャラのHPがじょじょに減る
            ;;(chara-hp-down gpin-p chara-manager kusa-time)
            ;;stateが1になったものを削除
            (gc-charas chara-manager)
            ;;ムックが全滅したか判定
            (victory? chara-manager stage)
            ;;境界線
            (draw-lines)
            ;;草が生える時間
            (if (> kusa-time 10000)
              (setf kusa-time 0))
            (incf kusa-time))
          (sdl:update-display))))))
