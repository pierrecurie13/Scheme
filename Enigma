;the primary function with required attributes
;newpos is the new position of the rotors, calculated just before the encryption of each letter
;letters encrypted one at at time
(define (enigma rotor-list ring-setting initial-positions message)
  (let ((newpos (shiftpos ring-setting initial-positions)))
  (if (null? message) '()
  (cons (eval (editrotlist rotor-list) (editposlist newpos) (car message))
	(enigma rotor-list ring-setting newpos (cdr message)) ) ) ) )

;shifts the positions of all necessary rotors
;checkpoints are places where each rotor causes the next rotor to rotate
;positions are the current positions of the rotors
;shiftedpos is only the positions with the 1st rotor rotated by 1
(define (shiftpos checkpoints positions) (let ((shiftedpos (remainder (if (null? positions) 0 (+ 1 (car positions))) (length alphabet)) ))
  (cond ((null? positions) '())
	((null? checkpoints) (list shiftedpos))
	((= (car checkpoints) shiftedpos) (cons shiftedpos (shiftpos (cdr checkpoints) (cdr positions))) )
	(else (cons shiftedpos (cdr positions))) ) ) )

;following 3 functions create rotor and position lists that specify the order of encryption over the entire encryption process

;turns list of rotors into a nested list, each list a rotor and the direction
;rotlist is the list of rotors
(define (editrotlist rotlist)
  (append (edithelp rotlist 'down) (edithelp (cdr (reverse rotlist)) 'up) ) )

;does the dirty work of the above, rotlist doesn't change and command is just the direction attached
(define (edithelp rotlist command)
  (if (null? rotlist) '() (cons (list (car rotlist) command) (edithelp (cdr rotlist) command)) ) )

;edits the list of positions, doesn't need nested list
(define (editposlist poslist)
  (if (null? poslist) poslist (append poslist (list 0) (reverse poslist)) ) )

;our good friend the evaluator
;rotor-list is the complete rotor-list of all rotors that will be needed to enrypt a letter (includes up and down)
;settings is a complete list of positions for each of the above rotors
;char is the letter that needs to be encrypted
;the ifs are ugly, but prevent errors regarding car/cdr
(define (eval rotor-list settings char)
  (if (null? (cdr rotor-list))
      ((caar rotor-list) (cadar rotor-list) (if (null? settings) 0 (car settings)) char)
      (eval (cdr rotor-list) (if (null? settings) '() (cdr settings)) ((caar rotor-list) (cadar rotor-list) (if (null? settings) '() (car settings)) char)) ) )

;each rotor is defined as a procedure
;direction is self explanatory, setting is the position of the rotor, and char is the letter to be encrypted by that rotor
;the large mess does the actual encryption
(define (new-rotor perm-list)
  (lambda (direction setting char)
    (let ((l (length alphabet)))
    (if (equal? direction 'down)
	(list-ref alphabet (remainder (+ (position (list-ref perm-list (remainder (+ l (- (position char alphabet) setting)) l)) alphabet) setting) l) )
	(list-ref alphabet (remainder (+ (position (list-ref alphabet (remainder (+ l (- (position char alphabet) setting)) l)) perm-list) setting) l) )
	) ) ) )

;everything else that follows are test cases or stuff that came with this

(define ALPHABET '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(define ROTOR-I
  (new-rotor '(e k m f l g d q v z n t o w y h x u s p a i b r c j)) )
(define ROTOR-II
  (new-rotor '(a j d k s i r u x b l h w t m c q g z n p y f v o e)) )
(define ROTOR-III 
  (new-rotor '(b d f h j l c p r t x v z n y e i w g a k m u s q o)) )
(define ROTOR-IV
  (new-rotor '(e s o v p z j a y q u i r h x l n f t g k d c m w b)) )
(define ROTOR-V
  (new-rotor '(v z b r g i t y u p s d n h l x a w m j q o f e c k)) )
(define REFLECTOR-I
  (new-rotor '(y r u h q s l d p x n g o k m i e b f z c w v j a t)) )
(define REFLECTOR-II
  (new-rotor '(f v p j i a o y e d r z x w g c t k u q s b n m h l)) )
(define REFLECTOR-III
  (new-rotor '(e n k q a u y w j i c o p b l m d x z v f t h r g s)) )
(define REFLECTOR-IV
  (new-rotor '(r d o b j n t k v e h m l f c w z a x g y i p s u q)) )
(define s-rotor-1
  (new-rotor '(d c b a)))  ;;; all small alphabet test cases are from GSI/newsgroup
(define s-rotor-2
  (new-rotor '(c d a b)))
(define s-rotor-3
  (new-rotor '(a b d c)))
(define s-rotor-4
  (new-rotor '(c b a d)))
(define s-rotor-5
  (new-rotor '(d a b c)))
(define s-reflector-1
  (new-rotor '(d b c a)))
(define s-reflector-2
  (new-rotor '(b a d c)))

(define (check rotor-list ring-settings initial-positions msg result)
  (if (equal? (enigma rotor-list ring-settings initial-positions msg) result) 
    'pass 'fail) )

(define (enigma-tests)
  (list
    (check (list REFLECTOR-I) '( ) '( ) '(l i s p) '(g p f i))
    (check (list reflector-i) '() '() '(g p f i) '(l i s p))
    (check (list rotor-i rotor-ii rotor-iii reflector-i) 
      '(2 0) '(0 0 0) '(c a l) '(u v u))
    (check (list rotor-i rotor-ii rotor-iii reflector-i)
	   '(2 0) '(0 0 0) '(u v u) '(c a l))
    (check (list rotor-i rotor-iv rotor-v reflector-i)
      '(3 5) '(0 4 0) '(a a a) '(i e k))
    (check (list rotor-i rotor-i rotor-i rotor-i reflector-i)
      '(2 0 8) '(23 25 7 9) 
      '(s t r e e t s p i r i t)
      '(y m u j s d u y s l e j))
    (check (list rotor-i rotor-i rotor-i rotor-i reflector-i)
      '(2 0 8) '(23 25 7 9)
      '(y m u j s d u y s l e j) '(s t r e e t s p i r i t))
    (check (list rotor-i rotor-ii rotor-iv reflector-i) '(1 1) '(25 0 0)
	   '(t h e e n i g m a h a s b e e n c r a c k e d)
	   '(v z g h y b a w p t y l x v j u a l f a q d b) )
    (check (list rotor-v rotor-iv rotor-iii rotor-ii rotor-i reflector-ii) ;; GSI
     '(5 13 0 7) '(0 12 25 9 3)
     '(m y x o m a t o s i s) '(f a h r q y s t m b j))
    (check (list rotor-iv rotor-ii reflector-iii) ;; GSI
     '(25) '(23 4)
     '(m o t i o n p i c t u r e s o u n d t r a c k)
     '(x a y y p i o z a y w m j r h p l v q o b a o))
    (begin (set! alphabet '(a b c d)) (check (list s-rotor-2 s-rotor-4 s-rotor-1 s-rotor-5 s-rotor-3 s-reflector-1)
     '(1 2 2 0) '(2 3 1 3 3)
     '(a b c d c b a b c d c b a)
     '(a c c a c b a c b d a b c)))
    (check (list s-rotor-1 s-rotor-2 s-rotor-3 s-rotor-4 s-reflector-2)
     '(1 2 2) '(3 2 2 0)
     '(a c d c a b d b) '(c a b a c d b d))
    (begin (set! alphabet '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
    (check (list REFLECTOR-I) '( ) '( ) '(l i s p) '(g p f i)))
    (check (list rotor-i rotor-ii rotor-iii rotor-iv rotor-v reflector-i ;;;;;;; following from newsgroup
 reflector-ii reflector-iii reflector-iv reflector-iii reflector-ii reflector-i rotor-v rotor-iv rotor-iii rotor-ii rotor-i reflector-iv) 
 '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
 '(16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1 0)
 '(a v p t p b l b c h m j j l h l v t d a k n a y o e l k q y y t h n n g g u w x g u w w d a g h a g f p g o i x l o f u w o i v d p n v u o y q x h b k y q f t q b j w v v g m y i b q q y d c b a f j m 
 v g k p y p y o z o k z j z i b b j x x q r f s u k s z k k m k u z f b 
 c r d j p g n t c f o f z m w y h h m q q b u h z w a n j x d o x v g c 
 t t v k s h a r m o d a l p z g m r t g p r p o m k i m s s l r v b d a 
 l m s k l o j h r d b p u t n m v s)
 '(i t x w a s x t h e x b e s t x o f x t i m e s x i t x w a s x t h e x w o r s t x o f x t i m e s x x i t x w a s x t h e x w i b b l e x w i b b l e x w i b b l e x b i b b l y x f l e e p e r x x i t x w a s x t h e x h e e b y x h e e b y x w h e e e e e e x d i n g d o n g x s n i g g l y x x h o n k x x b o o x l o o x l o o x d a x l e e x x b o i n g y x b o i n g y x b o i n g y x w h o o x b o i n g y x b o i n g y x w h o o x w h o o x x))
    (check (list rotor-i rotor-i rotor-i rotor-i rotor-i rotor-ii rotor-iii 
rotor-iii rotor-ii rotor-iii rotor-iii rotor-v rotor-v rotor-iv rotor-iv 
rotor-iv rotor-v rotor-iv rotor-v reflector-iii)
 	   '(1 3 4 5 7 8 9 11 12 13 14 15 16 17 18 19 20 21)
 	   '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)
 	   '(d q v p p j t m t x x i h c p) '(r e p u b l i c o f k o r e a) )
    (check (list rotor-ii rotor-iii rotor-i rotor-ii reflector-i) '(2 3 4) '(1 2 3 25)  ;;; random thing I made up
	   '(y f q c i z m q g o c p x x x v c t e a c r)
	   '(r s a e n c r y p t i o n h a s f a i l e d) )
    (check (list rotor-i rotor-i rotor-i rotor-i reflector-i) '(2 25 0) '(1 24 3 25) ;;; random thing I made up
	   '(g i e d k d t s l b h b n a r q z x p x f k)
	   '(e r r o r s e g m e n t a t i o n f a u l t))
    (check (list reflector-iv) '() '() '(i r l e e t) '(v a m j j g)) ;;; random thing I made up
    (check (list rotor-i rotor-iii rotor-iv reflector-i reflector-ii) '(1 1 1) '(0 0 0 0) '() '()) ;;; random thing I made up
    ) )
