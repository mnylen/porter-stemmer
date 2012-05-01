(ns porter-stemmer.core
  (:gen-class))

;; First some helper functions to make things easier later

(defn vowel? [c]
  (contains? #{\a \e \i \o \u} c))

(defn consonant? [c]
  (false? (vowel? c)))

(defn measure [word n]
  "Returns measure for first n characters of string for s so that

     measure = 1 when s = <C>VC<V>
     measure = 2 when s = <C>VCVC<V>
     measure = 3 when s = <C>VCVCVC<V>

   where C = seq of more than 1 consonants and
         V = seq of more than 1 vowels.

   For example:

     measure = 0 TR, EE, TREE, Y, BY
     measure = 1 TR(OUBL)E, (OATS), TR(EES), (IVY)
     measure = 2 TR(OUBL)(ES), PR(IV)(ATE), (OAT)(EN), (ORR)(ERY)"

  ;; loop through the string, character by character, looking for VC substrings
  ;;
  ;; The loop goes through various modes, which are:
  ;;
  ;; 0 - repeat while consonants are consumed from the string, transition to mode 1 on first vowel 
  ;; 1 - repeat while vowels are found, transition to mode 2 on first consonant
  ;; 2 - repeat while consonants are found, increment m and transition to mode 1 on first vowel
  ;;
  ;; If the last character of the string is consumed and we are in mode 2, increment m
  ;; and return - otherwise return m as it was
  (loop [i 0 m 0 mode 0]
    (if (< i n)
      (let [c (nth word i)] 
        (if (= mode 0)
          (recur (inc i) m (if (vowel? c) 1 0))
          (if (= mode 1)
            (recur (inc i) m (if (vowel? c) 1 2))
            (if (= mode 2)
              (recur (inc i) (if (vowel? c) (inc m) m) (if (vowel? c) 1 2))))))
      (if (= mode 2)
        (inc m)
        m))))

(defn char-class [c]
  "Returns the character class for given character : 0 for vowel, 1 for consonant"
  (if (vowel? c) :v :c))

(defn ends-with-any? [stem set]
  "Returns true if stem ends with any of the characters in the set"
  (contains? set (last stem)))


(defn ends-with-cvc? [stem]
  "Returns true if the stem ends with consonant-vowel-consonant and the second consonant
   is not W, X or Y"
  (let [len (count stem)]
    (if (< len 3)
      false
      (let [c1  (nth stem (- len 3))
            c2  (nth stem (- len 2))
            c3  (nth stem (- len 1))]
        (and
          (not (contains? #{\w \x \y} c3))
          (vowel? c2)
          (consonant? c1)
          (consonant? c3))))))

(defn ends-with-doublec? [stem]
  (let [n (- (count stem) 2)]
    (if (< n 0)
      false
      (let [x (nth stem n) y (nth stem (inc n))]
        (and (consonant? x) (= x y))))))

(defn ends-with? [s1 word]
  "Returns true if stem ends with the suffix"
  (if (= (seq s1) (take-last (count s1) word))
    true
    false))

(defn has-vowel? [word s1-length]
  (let [n (- (count word) s1-length)]
    (loop [i 0]
      (if (>= i n)
        false
        (if (vowel? (nth word i))
          true
          (recur (inc i)))))))

(defn replace-last [n s2 word]
  "Returns a string with the last n characters replaced with the given replacement string"
  (let [stem (take (- (count word) n) word)]
    (reduce conj (vec stem) s2)))

(defn penultimate [word]
  (if (< (count word) 3)
    nil
    (nth word (- (count word) 2))))


;; These functions return a condition that can be used with
;; applies-to-rule?

(defn c-m-gt [n]
  "Condition returns true if m > n"
  (fn [word s1]
    (> (measure word (- (count word) (count s1))) n)))

(defn c-m-eq [n]
  "Condition returns true if m = n"
  (fn [word s1]
    (= (measure word (- (count word) (count s1))) n)))

(defn c-v []
  "Condition returns true if word has vowel before suffix"
  (fn [word s1]
    (has-vowel? word (count s1))))

(defn c-o []
  "Condition returns true word ends with cvc"
  (fn [word s1]
    (ends-with-cvc? word)))

(defn c-not-o []
  "Condition returns true if stem ends with cvc"
  (fn [word s1]
    (not (ends-with-cvc? (take (- (count word) (count s1)) word)))))

(defn c-d []
  "Condition returns true if stem ends with double consonant"
  (fn [word s1]
    (ends-with-doublec? word))) 

(defn c-d-not [characters]
  "Condition returns true if word ends with double consonant and does not end in one of the given characters" 
  (fn [word s1]
    (and (ends-with-doublec? word) (not (ends-with-any? word characters)))))

(defn c-end [characters]
  "Condition returns true if stem ends with one of the given characters"
  (fn [word s1]
    (ends-with-any? (replace-last (count s1) "" word) characters)))

(defn c-not [suffix]
  "Condition returns true if word does not end in given suffix"
  (fn [word s1]
    (not (ends-with? suffix word))))

(defn applies-to-rule? [word s1 & fconds]
  (if-not (ends-with? s1 word)
    false
    (let [n (count fconds)]
      (loop [i 0]
        (if (>= i n)
          true
          (let [f (nth fconds i)]
            (if (f word s1)
              (recur (inc i))
              false)))))))

;;(defn applies-to-rule? [word s1 & fconds]
;;  (if-not (ends-with? s1 word)
;;    false
;;    (let [fails (filter #(false? %) (for [f fconds] (f word s1)))]
;;      (empty? fails))))


;; Step 1
;;
;; 1a)
;;
;;    SSES -> SS                         caresses  ->  caress
;;    IES  -> I                          ponies    ->  poni
;;                                       ties      ->  ti
;;    SS   -> SS                         caress    ->  caress
;;    S    ->                            cats      ->  cat
;;
;;
;; 1b)
;;
;;    (m>0) EED -> EE                    feed      ->  feed
;;                                       agreed    ->  agree
;;    (*v*) ED  ->                       plastered ->  plaster
;;                                       bled      ->  bled
;;    (*v*) ING ->                       motoring  ->  motor
;;                                       sing      ->  sing
;;
;; 1c) 
;;
;;    (*v*) Y -> I                    happy        ->  happi
;;                                    sky          ->  sky

(defn step1a [word]
  (let [pu (penultimate word)]
    (cond
      (= pu \e)
        (cond
          (applies-to-rule? word "sses") (replace-last 2 "" word)
          (applies-to-rule? word "ies") (replace-last 2 ""  word)
          :else word)
      (= pu \s)
        (cond
          (applies-to-rule? word "ss") word
          (applies-to-rule? word "s") (replace-last 1 "" word)
          :else word)
      :else word)))

(defn step1b-pass-two [word]
  (cond
    (applies-to-rule? word "at") (reduce conj word [\e])
    (applies-to-rule? word "bl") (reduce conj word [\e])
    (applies-to-rule? word "iz") (reduce conj word [\e])
    (applies-to-rule? word "" (c-d-not #{\l \s \z})) (replace-last 1 "" word)
    (applies-to-rule? word "" (c-m-eq 1) (c-o)) (reduce conj word [\e])
    :else word))

(defn step1b [word]
  (cond
    (applies-to-rule? word "eed" (c-m-gt 0)) (replace-last 1 "" word)
    
    ;; have to check that the word doesn't end with "eed" here, because
    ;; otherwise cases such as "feed" fall through
    (applies-to-rule? word "ed"  (c-not "eed") (c-v)) (step1b-pass-two (replace-last 2 "" word))
    (applies-to-rule? word "ing" (c-v)) (step1b-pass-two (replace-last 3 "" word))
    :else word))

(defn step1c [word]
  (if (applies-to-rule? word "y" (c-v))
    (replace-last 1 "i" word)
    word))

(defn step1 [word]
  (step1c (step1b (step1a word))))


;; Step 2: Shorten suffixes
;;
;;   (m>0) ATIONAL ->  ATE           relational     ->  relate
;;   (m>0) TIONAL  ->  TION          conditional    ->  condition
;;                                   rational       ->  rational
;;   (m>0) ENCI    ->  ENCE          valenci        ->  valence
;;   (m>0) ANCI    ->  ANCE          hesitanci      ->  hesitance
;;   (m>0) IZER    ->  IZE           digitizer      ->  digitize
;;   (m>0) ABLI    ->  ABLE          conformabli    ->  conformable
;;   (m>0) ALLI    ->  AL            radicalli      ->  radical
;;   (m>0) ENTLI   ->  ENT           differentli    ->  different
;;   (m>0) ELI     ->  E             vileli        - >  vile
;;   (m>0) OUSLI   ->  OUS           analogousli    ->  analogous
;;   (m>0) IZATION ->  IZE           vietnamization ->  vietnamize
;;   (m>0) ATION   ->  ATE           predication    ->  predicate
;;   (m>0) ATOR    ->  ATE           operator       ->  operate
;;   (m>0) ALISM   ->  AL            feudalism      ->  feudal
;;   (m>0) IVENESS ->  IVE           decisiveness   ->  decisive
;;   (m>0) FULNESS ->  FUL           hopefulness    ->  hopeful
;;   (m>0) OUSNESS ->  OUS           callousness    ->  callous
;;   (m>0) ALITI   ->  AL            formaliti      ->  formal
;;   (m>0) IVITI   ->  IVE           sensitiviti    ->  sensitive
;;   (m>0) BILITI  ->  BLE           sensibiliti    ->  sensible

(defn step2-case-a [word]
  (cond
    (applies-to-rule? word "ational" (c-m-gt 0)) (replace-last 5 "e" word)
    (applies-to-rule? word "tional"  (c-not "ational") (c-m-gt 0)) (replace-last 2 "" word)
    :else word))

(defn step2-case-c [word]
  (cond
    (applies-to-rule? word "enci" (c-m-gt 0)) (replace-last 1 "e" word)
    (applies-to-rule? word "anci" (c-m-gt 0)) (replace-last 1 "e" word)
    :else word))

(defn step2-case-e [word]
  (cond
    (applies-to-rule? word "izer" (c-m-gt 0)) (replace-last 1 "" word)
    :else word))

(defn step2-case-l [word]
  (cond
    (applies-to-rule? word "abli" (c-m-gt 0)) (replace-last 1 "e" word)
    (applies-to-rule? word "alli" (c-m-gt 0)) (replace-last 2 "" word)
    (applies-to-rule? word "entli" (c-m-gt 0)) (replace-last 2 "" word)
    (applies-to-rule? word "eli" (c-m-gt 0)) (replace-last 2 "" word)
    (applies-to-rule? word "ousli" (c-m-gt 0)) (replace-last 2 "" word)
    :else word))

(defn step2-case-o [word]
  (cond
    (applies-to-rule? word "ization" (c-m-gt 0)) (replace-last 5 "e" word)
    (applies-to-rule? word "ation" (c-m-gt 0) (c-not "ization")) (replace-last 3 "e" word)
    (applies-to-rule? word "ator" (c-m-gt 0)) (replace-last 2 "e" word)
    :else word))

(defn step2-case-s [word]
  (cond
    (applies-to-rule? word "alism" (c-m-gt 0)) (replace-last 3 "" word)
    (applies-to-rule? word "iveness" (c-m-gt 0)) (replace-last 4 "" word)
    (applies-to-rule? word "fulness" (c-m-gt 0)) (replace-last 4 "" word)
    (applies-to-rule? word "ousness" (c-m-gt 0)) (replace-last 4 "" word)
    :else word))

(defn step2-case-t [word]
  (cond
    (applies-to-rule? word "aliti" (c-m-gt 0)) (replace-last 3 "" word)
    (applies-to-rule? word "iviti" (c-m-gt 0)) (replace-last 3 "e" word)
    (applies-to-rule? word "biliti" (c-m-gt 0)) (replace-last 5 "le" word)
    :else word))

(defn step2 [word]
  (if (< (count word) 6)
    word
    (let [pu (penultimate word)]
      (cond
        (= pu \a) (step2-case-a word)
        (= pu \c) (step2-case-c word)
        (= pu \e) (step2-case-e word)
        (= pu \l) (step2-case-l word)
        (= pu \o) (step2-case-o word)
        (= pu \s) (step2-case-s word)
        (= pu \t) (step2-case-t word)
        :else word))))

;; 
;; Step 3: Shorten suffixes some more and remove of the suffixes
;;
;;    (m>0) ICATE ->  IC              triplicate     ->  triplic
;;    (m>0) ATIVE ->                  formative      ->  form
;;    (m>0) ALIZE ->  AL              formalize      ->  formal
;;    (m>0) ICITI ->  IC              electriciti    ->  electric
;;    (m>0) ICAL  ->  IC              electrical     ->  electric
;;    (m>0) FUL   ->                  hopeful        ->  hope
;;    (m>0) NESS  ->                  goodness       ->  good

(defn step3 [word]
  (if (< (count word) 5)
    word
    (cond
      (applies-to-rule? word "icate" (c-m-gt 0)) (replace-last 3 "" word)
      (applies-to-rule? word "ative" (c-m-gt 0)) (replace-last 5 "" word)
      (applies-to-rule? word "alize" (c-m-gt 0)) (replace-last 3 "" word)
      (applies-to-rule? word "iciti" (c-m-gt 0)) (replace-last 3 "" word)
      (applies-to-rule? word "ical"  (c-m-gt 0)) (replace-last 2 "" word)
      (applies-to-rule? word "ful"   (c-m-gt 0)) (replace-last 3 "" word)
      (applies-to-rule? word "ness"  (c-m-gt 0)) (replace-last 4 "" word)
      :else word)))


;; Step 4: Remove remaining suffixes
;;
;;    (m>1) AL    ->                  revival        ->  reviv
;;    (m>1) ANCE  ->                  allowance      ->  allow
;;    (m>1) ENCE  ->                  inference      ->  infer
;;    (m>1) ER    ->                  airliner       ->  airlin
;;    (m>1) IC    ->                  gyroscopic     ->  gyroscop
;;    (m>1) ABLE  ->                  adjustable     ->  adjust
;;    (m>1) IBLE  ->                  defensible     ->  defens
;;    (m>1) ANT   ->                  irritant       ->  irrit
;;    (m>1) EMENT ->                  replacement    ->  replac
;;    (m>1) MENT  ->                  adjustment     ->  adjust
;;    (m>1) ENT   ->                  dependent      ->  depend
;;    (m>1 and (*S or *T)) ION ->     adoption       ->  adopt
;;    (m>1) OU    ->                  homologou      ->  homolog
;;    (m>1) ISM   ->                  communism      ->  commun
;;    (m>1) ATE   ->                  activate       ->  activ
;;    (m>1) ITI   ->                  angulariti     ->  angular
;;    (m>1) OUS   ->                  homologous     ->  homolog
;;    (m>1) IVE   ->                  effective      ->  effect
;;    (m>1) IZE   ->                  bowdlerize     ->  bowdler
;;
;; As all cases require (m>1) the word must have at least 4 chars
;; before suffix and the smallest suffix is 2 chars. So, we
;; can safely skip any words smaller than 6 characters.

(defn step4-case-a [word]
  (cond
    (applies-to-rule? word "al" (c-m-gt 1)) (replace-last 2 "" word)
    :else word))

(defn step4-case-c [word]
  (cond
    (applies-to-rule? word "ance" (c-m-gt 1)) (replace-last 4 "" word)
    (applies-to-rule? word "ence" (c-m-gt 1)) (replace-last 4 "" word)
    :else word))

(defn step4-case-e [word]
  (if (applies-to-rule? word "er" (c-m-gt 1))
    (replace-last 2 "" word)
    word))

(defn step4-case-i [word]
  (cond
    (applies-to-rule? word "ic" (c-m-gt 1)) (replace-last 2 "" word)
    :else word))

(defn step4-case-l [word]
  (cond
    (applies-to-rule? word "able" (c-m-gt 1)) (replace-last 4 "" word)
    (applies-to-rule? word "ible" (c-m-gt 1)) (replace-last 4 "" word)
    :else word))

(defn step4-case-n [word]
  (cond
    (applies-to-rule? word "ant" (c-m-gt 1)) (replace-last 3 "" word)
    (applies-to-rule? word "ement" (c-m-gt 1)) (replace-last 5 "" word)
    (applies-to-rule? word "ment" (c-not "ement") (c-m-gt 1)) (replace-last 4 "" word)
    (applies-to-rule? word "ent" (c-not "ment") (c-m-gt 1)) (replace-last 3 "" word)
    :else word))

(defn step4-case-o [word]
  (cond
    (applies-to-rule? word "ion" (c-m-gt 1) (c-end #{\s \t})) (replace-last 3 "" word)
    (applies-to-rule? word "ou"  (c-m-gt 1)) (replace-last 2 "" word)
    :else word))

(defn step4-case-s [word]
  (cond
    (applies-to-rule? word "ism" (c-m-gt 1)) (replace-last 3 "" word)
    :else word))

(defn step4-case-t [word]
  (cond
    (applies-to-rule? word "ate" (c-m-gt 1)) (replace-last 3 "" word)
    (applies-to-rule? word "iti" (c-m-gt 1)) (replace-last 3 "" word)
    :else word))

(defn step4-case-u [word]
  (cond
    (applies-to-rule? word "ous" (c-m-gt 1)) (replace-last 3 "" word)
    :else word))

(defn step4-case-v [word]
  (cond
    (applies-to-rule? word "ive" (c-m-gt 1)) (replace-last 3 "" word)
    :else word))

(defn step4-case-z [word]
  (cond
    (applies-to-rule? word "ize" (c-m-gt 1)) (replace-last 3 "" word)
    :else word))

(defn step4 [word]
  (if (< (count word) 6)
    word
    (let [pu (penultimate word)]
      (cond
        (= pu \a) (step4-case-a word)
        (= pu \c) (step4-case-c word)
        (= pu \e) (step4-case-e word)
        (= pu \i) (step4-case-i word)
        (= pu \l) (step4-case-l word)
        (= pu \n) (step4-case-n word)
        (= pu \o) (step4-case-o word)
        (= pu \s) (step4-case-s word)
        (= pu \t) (step4-case-t word)
        (= pu \u) (step4-case-u word)
        (= pu \v) (step4-case-v word)
        (= pu \z) (step4-case-z word)
        :else word))))


;; Step 5: Cleaning up
;;
;; Step 5a
;;
;;    (m>1) E     ->                  probate        ->  probat
;;                                    rate           ->  rate
;;    (m=1 and not *o) E ->           cease          ->  ceas
;;
;; Step 5b
;;
;;    (m > 1 and *d and *L) -> single letter
;;                                    controll       ->  control
;;                                    roll           ->  roll

(defn step5b [word]
  (if (applies-to-rule? word "" (c-m-gt 1) (c-d) (c-end #{\l}))
    (replace-last 1 "" word)
    word))

(defn step5a [word]
  (cond
    (applies-to-rule? word "e" (c-m-gt 1)) (replace-last 1 "" word)
    (applies-to-rule? word "e" (c-m-eq 1) (c-not-o)) (replace-last 1 "" word)
    :else word))

(defn step5 [word]
  (if (< (count word) 3)
    word
    (step5b (step5a word))))


;; Now the actual stemming + main function

(defn stem [word]
  (if (> (count word) 2)
    (apply str (step5 (step4 (step3 (step2 (step1 word))))))
    word))

(defn -main [vocfile]
  (with-open [rdr (clojure.java.io/reader vocfile)]
    (println (clojure.string/join \newline (map stem (line-seq rdr))))))
