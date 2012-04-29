(ns porter-stemmer.core
  (:gen-class))

(defn vowel? [c]
  (contains? #{\a \e \i \o \u} c))

(defn consonant? [c]
  (false? (vowel? c)))


(defn measure [s]
  "Returns measure m for s so that
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
  (loop [c (first s) cs (rest s) m 0 mode 0]
    (if c
      (if (= mode 0)
        (recur (first cs) (rest cs) m (if (vowel? c) 1 0))
        (if (= mode 1)
          (recur (first cs) (rest cs) m (if (vowel? c) 1 2))
          (if (= mode 2)
            (recur (first cs) (rest cs) (if (vowel? c) (inc m) m) (if (vowel? c) 1 2)))))
      (if (= mode 2)
        (inc m)
        m))))  

(defn measure-before-suffix [word suffix-length]
  "Returns measure for stem before the suffix"
  (measure (take (- (count word) suffix-length) word)))

(defn char-class [c]
  "Returns the character class for given character : 0 for vowel, 1 for consonant"
  (if (vowel? c) 0 1))

(defn ends-with-any? [stem set]
  "Returns true if stem ends with any of the characters in the set"
  (let [last-char (last stem)]
    (if (contains? set last-char)
      true
      false)))

(defn ends-with-cvc? [stem]
  "Returns true if the stem ends with consonant-vowel-consonant and the second consonant
   is not W, X or Y"
  (if (not (ends-with-any? stem #{\w \x \y}))
    (let [char-classes (take 3 (map char-class (reverse stem)))]
      (if (= char-classes [1 0 1])
        true
        false))))

(defn ends-with-doublec? [stem]
  "Returns true if the stem ends with double consonant"
  (let [last-two (take 2 (reverse stem))]
   (if (= (nth last-two 0) (nth last-two 1))
      true
      false)))

(defn ends-with? [stem suffix]
  "Returns true if stem ends with the suffix"
  (if (= (seq suffix) (take-last (count suffix) stem))
    true
    false))

(defn contains-vowel? [word suffix-length]
  "Returns true if word contains a vowel before the last suffix-length characters that
   forms the suffix.

     (contains-vowel? [\\p \\l \\a \\s \\t \\e \\r \\e \\d] 2) => true
     (contains-vowel? [\\b \\l \\e \\d] 2) => false"
  (let [stem (take (- (count word) suffix-length) word)]
    (let [first-vowel (first (filter #(vowel? %) stem))]
      (if-not (nil? first-vowel)
        true
        false))))

(defn replace-suffix [word suffix-length replacement]
  "Returns a stem with the last suffix-length characters replaced with the replacement"
  (let [stem (take (- (count word) suffix-length) word)]
    (concat stem replacement)))

(defn porter-step1a [word]
  (cond
   (ends-with? word "sses") (replace-suffix word 4 "ss")
   (ends-with? word "ies")  (replace-suffix word 3 "i")
   (ends-with? word "ss")   (replace-suffix word 2 "ss")
   (ends-with? word "s")    (replace-suffix word 1 "s")
   :else                    word))

(defn porter-step1b-pass-two [word]
  (cond
   (and (ends-with-doublec? word) (not (ends-with-any? word #{\l \s \z}))) (replace-suffix word 1 "")
   (and (= (measure word) 1) (ends-with-cvc? word))                        (concat word "e")
   :else                                                                   word))

(defn porter-step1b [word]
  (cond
   (and (ends-with? word "eed") (> (measure-before-suffix word 3) 0)) (replace-suffix word 3 "ee")
   (and (ends-with? word "ed") (contains-vowel? word 2))              (porter-step1b-pass-two (replace-suffix word 2 ""))
   (and (ends-with? word "ing") (contains-vowel? word 3))             (porter-step1b-pass-two (replace-suffix word 3 ""))
   :else                                                              word))

(defn porter-step1c [word]
  (if (and (contains-vowel? word 1) (ends-with? word "y"))
    (replace-suffix word 1 "i")
    word))

(defn ends-with-and-measure-over-zero? [word suffix]
  (and (ends-with? word suffix) (> (measure-before-suffix word (count suffix)) 0)))

(defn ends-with-and-measure-over-one? [word suffix]
  (and (ends-with? word suffix) (> (measure-before-suffix word (count suffix)) 1)))

(defn penultimate [word]
  (if (< (count word) 3)
    nil
    (nth word (- (count word) 2))))

(defn porter-step2 [word]
  (cond
   (= (penultimate word) \a) (cond
                              (ends-with-and-measure-over-zero? word "ational") (replace-suffix word 7 "ate")
                              (ends-with-and-measure-over-zero? word "tional")  (replace-suffix word 6 "tion")
                              :else word)

   (= (penultimate word) \c) (cond
                              (ends-with-and-measure-over-zero? word "enci")    (replace-suffix word 4 "ence")
                              (ends-with-and-measure-over-zero? word "anci")    (replace-suffix word 4 "ance")
                              :else word)

   (= (penultimate word) \e) (cond
                              (ends-with-and-measure-over-zero? word "izer")    (replace-suffix word 4 "ize")
                              :else word)

   (= (penultimate word) \l) (cond
                              (ends-with-and-measure-over-zero? word "abli")    (replace-suffix word 4 "able")
                              (ends-with-and-measure-over-zero? word "alli")    (replace-suffix word 4 "al")
                              (ends-with-and-measure-over-zero? word "entli")   (replace-suffix word 5 "ent")
                              (ends-with-and-measure-over-zero? word "eli")     (replace-suffix word 3 "eli")
                              (ends-with-and-measure-over-zero? word "ousli")   (replace-suffix word 5 "ous")
                              :else word)

   (= (penultimate word) \o) (cond
                              (ends-with-and-measure-over-zero? word "ization") (replace-suffix word 7 "ize")
                              (ends-with-and-measure-over-zero? word "ation")   (replace-suffix word 5 "ate")
                              (ends-with-and-measure-over-zero? word "ator")    (replace-suffix word 4 "ate")
                              :else word)

   (= (penultimate word) \s) (cond
                              (ends-with-and-measure-over-zero? word "alism")   (replace-suffix word 5 "al")
                              (ends-with-and-measure-over-zero? word "iveness") (replace-suffix word 7 "ive")
                              (ends-with-and-measure-over-zero? word "fulness") (replace-suffix word 7 "ful")
                              (ends-with-and-measure-over-zero? word "ousness") (replace-suffix word 7 "ous")
                              :else word)

   (= (penultimate word) \t) (cond
                              (ends-with-and-measure-over-zero? word "aliti")   (replace-suffix word 5 "al")
                              (ends-with-and-measure-over-zero? word "iviti")   (replace-suffix word 5 "ive")
                              (ends-with-and-measure-over-zero? word "biliti")  (replace-suffix word 6 "ble")
                              :else word)

   :else word))

(defn porter-step3 [word]
  (cond
   (ends-with-and-measure-over-zero? word "icate") (replace-suffix word 5 "ic")
   (ends-with-and-measure-over-zero? word "ative") (replace-suffix word 5 "")
   (ends-with-and-measure-over-zero? word "alize") (replace-suffix word 5 "al")
   (ends-with-and-measure-over-zero? word "iciti") (replace-suffix word 5 "ic")
   (ends-with-and-measure-over-zero? word "ical") (replace-suffix word 4 "ic")
   (ends-with-and-measure-over-zero? word "ful") (replace-suffix word 3 "")
   (ends-with-and-measure-over-zero? word "ness") (replace-suffix word 4 "")
   :else word))

(defn porter-step4 [word]
  (let [c (penultimate word)]
    (cond
     (= c \a) (cond
               (ends-with-and-measure-over-one? word "al") (replace-suffix word 2 "")
               :else word)

     (= c \c) (cond
               (ends-with-and-measure-over-one? word "ance") (replace-suffix word 4 "")
               (ends-with-and-measure-over-one? word "ence") (replace-suffix word 4 "")
               :else word)

     (= c \e) (cond
               (ends-with-and-measure-over-one? word "er") (replace-suffix word 2 "")
               :else word)

     (= c \i) (cond
               (ends-with-and-measure-over-one? word "ic") (replace-suffix word 2 "")
               :else word)

     (= c \l) (cond
               (ends-with-and-measure-over-one? word "able") (replace-suffix word 4 "")
               (ends-with-and-measure-over-one? word "ible") (replace-suffix word 4 "")
               :else word)

     (= c \n) (cond
               (ends-with-and-measure-over-one? word "ant")   (replace-suffix word 3 "")
               (ends-with-and-measure-over-one? word "ement") (replace-suffix word 5 "")
               (ends-with-and-measure-over-one? word "ment")  (replace-suffix word 4 "")
               (ends-with-and-measure-over-one? word "ent")   (replace-suffix word 3 "")
               :else word)

     (= c \o) (cond
               (and (ends-with-and-measure-over-one? word "ion") (not (ends-with-any? word #{\s \t}))) (replace-suffix word 3 "")
               (ends-with-and-measure-over-one? word "ou") (replace-suffix word 2 "")
               :else word)

     (ends-with-and-measure-over-one? word "ism") (replace-suffix word 3 "")
     (ends-with-and-measure-over-one? word "ate") (replace-suffix word 3 "")
     (ends-with-and-measure-over-one? word "iti") (replace-suffix word 3 "")
     (ends-with-and-measure-over-one? word "ous") (replace-suffix word 3 "")
     (ends-with-and-measure-over-one? word "ive") (replace-suffix word 3 "")
     (ends-with-and-measure-over-one? word "ize") (replace-suffix word 3 "")
     :else word)))

(defn porter-step5a [word]
  (cond
   (ends-with-and-measure-over-one? word "e") (replace-suffix word 1 "")
   (and (ends-with? word "e") (not (ends-with-cvc? word)) (= (measure-before-suffix word 1))) (replace-suffix word 1 "")
   :else word))

(defn porter-step5b [word]
  (if (and (> (measure word) 1) (ends-with? word "l") (ends-with-doublec? word))
    (replace-suffix word 1 "")
    word))

(defn porter-step5 [word]
  (porter-step5a (porter-step5b word)))


(defn stem [word]
  (let [step1-result (porter-step1c (porter-step1b (porter-step1a word)))]
    (let [step2-result (porter-step2 step1-result)]
      (let [step3-result (porter-step3 step2-result)]
        (let [step4-result (porter-step4 step3-result)]
          (let [step5-result (porter-step5 step4-result)]
            (apply str step5-result)))))))

(defn -main
  [vocfile]
  (let [voc (clojure.string/split-lines (slurp vocfile))]
    (let [stemmed (map #(stem %) voc)]
      (println (clojure.string/join \newline stemmed)))))
