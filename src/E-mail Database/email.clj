
(use 'clojure.test)
(require '[clojure.string :as str])

(defn count-different-emails
  [emails]
  ;(map #(str/split (apply str (remove (fn [this] ((set "+") this))(apply str (remove (fn [this] ((set ".") this)) %)))) #"@") emails)
  (let [mails (map #(str/split % #"@") emails)]
    (let [providers (partition-by identity(sort (map #(second (str/split % #"@")) emails)))]
      (let [repeated (set (map #(if (> (count %) 1)(first %)) providers))]
        (let [users (remove nil? (map #(if (contains? repeated (second %)) (first %)) mails))]
          (map #(apply str (remove (fn [this] ((set "+") this)) (apply str (remove (fn [this] ((set ".") this)) %)))) users))))))

(deftest test-count-different-emails
  ;(is (= 2 (count-different-emails ["two.different.providers@now.here", "two.different.providers@nowhere"]))))
  ;(is (= 2 (count-different-emails ["1.2.3@testing"
  ;                       "testing@1.2.3"])))
  (is (= 5 (count-different-emails ["alice@e.mail"
                         "eve@another.mail"
                         "bob@e.mail"
                         "joe90@e.mail"
                         "b.o.b@e.mail"
                         "bob+new@e.mail"
                         "bob@another.provider"]))))


(run-tests)


(use 'clojure.test)

(defn count-different-emails
  [emails]
  (let [
        users (map (fn [email] (take-while #(and (not= % \@) (not= % \+)) email)) emails)
        domains (map (fn [email] (rest (drop-while #(not= % \@) email))) emails)
        unique #{}
        ]
    (->> users
         (map (fn [user] (filter #(not= % \.) user)))
         (map #(apply str (concat %2 %1)) domains)
         (into #{})
         (count)
         )))

(deftest test-count-different-emails
  (is (= 0 (count-different-emails ())))
  (is (= 2
         (count-different-emails
           ["two.different.providers@now.here"
            "two.different.providers@nowhere"]
           )))
  (is (= 2
         (count-different-emails
           ["1.2.3@testing"
            "testing@1.2.3"]
           )))
  (is (= 5
         (count-different-emails
           ["alice@e.mail"
            "eve@another.mail"
            "bob@e.mail"
            "joe90@e.mail"
            "b.o.b@e.mail"
            "bob+new@e.mail"
            "bob@another.provider"]
           ))))

(run-tests)