(ns inductive.cards
  (:require [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [inductive.core :as core]
            [clojure.string :as str])
  (:require-macros
   [devcards.core
    :as dc
    :refer [defcard defcard-doc defcard-rg deftest]]))

(enable-console-print!)

(defonce state (atom {
:facts [
  "user admin Passsword1 exists",
  "user admin has-privilege-to WRITE ALL USERS",
  "user admin has-privilege-to WRITE ALL TEAMS"
        ]
:behaviors [
{
:behavior "User login"
:given ["user $LOGIN $PASSWORD exists"]
:when "login $LOGIN $PASSWORD"
:then "user $LOGIN loggedin"
}

{:behavior "Infer user privilege"
:given ["team $TEAM exists",
 "team $TEAM contains $LOGIN",
 "team $TEAM has-privilege-to $ACTION $SCOPE $OBJECTS"]
:then "user $LOGIN has-privilege-to $ACTION $SCOPE $OBJECTS"}

{:behavior "Team create"
:given ["user $LOGIN loggedin",
       "user $LOGIN with-privilege-to WRITE ALL TEAMS"]
:when "create team $TEAM"
:then "team $TEAM exists"}

{:behavior "Team privileges"
:given ["user $LOGIN loggedin",
       "user $LOGIN with-privilege-to WRITE ALL TEAMS",
       "$ACTION in NONE READ WRITE",
       "$SCOPE in ALL NONE",
       "$OBJECTS in TEAMS USERS"]
:when "add-privilege $ACTION $SCOPE $OBJECT to-team $TEAM"
:then "team $TEAM has-privilege-to $ACTION $SCOPE $OBJECTS"}

{:behavior "Team user"
:given ["user $LOGIN loggedin",
        "user $LOGIN with-privilege-to WRITE ALL TEAMS"]
:when "add-user $LOGIN2 to-team $TEAM"
:then "team $TEAM contains $LOGIN"}]}))

(defcard-rg facts-card
  (fn [data-atom owner]
  [:ol (for [f (:facts @data-atom)] [:li f])])
  state)

(defn render-behavior [b]
  [:dl [:dt "Behavior"] [:dd (:behavior b)]
       [:dt "Given"] [:dd
         [:ol (for [g (:given b)] [:li g])]]
       [:dt "Then"] [:dd (:then b)]])


(defn render-behavior-abbr [b]
 [:dl [:dt (:behavior b)]
      [:dd (:then b)]])

(defn get-variables [condition]
  (str/split condition #" "))

(defcard-rg preconditions
  (fn [data-atom owner]
    (let [conditions (->> @data-atom :behaviors (map :given) flatten (into #{}))]
      [:ol
        (for [condition conditions] [:li condition #_(get-variables condition)])]
    ))
    state)

(defcard-rg behaviors-abbr
   (fn [data-atom owner]
     [:div
       (for [b (:behaviors @data-atom)]
         (render-behavior-abbr b))])
   state)

(defcard-rg behaviors-card
    (fn [data-atom owner]
      [:div
        (for [b (:behaviors @data-atom)]
          (render-behavior b))])
    state)

#_(defcard-rg second-card
    (fn [data-atom owner]
    [:div
      [:button {:onClick (fn [] (swap! data-atom update-in [:count] dec))}
      "dec"]])
    first-example-state)

(defcard-rg home-page-card
  [core/home-page])

(reagent/render [:div] (.getElementById js/document "app"))

;; remember to run 'lein figwheel devcards' and then browse to
;; http://localhost:3449/cards
