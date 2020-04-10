(ns rtc.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]
   ["moment" :as moment]
   ["@fullcalendar/react" :default FullCalendar]
   ["@fullcalendar/daygrid" :default dayGridPlugin]
   ["@fullcalendar/timegrid" :default timeGridPlugin]
   [clojure.string :as string :refer [split join]]
   [clojure.walk :as walk :refer [keywordize-keys]]))


(defonce app (r/atom {:filtered-providers #{1 2 3}
                      :filtered-types #{:appointment :availability}

                      :providers [{:id 1
                                   :name "Jay Patel"
                                   :region "WA"
                                   :availability-color "#80008096"
                                   :color "purple"}
                                  {:id 3
                                   :name "Pat Vigil"
                                   :region "WA"
                                   :availability-color "#00008096"
                                   :color "blue"}
                                  {:id 2
                                   :name "Krishna Patel"
                                   :region "CA"
                                   :availability-color "#00800096"
                                   :color "green"}]

                      :focused-appointment nil

                      :events [{:name "Octavia"
                                :id 1
                                :pronouns "she/her"
                                :ok-to-text? true
                                :email "octaviabutler@earthseed.com"
                                :phone "555-555-5555"
                                :type :appointment
                                :start "2020-04-06 09:30:00"
                                :end "2020-04-06 10:00:00"
                                :doctor-id 1}
                               {:name "Ursula"
                                :id 2
                                :pronouns "she/her"
                                :email "shevek@anarres.net"
                                :ok-to-text? false
                                :phone "555-555-5555"
                                :type :appointment
                                :start "2020-04-08 11:00:00"
                                :end "2020-04-08 11:40:00"
                                :doctor-id 2}
                               {:id 3
                                :name "Toni"
                                :pronouns "she/her"
                                :email "beloved@morrison.email"
                                :ok-to-text? true
                                :phone "555-555-5555"
                                :type :appointment
                                :start "2020-04-09 13:00:00"
                                :end "2020-04-09 14:00:00"
                                :doctor-id 1}
                               {:id 4
                                :name "Angela"
                                :pronouns "she/her"
                                :email "adavis@riseup.net"
                                :ok-to-text? false
                                :phone ""
                                :type :appointment
                                :start "2020-04-10 11:00:00"
                                :end "2020-04-10 11:40:00"
                                :doctor-id 2}
                               {:id 11
                                :name "Langston Hughes"
                                :pronouns "he/him"
                                :email "lhughes1902@aol.com"
                                :ok-to-text? false
                                :phone ""
                                :type :appointment
                                :start "2020-04-10 12:00:00"
                                :end "2020-04-10 12:40:00"
                                :doctor-id 3}

                               {:id 5
                                :start "2020-04-06 12:00:00"
                                :end "2020-04-06 16:30:00"
                                :type :availability
                                :doctor-id 2}
                               {:id 6
                                :start "2020-04-07 10:00:00"
                                :end "2020-04-07 16:30:00"
                                :type :availability
                                :doctor-id 2}
                               {:id 7
                                :start "2020-04-08 10:00:00"
                                :end "2020-04-08 16:30:00"
                                :type :availability
                                :doctor-id 2}
                               {:id 8
                                :start "2020-04-08 10:00:00"
                                :end "2020-04-08 16:30:00"
                                :type :availability
                                :doctor-id 1}
                               {:id 9
                                :start "2020-04-09 10:00:00"
                                :end "2020-04-09 16:30:00"
                                :type :availability
                                :doctor-id 1}
                               {:id 12
                                :start "2020-04-10 10:30:00"
                                :end "2020-04-10 17:30:00"
                                :type :availability
                                :doctor-id 3}
                               {:id 10
                                :start "2020-04-09 10:00:00"
                                :end "2020-04-09 14:00:00"
                                :type :availability
                                :doctor-id 2}]}))

(declare providers)


(defn id->provider* [id]
  (first (filter #(= id (:id %)) @providers)))
(def id->provider (memoize id->provider*))

(defn event->provider [e]
  (id->provider (:doctor-id e)))

(defn availability? [e]
  (= :availability (:type e)))

(defn appointment? [e]
  (= :appointment (:type e)))

(defn abbrev [s]
  (join "" (map first (split s #" "))))

(defn ->fc-event [e]
  (let [base-color (:color (event->provider e))
        avail-color (:availability-color (event->provider e))
        color (if (availability? e) avail-color base-color)
        class (name (:type e))
        nm (if (appointment? e) (:name e) (abbrev (:name (event->provider e))))]
    (conj e {:title nm
             :backgroundColor color
             :borderColor color
             :className class})))


(def events (r/cursor app [:events]))
(def providers (r/cursor app [:providers]))
(def filtered-providers (r/cursor app [:filtered-providers]))
(def filtered-types (r/cursor app [:filtered-types]))
(def focused-appointment (r/cursor app [:focused-appointment]))

(defn for-provider? [doc-id e]
  (= doc-id (:doctor-id e)))

(defn event-type? [t e]
  (= t (:type e)))

(defn filter-by-provider [providers events]
  (reduce (fn [visible doc-id]
            (concat visible (filter (partial for-provider? doc-id) events)))
          []
          providers))

(defn filter-by-type [types events]
  (reduce (fn [visible type]
            (concat visible (filter (partial event-type? type) events)))
          []
          types))

(defn visible-events []
  (->> @events
       (filter-by-provider @filtered-providers)
       (filter-by-type @filtered-types)
       (map ->fc-event)))


(defn toggle-member [s member]
  (let [op (if (contains? s member) disj conj)]
    (set (op s member))))

(defn toggle-provider! [id]
  (let [op (if (contains? @filtered-providers id) disj conj)]
    (swap! app update :filtered-providers op id)))

(defn focus-appointment! [e]
  (swap! app assoc :focused-appointment e))

(defn toggle-type-filter! [type]
  (swap! app update :filtered-types toggle-member type))

;; -------------------------
;; Views

(defn appointment-details [appt]
  (let [{:keys [start name pronouns phone email ok-to-text?]} appt]
    [:div.appointment-details
     [:h2 name " (" pronouns ")"]
     [:h4 (.format (moment start) "h:mma ddd, MMM D")]
     [:h3 "Contact"]
     [:dl
      [:dt "Email"]
      [:dd [:a {:href (str "mailto:" email)} email]]
      [:dt "Phone"]
      [:dd [:a {:href (str "tel:" phone)} phone]]
      [:dt "OK to text?"]
      [:dd (if ok-to-text? "yes" "no")]]
     [:h3 "Access Needs"]
     [:dl
      [:dt "Interpretation"]
      [:dd "Tagalog" [:a.text-btn.schedule-interpreter {:href "#"} "Schedule interpreter"]]
      [:dt "Other"]
      [:dd "CC, VRS, Other: " [:i "Lorem ipsum dolor sit amet"]]]
     [:h3 "Medical Needs"]
     [:dl
      [:dt "Description"]
      [:dd [:i "Praesentium eligendi laborum harum optio cupidatat soluta incididunt."]]
      [:dt "Misc."]
      [:dd [:i "Labore deserunt nulla eligendi ex."]]]]))

(defn modal [children]
  [:div.modal-bg
   [:aside.modal
    [:div.modal__contents
     [:<> children]
     [:span.modal__close {:on-click #(focus-appointment! nil)} "×"]]]])

(defn filter-controls []
  [:div.filters
   [:div.filter-group
    [:h4 "Filter by provider"]
    (doall (map (fn [{:keys [name id availability-color]}]
                  ^{:key id}
                  [:div.provider-filter
                   [:input {:id (str "provider-chk-" id)
                            :type :checkbox
                            :checked (contains? @filtered-providers id)
                            :on-change #(toggle-provider! id)}]
                   [:label.provider-filter__label {:for (str "provider-chk-" id)
                                                   :style {:border-color availability-color}}
                    name]])
                @providers))]
   [:div.filter-group
    (doall (map (fn [{:keys [event-type label]}]
                  (let [id (name event-type)]
                    ^{:key event-type}
                    [:div.type-filter
                     [:input {:id id
                              :type :checkbox
                              :checked (contains? @filtered-types event-type)
                              :on-change #(toggle-type-filter! event-type)}]
                     [:label.type-filter__label {:for id}
                      label]]))
                [{:event-type :availability
                  :label "Show availability"}
                 {:event-type :appointment
                  :label "Show appointments"}]))]])

(defn scheduler []
  [:div.scheduler-app
   [:h1 "Care Schedule"]
   (when @focused-appointment
     [modal
      [appointment-details @focused-appointment]])
   [:main
    [:div.controls
     [filter-controls]]
    [:div.calendar
     ;; TODO look into timeline view with Resources
     ;; https://fullcalendar.io/docs/timeline-view
     [:> FullCalendar {:default-view "timeGridWeek"
                       :events @(r/track visible-events)
                       :eventClick (fn [info]
                                     (let [e (.. info -event -_def -extendedProps)
                                           start (.. info -event -start)
                                           end (.. info -event -end)]
                                       (when (= "appointment" (.-type e))
                                         (focus-appointment! (conj (keywordize-keys (js->clj e))
                                                                   {:start start
                                                                    :end end})))))
                       :plugins [dayGridPlugin timeGridPlugin]}]]]])

;; -------------------------
;; Initialize app

(defn ^:dev/after-load mount-root []
  (d/render [scheduler] (.getElementById js/document "app")))

(defn init! []
  (mount-root))


(comment

  (id->provider 2)

  (toggle-provider! 1)
  (toggle-provider! 2)

  (toggle-type-filter! :appointment)
  (toggle-type-filter! :availability)
  @filtered-types

  (swap! app assoc-in [:events 1 :email] "shevek@anarres.net"))