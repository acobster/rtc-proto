(ns rtc.core
  (:require
   [reagent.core :as r]
   [reagent.dom :as d]
   ["@fullcalendar/react" :default FullCalendar]
   ["@fullcalendar/daygrid" :default dayGridPlugin]
   ["@fullcalendar/timegrid" :default timeGridPlugin]))


(defonce app (r/atom {:name "Coby"
                      :filtered-providers #{1 2}
                      :providers [{:id 1
                                   :name "Jay Patel"
                                   :region "WA"
                                   :availability-color "#80008096"
                                   :color "purple"}
                                  {:id 2
                                   :name "Krishna Patel"
                                   :region "CA"
                                   :availability-color "#008000a8"
                                   :color "green"}]
                      :events [{:title "Octavia"
                                :type :appointment
                                :start "2020-04-06 09:30:00"
                                :end "2020-04-06 10:00:00"
                                :doctor-id 1}
                               {:title "Ursula"
                                :type :appointment
                                :start "2020-04-08 11:00:00"
                                :end "2020-04-08 11:40:00"
                                :doctor-id 2}
                               {:title "Toni"
                                :type :appointment
                                :start "2020-04-09 13:00:00"
                                :end "2020-04-09 14:00:00"
                                :doctor-id 1}
                               {:title "Angela"
                                :type :appointment
                                :start "2020-04-10 11:00:00"
                                :end "2020-04-10 11:40:00"
                                :doctor-id 2}

                               {:start "2020-04-06 12:00:00"
                                :end "2020-04-06 16:30:00"
                                :type :availability
                                :doctor-id 2}
                               {:start "2020-04-07 10:00:00"
                                :end "2020-04-07 16:30:00"
                                :type :availability
                                :doctor-id 2}
                               {:start "2020-04-08 10:00:00"
                                :end "2020-04-08 16:30:00"
                                :type :availability
                                :doctor-id 2}
                               {:start "2020-04-08 10:00:00"
                                :end "2020-04-08 16:30:00"
                                :type :availability
                                :doctor-id 1}
                               {:start "2020-04-09 10:00:00"
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

(defn ->fc-event [e]
  (let [base-color (:color (event->provider e))
        avail-color (:availability-color (event->provider e))
        color (if (availability? e) avail-color base-color)]
    (conj e {:backgroundColor color
             :borderColor color})))


(def events (r/cursor app [:events]))
(def providers (r/cursor app [:providers]))
(def filtered-providers (r/cursor app [:filtered-providers]))

(defn visible-events []
  (reduce (fn [visible doc-id]
            (concat visible (filter #(= doc-id (:doctor-id %))
                                    (map ->fc-event @events))))
          []
          @filtered-providers))


(defn toggle-provider! [id]
  (let [op (if (contains? @filtered-providers id) disj conj)]
    (swap! app update :filtered-providers op id)))

;; -------------------------
;; Views

(defn scheduler []
  [:div [:h2 "Hello, " (:name @app) "!"]
   [:main
    [:div.calendar
     [:> FullCalendar {:default-view "timeGridWeek"
                       :plugins [dayGridPlugin timeGridPlugin]
                       :events @(r/track visible-events)}]]
    [:div.filters
     (doall (map (fn [{:keys [name id]}]
                   ^{:key id}
                   [:div.provider-filter
                    [:input {:id (str "provider-chk-" id)
                             :type :checkbox
                             :checked (contains? @filtered-providers id)
                             :on-change #(toggle-provider! id)}]
                    [:label {:for (str "provider-chk-" id)}
                     name]])
                 @providers))]]])

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

  (swap! app assoc-in [:events 0 :end] "2020-04-06 10:00:00")
  (swap! app assoc-in [:events 0 :end] "2020-04-06 10:30:00")

  (swap! app assoc :name "Coby")
  (swap! app assoc :name "Monique"))