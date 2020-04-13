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


(declare providers)

(defonce app (r/atom {:view :pending
                      :filtered-providers #{1 2 3}
                      :filtered-types #{:appointment :availability}
                      :filtered-needs #{}

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

                      :new-careseekers [{:name "Hannah Arendt"
                                         :pronouns "she/her"
                                         :ok-to-text? false
                                         :email "noright2@obey.com"
                                         :phone "555-123-5476"}
                                        {:name "Zoe Leonard"
                                         :pronouns "they/them"
                                         :ok-to-text? false
                                         :email "dyke4prez2020@yahoo.com"
                                         :phone "555-123-5476"}]

                      :events [{:name "Octavia"
                                :id 1
                                :pronouns "she/her"
                                :ok-to-text? true
                                :email "octaviabutler@earthseed.com"
                                :phone "555-555-5555"
                                :type :appointment
                                :start "2020-04-13 09:30:00"
                                :end "2020-04-13 10:00:00"
                                :needs-interpreter-for "Tagalog"
                                :scheduled-interpreter {:name "Edward Tamayo"
                                                        :phone "123-555-1234"
                                                        :email "eddie@tamayo.email"}
                                :doctor-id 1}
                               {:name "Ursula"
                                :id 2
                                :pronouns "she/her"
                                :email "shevek@anarres.net"
                                :ok-to-text? false
                                :phone "555-555-5555"
                                :type :appointment
                                :start "2020-04-15 11:00:00"
                                :end "2020-04-15 11:40:00"
                                :needs-interpreter-for "Chinese Mandarin"
                                :doctor-id 2}
                               {:id 3
                                :name "Toni"
                                :pronouns "she/her"
                                :email "beloved@morrison.email"
                                :ok-to-text? true
                                :phone "555-555-5555"
                                :type :appointment
                                :start "2020-04-16 13:00:00"
                                :end "2020-04-16 14:00:00"
                                :needs-closed-captioning? true
                                :scheduled-captioner {:name "Kaitlyn Armstrong"
                                                      :phone "123-555-1234"
                                                      :email "kaitlyn@riseup.net"}
                                :doctor-id 1}
                               {:id 4
                                :name "Angela"
                                :pronouns "she/her"
                                :email "adavis@riseup.net"
                                :ok-to-text? false
                                :phone ""
                                :type :appointment
                                :start "2020-04-17 11:00:00"
                                :end "2020-04-17 11:40:00"
                                :needs-closed-captioning? true
                                :doctor-id 2}
                               {:id 11
                                :name "Langston Hughes"
                                :pronouns "he/him"
                                :email "lhughes1902@aol.com"
                                :ok-to-text? false
                                :phone ""
                                :type :appointment
                                :start "2020-04-17 12:00:00"
                                :end "2020-04-17 12:40:00"
                                :doctor-id 3}

                               {:id 5
                                :start "2020-04-13 12:00:00"
                                :end "2020-04-13 16:30:00"
                                :type :availability
                                :doctor-id 2}
                               {:id 6
                                :start "2020-04-14 10:00:00"
                                :end "2020-04-14 16:30:00"
                                :type :availability
                                :doctor-id 2}
                               {:id 7
                                :start "2020-04-15 10:00:00"
                                :end "2020-04-15 16:30:00"
                                :type :availability
                                :doctor-id 2}
                               {:id 8
                                :start "2020-04-15 10:00:00"
                                :end "2020-04-15 16:30:00"
                                :type :availability
                                :doctor-id 1}
                               {:id 9
                                :start "2020-04-16 10:00:00"
                                :end "2020-04-16 16:30:00"
                                :type :availability
                                :doctor-id 1}
                               {:id 12
                                :start "2020-04-17 10:30:00"
                                :end "2020-04-17 17:30:00"
                                :type :availability
                                :doctor-id 3}
                               {:id 10
                                :start "2020-04-16 10:00:00"
                                :end "2020-04-16 14:00:00"
                                :type :availability
                                :doctor-id 2}]}))


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


(def new-careseekers (r/cursor app [:new-careseekers]))
(def events (r/cursor app [:events]))
(def providers (r/cursor app [:providers]))
(def filtered-providers (r/cursor app [:filtered-providers]))
(def filtered-types (r/cursor app [:filtered-types]))
(def filtered-needs (r/cursor app [:filtered-needs]))
(def focused-appointment (r/cursor app [:focused-appointment]))
(def view (r/cursor app [:view]))

(defn for-provider? [doc-id e]
  (= doc-id (:doctor-id e)))

(defn event-type? [t e]
  (= t (:type e)))

(defn interpreter-scheduled? [appt]
  (boolean (:scheduled-interpreter appt)))
(defn captioner-scheduled? [appt]
  (boolean (:scheduled-captioner appt)))

(defmulti needs? (fn [need _] need))

(defmethod needs? :default [_ _] false)

(defmethod needs? :scheduled-interpreter [_ event]
  (boolean (and (:needs-interpreter-for event)
                (nil? (:scheduled-interpreter event)))))

(defmethod needs? :closed-captioning [_ event]
  (boolean (and (:needs-closed-captioning? event)
                (nil? (:scheduled-captioner event)))))

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

(defn filter-by-needs [needs events]
  (filter (fn [event]
              (or (not (appointment? event))
                  (empty? needs)
                  (some #(needs? % event) needs)))
          events))

(defn visible-events []
  (->> @events
       (filter-by-provider @filtered-providers)
       (filter-by-type @filtered-types)
       (filter-by-needs @filtered-needs)
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

(defn toggle-need-filter! [need]
  (swap! app update :filtered-needs toggle-member need))

;; -------------------------
;; Views

(defn appointment-interpreter-details [appt]
  (cond
    (needs? :scheduled-interpreter appt)
    [:a.text-btn.schedule-interpreter {:href "#" :title "This doesn't do anything (yet)"} "Schedule interpreter"]

    (interpreter-scheduled? appt)
    [:span.appt-info.appt-info--interpreter
     [:b "Scheduled: "]
     [:span (:name (:scheduled-interpreter appt)) " "]
     (when-let [phone (:phone (:scheduled-interpreter appt))]
       [:a {:href (str "tel:" phone)} phone])
     " "
     (when-let [email (:email (:scheduled-interpreter appt))]
       [:a {:href (str "mailto:" email)} email])]
    
    :else [:<>]))

(defn appointment-captioner-details [appt]
  (cond
    (needs? :scheduled-captioner appt)
    [:a.text-btn.schedule-interpreter {:href "#" :title "This doesn't do anything (yet)"} "Schedule interpreter"]

    (captioner-scheduled? appt)
    [:span.appt-info.appt-info--captioner
     [:b "Scheduled: "]
     [:span (:name (:scheduled-captioner appt)) " "]
     (when-let [phone (:phone (:scheduled-captioner appt))]
       [:a {:href (str "tel:" phone)} phone])
     " "
     (when-let [email (:email (:scheduled-captioner appt))]
       [:a {:href (str "mailto:" email)} email])]
    
    :else [:<>]))

(defn appointment-details [appt]
  (let [{:keys [start name pronouns phone email ok-to-text?]} appt]
    [:div.appointment-details
     [:h2 name " (" pronouns ")"]
     [:p (.format (moment start) "h:mm A ddd, MMM D")]
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
      [:dd (or (:needs-interpreter-for appt) "None") [appointment-interpreter-details appt]]
      [:dt "Closed Captions"]
      [:dd (if (needs? :closed-captioning appt) "Yes" "No") [appointment-captioner-details appt]]
      [:dt "VRS"]
      [:dd "No"]
      [:dt "Other"]
      [:dd "No"]]
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
                  :label "Show appointments"}]))]
   
   [:div.filter-group
    [:h4 "Filter by access needs"]
    (doall (map (fn [{:keys [need label]}]
                  (let [id (name need)]
                    ^{:key id}
                    [:div.needs-filter
                     [:div.access-needs-filter
                      [:input {:id id
                               :type :checkbox
                               :checked (contains? @filtered-needs need)
                               :on-change #(toggle-need-filter! need)}]
                      [:label {:for id}
                       label]]]))
                [{:need :scheduled-interpreter
                  :label "Needs interp. scheduled"}
                 {:need :closed-captioning
                  :label "Needs captioner scheduled"}
                 {:need :vrs
                  :label "Needs VRS"}
                 {:need :other
                  :label "Other needs"}]))]])

(defn calendar-view []
  [:div.calendar-view
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
                      :plugins [dayGridPlugin timeGridPlugin]}]]])

(defn pending-view []
  [:div.pending-view
   [:h2 "New Care Seekers"]
   [:ul
    (doall (map (fn [{:keys [name pronouns ok-to-text? email phone]}]
                  ^{:key name}
                  [:li.careseeker--pending
                   [:h5 (str name " (" pronouns ")")]
                   [:a.text-btn {:href "#" :title "This doesn't do anything (yet)"} "Schedule an appointment"]
                   [:a {:href (str "tel:" phone)} phone]
                   [:br]
                   [:a {:href (str "mailto:" email)} email]])
                @new-careseekers))]])

(defn scheduler []
  [:div.scheduler-app
   [:header
    [:h1 "Care Schedule"]
    [:nav
     [:ul.nav
      [:li [:a {:on-click #(swap! app assoc :view :pending)} "Pending"]]
      [:li [:a {:on-click #(swap! app assoc :view :calendar)} "Calendar"]]]]]
   (when @focused-appointment
     [modal
      [appointment-details @focused-appointment]])
   [:main
    (case @view
      :pending  [pending-view]
      :calendar [calendar-view])]])

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

  (needs? :closed-captioning (get @events 2))
  (needs? :closed-captioning (get @events 3))

  (toggle-need-filter! :scheduled-interpreter)
  (toggle-need-filter! :closed-captioning)
  @filtered-needs)