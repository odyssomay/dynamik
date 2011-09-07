(ns dynamik.core
  (:import (javax.swing JSplitPane JLayeredPane)
           java.awt.BorderLayout))

;; protocols

(defprotocol Type
  (setType [this type])
  (getType [this]))

(defprotocol Drawable
  (drawSplit [this coordinate direction] )
  (drawMrg [this direction]))

(defprotocol View
  (setCardPanel [this card-panel])
  (getCardPanel [this]))

(defprotocol Tile
  (split [this coordinate direction] )
  (mrg [this id direction] )
  (sendDrawMrg [this id direction])
  (dotoMergingSplit [this id direction f] )
  (getId [this])
  (setContentPanel [this p])
  (getContentPanel [this])
  (getSplitPane [this]))

(defprotocol Layout
  (getTileLayout [this] )
  (setTileLayout [this layout])
  (setEditable [this editable?]))

;; drawing

(defn draw-split [g coordinate direction]
  (let [b (.getClipBounds g)]
    (case direction
      :vertical (.drawLine g coordinate 0 coordinate (.height b))
      :horizontal (.drawLine g 0 coordinate (.width b) coordinate))))

(defn draw-mrg-arrow [g direction]
  (let [gc (.create g)
        b (.getClipBounds gc)
        fx (case direction
             :right 0 :left (.width b)
             :up 0 :down 0)
        sx (case direction
             :right 0 :left (.width b)
             :up (.width b) :down (.width b))
        fy (case direction
             :right 0 :left 0
             :up (.height b) :down 0)
        sy (case direction
             :right (.height b) :left (.height b)
             :up (.height b) :down 0)]
    (.setColor gc (java.awt.Color. 100 100 100))
    (.fillPolygon gc
      (into-array Integer/TYPE
                  [(int (/ (.width b) 2))
                   fx sx
                   ])
      (into-array Integer/TYPE
                  [(int (/ (.height b) 2))
                   fy sy
                   ])
      3)))

;  (let [gc (.create g)
;        b (.getClipBounds gc)]
;    (case direction
;      :up (.translate gc (double (/ (.width b) 2) (.height b)))
;      :down (.translate gc (double (/ (.width b) 2)) 0.0)
;      :right (.translate gc 0.0 (double (/ (.height b) 2)))
;      :left (.translate gc (double (.width b)) (double (/ (.height b) 2))))
;    (cond 
;      (or (= direction :up) (= direction :down))
;      (.fillOval 
;              
;              (.translate gc (double (/ (.width b) 2)) 0.0)
;                (.fillRect gc
;                  (- (int (/ (.width b) 3)))
;                  0 (int (* (int (/ (.width b) 3)) 2))
;                  (int (/ (.height b) 2)))))))

;; card-layout

(defn show-card [p s]
  (.show (.getLayout p) p s)
  (.revalidate p)
  (.repaint p))

;; type components

(defn type-combo-box [options]
  (let [cb (javax.swing.JComboBox. (into-array (:types options)))]
    cb))

(defn type-panel [f]
  (let [layout (java.awt.CardLayout.)
        types (atom #{})
        p (proxy [javax.swing.JPanel dynamik.core.Type] [(java.awt.CardLayout.)]
            (setType [type]
              (if (contains? @types type)
                (show-card this type)
                (do (.add this (f type) type)
                    (show-card this type)
                    (swap! types conj type)))))]
    p))

(defn content-menu-panel [{:keys [create-content default-type]}]
  (let [ct (memoize create-content)
        cp (type-panel #(:content (ct %)))
        mp (type-panel #(:menu (ct %)))]
    (.setType cp default-type)
    (.setType mp default-type)
    {:content cp :menu mp}))

;; listening

(def split-block-size 30)

(defn add-mouse-listener [component enclosing-panel]
  (let [start-pos (atom nil)
        action (atom nil)
        direction (atom nil)
        inside-split-block? 
        (fn [x y]
          (and (< (Math/abs (int (- (/ (.getWidth component) 2)  x))) split-block-size)
               (< (Math/abs (int (- (/ (.getHeight component) 2) y))) split-block-size)))
        l (proxy [java.awt.event.MouseAdapter] []
            (mousePressed [e]
              (let [x (.getX e)
                    y (.getY e)]
                (reset! action (if (inside-split-block? x y) :split :merge))
                (reset! start-pos [x y])))
            (mouseDragged [e]
              (let [x (.getX e)
                    y (.getY e)
                    [x-start y-start] @start-pos
                    x-diff (- x x-start)
                    y-diff (- y y-start)]
                (case @action
                  :split (let [d (if (> (Math/abs x-diff) (Math/abs y-diff))
                                   :vertical :horizontal)]
                           (reset! direction d)
                           (.drawSplit enclosing-panel (case d :vertical x :horizontal y) d))
                  :merge (let [d (if (> (Math/abs x-diff) (Math/abs y-diff))
                                   (if (> x-diff 0)
                                     :right :left)
                                   (if (> y-diff 0)
                                     :down :up))]
                           (reset! direction d)
                           (.sendDrawMrg (.getCardPanel enclosing-panel) nil d)))))
            (mouseReleased [e]
              (case @action 
                :split (.split (.getCardPanel enclosing-panel)
                         (case @direction
                           :horizontal (.getY e)
                           :vertical (.getX e)) @direction)
                :merge (.mrg (.getCardPanel enclosing-panel) nil @direction)
                nil)
              (reset! action nil)
              (reset! start-pos nil)))]
    (.addMouseMotionListener component l)
    (.addMouseListener component l)
    l))

;; panel

(defn edit-panel []
  (let [action (atom nil)
        direction (atom nil)
        coordinate (atom nil)
        reset-all (fn [] (reset! action nil) (reset! direction nil) (reset! coordinate nil))]
    (proxy [javax.swing.JPanel dynamik.core.Drawable] []
      (paintComponent [g]
        (let [gc (.create g)
              b (.getClipBounds g)]
          (.setColor gc java.awt.Color/gray)
          (.fillRect gc 0 0 (.width b) (.height b))
          (.setColor gc java.awt.Color/black)
          (.translate gc (double (/ (.width b) 2)) (double (/ (.height b) 2)))
          (.fill3DRect gc (int (- (/ split-block-size 2)))
                          (int (- (/ split-block-size 2)))
                          split-block-size split-block-size
                          true))
        (let [gc (.create g)
              b (.getClipBounds g)]
          (.setColor gc java.awt.Color/black)
          (case @action
            :split (do (draw-split gc @coordinate @direction)
                       (reset-all))
            :merge (do (draw-mrg-arrow gc @direction)
                       (reset-all))
            nil)))
      (drawSplit [c d]
        (reset! action :split)
        (reset! direction d)
        (reset! coordinate c)
        (.repaint this))
      (drawMrg [d]
        (reset! action :merge)
        (reset! direction d)
        (.repaint this)))))

(defn view [options]
  (let [view-panel (javax.swing.JPanel. (java.awt.CardLayout.))
        epanel (edit-panel)
        cardp (atom nil)
        typec (type-combo-box options)
        {:keys [content menu]} (content-menu-panel options)
        menu-panel (javax.swing.JPanel. (BorderLayout.))
        enclosing-panel (proxy [javax.swing.JPanel dynamik.core.Drawable
                                dynamik.core.View dynamik.core.Layout] [(BorderLayout.)]
                          (drawSplit [coordinate direction] (.drawSplit epanel coordinate direction))
                          (drawMrg [d] (.drawMrg epanel d))
                          (setCardPanel [c-p] (reset! cardp c-p))
                          (getCardPanel []  @cardp)
                          (getTileLayout [] (.getSelectedItem typec))
                          (setTileLayout [layout] 
                            (.setSelectedItem typec layout))
                          (setEditable [editable?]
                            (.show (.getLayout view-panel) view-panel (if editable? "edit" "content"))))]
    (add-mouse-listener epanel enclosing-panel)
    (.add view-panel content "content")
    (.add view-panel epanel "edit")
    (.addActionListener typec
      (reify java.awt.event.ActionListener
        (actionPerformed [_ _]
          (let [t (.getSelectedItem typec)]
            (.setType content t)
            (.setType menu t)))))
    (doto menu-panel
      (.add typec BorderLayout/WEST)
      (.add menu BorderLayout/CENTER))
    (doto enclosing-panel
      (.add view-panel BorderLayout/CENTER)
      (.add menu-panel BorderLayout/NORTH))
    enclosing-panel))

(defn tile [{:keys [parent contp] :as options}]
  (let [id (str (gensym "card panel"))
        editable? (atom false)
        contp-atom (atom (if contp contp (view options)))
        splitp-atom (atom nil)
        set-view (fn [p c]
                   (let [s (str (gensym))]
                     (.add p c s)
                     (.show (.getLayout p) p s)
                     (.revalidate p)
                     (.repaint p)))
        cardp (proxy [javax.swing.JPanel dynamik.core.Tile dynamik.core.Layout] [(java.awt.CardLayout.)]
                (split [coordinate direction]
                  (let [t1 (tile (merge options {:parent this :contp @contp-atom}))
                        t2 (tile (merge options {:parent this :contp nil}))
                        splitp (JSplitPane. (case direction
                                              :horizontal JSplitPane/VERTICAL_SPLIT
                                              :vertical   JSplitPane/HORIZONTAL_SPLIT)
                                 true
                                 t1 t2)]
                    (.setEditable t1 @editable?)
                    (.setEditable t2 @editable?)
                    (doto splitp
                      (.setResizeWeight 0.5)
                      (.setBorder (javax.swing.border.EmptyBorder. 0 0 0 0))
                      (.setDividerSize 5)
                      (.setDividerLocation (int coordinate)))
                    (set-view this splitp)
                    (reset! splitp-atom splitp)))
                (dotoMergingSplit [id direction f]
                  (let [sp (.getSplitPane this)]
                    (if (and id sp)
                      (condp = (.getOrientation sp)
                        JSplitPane/VERTICAL_SPLIT
                        (cond 
                          (and (= id (.getId (.getTopComponent sp)))
                               (= direction :down))
                            (f this id direction)
                          (and (= id (.getId (.getBottomComponent sp)))
                               (= direction :up))
                            (f this id direction)
                          :else (if parent 
                                  (.dotoMergingSplit parent (.getId this) direction f)
                                  (println "merging doesn't make sense!")))
                        JSplitPane/HORIZONTAL_SPLIT
                        (cond
                          (and (= id (.getId (.getLeftComponent sp)))
                               (= direction :right))
                            (f this id direction)
                          (and (= id (.getId (.getRightComponent sp)))
                               (= direction :left))
                            (f this id direction)
                          :else (if parent 
                                  (.dotoMergingSplit parent (.getId this) direction f)
                                  (println "merging doesn't make sense!"))))
                      (if parent
                        (.dotoMergingSplit parent (.getId this) direction f)))))
                (mrg [id direction]
                  (.dotoMergingSplit this id direction 
                    (fn [cardp id direction]
                      (let [sp (.getSplitPane cardp)
                            c (case direction
                                :right (.getLeftComponent sp)
                                :left (.getRightComponent sp)
                                :up (.getBottomComponent sp)
                                :down (.getTopComponent sp))]
                        (.setContentPanel cardp c)))))
                (sendDrawMrg [id direction]
                  (.dotoMergingSplit this id direction
                    (fn [cardp id direction]
                      (let [sp (.getSplitPane cardp)
                            c (case direction
                                :right (.getRightComponent sp)
                                :left (.getLeftComponent sp)
                                :up (.getTopComponent sp)
                                :down (.getBottomComponent sp))]
                        (.drawMrg (.getContentPanel c) direction)))))
                (getId [] id)
                (setContentPanel [p]
                  (reset! contp-atom p)
                  (reset! splitp-atom nil)
                  (set-view this p))
                (getContentPanel [] @contp-atom)
                (getSplitPane [] @splitp-atom)
                (getTileLayout []
                  (if-let [sp (.getSplitPane this)]
                    (condp = (.getOrientation sp)
                      JSplitPane/VERTICAL_SPLIT
                      {:direction :horizontal 
                       :location (.getDividerLocation sp)
                       :top    (.getTileLayout (.getTopComponent sp))
                       :bottom (.getTileLayout (.getBottomComponent sp))}
                      JSplitPane/HORIZONTAL_SPLIT
                      {:direction :vertical
                       :location (.getDividerLocation sp)
                       :left  (.getTileLayout (.getLeftComponent sp))
                       :right (.getTileLayout (.getRightComponent sp))})
                    (.getTileLayout (.getContentPanel this))))
                (setTileLayout [layout]
                  (if (map? layout)
                    (let [{:keys [direction]} layout]
                      (.split this 0 (:direction layout))
                      (let [sp (.getSplitPane this)]
                        (case direction
                          :horizontal
                          (let [{:keys [top bottom location]} layout]
                            (.setTileLayout (.getTopComponent sp) top)
                            (.setTileLayout (.getBottomComponent sp) bottom)
                            (.setDividerLocation sp location))
                          :vertical
                          (let [{:keys [left right location]} layout]
                            (.setTileLayout (.getLeftComponent sp) left)
                            (.setTileLayout (.getRightComponent sp) right)
                            (.setDividerLocation sp location)))))
                    (.setTileLayout (.getContentPanel this) layout)))
                (setEditable [e?] 
                  (reset! editable? e?) 
                  (if-let [sp (.getSplitPane this)]
                    (do (.setEditable (.getLeftComponent sp) e?)
                        (.setEditable (.getRightComponent sp) e?))
                    (.setEditable (.getContentPanel this) e?))))]
    (.setCardPanel @contp-atom cardp)
    (.add cardp @contp-atom "no split")
    cardp))

(defn test-component []
  (tile {:create-content (fn [type] 
                           (let [c (javax.swing.JTextArea. (str (gensym type)))
                                 m (javax.swing.JButton. "insert")]
                             (.addActionListener m
                               (reify java.awt.event.ActionListener
                                 (actionPerformed [_ _]
                                   (.append c "insert"))))
                             {:content c :menu m}))
         :default-type "type1"
         :types (into-array ["type1" "type2" "type3"])}))
