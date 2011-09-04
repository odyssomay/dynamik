(ns dynamik.core
  (:import (javax.swing JSplitPane JLayeredPane)
           java.awt.BorderLayout))

;; protocols

(defprotocol Type
  (setType [this type])
  (getType [this]))

(defprotocol View
  (drawSplit [this coordinate direction] )
  (drawMrg [this direction])
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
  (setTileLayout [this layout]))

;; drawing

(defn draw-corner [g triangle-size direction]
  (let [gc (.create g)]
    (dotimes [_ 3]
      (.drawLine gc 0 0 triangle-size triangle-size)
      (case direction
        :right (.translate gc (double (/ triangle-size 3)) 0.0)
        :down  (.translate gc 0.0 (double (/ triangle-size 3)))))))

(defn draw-lines [g triangle-size]
  (let [gup (.create g)
        b (.getClipBounds gup)]
    (.translate gup (- (.width b) triangle-size) 0)
    (draw-corner gup triangle-size :right))
  (let [gdown (.create g)
        b (.getClipBounds gdown)]
    (.translate gdown 0 (- (.height b) triangle-size))
    (draw-corner gdown triangle-size :down)))

(defn draw-split [g coordinate direction]
  (let [b (.getClipBounds g)]
    (case direction
      :vertical (.drawLine g coordinate 0 coordinate (.height b))
      :horizontal (.drawLine g 0 coordinate (.width b) coordinate))))

(defn draw-mrg-arrow [g direction]
  (let [gc (.create g)
        b (.getClipBounds gc)]
    (case direction
      :down (do (.translate gc (double (/ (.width b) 2)) 0.0)
                (.fillRect gc
                  (- (int (/ (.width b) 3)))
                  0 (int (* (int (/ (.width b) 3)) 2))
                  (int (/ (.height b) 2)))))))

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

(defn add-mouse-listener [component enclosing-panel location]
  (let [start-pos (atom nil)
        corner    (atom nil)
        direction (atom nil)
        splitting? (atom nil)
        split-direction (atom nil)
        merging? (atom nil)
        merge-direction (atom nil)
        disable-splitting (fn [] (reset! splitting? nil)
                                 (reset! split-direction nil))
        disable-mrg (fn [] (reset! merging? nil)
                           (reset! merge-direction nil))
        split (fn [coordinate direction]
                (reset! splitting? true)
                (reset! split-direction direction)
                (.drawSplit enclosing-panel coordinate direction)
                (disable-mrg))
        mrg   (fn [direction]
                (.sendDrawMrg (.getCardPanel enclosing-panel) nil direction)
                (reset! merging? true)
                (reset! merge-direction direction)
                (disable-splitting))
        start (fn [c x y]
                (reset! corner c)
                (reset! start-pos [x y]))
        l (proxy [java.awt.event.MouseAdapter] []
            (mouseDragged [e]
              (let [x (.getX e)
                    y (- (.getY e))]
                (case location
                  :west (if (> y 0)
                          (mrg :up)
                          (if (> x 0)
                            (split x :vertical)
                            (mrg :left)))
                  :south (if (> x (.getWidth component))
                           (mrg :right)
                           (if (> y 0)
                             (split y :horizontal)
                             (mrg :down))))))
            (mouseReleased [e]
              (when (and @splitting?
                         @split-direction)
                (.split (.getCardPanel enclosing-panel)
                  (case @split-direction
                    :horizontal (.getY e)
                    :vertical (.getX e)) @split-direction)
                (reset! splitting? nil)
                (reset! split-direction nil))
              (when (and @merging?
                         @merge-direction)
                (.mrg (.getCardPanel enclosing-panel) nil @merge-direction)
                (reset! merging? nil)
                (reset! merge-direction nil))
              (reset! start-pos nil)
              (reset! corner nil)))]
    (.addMouseMotionListener component l)
    (.addMouseListener component l)
    l))

;; panel

(defn edit-bar [location content]
  (proxy [javax.swing.JComponent] []
    (paintComponent [g]
      (let [gc (.create g)
            b (.getClipBounds g)]
        (.fillRect gc 0 0 (.width b) (.height b))))
    (getPreferredSize []
      (let [d (.getPreferredSize content)]
        (case location
          :south (java.awt.Dimension. (.width d) 5)
          :west (java.awt.Dimension. 5 (.height d)))))))

(defn view [options]
  (let [card-panel-atom (atom nil)
        split-direction (atom nil)
        split-coordinate (atom nil)
        mrg-draw-direction (atom nil)
        cardp (atom nil)
        typec (type-combo-box options)
        {:keys [content menu]} (content-menu-panel options)
        menu-panel (javax.swing.JPanel. (BorderLayout.))
        edit-bar-west (edit-bar :west content)
        edit-bar-south (edit-bar :south content)
        enclosing-panel (proxy [javax.swing.JPanel dynamik.core.View dynamik.core.Layout] [(BorderLayout.)]
                          (drawSplit [coordinate direction] )
                          (drawMrg [d] )
                          (setCardPanel [c-p] (reset! cardp c-p))
                          (getCardPanel []  @cardp)
                          (getTileLayout [] (.getSelectedItem typec))
                          (setTileLayout [layout] 
                            (.setSelectedItem typec layout)))]
    (add-mouse-listener edit-bar-west enclosing-panel :west)
    (add-mouse-listener edit-bar-south enclosing-panel :south)
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
      (.add edit-bar-west BorderLayout/WEST)
      (.add edit-bar-south BorderLayout/SOUTH)
      (.add content BorderLayout/CENTER)
      (.add menu-panel BorderLayout/NORTH))
    enclosing-panel))

(defn tile [{:keys [parent contp] :as options}]
  (let [id (str (gensym "card panel"))
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
                  (let [splitp (JSplitPane. (case direction
                                              :horizontal JSplitPane/VERTICAL_SPLIT
                                              :vertical   JSplitPane/HORIZONTAL_SPLIT)
                                 true
                                 (tile (merge options {:parent this :contp @contp-atom}))
                                 (tile (merge options {:parent this :contp nil})))]
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
                       :top    (.getTileLayout (.getTopComponent sp))
                       :bottom (.getTileLayout (.getBottomComponent sp))}
                      JSplitPane/HORIZONTAL_SPLIT
                      {:direction :vertical
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
                          (let [{:keys [top bottom]} layout]
                            (.setTileLayout (.getTopComponent sp) top)
                            (.setTileLayout (.getBottomComponent sp) bottom))
                          :vertical
                          (let [{:keys [left right]} layout]
                            (.setTileLayout (.getLeftComponent sp) left)
                            (.setTileLayout (.getRightComponent sp) right)))))
                    (.setTileLayout (.getContentPanel this) layout))))]
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
