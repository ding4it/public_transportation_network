(ns public-transportation-network.view
  (:use public-transportation-network.core)
  (:import
    (java.awt BorderLayout Graphics FlowLayout)
    (java.awt.event ActionListener WindowAdapter MouseAdapter )
    (javax.swing JButton JFrame JLabel JOptionPane JPanel JTextField BorderFactory)))

;;;define the width of a point circle
(def width 20)

;;;define the hight of a point circle
(def height 20)

;;;define the point type
(defrecord point [name x y ])


(defn addLine [lines prePoint point]
  "
  lines is a atom value like #{[p1 p2] [p3 p4]}
  prePoint is the start point of the line
  point is the end point of a line
  return @lines
  "
  (if (contains? @lines [point prePoint])
    nil
    (swap! lines (fn[ls] (conj ls [prePoint point])))))

(defn isPointsTooClose? [{name1 :name x1 :x y1 :y} {name2 :name x2 :x y2 :y}]
  "
  is p1 and p2 close to each other?
  return True or False
  "
  (and (< -40 (- x1 x2) 40) (< -40 (- y1 y2) 40)))

(defn addPoint[points point]  
  "
if point is not close to the point in points
conj point points
return points
  "
  (if (empty? (filter (partial isPointsTooClose? point) @points))
    (swap! points (fn[ps] (conj ps point)))
    @points))

(defn getNodeNum [node_inc] (swap! node_inc inc))

(defn nearEqual [a b] 
  "if a and b are neal"
  (and (< -0 (- b a ) 20)))

(defn getPointFromPoints[points x y] 
  (first (filter (fn[{name :name px :x py :y}] (and (nearEqual px x) (nearEqual py y))) points)))

(defn drawLines [^Graphics g lines]
  (doall (map (fn [[l1 l2]]
                (let [x1 (:x l1)
                      y1 (:y l1)
                      x2 (:x l2)
                      y2 (:y l2)]
                  (doto g
                    (.drawLine (+ (/ width 2) x1) (+ (/ height 2) y1)
                      (+ (/ width 2) x2) (+ (/ height 2) y2)))))
           lines )))

(defn drawPoints[^Graphics g points]
  (doall (map (fn[pt]
                (let [x (:x pt)
                      y (:y pt)
                      name (:name pt)]
                  (.drawOval g x y width height)
                  (.drawString g (str name) x y)))
           points)))

(defn demo[]
  "demo"
  (let [frame (JFrame.)
        load_file (JButton. "Load File")
        point_button (JButton. "Point")
        line_button (JButton. "Line")
        top (JPanel.)
        points (atom #{})
        lines (atom #{})
        prePoint (atom nil)
        node_inc (atom 2)
        panel (proxy [JPanel] []
                (paintComponent [^Graphics g]
                  (proxy-super paintComponent g)
                  (drawPoints g @points)
                  (drawLines g @lines)))
        drawing (atom 0)
        ]
    (.addActionListener point_button (proxy [ActionListener] []
                                       (actionPerformed [e]
                                         (swap! drawing (fn[_] 0)))))
    (.addActionListener line_button (proxy [ActionListener] []
                                      (actionPerformed [e]
                                        (swap! drawing (fn[_] 1)))))
    (doto top
      (.add load_file)
      (.add point_button)
      (.add line_button)
      (.setBorder (BorderFactory/createTitledBorder "Option")))
    (doto panel
      (.addMouseListener (proxy [MouseAdapter] []
                           (mouseClicked [e]
                             (let [x (.getX e)
                                   y (.getY e)
                                   ]
                               (if (zero? @drawing)
                                 (addPoint points (point. (str "node" (getNodeNum node_inc)) x y))
                                 (if-let [point (getPointFromPoints @points x y)]
                                   (if (or (nil? @prePoint) (= @prePoint point))
                                     (reset! prePoint point)
                                     (doall
                                       (addLine lines @prePoint point)
                                       (reset! prePoint nil)))))
                               (.repaint panel))))))
    (doto frame
      (.add top BorderLayout/NORTH)
      (.add panel)
      (.pack)
                                        ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setSize 800 600)
      (.setVisible true)))
  0)

(defn -main []
  (demo))
