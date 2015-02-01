(ns public-transportation-network.view
  (:use public-transportation-network.core)
  (:import
    (java.awt BorderLayout Graphics FlowLayout Graphics2D BasicStroke)
    (java.awt.event ActionListener WindowAdapter MouseAdapter WindowEvent)
    (javax.swing JFileChooser JButton JFrame JLabel JOptionPane JPanel
                 JTextField BorderFactory JTable JScrollPane JComboBox
                 filechooser.FileNameExtensionFilter)))

(declare toMapType)
;;;define the width of a point circle
(def width 20)

;;;define the hight of a point circle
(def height 20)

;;;define the point type
(defrecord point [name x y ])


(defn addLine [lines prePoint point length]
  "
  lines is a atom value like #{[p1 p2] [p3 p4]}
  prePoint is the start point of the line
  point is the end point of a line
  return @lines or nil
  "
  (if (contains? @lines [point prePoint])
    nil
    (swap! lines (fn[ls] (assoc ls [prePoint point] length )))))

(defn isPointsTooClose? [{name1 :name x1 :x y1 :y} {name2 :name x2 :x y2 :y}]
  "
  is p1 and p2 close to each other?
  return True or False
  "
  (and (< -40 (- x1 x2) 40) (< -40 (- y1 y2) 40)))

(defn addPoint[points point]  
  "if point is not close to the point in points
conj point points
return points"
  (if (empty? (filter (partial isPointsTooClose? point) @points))
    (swap! points (fn[ps] (conj ps point)))
    @points))

(defn getNodeNum [node_inc] (swap! node_inc inc))

(defn nearEqual [a b] 
  "if a and b are neal"
  (and (< -0 (- b a ) 20)))

(defn getPointFromPoints[points x y] 
  (first (filter (fn[{name :name px :x py :y}] (and (nearEqual px x) (nearEqual py y))) points)))

(defn drawShortestPath [^Graphics g path]
  (if (< (count path) 2)
    nil
    (let [g2d (cast Graphics2D g)]
      (.setBackground g2d (java.awt.Color/RED))
      (.setStroke g2d (BasicStroke. 5))
      (doall (reduce (fn [p1 p2]
                       (let [x1 (:x p1)
                             y1 (:y p1)
                             x2 (:x p2)
                             y2 (:y p2)]
                         (.drawLine g (+ (/ width 2) x1) (+ (/ height 2) y1)
                           (+ (/ width 2) x2) (+ (/ height 2) y2))
                         p2))
                     (first path))))))

(defn drawLines [^Graphics g lines]
  (doall (map (fn [[p1 p2]]
                (let [x1 (:x p1)
                      y1 (:y p1)
                      x2 (:x p2)
                      y2 (:y p2)]
                  (.drawLine g (+ (/ width 2) x1) (+ (/ height 2) y1)
                    (+ (/ width 2) x2) (+ (/ height 2) y2))))
              (keys lines) )))

(defn drawPoints[^Graphics g points]
    (doall (map (fn[pt]
                  (let [x (:x pt)
                        y (:y pt)
                        name (:name pt)]
                    (.drawOval g x y width height)
                    (.drawString g (str name) x y)))
             points)))

(defn getLine [lines from to]
  (first(filter (fn[[[p1 p2] v]](and (= (:name p1) from) (= (:name p2) to))) lines)))

(defn chooseStartAndEndPoint [points lines shortestPath panel]
  (let [frame (JFrame.)
        contentPane (JPanel.)
        lable1 (JLabel. "Start")
        start (JComboBox.)
        lable2 (JLabel. "End")
        end (JComboBox.)
        submit (JButton. "Find Shortest Path")
        ]
    (doall 
      (for [p points]
        (do
          (.addItem start (:name p))
          (.addItem end (:name p))
          nil)))
     (.addActionListener 
          submit  
          (proxy [ActionListener] []
            (actionPerformed [e]
              (.dispatchEvent frame (WindowEvent. frame WindowEvent/WINDOW_CLOSING)))))
     (.addWindowListener frame 
       (proxy [WindowAdapter] []
         (windowClosing [e]
           (let [from (.getSelectedItem start)
                 to (.getSelectedItem end)]
           (reset! shortestPath (shortest-path 
                                  (toMapType points lines) 
                                  (first (filter (fn[p] (= from (:name p))) points)) 
                                  (first (filter (fn[p] (= to (:name p))) points)))))
           (.repaint panel))))
    (doto contentPane 
      (.add lable1)
      (.add start)
      (.add lable2)
      (.add end)
      (.add submit))
    (doto frame 
      (.add contentPane)
      (.setBounds 350 150 300 120)
      (.setVisible true))))

        
        

(defn showLengthWindow [father lines]
  "open the window to set the length"
  (let [frame (JFrame.)
        column (java.util.Vector. ["From" "To" "Length"])
    data (java.util.Vector. (mapv (fn[[[from to] length]] (java.util.Vector. [(:name from) (:name to) length])) @lines))
        table (proxy [JTable] [ data column]
                (isCellEditable [r c]
                  (if (< c 2) false true))
                (setValueAt [value row column]
                  (try 
                    (if (= 0 (java.lang.Integer/parseInt value))
                      (throw (Exception. "value is 0")) 
                      (do 
                        (swap! lines 
                               (fn[ls]
                                 (assoc 
                                   ls 
                                   (first 
                                     (getLine 
                                       ls 
                                       (proxy-super getValueAt row 0) 
                                       (proxy-super getValueAt row 1)))  
                                   (java.lang.Integer/parseInt value))))
                        (.set (.get data row) 2 (java.lang.Integer/parseInt value))))
                    (catch Exception e
                      (javax.swing.JOptionPane/showMessageDialog nil "距离需要是正整数")))))
        
        
        scroll (JScrollPane. table)]
    (doto frame
      (.add scroll)
      (.setVisible true)
      (.setBounds 350 150 600 400)
      (.addWindowListener  (proxy [WindowAdapter] []
                             (windowClosing [e]
                           (.setEnabled father true)
                           ))))))
(defn toMapType [points lines]
  (reduce 
    (fn[rs [[p1 p2] v]]
      (assoc-in (assoc-in rs [p1 p2] v) [p2 p1] v))
    (reduce (fn[rs p](assoc rs p {})) {} points)
    lines))

(defn demo[]
  "demo"
  (let [frame (JFrame.)
        save_file (JButton. "Save File")
        load_file (JButton. "Load File")
        point_button (JButton. "Add Point")
        line_button (JButton. "Add Line")
        length_button (JButton. "Add Length")
        find_shortest_Path (JButton. "Find Shortest Path")
        reset_button (JButton. "Reset")
        top (JPanel.)
        points (atom #{})
        lines (atom {})
        shortestPath (atom [])
        prePoint (atom nil)
        node_inc (atom 2)
        panel (proxy [JPanel] []
                (paintComponent [^Graphics g]
                  (proxy-super paintComponent g)
                  (drawPoints g @points)
                  (drawLines g @lines)
                  (drawShortestPath g @shortestPath)
                  ))
        drawing (atom 0)

        ]
    (.addActionListener 
      save_file
      (proxy [ActionListener] []
        (actionPerformed [e]
          (let [chooser (JFileChooser.)
                fileFilter 
                (FileNameExtensionFilter. "text file" (into-array ["txt"]))]
            (.setFileFilter chooser fileFilter)
            (if (= JFileChooser/APPROVE_OPTION (.showSaveDialog chooser nil))
              (let [path (.getPath (.getSelectedFile chooser))
                    path (if (.endsWith path ".txt") path (str path ".txt"))]
              (with-open [w (clojure.java.io/writer path :append false)]
                (.write w (str (count @points)))
                (.newLine w)
                (doall 
                  (map 
                    (fn [p] 
                      (.write w (str (:name p) " " (:x p) " " (:y p))) 
                      (.newLine w))
                    @points))
                (.write w (str(count @lines)))
                (.newLine w)
                (doall
                  (map (fn[[[p1 p2] l]]
                         (.write w (str (:name p1) " " (:name p2) " " l ))
                         (.newLine w))
                       @lines))
                )))))))
    
    (.addActionListener 
      load_file
      (proxy [ActionListener] []
        (actionPerformed [e]
          (reset! points #{})
          (reset! lines {})
          (reset! shortestPath [])
          (let [chooser (JFileChooser.)
                fileFilter 
                (FileNameExtensionFilter. "text file" (into-array ["txt"]))]
            (.setFileFilter chooser fileFilter)
            (if (= JFileChooser/APPROVE_OPTION (.showOpenDialog chooser nil))
              (with-open [r (clojure.java.io/reader (.getPath (.getSelectedFile chooser)))]
                (doall
                  (for [i (range (java.lang.Integer/parseInt (.readLine r)))
                        :let [[name x y] (clojure.string/split (.readLine r) #" ")]]
                    (do
                      (addPoint points (point. name (java.lang.Integer/parseInt x) (java.lang.Integer/parseInt y)))
                      nil)))
                (doall
                  (for [i (range (java.lang.Integer/parseInt (.readLine r)))
                        :let [[p1 p2 l] (clojure.string/split (.readLine r) #" ")
                              p1 (first (filter #(= p1 (:name %)) @points)) 
                              p2 (first (filter #(= p2 (:name %)) @points))]]
                    (addLine lines p1 p2 (java.lang.Integer/parseInt l))))
                (.repaint panel))
              nil)))))
                    
                  
                  
              
    (.addActionListener 
      point_button 
      (proxy [ActionListener] []
        (actionPerformed [e]
          (swap! drawing (fn[_] 0)))))
    (.addActionListener 
      line_button 
      (proxy [ActionListener] []
        (actionPerformed [e]
          (swap! drawing (fn[_] 1)))))
    (.addActionListener 
      length_button 
      (proxy [ActionListener] []
    (actionPerformed [e]
          (.setEnabled frame false)
          (showLengthWindow frame lines))))
    (.addActionListener 
      find_shortest_Path 
      (proxy [ActionListener] []
        (actionPerformed [e]
          (chooseStartAndEndPoint @points @lines shortestPath panel))))
    (.addActionListener 
      reset_button
      (proxy [ActionListener] []
        (actionPerformed [e]
          (reset! points #{})
          (reset! lines {})
          (reset! shortestPath [])
          (.repaint panel))))
    
    (doto top
      (.add save_file)
      (.add load_file)
      (.add point_button)
      (.add line_button)
      (.add length_button)
      (.add find_shortest_Path)
      (.add reset_button)
      (.setBorder (BorderFactory/createTitledBorder "Option")))
        
    (.addMouseListener panel 
      (proxy [MouseAdapter] []
        (mouseClicked [e]
          (let [x (.getX e) y (.getY e)]
            (if (zero? @drawing)
              (addPoint points (point. (str "node" (count @points)) x y))
              (if-let [point (getPointFromPoints @points x y)]
                (if (or (nil? @prePoint) (= @prePoint point))
                  (reset! prePoint point)
                  (doall
                    (addLine lines @prePoint point 1)
                    (reset! prePoint nil)))))
            (.repaint panel)))))
    (doto frame
      (.add top BorderLayout/NORTH)
      (.add panel)
      (.pack)
      ;(.setDefaultCloseOperation JFrame/EXIT_ON_CLOSE)
      (.setBounds 300 100 800 600)
      (.setVisible true))
    0))

(defn -main []
  (demo))
