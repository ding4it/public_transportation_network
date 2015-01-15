(ns public-transportation-network.core)
(declare dijkstra build-path add-rdist update-rdists take-minnode)



(defn- children [net node]
  "get the children of the node"
  (keys (net node)))

(defn- distance [net nodesrc nodedst]
  "get the distance of the nodesrc to nodedst"
  ((net nodesrc) nodedst))

(defn shortest-path
  ([net root nodedst children distance]
     " return [path dist]"
     " net is the graph "
     " root the source node "
     " nodedst the destination "
     " children a function returning the children for a node "
     " distance a function returning the distance between two nodes "
     (let [preds (dijkstra net root nodedst children distance)
           path (build-path preds root nodedst)]
       (if (nil? path)
         nil
         [path (second (preds nodedst))])))
  ([net root nodedst ]
    (shortest-path net root nodedst children distance)))


(defn- ListToMap [l]
  "l is a list 
return a map constructing from l"
  (zipmap (range (count l))
          (map (fn[val]
                 (reduce conj {}
                         (filter (fn[[a b]] (not (zero? b)))
                                 (zipmap (range (count val)) val))))
               l)))


(defn- dijkstra [net root nodedst children distance]
  "dijkstra
net is a map
root is the root point
nodedst it the destination
return the way ,a list from root to nodedst
"
  (loop [rdists (sorted-map 0 {root root})
         minnode root
         preds {root [root 0]}
         dist 0]
    (if (empty? rdists)
      preds
      (let [[nminnode ndist nrdists npreds] (take-minnode rdists preds)
            [nnrdists nnpreds] (update-rdists nrdists
                                              npreds
                                              net
                                              nminnode
                                              ndist
                                              children distance)]
        (recur nnrdists nminnode nnpreds ndist)))))

(defn- build-path [preds root nodedst]
  "reverse walk on preds to reconstruct the shortest path"
  (loop [[pred dist] (preds nodedst)
         path (list nodedst)]
    (if (nil? pred)
      nil
      (if (= pred root)
        (cons root path)
        (recur (preds pred) (cons pred path))))))

(defn- add-rdist
  ([rdists node pred dist]
    "add a known rdist (rdist = distance to the root)"
    (if-let [nodes (rdists dist)]
      (assoc rdists dist (assoc nodes node pred))
      (assoc rdists dist {node pred})))
  ([rdists node pred dist prevdist]
    "remove the node from prevdist and add it to the new dist"
    (let [nrdists (add-rdist rdists node pred dist)
          minnodes (rdists prevdist)
          nminnodes (dissoc minnodes node)]
      (if (empty? nminnodes)
        (dissoc nrdists prevdist)
        (assoc nrdists prevdist nminnodes)))))

(defn- update-rdists [rdists preds net node dist children distance]
  "return [rdists preds] updated"
  (reduce (fn [acc x]
            (let [curdist (+ dist (distance net node x))
                  prevdist (second (preds x))
                  nrdists (first acc)
                  npreds (second acc)]
              (if (nil? prevdist)
                [(add-rdist nrdists x node curdist) (assoc npreds x [node curdist])]
                (if (< curdist prevdist)
                  [(add-rdist nrdists x node curdist prevdist)
                   (assoc npreds x [node curdist])]
                  [nrdists npreds]))))
          [rdists preds]
          (children net node)))

(defn- take-minnode [rdists preds]
  "return a vector [minnode dist rdists preds]"
  (let [ [dist minnodes] (first rdists)
        [minnode pred] (first minnodes)
        others (rest minnodes)]
    [minnode
     dist
     (if (empty? others)
       (dissoc rdists dist)
       (assoc rdists dist others))
     (assoc preds minnode [pred dist]) ]))
(def transportation [[0 4 3] [4 0 5][3 5 0]])


(comment


(def net {:A {:B 85, :C 217, :E 173},
          :B {:F 80},
          :C {:G 186 :H 103},
          :D {},
          :E {:J 502},
          :F {:I 250}
          :G {},
          :H {:D 183 :J 167}
          :I {:J 84},
          :J {}
          })

(let [pathinfo (shortest-path net :A :J children distance)]
  (printf "path = %s\n" pathinfo)) ;; [(:A :C :H :J) 487]

)
