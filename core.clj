(ns adventure.core
  (:require [clojure.core.match :refer [match]]
            [clojure.string :as str])
  (:gen-class))

(def the-map
	{:foyer {:desc "The walls are freshly painted but do not have any pictures.  You get the feeling it was just created  for a game or something. Your adventure begins. Your purpose is to find a way back home. You look through the mansion, and you find two  staircases, one leads up, the other one leads down. Afterwards, you step outside and there are four different direction that you can go. "
           :desc1 ""
   		   :title "in the foyer"
           :dir {:north :forest, :south :sea, :east :mountain, :west :volcano, :up :sub_top, :down :basement, :grue :grue-pen}
           :contents #{:Chipotle}
		   :additional #{:coin}}
         

	:forest {:desc "You are in a beautiful green forest with a huge red sakura tree right in the middle. Tickled with curiosity,  you decided to come closer to it. The tree has a hole, which was glowing green, in the middle. "
             :desc1 "It was guarded by a beautiful Fairy. You may need what is inside the tree for future encounters. But are you ready to face the Fairy? "
              :title "in the forest"
              :dir {:south :foyer}
              :contents #{:wood}
			  :additional #{}}
			  
	:sea {:desc "You are looking at a vast sea. "
			  :desc1 "There is a beautiful voice out of no where. Enchanted by the voice, you followed it away from the shore. You suddenly came to consciousness and realized that you were enchanted by a Siren floating in the middle of the sea. You should run away! However, there is this deep blue glowing right where the Siren is. You may need what the Siren is holding for future encounters. But are you ready to face the siren? "
              :title "in the sea"
              :dir {:north :foyer}
              :contents #{:water}
			  :additional #{:healing_water}}


			  
    :mountain {:desc "You are under an enormous mountain named Mount Everesting. There is a calming white glow on the top of the mountain. "
			  :desc1 "You may need what is on top of the mountain for future encounters. But are you ready to face this gingatic mountain? "
              :title "under the mountain"
              :dir {:west :foyer, :up :mountain1}
              :contents #{}
			  :additional #{}}

	:mountain1 {:desc "As you begin your hike up the mountain side, you arrive at a safety shelter.  As you walk past it, you notice a storm brewing on the horizon. It headed straight for the mountain and is approaching at an unatural pace. What do you do? Do you take shelter or push forward? "
			  :desc1 "You may need what is on top of the mountain for future encounters. But are you ready to face this gingatic mountain? "
              :title "progressing up the mountain"
              :dir {:forward :mountain2, :shelter :mountainf1}
              :contents #{}
			  :additional #{}}

	:mountain2 {:desc "You decide to push past the storm. You increase your pace and make a sprint for it. The storm ravages across the mountain and you are now drenched and freezing.  All of a sudden, the storm vanishes, within a blink of an eye.  You look back and see the destruction that the storm has left.  Luckily you did not stay in the shelter, as you now see peices of it across the mountain side. Infront of you is a bridge. You also see a man dressed in a monk's attire meditating before it. As you approach, the man sticks out his hands as if asking for something from you.  What do you do?"
			  :desc1 "You may need what is on top of the mountain for future encounters. But are you ready to face this gingatic mountain? "
              :title "progressing up the mountain"
              :dir {:ignore :mountainf2, :give_coin :mountain3}
              :contents #{}
			  :additional #{}}

	:mountain3 {:desc "You decide to give the coin you obtained from the bottom of the fairy to the man.  You hear a soft 'thank you' come from the man, but you don't notice his lips move at all. You continue on across the bridge.  Half way through, you notice a cold breeze blow across, giving you shivers to the bone. You look back, and the man is gone, with no trail of where he may have went nor even where he sat.  You feel something cold in your hands.  Opening your right palm, find the coin you gave to the man appear in your hands.  What a lucky break! You continue along the track up the mountain.  As you reach the top, you enter a save.  Inside the cave, you come across a white elemental orb."
			  :desc1 "You may need what is on top of the mountain for future encounters. But are you ready to face this gingatic mountain? "
              :title "at the top of the mountain"
              :dir {:down :mountain}
              :contents #{:air}
			  :additional #{}}

	:mountainf1 {:desc "You decide to stay in the shelter and try to weather it out.  As the storm approaches, you hear the thundering roars echo inside the shelter along with the screeching wind blowing through the wear and tear holes of the old shelter. Not long after, the rain begins, and it quickly becomes evident that this shelter will not protect you.  You decide to make a run for out, but as you approach the entrence, the door flies open and knocks you unconscious.  You wake up back at the bottom of the mountain along with a massive bruise. (-25hp)  "
			  :desc1 "You may need what is on top of the mountain for future encounters. But are you ready to face this gingatic mountain? "
              :title "under the mountain"
              :dir {:west :foyer, :up :mountain1}
              :contents #{}
			  :additional #{}}

	:mountainf2 {:desc "You decide to ignore the man (either through greed or you have nothing to give him) and continue to cross the bridge.  Half way across, the monk stands up, walks to the mouth of the bridge, crouches down, and gently lays one finger onto the bridge.  In an instant, the bridge breaks apart, but in a way that seemed abnormal, almost as if it was controlled.  As you fall, you notice the bridge rebuilding itself.  You soon black out.  You wake up back at the bottom of the mountain, with a massive bump on your head (-50hp)! "
			  :desc1 "You may need what is on top of the mountain for future encounters. But are you ready to face this gingatic mountain? "
              :title "under the mountain"
              :dir {:west :foyer, :up :mountain1}
              :contents #{}
			  :additional #{}}
			  
    :sub_top {:desc "You are on the top of the cattle. "
		      :desc1 "There is a huge door that is glowing rainbow color. You will need all the elements to open the door. Once the door is open you can go up again to get to the top. "
              :title "on top"
              :dir {:down :foyer, :up :top}
              :contents #{}
			  :additional #{}}

    :top {:desc ""
		      :desc1 "You are in the same room with an angel! Are you ready to face her? This angel is guarding a glowing bright white light. You will need that to defeat the dragon. "
              :title "top floor"
              :dir {:down :foyer}
              :contents #{:light}
			  :additional #{:little_angel}}
 
     :basement {:desc "You are in the basement of the cattle. "
			  :desc1 "There is a huge dragon sleeping in the dark, guarding something like a black hole. That maybe your way home! But beware! Only light element can damage the Dragon of Darkness! "
              :title "in the basement"
              :dir {:up :foyer}
              :contents #{:teleporter_door}
			  :additional #{}}
    
	:grue-pen {:desc "gIt is very dark.  You are about to be eaten by a grue. "
 			  :desc1 "" 
			  :title "in the grue pen"
              :dir {:north :foyer}
              :contents #{}
			  :additional #{}}
			  
	:volcano {:desc "You are in front of a huge volcano that looks like it is about to erupt any moment. "
              :desc1 "Flying on top of the hot volcano is a glorious phoenix! You should run away! However, there is this hot red glow on top of the volcano right where the phoenix is . You may need what the phoenix is holding for future encounters. But are you ready to face the phoenix? "
			  :title "under the volcano"
              :dir {:east :foyer}
              :contents #{:fire}
			  :additional #{:phoenix_feather}}
			  
    :end {:desc "It is the end."
		  :desc1 ""
		  :title "at THE END"
		  :dir {}
		  :contents #{}
		  :additional #{}}

   })


(def adventurer
  {:location :foyer
   :inventory #{}
   :lives 3
   :hp 100
   :tick 0
   :door 0
   :healing_water -1
   :phoenix_feather -1
   :little_angel -1
   :coin -1
   :fairy 10
   :siren 10
   :phoenix 10
   :angel 10
   :dragon 10
   :seen #{}})



(defn status [player]
  (let [location (player :location)]
	(print (str "You are " (-> the-map location :title) ". "))
	(print (str "Lives: " (-> player :lives) ". "))
	(print (str "HP: " (-> player :hp) ". ")) 
    (when-not ((player :seen) location)
      (print (-> the-map location :desc))
	  (if (= location :forest) (if (pos? (player :fairy))(print (-> the-map location :desc1)))
	  (if (= location :volcano) (if (pos? (player :phoenix))(print (-> the-map location :desc1)))
	  (if (= location :sea) (if (pos? (player :siren))(print (-> the-map location :desc1)))
	  (if (= location :top) (if (pos? (player :angel))(print (-> the-map location :desc1)))
	  (if (= location :sub_top) (if (and (contains?(player :inventory):air) (contains?(player :inventory):fire) 
	  (contains?(player :inventory):wood) (contains?(player :inventory):water))(print (-> the-map location :desc1))) 
	  (if (= location :basement) (if (pos? (player :dragon))(print (-> the-map location :desc1)))player)))))))
      (update-in player [:seen] #(conj % location))))


	
(defn to-keywords [commands]
  (mapv keyword (str/split commands #"[.,?! ]+")))

(defn go [dir player]
  (if (= dir :end) (assoc-in player [:location] :end) (let [location (player :location)
        dest (->> the-map location :dir dir)]
    (if (nil? dest)
      (do (println "You can't go that way.")
          player)
      (if (and (=(player :location) :sub_top)(= (player :door) 0)) (assoc-in player [:location] location) (assoc-in player [:location] dest))))))

(defn touch_ [player]
	(let [location (player :location)]
	(if (and (= location :forest) (zero? (player :fairy))) (do (println "You pick up the wood element!")
	(update-in player [:inventory] #(conj %(first (-> the-map location :contents)))))
									
	(if (and (= location :sea) (zero? (player :siren))) (do (println "You pick up the water element!")
	(update-in player [:inventory] #(conj %(first (-> the-map location :contents)))))
	
	(if (= location :mountain3) (do (println "You pick up the air element!")
	(update-in player [:inventory] #(conj %(first (-> the-map location :contents)))))
									
	(if (and (= location :volcano) (zero? (player :phoenix))) (do (println "You pick up the fire element!")
	(update-in player [:inventory] #(conj %(first (-> the-map location :contents)))))
									
	(if (and (= location :top) (zero? (player :angel))) (do (println "You pick up the light element!")
	(update-in player [:inventory] #(conj %(first (-> the-map location :contents)))))
									
	(if (and (= location :basement) (zero? (player :dragon))) (do (println "You pick up the teleporter! Now you can 'go_home' or continue to explore the place again. ")
	(update-in player [:inventory] #(conj %(first (-> the-map location :contents)))))
	
	(if (= location :foyer) (do (println "You pick up a Chipotle!")
	(update-in player [:inventory] #(conj %(first (-> the-map location :contents)))))
									
	(do (println "Nothing to touch here") player))))))))))

(defn look_around_ [player]
	(let [location (player :location) item (first (-> the-map location :additional))]
	(if (nil? item)(do (println "There is nothing to look.") false)
		
		(if (= item :healing_water) (if (zero? (player :healing_water))(do (println "There is nothing to look anymore.") false)
		(do (println "You pick up a healing potion! This is a one-time thing"):healing_water))
		
		(if (= item :phoenix_feather)(if (zero? (player :phoenix_feather))(do (println "There is nothing to look anymore.") false)
		(do (println "You pick up a phoenix feather! This is a one-time thing"):phoenix_feather))	

		(if (= item :coin)(if (zero? (player :coin))(do (println "There is nothing to look anymore.") false)
		(do (println "You pick up a coin! This is a one-time thing"):coin))	
		
        (if (= item :little_angel)(if (zero? (player :little_angel))(do (println "There is nothing to look anymore.") false)
		(do (println "You pick up a little angel! This is a one-time thing"):little_angel)))))))))		
	  
(defn Lchange [change player]
	(if (pos? change)
		(assoc-in player [:lives] (+ (-> player :lives) 1))
		(if (neg? (- (-> player :lives) 1)) (do (println "You are out of lives! Game Over!!.") (let [dd (assoc-in player [:lives] 0)] (go :end dd))) 
		(assoc-in player [:lives] (- (-> player :lives) 1)))))
	  
	  
(defn health [change player XX]
  (let [health (player :hp)]
	(if (pos? change)
		(assoc-in player [:hp] (+ (-> player :hp) XX))
		(if (neg?  (- health XX)) (do (println "You have died dead.") (let [hz1 (assoc-in player [:hp] 0)] (if (pos? (player :lives))
																(do (let [hz2 (assoc-in hz1 [:hp] 100)] (Lchange 0 hz2))) (go :end hz1))))
																
		(if (zero? (- health XX)) (do (println "You have died dead.") (let [hp3 (assoc-in player [:hp] 0)] (if (pos? (player :lives))
																(do (let [hp4 (assoc-in hp3 [:hp] 100)] (Lchange 0 hp4))) (go :end hp3))))
																
		(assoc-in player [:hp] (- (-> player :hp) XX)))))))
		


(defn fight [player]
	(let [location (player :location)]
	(if (and (= location :forest) (pos? (player :fairy))) (do (println "The fairy attacks you for 10 damage!")(println "You attack the fairy back for 5 dmg! Its remaing hp is: " (- (-> player :fairy) 5))
									:forest)
									
	(if (and (= location :sea) (pos? (player :siren))) (do (println "The siren attacks you for 10 damage!") (println "You attack the siren back for 5 dmg! " (- (-> player :siren) 5))
								:sea)
									
	(if (and (= location :volcano) (pos? (player :phoenix))) (do (println "The phoenix attacks you for 10 damage!") (println "You attack the phoenix back for 5 dmg! " (- (-> player :phoenix) 5))
									:volcano)
									
	(if (and (= location :top) (pos? (player :angel))) (do (println "The angel attacks you for 10 damage!") (println "You attack the angel back for 5 dmg! " (- (-> player :angel) 5))
								:top)
									
	(if (and (= location :basement) (pos? (player :dragon))) (if (contains? (player :inventory) :light) (do (println "The dragon attacks you back for 10 damage!")
															(println "You attack the dragon back for 5 dmg! Its remaing hp is: " (- (-> player :dragon) 5)) :basement)

															(do (println "The dragon attacks you for 10 damage!")
															(println "You attack the dragon for 0 dmg! The dragon's darkness is too strong! Its remaing hp is: " (-> player :dragon)) :basement))
									
	(do (println "Nothing to attack here") false))))))))


(defn tock [player]
  (update-in player [:tick] inc))

(defn respond [player command ]
  (match command
         [:look] (update-in player [:seen] #(disj % (-> player :location)))
         (:or [:n] [:north] ) (go :north player)
         [:south] (go :south player)
		 [:east] (go :east player)
		 [:west] (go :west player)
		 [:up] (go :up player)
		 [:down] (go :down player)
		 [:forward] (go :forward player)
		 [:ignore] (let [np1 (health 0 player 50)] (go :ignore np1))
		 [:give_coin] (if (contains?(player :inventory) :coin) (go :give_coin player) (do (println (str "You do not have anything to give")) (let [np1 (health 0 player 50)] (go :ignore np1))))
		 [:shelter] (let [np1 (health 0 player 25)] (go :shelter np1))
		 [:grue] (go :grue player)
		 [:eat] (health 1 player 10)
		 [:take_damage] (health 0 player 10)
		 [:touch] (touch_ player)
		 [:look_around](let [item (look_around_ player) location (player :location)]
		 			(if (= item :healing_water) (do (let [np1 (assoc-in player [:healing_water] 0)]
					(println "You gained 10 health.") (let [np2 (health 1 np1 10)] 
					(update-in np2 [:inventory] #(conj %(first (-> the-map location :additional)))))))
					
					(if (= item :phoenix_feather) (do (let [np3 (assoc-in player [:phoenix_feather] 0)] 
					(println "You gained 1 life.") (let [np4 (Lchange 1 np3)] 
					(update-in np4 [:inventory] #(conj %(first (-> the-map location :additional)))))))

					(if (= item :coin) (do (let [np6 (assoc-in player [:coin] 0)] 
					(println "You get the coin!") 
					(update-in np6 [:inventory] #(conj %(first (-> the-map location :additional))))))
					
					(if (= item :little_angel) (do (let [np5 (assoc-in player [:little_angel] 0)]
					(println "You found a little angel that can help you on your final fight.")  
					(update-in np5 [:inventory] #(conj %(first (-> the-map location :additional))))))player)))))


					
		 [:view_inventory] (do (println (str "Inventory: " (player :inventory))) player)			 
		 [:go_home] (if (contains?(player :inventory):teleporter_door)
		 (do (println (str "Congratulation! you solved the game! Let go home!")) (go :end player))
		 (do (println (str "Sorry. You have not found a way home yet!"))player)) 

		 [:open_door] (if (and (contains?(player :inventory):air) (contains?(player :inventory):fire) (contains?(player :inventory):wood) (contains?(player :inventory):water)) 
		 (do (println (str "You can open the door to the angel. Fight her or charm her!")) (assoc-in player [:door] 1))
		 (do (println (str "Sorry. You cannot open the door!"))player))
		 [:fight] (let [loc (fight player)]
					(if (and (= loc :forest  ) (> (-> player :fairy)   0)) (do (let [np1 (assoc-in player [:fairy]    (- (-> player :fairy)   5))] (health 0 np1 10)))
					(if (and (= loc :sea     ) (> (-> player :siren)   0)) (do (let [np2 (assoc-in player [:siren]    (- (-> player :siren)   5))] (health 0 np2 10)))
					(if (and (= loc :volcano ) (> (-> player :phoenix) 0)) (do (let [np3 (assoc-in player [:phoenix]  (- (-> player :phoenix) 5))] (health 0 np3 10)))
					(if (and (= loc :top     ) (> (-> player :angel)   0)) (do (let [np4 (assoc-in player [:angel]    (- (-> player :angel)   5))] (health 0 np4 10)))
					(if (and (= loc :basement) (> (-> player :dragon)  0)) (if (contains? (player :inventory) :light) 
																		   (do (let [np5 (assoc-in player [:dragon]   (- (-> player :dragon)  5))] (health 0 np5 10)))
																		   (health 0 player 10))
					 player))))))

         _ (do (println "I don't understand you.")
               player)
         )) 

(defn -main
    "Welcome to our awesome adventures of many different key elements in life. Hope you will enjoy this game! "
  [& args]
  (loop [local-map the-map
         local-player adventurer]
    (let [pl (status local-player)
          _  (println "What do you want to do?")
          command (read-line)]
      (recur local-map (respond pl (to-keywords command))))))

