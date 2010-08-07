package dk.itu.mario.level.generator;

import dk.itu.mario.MarioInterface.GamePlay;
import dk.itu.mario.MarioInterface.LevelGenerator;
import dk.itu.mario.MarioInterface.LevelInterface;
import dk.itu.mario.engine.PlayCustomized;
//import nathansorenson.ClojureLevelGenerator;

public class CustomizedLevelGenerator implements LevelGenerator{

	public LevelInterface generateLevel(GamePlay playerMetrics) {
		LevelInterface level;
		if(PlayCustomized.generatedLevel == null) {
			//this is what the production code will do without thinking about it.
			level = null;
			
			try {
					//	ClojureLevelGenerator clg = new ClojureLevelGenerator();
					level = null;//clg.randLevel();
			} catch (Exception e) {
				// error with clojure code.
				e.printStackTrace();
			}
		} else {
			level = PlayCustomized.generatedLevel;
		}
		return level;
	}
}
