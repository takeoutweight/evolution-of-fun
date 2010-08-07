package nathansorenson;

import java.util.Random;

import dk.itu.mario.MarioInterface.Constraints;
import dk.itu.mario.MarioInterface.GamePlay;
import dk.itu.mario.MarioInterface.LevelGenerator;
import dk.itu.mario.MarioInterface.LevelInterface;
import dk.itu.mario.engine.PlayCustomized;
import dk.itu.mario.level.CustomizedLevel;
import nathansorenson.ClojureLevelGenerator;

public class NathanSorensonLevelGenerator implements LevelGenerator{

	public LevelInterface generateLevel(GamePlay playerMetrics) {
		LevelInterface level = null;
		try {
				ClojureLevelGenerator clg = new ClojureLevelGenerator();
				level = clg.randLevel();
			} catch (Exception e) {
				// error with clojure code.
				e.printStackTrace();
			}
		return level;
	}
}
