// By Nathan Sorenson
package JaCoP.core;

public class GoalVar extends IntVar {
	public int mgoal;
	public GoalVar(Store store, String name, int min, int max, int goal) {
		super(store,name,min,max);
		this.mgoal = goal;
	}

	public int closestToGoal() {
		if(this.domain.contains(mgoal)) {
			return mgoal;
		} else {
			int distance = Integer.MAX_VALUE;
			int currentGoal = this.domain.min();
			for(int i = 0; i < ((IntervalDomain)(this.domain)).size; i++) {
				Interval interval = this.domain.getInterval(i);
				int imin = interval.min();
				int imax = interval.max();
				if(Math.abs(imin - mgoal) < distance) {
					distance = Math.abs(imin - mgoal);
					currentGoal = imin;
				}
				if(Math.abs(imax - mgoal) < distance) {
					distance = Math.abs(imax - mgoal);
					currentGoal = imax;
				}
			}
			return currentGoal;
		}
	}
}
