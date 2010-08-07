/**
 * IndomainGoal - implements enumeration method based on the selection of the
 * minimal value in the domain of variable
 * 
 * @author Nathan Sorenson
 * @version 2.4
 */

package JaCoP.search;

import JaCoP.core.Domain;
import JaCoP.core.IntVar;
import JaCoP.core.GoalVar;

public class IndomainGoal implements Indomain<IntVar> {

	/**
	 * It creates Indomain heuristic, which will choose the minimal value
	 * from the variable domain.
	 */
	public IndomainGoal() {
	}

	//	public int indomain(Domain dom) {
	//		return dom.min();
	//	}

	//TODO: avoiding reflection may make this faster.
	public int indomain(IntVar var) {
		if(var instanceof GoalVar) {
			return ((GoalVar)var).closestToGoal();
		} else {
			return var.min();
		}
	}
}
