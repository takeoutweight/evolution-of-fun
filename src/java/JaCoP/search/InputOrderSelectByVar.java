// By Nathan Sorenson
package JaCoP.search;

import JaCoP.core.*;

public class InputOrderSelectByVar extends InputOrderSelect {

	public InputOrderSelectByVar(Store store, IntVar[] variables,
			Indomain indomain) {
		super(store, variables, indomain);
	}
	
	@Override
	public int getChoiceValue() {
			//Why does the compiler need me to cast these?
			assert ((Integer)currentIndex.value() >= 0);
			assert ((Integer)currentIndex.value() < searchVariables.length);
			//			assert (searchVariables[(Integer)currentIndex.value()].domain != null);
		
		//indomain with the Variable itself, NOT the variable's domain.
			return valueOrdering.indomain(searchVariables[(Integer)currentIndex.value()]);
	}
}
