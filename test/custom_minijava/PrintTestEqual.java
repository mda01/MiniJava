class PrintTestEqual {
    public static void main(String[] a) {
		System.out.println(new Printer().conditionnalDisplay());
    }
}

class Printer {
	int number;

	public boolean init () {
		number = 42;
		return true;
	}

	public int getNumber() {
		return number;
	}
	
	// System.out.println(35 + 2 * 3 + 1);
	public int conditionnalDisplay () {
		boolean collector;
		int res;
		Printer p;

		p = new Printer();
		collector = this.init();

		// Addition of operator ==
		if (number == 42 && !(number == 5) && number != 43) {
			res = 1;
		} else {
			res = 0;
		}
		return res;
	}
}