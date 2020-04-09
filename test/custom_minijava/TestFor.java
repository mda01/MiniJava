class PrintTestEqual {
    public static void main(String[] a) {
		System.out.println(new Printer().loop());
    }
}

class Printer {
	int number;

	public int loop () {
		int i;
		int j;
		boolean a;
		a = true;
		i = 0;
		j = 0;

		while(i <= 5) {
			System.out.println(i);
			i = i + 1;
		}

		for (i=0; i < 6; i+1){
			System.out.println(i);
		}

		/* Creates an infinite loop, as expected */
		/*for(i = 0; a ; i+1) {
			System.out.println(i);
		}*/
		
		/* Test j reinitialisation in loop as well as nested loops */
		for(j=333; j != 0; j-111) {
			System.out.println(j);
			for (i=0; i < 6; i+1){
				System.out.println(i);
			}
		}
		/* Does not seem to support nested loops*/

		return 42;
	}
}