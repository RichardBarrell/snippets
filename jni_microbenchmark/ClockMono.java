class ClockMono {
	native double clockMono();
	static {
		System.loadLibrary("jclockmono");
	}

	public static void main(String args[]) {
		ClockMono cm = new ClockMono();
		double t0 = cm.clockMono();
		for (int i=0; i<5000000; i++) {
			cm.clockMono();
		}
		double t1 = cm.clockMono();
		System.out.format("java/jni: %f\n", t1 - t0);
	}
}
