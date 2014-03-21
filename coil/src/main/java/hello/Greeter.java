package hello;

import org.joda.time.LocalTime;

public class Greeter {
	public String sayHello() {
		LocalTime currentTime = new LocalTime();
		StringBuilder s = new StringBuilder();
		s.append("All's well at ");
		s.append(currentTime.toString());
		s.append("\n");
		s.append("Hello, world!");
		return s.toString();
	}
}
