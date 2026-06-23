#include <chrono>
#include <thread>
#include <atomic>
#include <cstdio>
#include <cstdint>
#include <cstdlib>

// Time: how long does it take to ping-pong messages between a pair of threads
// when the 'messages' are spinning reads/writes to std::atomic<uint64_t> with
// acquire and release semantics?
//
// Usage: time ./thread_tennis 24

// (Results seen in 2026: 16.67 million iterations take about 1.2s, or about
//  72 nanoseconds each on a Ryzen 9 5900X.)

struct data {
	std::atomic<uint64_t> data;
	char id;
	char padding[55];
};

void threadLoop(uint64_t iterations, data *input, data *output) {
	printf("%zu iterations, %zx input, %zx output\n", iterations, (size_t)input, (size_t)output);
	uint64_t state = 0;
	char id = input->id;
	for (;;) {
		uint64_t theirValue = input->data.load(std::memory_order_acquire);
		//printf("%c read %zu\n", id, theirValue);
		if (theirValue >= state) {
			//std::this_thread::sleep_for(std::chrono::milliseconds(1000));
			state = theirValue;
			uint64_t myNewValue = state + (id == 'b' ? 1 : 0);
			output->data.store(myNewValue, std::memory_order_release);
			//printf("%c wrote %zu\n", id, myNewValue);
		}
		if (theirValue >= iterations) {
			printf("%c all done\n", id);
			break;
		}
	}
}

int main(int argc, char **argv) {
	printf("yolo %zu\n", sizeof(data));
	uint64_t iterations = 1;
	int logIterations = 1;
	if (argc == 2) {
		logIterations = atoi(argv[1]);
		if (iterations < 63) {
			iterations <<= logIterations;
		}
	}
	printf("running for %ld iterations\n", iterations);
	data pd1, pd2;
	pd1.data.store(0);
	pd1.id = 'a';
	pd2.data.store(0);
	pd2.id = 'b';
	std::thread t1(threadLoop, iterations, &pd1, &pd2);
	std::thread t2(threadLoop, iterations, &pd2, &pd1);
	t1.join();
	t2.join();
	printf("all done\n");
	return 0;
}
